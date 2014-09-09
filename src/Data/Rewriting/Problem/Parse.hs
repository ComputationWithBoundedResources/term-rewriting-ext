-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christian Sternagel, Manuel Schneckenreither


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Data.Rewriting.Problem.Parse (
  parseIO,
  parseFileIO,
  fromString,
  fromFile,
  fromCharStream,
  ProblemParseError (..)
  ) where

import Data.Rewriting.Utils.Parse (lex, par, ident)
import qualified Data.Rewriting.Problem.Type as Prob
import Data.Rewriting.Problem.Type (Problem)
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Term as Term
import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Datatype as Dt (parse, recursiveSymbol)
import Data.Rewriting.Datatype.Type (Datatype (datatype, constructors), Constructor (..))
import qualified Data.Rewriting.Signature as Sig

import Data.List (partition, union)
import Data.Maybe (isJust, fromJust)
import Prelude hiding (lex, catch)
import Control.Exception (catch)
import Control.Monad.Error
import Control.Monad (liftM, liftM3)
import Text.Parsec hiding (parse)

#ifdef DEBUG
import Debug.Trace (trace)
#endif

data ProblemParseError = UnknownParseError String
                       | UnsupportedStrategy String
                       | FileReadError IOError
                       | UnsupportedDeclaration String
                       | SomeParseError ParseError deriving (Show)

instance Error ProblemParseError where strMsg = UnknownParseError

parseFileIO :: FilePath -> IO (Problem String String String String String Int)
parseFileIO file = do r <- fromFile file
                      case r of
                        Left err -> do { putStrLn "following error occured:"; print err; mzero }
                        Right t  -> return t

parseIO :: String -> IO (Problem String String String String String Int)
parseIO string = case fromString string of
                    Left err -> do { putStrLn "following error occured:"; print err; mzero }
                    Right t  -> return t

fromFile :: FilePath -> IO (Either ProblemParseError (Problem String String String String String Int))
fromFile file = fromFile' `catch` (return . Left . FileReadError) where
  fromFile' = fromCharStream sn `liftM` readFile file
  sn         = "<file " ++ file ++ ">"

fromString :: String -> Either ProblemParseError (Problem String String String String String Int)
fromString = fromCharStream "supplied string"

fromCharStream :: (Stream s (Either ProblemParseError) Char) => SourceName -> s
               -> Either ProblemParseError (Problem String String String String String Int)
fromCharStream sourcename input =
  case runParserT parse initialState sourcename input of
    Right (Left e)  -> Left $ SomeParseError e
    Right (Right p) -> Right p
    Left e          -> Left e
  where initialState = Prob.Problem { Prob.startTerms = Prob.AllTerms ,
                                      Prob.strategy   = Prob.Full ,
                                      Prob.theory     = Nothing ,
                                      Prob.datatypes  = Nothing,
                                      Prob.signatures = Nothing,
                                      Prob.rules      = Prob.RulesPair { Prob.strictRules = [],
                                                                         Prob.weakRules = [] } ,
                                      Prob.variables  = [] ,
                                      Prob.symbols    = [] ,
                                      Prob.comment    = Nothing }


type ParserState = Problem String String String String String Int

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a

modifyProblem :: (Problem String String String String String Int
                     -> Problem String String String String String Int) -> WSTParser s ()
modifyProblem = modifyState

parsedVariables :: WSTParser s [String]
parsedVariables = Prob.variables `liftM` getState

parsedDatatypes :: WSTParser s (Maybe [Datatype String String Int])
parsedDatatypes = Prob.datatypes `liftM` getState


parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Problem String String String String String Int)
parse = spaces >> parseDecls >> eof >> getState where
  parseDecls = many1 (par parseDecl)
  parseDecl =  decl "VAR"        vars        (\ e p -> p {Prob.variables = e `union` Prob.variables p})
           <|> decl "THEORY"     theory      (\ e p -> p {Prob.theory = maybeAppend Prob.theory e p})
           <|> decl "RULES"      rules       (\ e p -> p {Prob.rules   = e, --FIXME multiple RULES blocks?
                                                          Prob.symbols = Rules.funsDL (Prob.allRules e) [] })
           <|> decl "STRATEGY"   strategy    (\ e p -> p {Prob.strategy = e})
           <|> decl "STARTTERM"  startterms  (\ e p -> p {Prob.startTerms = e})
           <|> decl "DATATYPES"  datatypes   (\ e p -> p {Prob.datatypes = maybeAppend Prob.datatypes e p})
           <|> decl "SIGNATURES" signatures  (\ e p -> p {Prob.signatures = maybeAppend Prob.signatures e p})
           <|> (comment >>= modifyProblem . (\ e p -> p {Prob.comment = maybeAppend Prob.comment e p}) <?> "comment")

  decl name p f =
  -- Note: A try here, inhibits useful error messages!

  -- BUG: Furthermore it results in throwing everything which is not properly
  -- parsed to the comment section, if it is not a required block! This may be a
  -- bug and could easily cause one to overlook parse errors. FIX: put 'par' up
  -- to line 100 (many1 (par parseDecl)) and the rest as follows:
      do _ <- try (lex $ string name >> space)
         r <- p -- <?> "Error parsing " ++ name ++ " block"
         modifyProblem $ f r

  maybeAppend fld e p = Just $ maybe [] id (fld p) ++ e


vars :: (Stream s (Either ProblemParseError) Char) => WSTParser s [String]
vars = many (lex $ ident "()," [])


theory :: (Stream s (Either ProblemParseError) Char) => WSTParser s [Prob.Theory String String]
theory = many thdecl where
    thdecl     = par ((equations >>= return . Prob.Equations)
              <|>     (idlist    >>= \ (x:xs) -> return $ Prob.SymbolProperty x xs))
    equations  = try (do
        vs <- parsedVariables
        lex $ string "EQUATIONS"
        many $ equation vs) <?> "EQUATIONS block"
    equation vs = do
        l <- Term.parseWST vs
        lex $ string "=="
        r <- Term.parseWST vs
        return $ Rule l r
    idlist      = many1 $ (lex $ ident "()," [])


datatypes :: (Stream s (Either ProblemParseError) Char) =>
               WSTParser s [Datatype String String Int]
datatypes = do vs <- parsedVariables
               lst <- many (spaces >> Dt.parse vs)
               let dts = map fst lst
                   chk = concatMap snd lst
                   multDecl = checkMultDecl ([],[]) dts
               when (isJust multDecl)
                        (fail $ "Error: " ++ fromJust multDecl)
               ctrDtCheck <- checkCtrDts chk dts -- check data-types used in constructors
               when (or ctrDtCheck)
                    (fail $ "Datatype problem detected.") -- not gonna get thrown!
               return dts <?> "Datatypes"
    where
      -- TODO: check length of costs for ConstructorDatatype

      -- check for multiple constructor symbols across all data-types declarations
      checkMultDecl :: ([String],[String]) -> [Datatype String String Int] -> Maybe String
      checkMultDecl _ []                   = Nothing
      checkMultDecl (dtSs, ctrSs) (d:ds)
          | dtName `elem` dtSs             = Just $ "datatype " ++ dtName ++ " declared more than once."
          | isJust (multCtrs ctrSs nCtrSs) = multCtrs ctrSs nCtrSs
          | otherwise                      = checkMultDecl (dtName : dtSs, nCtrSs ++ ctrSs) ds
          where
            dtName = datatype d
            nCtrSs = map (\(Constructor n _ _) -> n) (constructors d)

            -- Check for multiple constructor symbols inside of one data-type declaration
            multCtrs             :: [String] -> [String] -> Maybe String
            multCtrs _ []        = Nothing
            multCtrs ctrNs (n : ns)
                | n `elem` ctrNs = Just $ "Constructor " ++ n ++ " is defined more than once."
                | otherwise      = multCtrs (n : ctrNs) ns

      checkCtrDts :: Monad m => [String] -> [Datatype String cn c] -> m [Bool]
      checkCtrDts str dts' = mapM (\x -> do
                                     when (x `notElem` dtsStr && x /= Dt.recursiveSymbol)
                                              (fail $ "ERROR: Datatype " ++ show x ++
                                               " not defined but used in constructor!")
                                     return False;
                                  ) str
          where dtsStr = map datatype dts'


signatures :: (Stream s (Either ProblemParseError) Char) => WSTParser s [Sig.Signature String String]
signatures = do mDts <- parsedDatatypes
                case mDts of    -- ensure to fail in Parser Monad, not in [] Monad with fromMaybe!
                  Nothing -> fail $ "expecting non-empty datatypes block (DATATYPES) " ++ --
                             "before signatures block (SIGNATURES). Ensure that" ++
                             " DATATYPES is spelled correctly in input file."
                  Just dts -> do
                            vs <- parsedVariables
                            many (spaces >> Sig.parse vs (map datatype dts))


rules :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Prob.RulesPair String String)
rules = do vs <- parsedVariables
           rs <- many $ rule vs
           let (s,w) = partition fst rs
           return Prob.RulesPair { Prob.strictRules = map snd s ,
                                   Prob.weakRules   = map snd w }
  where rule vs = do l <- Term.parseWST vs
                     sep <- lex $ (try $ string "->=") <|> string "->"
                     r <- Term.parseWST vs
                     return (sep == "->", Rule {lhs = l, rhs = r})

strategy :: (Stream s (Either ProblemParseError) Char) => WSTParser s Prob.Strategy
strategy = innermost <|> outermost where
  innermost = string "INNERMOST" >> return Prob.Innermost
  outermost = string "OUTERMOST" >> return Prob.Outermost

startterms :: (Stream s (Either ProblemParseError) Char) => WSTParser s Prob.StartTerms
startterms = basic <|> terms where
  basic = string "CONSTRUCTOR-BASED" >> return Prob.BasicTerms
  terms = string "FULL" >> return Prob.AllTerms


comment :: (Stream s (Either ProblemParseError) Char) => WSTParser s String
comment = withpars <|> liftM2 (++) idents comment <|> return ""
  where idents = many1 (noneOf "()")
        withpars = do _ <- char '('
                      pre <- comment
                      _ <- char ')'
                      suf <- comment
                      return $ "(" ++ pre ++ ")" ++ suf


