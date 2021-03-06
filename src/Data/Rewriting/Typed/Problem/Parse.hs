-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christian Sternagel, Manuel Schneckenreither


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Data.Rewriting.Typed.Problem.Parse (
  parseIO,
  parseFileIO,
  fromString,
  fromFile,
  fromCharStream,
  -- ProblemParseError (..)
  ) where

import Data.Rewriting.Typed.Utils.Parse (lex, par, ident)
import qualified Data.Rewriting.Typed.Problem.Type as Prob
import Data.Rewriting.Typed.Problem.Type (Problem)
import Data.Rewriting.Typed.Rule (Rule (..))
import qualified Data.Rewriting.Typed.Term as Term
import qualified Data.Rewriting.Typed.Rules as Rules
import qualified Data.Rewriting.Typed.Datatype as Dt (parse, recursiveSymbol)
import Data.Rewriting.Typed.Datatype.Type (Datatype (datatype, constructors), Constructor (..))
import qualified Data.Rewriting.Typed.Signature as Sig

#ifdef DEBUG
import Debug.Trace (trace)
#endif

import Data.List (partition, union, find)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Prelude hiding (lex)
import Control.Exception (toException, Exception, SomeException, throwIO, catch)
import Control.Monad (mzero, liftM2, when, liftM)
import Text.Parsec hiding (parse)

data ProblemParseError = UnknownParseError String
                       | UnsupportedStrategy String
                       | FileReadError IOError
                       | UnsupportedDeclaration String
                       | SomeParseError ParseError deriving (Show)

instance Exception ProblemParseError

parseFileIO :: FilePath -> IO (Problem String String String String String String)
parseFileIO file = do r <- fromFile file
                      case r of
                        Left err -> fail (show err)
                        Right t  -> return t

parseIO :: String -> IO (Problem String String String String String String)
parseIO string = case fromString string of
                    Left err -> fail (show err)
                    Right t  -> return t

fromFile :: FilePath -> IO (Either ProblemParseError (Problem String String String String String String))
fromFile file = fromFile' `catch` (return . Left . FileReadError) where
  fromFile' = fromCharStream sn `liftM` readFile file
  sn         = "<file " ++ file ++ ">"

fromString :: String -> Either ProblemParseError (Problem String String String String String String)
fromString = fromCharStream "supplied string"

fromCharStream :: (Stream s (Either ProblemParseError) Char) => SourceName -> s
               -> Either ProblemParseError (Problem String String String String String String)
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


type ParserState = Problem String String String String String String

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a

modifyProblem :: (Problem String String String String String String
                     -> Problem String String String String String String) -> WSTParser s ()
modifyProblem = modifyState

parsedVariables :: WSTParser s [String]
parsedVariables = Prob.variables `liftM` getState

parsedDatatypes :: WSTParser s (Maybe [Datatype String String])
parsedDatatypes = Prob.datatypes `liftM` getState

parsedSignatures :: WSTParser s (Maybe [Sig.Signature String String])
parsedSignatures = Prob.signatures `liftM` getState

parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Problem String String String String String String)
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

  maybeAppend fld e p = Just $ fromMaybe [] (fld p) ++ e


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
               WSTParser s [Datatype String String]
datatypes = do vs <- parsedVariables
               lst <- many (spaces >> Dt.parse vs)
               let dts = map fst lst
                   chk = concatMap snd lst
                   multDecl = checkMultDecl ([],[]) dts
               when (isJust multDecl)
                        (fail $ "Error: " ++ fromJust multDecl)
               ctrDtCheck <- checkCtrDts chk dts -- check data-types used in constructors
               when (or ctrDtCheck)             -- needed to ensure evaluation.
                    (fail "Datatype problem detected.") -- not gonna get thrown!
               return dts <?> "Datatypes"
    where
      -- check for multiple constructor symbols across all data-types declarations
      checkMultDecl :: ([String],[String]) -> [Datatype String String] -> Maybe String
      checkMultDecl _ []                   = Nothing
      checkMultDecl (dtSs, ctrSs) (d:ds)
          | dtName `elem` dtSs             = Just $ "datatype " ++ dtName ++ " declared more than once."
          -- | isJust (multCtrs ctrSs nCtrSs) = multCtrs ctrSs nCtrSs
          | otherwise                      = checkMultDecl (dtName : dtSs, nCtrSs ++ ctrSs) ds
          where
            dtName = datatype d
            nCtrSs = map (\(Constructor n _) -> n) (constructors d)

            -- Check for multiple constructor symbols inside of one data-type declaration
            multCtrs             :: [String] -> [String] -> Maybe String
            multCtrs _ []        = Nothing
            multCtrs ctrNs (n : ns)
                | n `elem` ctrNs = Just $ "Constructor " ++ n ++ " is defined more than once."
                | otherwise      = multCtrs (n : ctrNs) ns

      checkCtrDts :: MonadFail m => [String] -> [Datatype String cn] -> m [Bool]
      checkCtrDts str dts' = mapM (\x -> do
                                     when (x `notElem` dtsStr && x /= Dt.recursiveSymbol)
                                              (fail $ "ERROR: Datatype " ++ show x ++
                                               " not defined but used in constructor!")
                                     return False;
                                  ) str
          where dtsStr = map datatype dts'


signatures :: (Stream s (Either ProblemParseError) Char) => WSTParser s [Sig.Signature String String]
signatures = do mDts <- parsedDatatypes
                sigs <- parsedSignatures
                case mDts of    -- ensure to fail in Parser Monad, not in [] Monad with fromMaybe!
                  Nothing -> fail $ "expecting non-empty datatypes block (DATATYPES) " ++ --
                             "before signatures block (SIGNATURES). Ensure that" ++
                             " DATATYPES is spelled correctly in the input file."
                  Just dts -> do
                             vs <- parsedVariables
                             nSig <- many (spaces >> Sig.parse vs (map datatype dts))
                             let allSigs = nSig ++ fromMaybe [] sigs
                             if and (checkMultSigs allSigs)
                                then return nSig
                                else fail $ "Multiple signature declarations of the same function"
                                       ++" are not allowed. Function name: "
                                       ++ show (Sig.lhsRootSym $
                                                allSigs !! (length (checkMultSigs allSigs) - 1))

  where checkMultSigs :: [Sig.Signature String String] -> [Bool]
        checkMultSigs [] = [True]
        checkMultSigs s = fst (foldl fun ([], tail s) s)

        fun (acc,[]) _ = (acc, [])
        fun (acc,sigs') (Sig.Signature s _ _) =
          if isNothing (find (\x -> Sig.lhsRootSym x == s) sigs')
              then (True : acc,tail sigs')
              else (False : acc, [])


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


