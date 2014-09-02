-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christian Sternagel, Manuel Schneckenreither


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Data.Rewriting.Problem.Parse (
  parseIO,
  parseFileIO,
  fromString,
  fromFile,
  fromCharStream,
  ProblemParseError (..)
  ) where

import Data.Rewriting.Utils.Parse (lex, par, angleBrackets, ident)
import qualified Data.Rewriting.Problem.Type as Prob
import Data.Rewriting.Problem.Type (Problem)
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Term as Term
import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Datatype.Type as Dt
import Data.Rewriting.Datatype.Parse (parseDatatype)
import Data.Rewriting.Datatype.Type (Datatype (..))


import Data.List (partition, union)
import Data.Maybe (isJust)
import Prelude hiding (lex, catch)
import Control.Exception (catch)
import Control.Monad.Error
import Control.Monad (liftM, liftM3)
import Text.Parsec hiding (parse)
import System.IO (readFile)
#ifdef DEBUG
import Debug.Trace (trace)
#endif

data ProblemParseError = UnknownParseError String
                       | UnsupportedStrategy String
                       | FileReadError IOError
                       | UnsupportedDeclaration String
                       | SomeParseError ParseError deriving (Show)

instance Error ProblemParseError where strMsg = UnknownParseError

parseFileIO :: FilePath -> IO (Problem String String String String Int)
parseFileIO file = do r <- fromFile file
                      case r of
                        Left err -> do { putStrLn "following error occured:"; print err; mzero }
                        Right t  -> return t

parseIO :: String -> IO (Problem String String String String Int)
parseIO string = case fromString string of
                    Left err -> do { putStrLn "following error occured:"; print err; mzero }
                    Right t  -> return t

fromFile :: FilePath -> IO (Either ProblemParseError (Problem String String String String Int))
fromFile file = fromFile' `catch` (return . Left . FileReadError) where
  fromFile' = fromCharStream sn `liftM` readFile file
  sn         = "<file " ++ file ++ ">"

fromString :: String -> Either ProblemParseError (Problem String String String String Int)
fromString = fromCharStream "supplied string"

fromCharStream :: (Stream s (Either ProblemParseError) Char) => SourceName -> s
               -> Either ProblemParseError (Problem String String String String Int)
fromCharStream sourcename input =
  case runParserT parse initialState sourcename input of
    Right (Left e)  -> Left $ SomeParseError e
    Right (Right p) -> Right p
    Left e          -> Left e
  where initialState = Prob.Problem { Prob.startTerms = Prob.AllTerms ,
                                      Prob.strategy   = Prob.Full ,
                                      Prob.theory     = Nothing ,
                                      Prob.datatypes  = Nothing,
                                      Prob.rules      = Prob.RulesPair { Prob.strictRules = [],
                                                                         Prob.weakRules = [] } ,
                                      Prob.variables  = [] ,
                                      Prob.symbols    = [] ,
                                      Prob.comment    = Nothing }


type ParserState = Problem String String String String Int

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a

modifyProblem :: (Problem String String String String Int
                     -> Problem String String String String Int) -> WSTParser s ()
modifyProblem = modifyState

parsedVariables :: WSTParser s [String]
parsedVariables = Prob.variables `liftM` getState

parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Problem String String String String Int)
parse = spaces >> parseDecls >> eof >> getState where
  parseDecls = many1 parseDecl
  parseDecl =  decl "VAR"       vars       (\ e p -> p {Prob.variables = e `union` Prob.variables p})
           <|> decl "THEORY"    theory     (\ e p -> p {Prob.theory = maybeAppend Prob.theory e p})
           <|> decl "RULES"     rules      (\ e p -> p {Prob.rules   = e, --FIXME multiple RULES blocks?
                                                        Prob.symbols = Rules.funsDL (Prob.allRules e) [] })
           <|> decl "STRATEGY"  strategy   (\ e p -> p {Prob.strategy = e})
           <|> decl "DATATYPES"  datatypes    (\ e p -> p {Prob.datatypes = maybeAppend Prob.datatypes e p})
           <|> decl "STARTTERM" startterms (\ e p -> p {Prob.startTerms = e})
           -- <|> (par comment >>= modifyProblem . (\ e p -> p {Prob.comment = maybeAppend Prob.comment e p}) <?> "comment")
  decl name p f = try (par $ do
      lex $ string name
      r <- p
      modifyProblem $ f r) <?> (name ++ " block")
  maybeAppend fld e p = Just $ maybe [] id (fld p) ++ e

vars :: (Stream s (Either ProblemParseError) Char) => WSTParser s [String]
vars = do vs <- many (lex $ ident "()," [])
          return vs


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
               WSTParser s [Dt.Datatype String String Int]
datatypes = do vs <- parsedVariables
               lst <- many (try (spaces >> parseDatatype vs))
               let dts = map fst lst
                   chk = concatMap snd lst
               when (checkDts chk dts)
                    (undefined)
               return dts

    where
      checkDts          :: (Eq a, Show a) => [a] -> [Datatype a b c] -> Bool
      checkDts str dts' = any (\x -> if x `elem` dtsStr
                                    then False
                                    else error $ "Datatype " ++ show x ++ "not defined"
                              ) (str)
          where
            dtsStr = map datatype dts'


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


