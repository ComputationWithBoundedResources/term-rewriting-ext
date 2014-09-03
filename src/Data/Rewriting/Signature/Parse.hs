-- This file is part of the 'term-rewriting' library-fork. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Rewriting.Signature.Parse
    ( parse
    ) where


import Data.Rewriting.Signature.Type
import Data.Rewriting.Utils.Parse (lex, boxBrackets, ident)
import Data.Rewriting.Datatype.Parse (parseDatatypeName)
import Text.Parsec hiding (parse)
import Prelude hiding (lex)
import Data.List (intersperse)

#ifdef DEBUG
import Debug.Trace (trace)
#endif


parse        :: Stream s m Char => [String] -> [String] -> ParsecT s u m (Signature String String)
parse vs dts = do
  n <- parseRuleRootSym vs
  _ <- spaces >> string "::" >> spaces
  lhs <- boxBrackets (parseSigLhs dts)  <|> parseSigLhs dts <|> return []
  _ <- spaces >> string "->" >> spaces
  rhs <- parseSigRhs dts <?> "rhs of signature"
  _ <- spaces
  return $ Signature n lhs rhs


parseRuleRootSym    :: Stream s m Char => [String] -> ParsecT s u m String
parseRuleRootSym vs = ident ":()," vs <?> "data-type symbol"


parseSigLhs :: Stream s m Char => [String] -> ParsecT s u m [String]
parseSigLhs dts = (lex (checkDts dts parseDatatypeName)) `sepBy` (char 'x' >> spaces)


parseSigRhs :: Stream s m Char => [String] -> ParsecT s u m String
parseSigRhs dts = checkDts dts parseDatatypeName


-- | @checkDts dts p@ parses with parser @p@ and then checks if the parsed value
-- is in the list @dts@. If so, it returns the parsed value, otherwise, it
-- throws an error using @fail@.
checkDts       :: Monad m => [String] -> m String -> m String
checkDts dts p = p >>= (\x -> if x `elem` dts then return x
                             else fail $ "expecting any of (case sensitive): "
                                      ++ concat (intersperse ", " dts))


