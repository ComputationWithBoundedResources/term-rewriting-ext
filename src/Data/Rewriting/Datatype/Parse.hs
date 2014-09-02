-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Manuel Schneckenreither

{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE CPP #-}
module Data.Rewriting.Datatype.Parse
    ( parseDatatype
    , parseCtr
    ) where

import Text.Parsec hiding (parse)
import Data.Rewriting.Term.Type hiding (map)
import Data.Rewriting.Utils.Parse (lex, par, angleBrackets, ident)
import Data.Rewriting.Datatype.Type
import Data.Rewriting.Term.Parse
import Control.Monad
import Data.Rewriting.Cost.Type
import Prelude hiding (lex)

#ifdef DEBUG
import Debug.Trace (trace)
#endif

recursiveSymbol :: String
recursiveSymbol = "X"


-- | @parseDatatype vs dts@ parses data-types like 'NAT = µX.< 0:0, s(X) >'
-- returning a @Datatype String String Int@ data structure. The list @vs@
-- contains the words which are prohibited for constructor names (e.g. the
-- variable names). The parameter @dts@ holds the already known/parsed
-- data-types.
--
-- TODO: maybe change parseCtrChDt such that it parses any String,
-- and check later on!
parseDatatype    :: Stream s m Char => [String] ->
                   ParsecT s u m (Datatype String String Int, [String])
parseDatatype vs = do
  dt <- lex parseDatatypeName
  _ <- (char '=' >> spaces)
  r <- (string "µX." <|> string "uX.") <|> return ""
  lst <- angleBrackets ((lex $ parseCtr (not $ null r) vs) `sepBy` (char ',' >> spaces))
  let cs = map fst lst
      chk = concatMap snd lst
  return $ (Datatype dt cs, chk)


-- | @parseDatatypeName@ parses a string.
parseDatatypeName :: Stream s m Char => ParsecT s u m String
parseDatatypeName = ident "" []


-- | @parseCtr isRec vs@ is a parser for constructors similar to the conventions
-- of the ancient ASCII input format for terms of the termination competition:
-- every @Char@ that is neither a white space (according to 'Data.Char.isSpace')
-- nor one of '@(@', '@)@', or '@,@', is considered a letter. However, this list
-- of disallowed characters is expanded by adding '@:@' to enable manual
-- constructor cost parsing (e.g. constructor 's(X)' with cost '0' can be
-- specified as s(X):0). If @isRec@ is True, the constructor can contain the
-- recursive element 'X'. Furthermore, a constructor identifier is a non-empty
-- sequence of letters and it is treated as variable iff it is not contained in
-- @vs@ (ought to be the list of variables). Adding to this, the cost for the
-- constructor may not be specified (e.g. by just specifying 's(X)'). However,
-- if costs are specified.
parseCtr          :: Stream s m Char => Bool -> [String]
                  -> ParsecT s u m (Constructor String String Int, [String])
parseCtr isRec vs = do
  n <- parseCtrSymbol vs
  lst <- (par ((spaces >> (parseCtrChDt <|> parseCtrChRec isRec)))
       `sepBy1` (spaces >> lex (char ','))) <|> return []
  let ch = map fst lst
      chk = map snd lst
  c <- (char ':' >> parseCost) <|> return CostEmpty
  return $ (Constructor n ch c, chk)


-- | @parseCtrSymbol vs@ is a parser for constructors similar to the conventions
-- of the ancient ASCII input format for terms of the termination competition:
-- every @Char@ that is neither a white space (according to 'Data.Char.isSpace')
-- nor one of '@(@', '@)@', or '@,@', is considered a letter. However, this list
-- of disallowed characters is expanded by adding '@:@' to enable manual
-- constructor cost parsing (e.g. constructor 's(X)' with cost '0' can be
-- specified as s(X):0). Furthermore, a constructor identifier is a non-empty
-- sequence of letters and it is treated as variable iff it is not contained in
-- @vs@ (ought to be the list of variables).
parseCtrSymbol :: Stream s m Char => [String] -> ParsecT s u m String
parseCtrSymbol vs = ident ":()," vs <?> "constructor symbol"

-- | @parseCost@ parses an int.
parseCost :: Stream s m Char => ParsecT s u m (Cost Int)
parseCost = many1 digit >>= (\x -> return $ Cost $ read x)
            <?> "a cost as an integer, like the '0' in s(X):0"

-- | @parseCtrChCostDt@ parses costs separated by commas (','). It ignores any
-- whitespaces, which are before the cost, before the comma, or after the comma.
parseCtrChCostDt :: Stream s m Char => ParsecT s u m [Cost Int]
parseCtrChCostDt = (spaces >> parseCost) `sepBy1` (spaces >> lex (char ','))

-- | @parseCtrChldFun isRec dts@ parses the children of a @ConstructorFunction@
-- where isRec specifies if the data-type is recursive, and dts the known
-- data-types.
parseCtrChRec       :: Stream s m Char =>
                      Bool -> ParsecT s u m (ConstructorChild String Int, String)
parseCtrChRec isRec = if isRec then string recursiveSymbol >> return (ConstructorRecursive, [])
                      else fail "constructor no recursive (µX.<...>)"


-- | @parseCtrDt dts@ parses a @Constructor String@ where the argument @dts@
-- specifies disallowed data-type names. @isRec@ specifies if the recursive
-- element 'X' is allowed to be parsed.
parseCtrChDt :: Stream s m Char =>
               ParsecT s u m (ConstructorChild String Int, String)
parseCtrChDt = do
  -- dt <- choice $ map string dts

  -- This would need something like foldl for parsec, which does not exits.
  -- Therefore, this check will be done later on. Therefore returning a tuple.

  dt <- ident "()," []
  c <- par parseCtrChCostDt <|> return []
  return $ (ConstructorDatatype dt c, dt)


