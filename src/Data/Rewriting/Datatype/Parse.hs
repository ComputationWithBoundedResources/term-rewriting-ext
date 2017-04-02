-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE CPP #-}
module Data.Rewriting.Datatype.Parse
    ( recursiveSymbol
    , parse
    , parseCtr
    , parseDatatypeName
    ) where

import Text.Parsec hiding (parse)
import Data.Rewriting.Utils.Parse (lex, par, angleBrackets, ident)
import Data.Rewriting.Datatype.Type
import Data.Char (isSpace)
import Control.Monad
import Prelude hiding (lex)


recursiveSymbol :: String
recursiveSymbol = "X"


-- | @parse vs dts@ parses data-types like 'NAT = µX.< 0:0, s(X) >'
-- returning a @Datatype String String Int@ data structure. The list @vs@
-- contains the words which are prohibited for constructor names (e.g. the
-- variable names). The parameter @dts@ holds the already known/parsed
-- data-types.
parse    :: Stream s m Char => [String] ->
           ParsecT s u m (Datatype String String, [String])
parse vs = do
  dt <- lex parseDatatypeName <?> "datatype symbol"
  _ <- (char '=' >> spaces) <?> "="
  r <- (string "µX." <|> string "uX.") <|> return ""
  _ <- spaces
  lst <- angleBrackets (lex (parseCtr (not $ null r) vs) `sepBy` (char ',' >> spaces))
        <?> "constructors defined in between of angle brackets ('<' and '>')"
  let cs = map fst lst
      chk = concatMap snd lst
  return (Datatype dt cs, chk)


-- | @parseDatatypeName@ parses a string, without any character in '@(,)[]@',
-- nor the strings '@->@' and '@::@', nor the recursive symbol.
parseDatatypeName :: Stream s m Char => ParsecT s u m String
parseDatatypeName = ident "(,)[]" ["->", "::"]


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
                  -> ParsecT s u m (Constructor String String, [String])
parseCtr isRec vs = do
  n <- parseCtrSymbol vs
  lst <- par ((spaces >> (parseCtrChRec isRec <|> parseCtrChDt ))
       `sepBy1` (spaces >> lex (char ','))) <|> return []
  let ch = map fst lst
      chk = filter (not . null) (map snd lst)
  -- c <- (char ':' >> parseCost) <|> return
  return (Constructor n ch, chk)


-- | @parseCtrSymbol vs@ is a parser for constructors similar to the conventions
-- of the ancient ASCII input format for terms of the termination competition:
-- every @Char@ that is neither a white space (according to 'Data.Char.isSpace')
-- nor one of '@(@', '@)@', or '@,@', is considered a letter. However, this list
-- of disallowed characters is expanded by adding '@:@', '@<@' and '@>@' to
-- enable manual constructor cost parsing (e.g. constructor 's(X)' with cost '0'
-- can be specified as s(X):0). Furthermore, a constructor identifier is a
-- non-empty sequence of letters and it is treated as variable iff it is not
-- contained in @vs@ (ought to be the list of variables).
parseCtrSymbol :: Stream s m Char => [String] -> ParsecT s u m String
parseCtrSymbol vs = ident "(),>" vs <?> "constructor symbol"
  -- s <- many1 (satisfy (\c -> not (isSpace c) && c `notElem` "(),>")) <|>
  --      string ">("
  -- guard (s `notElem` vs)
  -- return s


-- | @parseCost@ parses an int.
-- parseCost :: Stream s m Char => ParsecT s u m (Cost Int)
-- parseCost = many1 digit >>= (\x -> return $ Cost $ read x)
--             <?> "a cost as an integer, like the '0' in s(X):0"

-- | @parseCtrChCostDt@ parses costs separated by commas (','). It ignores any
-- whitespaces, which are before the cost, before the comma, or after the comma.
-- parseCtrChCostDt :: Stream s m Char => ParsecT s u m [Cost Int]
-- parseCtrChCostDt = (spaces >> parseCost) `sepBy1` (spaces >> lex (char ','))

-- | @parseCtrChldFun isRec dts@ parses the children of a @ConstructorFunction@
-- where isRec specifies if the data-type is recursive, and dts the known
-- data-types.
parseCtrChRec       :: Stream s m Char =>
                      Bool -> ParsecT s u m (ConstructorChild String, String)
parseCtrChRec isRec = if isRec
                      then string recursiveSymbol >> return (ConstructorRecursive, [])
                      else fail "data-type not declared as recursive (µX.< ... >)"


-- | @parseCtrDt dts@ parses a @Constructor String@ where the argument @dts@
-- specifies disallowed data-type names. @isRec@ specifies if the recursive
-- element 'X' is allowed to be parsed.
parseCtrChDt :: Stream s m Char =>
               ParsecT s u m (ConstructorChild String, String)
parseCtrChDt = do
  -- dt <- choice $ map string dts -- where dts are the already parsed data-types

  -- This would need something like foldl for parsec, which does not exits.
  -- Therefore, this check will be done later on. This is the reason for
  -- returning a tuple with the datatype string.

  dt <- ident "()," [recursiveSymbol]
  -- c <- par parseCtrChCostDt <|> return []
  return (ConstructorDatatype dt, dt)


