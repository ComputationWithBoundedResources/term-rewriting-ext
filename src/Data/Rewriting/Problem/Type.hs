-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christian Sternagel, Manuel Schneckenreither

module Data.Rewriting.Problem.Type (
  StartTerms (..),
  Strategy (..),
  RulesPair (..),
  Problem (..),
  Theory (..),
  allRules
 ) where

import           Data.Rewriting.Datatype.Type  (Datatype)
import           Data.Rewriting.Rule.Type      (Rule (..))
import           Data.Rewriting.Signature.Type (Signature)


data StartTerms = AllTerms
                | BasicTerms deriving (Eq, Show)


data Strategy = Innermost
              | Full
              | Outermost deriving (Eq, Show)


data RulesPair f v = RulesPair { strictRules :: [Rule f v]
                               , weakRules   :: [Rule f v] } deriving (Eq, Show)


data Theory f v = SymbolProperty String [f]
                | Equations [Rule f v] deriving (Eq, Show)


data Problem f v s sDt dt cn = Problem
    { startTerms :: StartTerms
    , strategy   :: Strategy
    , theory     :: Maybe [Theory f v]
    , datatypes  :: Maybe [Datatype dt cn]
    , signatures :: Maybe [Signature s sDt]
    , rules      :: RulesPair f v
    , variables  :: [v]
    , symbols    :: [f]
    , comment    :: Maybe String
    } deriving (Show)


allRules :: RulesPair f v -> [Rule f v]
allRules rp = strictRules rp ++ weakRules rp
