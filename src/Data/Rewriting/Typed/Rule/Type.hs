-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.Typed.Rule.Type (
    module Data.Rewriting.Typed.Term.Type,
    Rule (..),
) where

import           Data.Rewriting.Typed.Term.Type hiding (fold, map)

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }
    deriving (Ord, Eq, Show)

-- mapRule :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
-- mapRule f r = Rule{ lhs = f (lhs r), rhs = f (rhs r) }

