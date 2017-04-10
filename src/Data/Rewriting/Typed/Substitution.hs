-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Typed.Substitution (
    GSubst,
    Subst,
    -- * Important operations
    gApply,
    apply,
    compose,
    -- * Reexported modules
    module Data.Rewriting.Typed.Substitution.Type,
    module Data.Rewriting.Typed.Substitution.Ops,
    module Data.Rewriting.Typed.Substitution.Match,
    module Data.Rewriting.Typed.Substitution.Unify,
    module Data.Rewriting.Typed.Substitution.Pretty,
    module Data.Rewriting.Typed.Substitution.Parse,
) where

import           Data.Rewriting.Typed.Substitution.Match
import           Data.Rewriting.Typed.Substitution.Ops
import           Data.Rewriting.Typed.Substitution.Parse
import           Data.Rewriting.Typed.Substitution.Pretty
import           Data.Rewriting.Typed.Substitution.Type   hiding (fromMap,
                                                           toMap)
import           Data.Rewriting.Typed.Substitution.Unify
