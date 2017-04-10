-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Typed.Term (
    Term (..),
    -- * Important operations
    fold, map, vars, funs,
    -- * Reexported modules
    module Data.Rewriting.Typed.Term.Type,
    module Data.Rewriting.Typed.Term.Ops,
    module Data.Rewriting.Typed.Term.Pretty,
    module Data.Rewriting.Typed.Term.Parse
) where

import           Data.Rewriting.Typed.Term.Ops
import           Data.Rewriting.Typed.Term.Parse
import           Data.Rewriting.Typed.Term.Pretty
import           Data.Rewriting.Typed.Term.Type
import           Prelude                          ()
