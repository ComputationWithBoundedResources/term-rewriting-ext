-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

-- | Termination problem type, based on WST format.
module Data.Rewriting.Typed.Problem (
    Problem,
    -- * Reexported modules
    module Data.Rewriting.Typed.Problem.Type,
    module Data.Rewriting.Typed.Problem.Parse,
    module Data.Rewriting.Typed.Problem.Pretty,
) where

import           Data.Rewriting.Typed.Problem.Parse
import           Data.Rewriting.Typed.Problem.Pretty
import           Data.Rewriting.Typed.Problem.Type
