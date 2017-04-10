-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Bertram Felgenhauer

module Data.Rewriting.Typed.Context (
    Ctxt,
    -- * Important operations
    ofTerm,
    apply,
    -- * Reexported modules
    module Data.Rewriting.Typed.Context.Type,
    module Data.Rewriting.Typed.Context.Ops,
) where

import           Data.Rewriting.Typed.Context.Ops
import           Data.Rewriting.Typed.Context.Type
