-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.Typed.CriticalPair (
    CP,
    -- * Important operations
    cps',
    cps,
    -- * Reexported modules
    module Data.Rewriting.Typed.CriticalPair.Type,
    module Data.Rewriting.Typed.CriticalPair.Ops,
) where

import           Data.Rewriting.Typed.CriticalPair.Ops
import           Data.Rewriting.Typed.CriticalPair.Type
