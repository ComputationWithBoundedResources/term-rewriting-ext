-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Typed.Rule (
    Rule (..),
    -- * Reexported modules
    module Data.Rewriting.Typed.Rule.Type,
    module Data.Rewriting.Typed.Rule.Ops,
    module Data.Rewriting.Typed.Rule.Pretty,
) where

import           Data.Rewriting.Typed.Rule.Ops
import           Data.Rewriting.Typed.Rule.Pretty
import           Data.Rewriting.Typed.Rule.Type
