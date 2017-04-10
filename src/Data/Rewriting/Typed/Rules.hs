-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

-- | Operations on lists of rules.
--
-- See also "Data.Rewriting.Typed.CriticalPair"
module Data.Rewriting.Typed.Rules (
    -- * Important operations
    fullRewrite,
    -- * Reexported modules
    module Data.Rewriting.Typed.Rules.Rewrite,
    module Data.Rewriting.Typed.Rules.Ops,
) where

import           Data.Rewriting.Typed.Rules.Ops
import           Data.Rewriting.Typed.Rules.Rewrite hiding (listContexts,
                                                     nested)


