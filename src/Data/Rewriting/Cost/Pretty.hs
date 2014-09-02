-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

module Data.Rewriting.Cost.Pretty
    ( prettyCost
    ) where

import           Data.Rewriting.Cost.Type
import           Text.PrettyPrint.ANSI.Leijen


prettyCost               :: (c -> Doc) -> Cost c -> Doc
prettyCost _ (CostEmpty) = empty
prettyCost pC (Cost c)   = pC c


instance (Pretty c) => Pretty (Cost c) where
    pretty = prettyCost pretty


