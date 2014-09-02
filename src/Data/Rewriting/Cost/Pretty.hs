-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Manuel Schneckenreither

module Data.Rewriting.Cost.Pretty
    ( prettyCost
    ) where

import           Data.Rewriting.Cost.Type
import           Text.PrettyPrint.ANSI.Leijen


prettyCost :: (Pretty c) => Cost c -> Doc
prettyCost (CostEmpty) = empty
prettyCost (Cost c)    = pretty c


instance (Pretty c) => Pretty (Cost c) where
    pretty = prettyCost


