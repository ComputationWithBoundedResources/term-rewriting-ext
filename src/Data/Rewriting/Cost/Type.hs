-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Manuel Schneckenreither

module Data.Rewriting.Cost.Type
    ( Cost(..)
    ) where


data Cost a = CostEmpty | Cost a
            deriving (Show, Eq)
