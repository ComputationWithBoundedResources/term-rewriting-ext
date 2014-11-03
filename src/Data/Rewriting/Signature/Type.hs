-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither


module Data.Rewriting.Signature.Type
    ( Signature (..)
    ) where


data Signature s dt = Signature
    { lhsRootSym :: s
    , lhsSig     :: [dt]
    , rhsSig     :: dt
    } deriving (Show, Read, Eq)
