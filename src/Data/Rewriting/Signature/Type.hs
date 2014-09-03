-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither


module Data.Rewriting.Signature.Type
    ( Signature (..)
    ) where


data Signature f dt = Signature
    { lhsRootSym :: f
    , lhsSig     :: [dt]
    , rhsSig     :: dt
    } deriving (Show, Eq)
