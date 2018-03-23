-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither


module Data.Rewriting.Typed.Signature.Type
    ( Signature (..)
    , mapSig
    ) where


data Signature s dt = Signature
    { lhsRootSym :: s
    , lhsSig     :: [dt]
    , rhsSig     :: dt
    } deriving (Show, Read, Eq)


mapSig :: (s -> s') -> (dt -> dt') -> Signature s dt -> Signature s' dt'
mapSig f g (Signature root lhs rhs) = Signature (f root) (fmap g lhs) (g rhs)
