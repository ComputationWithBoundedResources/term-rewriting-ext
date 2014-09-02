-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

module Data.Rewriting.Datatype.Type
    ( module Data.Rewriting.Term.Type
    , Datatype (..)
    , Constructor (..)
    , ConstructorChild (..)
    ) where

import           Data.Rewriting.Cost.Type
import           Data.Rewriting.Term.Type hiding (fold, map)


data Datatype dt cn c = Datatype
    { datatype :: dt
    , ctrs     :: [Constructor dt cn c]
    } deriving (Show, Eq)


data Constructor dt cn c = Constructor cn [ConstructorChild dt c] (Cost c)
                          deriving (Show, Eq)


data ConstructorChild dt c = ConstructorRecursive
                           | ConstructorDatatype dt [Cost c]
                             deriving (Show, Eq)
