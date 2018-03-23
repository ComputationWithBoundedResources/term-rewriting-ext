-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

module Data.Rewriting.Typed.Datatype.Type
    ( module Data.Rewriting.Typed.Term.Type
    , Datatype (..)
    , Constructor (..)
    , ConstructorChild (..)
    , mapDt
    ) where


import           Data.Rewriting.Typed.Term.Type hiding (fold, map)


data Datatype dt cn = Datatype
    { datatype     :: dt
    , constructors :: [Constructor dt cn]
    } deriving (Show, Eq)


data Constructor dt cn = Constructor cn [ConstructorChild dt]
                           deriving (Show, Eq)


data ConstructorChild dt = ConstructorRecursive
                         | ConstructorDatatype dt
                             deriving (Show, Eq)


mapDt :: (dt1 -> dt2) -> (cn1 -> cn2) -> Datatype dt1 cn1 -> Datatype dt2 cn2
mapDt f g (Datatype dt cs) = Datatype (f dt) (fmap mapCtr cs)
  where mapCtr (Constructor cn dts) = Constructor (g cn) (fmap mapCtrChld dts)
        mapCtrChld (ConstructorDatatype dt) = ConstructorDatatype (f dt)
        mapCtrChld ConstructorRecursive     = ConstructorRecursive

