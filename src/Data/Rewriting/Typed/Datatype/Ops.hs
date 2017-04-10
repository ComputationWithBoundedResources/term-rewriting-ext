-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 09:33:58 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Apr  8 16:27:08 2017 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 29
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--

--
--

-- Code:

-- | TODO: comment this module
module Data.Rewriting.Typed.Datatype.Ops
    ( getDatatypeWith
    )
    where

import           Data.Rewriting.Typed.Datatype.Type
import           Data.Rewriting.Typed.Problem.Type

import           Data.List                          (find)
import           Data.Maybe                         (fromMaybe)

getDatatypeWith :: Problem f v s sDt dt cn -> (Datatype dt cn -> Bool) -> Maybe (Datatype dt cn)
getDatatypeWith prob f = find f (fromMaybe [] (datatypes prob))


--
-- Ops.hs ends here
