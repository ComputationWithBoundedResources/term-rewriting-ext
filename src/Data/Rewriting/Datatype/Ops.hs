-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Mon Sep 15 09:33:58 2014 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Mon Sep 29 13:39:06 2014 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 26
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
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 3, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth
-- Floor, Boston, MA 02110-1301, USA.
--
--

-- Code:

-- | TODO: comment this module
module Data.Rewriting.Datatype.Ops
    ( getDatatypeWith
    )
    where

import           Data.Rewriting.Datatype.Type
import           Data.Rewriting.Problem.Type

import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe)

getDatatypeWith :: Problem f v s sDt dt cn -> (Datatype dt cn -> Bool) -> Maybe (Datatype dt cn)
getDatatypeWith prob f = find f (fromMaybe [] (datatypes prob))


--
-- Ops.hs ends here
