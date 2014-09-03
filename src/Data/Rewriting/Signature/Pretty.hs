-- This file is part of the 'term-rewriting' library-fork. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither


module Data.Rewriting.Signature.Pretty
    ( prettySignature
    ) where


import           Data.Rewriting.Signature.Type
import           Text.PrettyPrint.ANSI.Leijen

import           Data.List                     (intersperse)


prettySignature :: Doc -> Doc -> (f -> Doc) -> (dt -> Doc) -> Signature f dt -> Doc
prettySignature arr0 arr1 pF pDt (Signature r lhs rhs) =
    hang 2 $ pF r <+> arr0 <+> pLhs <+> arr1 <+> pDt rhs
        where
          pLhs = text "[" <> (hcat $ (intersperse (text " x ")) (map pDt lhs)) <> text "]"

instance (Pretty f, Pretty dt) => Pretty (Signature f dt) where
    pretty = prettySignature (text "::") (text "->") pretty pretty
