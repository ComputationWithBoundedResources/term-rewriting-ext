-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Manuel Schneckenreither

module Data.Rewriting.Datatype.Pretty
    ( prettyConstructor
    ) where

import           Data.Rewriting.Cost.Pretty   (prettyCost)
import           Data.Rewriting.Cost.Type     (Cost (..))
import           Data.Rewriting.Datatype.Type
import           Data.Rewriting.Term          (prettyTerm)

import           Data.List                    (intersperse)
import           Text.PrettyPrint.ANSI.Leijen


prettyDatatype :: (Pretty dt, Pretty cn, Pretty c) => Doc -> (dt -> Doc) ->
                 (cn -> Doc) -> (c -> Doc) -> Datatype dt cn c -> Doc
prettyDatatype arr dts cns cst (Datatype n cs) =
    hang 2 $ pretty n <+> arr <+>
         (if isRecursive cs
          then (text "µX.< ")
          else (text "< ")) <+>
         (hsep $ (intersperse (text ", ")) (map ctr cs)) <+> text " >"
    where
      isRecursive cs = True     -- TODO: !!!
      ctr = prettyConstructor (text ":")


prettyConstructor :: (Pretty dt, Pretty cn, Pretty c) => Doc -> (ConstructorChild dt cn -> Doc) ->
                    (cn -> Doc) -> (Cost c -> Doc) -> Constructor dt cn c -> Doc
prettyConstructor arr pDt pCn pC (Constructor cn [] cst) =
    pCn cn <+> arr <+> pC cst
prettyConstructor arr pDt pCn pC (Constructor cn chlds csts) =
    pCn cn <+> text "(" <+> (hsep $ intersperse (text ", ") (map pCn chlds)
)

prettyConstructorChild :: (Pretty dt, Pretty c) => ConstructorChild dt c -> Doc
prettyConstructorChild (ConstructorRecursive)        = text "X"
prettyConstructorChild (ConstructorDatatype dt [])   = pretty dt
prettyConstructorChild (ConstructorDatatype dt csts) =
    pretty dt <+> text "(" <+> (hsep $ (intersperse (text ", ") (map pretty csts))) <+> text ")"


-- prettyConstructor arr fun var (Constructor d c) =
--     hang 2 $ (text d) <+> arr <+> (text "µX.<") <+>
--          (hsep $ (intersperse (text ", ")) (map term c)) <+> (text ">")
--              where
--                term = prettyTerm fun var

instance (Pretty dt, Pretty c) => Pretty (ConstructorChild dt c) where
    pretty = prettyConstructorChild


instance (Pretty dt, Pretty cn, Pretty c) => Pretty (Constructor dt cn c) where
    pretty = prettyConstructor (text "=")


instance (Pretty dt, Pretty cn, Pretty c) => Pretty (Datatype dt cn c) where
    pretty = prettyDatatype (text " = ")
