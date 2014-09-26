-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Manuel Schneckenreither

module Data.Rewriting.Datatype.Pretty
    ( prettyDatatype
    , prettyConstructor
    , prettyConstructorChild
    ) where


import           Data.Rewriting.Datatype.Type

import           Data.List                    (intersperse)
import           Text.PrettyPrint.ANSI.Leijen


prettyDatatype :: Doc -> (dt -> Doc) -> (cn -> Doc) -> Datatype dt cn -> Doc
prettyDatatype arr pDt pCn (Datatype dtName cs) =
    hang 2 $ pDt dtName <+> arr <+>
     text (if isRecursive cs then "µX.<" else "<")
     <+> hcat (intersperse (text ", ") (map pCtr cs)) <+> text ">"
    where
      isRecursive = any (\(Constructor _ ch) -> any (\ctrCh -> case ctrCh of
                                                                  ConstructorRecursive -> True
                                                                  _ -> False
                                                                  ) ch)
      pCtr = prettyConstructor pDt pCn


prettyConstructor :: (dt -> Doc) -> (cn -> Doc) -> Constructor dt cn -> Doc
prettyConstructor _   pCn (Constructor cn []) =
    pCn cn -- <> (case cst of; CostEmpty -> empty; _ -> arr <> prettyCost pC cst)
prettyConstructor pDt pCn (Constructor cn chlds) =
    pCn cn <> text "(" <> hcat (intersperse (text ", ") (map pCChld chlds)) <>
        text ")" -- <> (case cst of; CostEmpty -> empty; _ -> arr <> prettyCost pC cst)
    where
      pCChld = prettyConstructorChild pDt


prettyConstructorChild :: (dt -> Doc) -> ConstructorChild dt -> Doc
prettyConstructorChild _   (ConstructorRecursive)        = text "X"
prettyConstructorChild pDt (ConstructorDatatype dt)   = pDt dt


instance (Pretty dt) => Pretty (ConstructorChild dt) where
    pretty = prettyConstructorChild pretty


instance (Pretty dt, Pretty cn) => Pretty (Constructor dt cn) where
    pretty = prettyConstructor pretty pretty


instance (Pretty dt, Pretty cn) => Pretty (Datatype dt cn) where
    pretty = prettyDatatype (text "=") pretty pretty


