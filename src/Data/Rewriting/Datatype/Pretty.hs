-- This file is part of the 'term-rewriting' library-fork. It is licensed under
-- an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Manuel Schneckenreither

module Data.Rewriting.Datatype.Pretty
    ( prettyDatatype
    , prettyConstructor
    , prettyConstructorChild
    ) where

import           Data.Rewriting.Cost.Pretty   (prettyCost)
import           Data.Rewriting.Cost.Type     (Cost (..))
import           Data.Rewriting.Datatype.Type

import           Data.List                    (intersperse)
import           Text.PrettyPrint.ANSI.Leijen


prettyDatatype :: Doc -> (dt -> Doc) -> (cn -> Doc) -> (c -> Doc) -> Datatype dt cn c -> Doc
prettyDatatype arr pDt pCn pC (Datatype dtName cs) =
    hang 2 $ pDt dtName <+> arr <+>
         (if isRecursive cs
          then (text "µX.< ")
          else (text "< ")) <+>
         (hsep $ (intersperse (text ", ")) (map pCtr cs)) <+> text " >"
    where
      isRecursive cs = True     -- TODO: !!!
      pCtr = prettyConstructor (text ":") pDt pCn pC


prettyConstructor :: Doc -> (dt -> Doc) -> (cn -> Doc) -> (c -> Doc) -> Constructor dt cn c -> Doc
prettyConstructor arr _   pCn pC (Constructor cn [] cst) =
    pCn cn <> (case cst of
                  CostEmpty -> empty
                  _ -> arr <> prettyCost pC cst)
prettyConstructor arr pDt pCn pC  (Constructor cn chlds cst) =
    pCn cn <+> text "(" <+> (hsep $ intersperse (text ", ") (map pCChld chlds)) <+>
        text ")" <> (case cst of
                       CostEmpty -> empty
                       _ -> arr <> prettyCost pC cst)
    where
      pCChld = prettyConstructorChild pDt pC


prettyConstructorChild :: (dt -> Doc) -> (c -> Doc) -> ConstructorChild dt c -> Doc
prettyConstructorChild _   _  (ConstructorRecursive)        = text "X"
prettyConstructorChild pDt _  (ConstructorDatatype dt [])   = pDt dt
prettyConstructorChild pDt pC (ConstructorDatatype dt csts) =
    pDt dt <+> text "(" <+> (hsep $ (intersperse (text ", ") (map pCost csts))) <+> text ")"
        where pCost = prettyCost pC


instance (Pretty dt, Pretty c) => Pretty (ConstructorChild dt c) where
    pretty = prettyConstructorChild pretty pretty


instance (Pretty dt, Pretty cn, Pretty c) => Pretty (Constructor dt cn c) where
    pretty = prettyConstructor (text "=") pretty pretty pretty


instance (Pretty dt, Pretty cn, Pretty c) => Pretty (Datatype dt cn c) where
    pretty = prettyDatatype (text "=") pretty pretty pretty


