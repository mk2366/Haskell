{-# LANGUAGE NoImplicitPrelude #-}

module ShowTree where

import Prelude(String, length)
import Text.PrettyPrint.Leijen
    

data Tree = Node String [Tree]

showTree (Node s ts) = text s <> nest (length s) (showList ts)

showList [] = text "[]"
showList ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees [t]     = showTree t
showTrees (t:ts)  = showTree t <> text "," <> line <> showTrees ts