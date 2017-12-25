{-# LANGUAGE TemplateHaskell #-}

module Th (
        chooseByIndices
) where

import           Control.Monad       (replicateM)
import           Language.Haskell.TH (Exp, Q, lamE, newName, tupE, tupP, varE,
                                      varP)


chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n ids = do
        as <- replicateM n (newName "a")
        lamE [tupP (map varP as)] $ tupE (map (map varE as!!) ids)
