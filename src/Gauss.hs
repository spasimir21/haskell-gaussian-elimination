module Gauss where

import Data.Maybe (fromJust)
import Data.List (foldl')

import Utils

type GaussianRow = [Float]
type GaussianTable = [GaussianRow]

newtype SimplifiedGaussianTable = SimplifiedGaussianTable GaussianTable

simplifyGaussianTable :: GaussianTable -> SimplifiedGaussianTable
simplifyGaussianTable table = SimplifiedGaussianTable $ mapWithPreviousAndIndex simplifyRow table
  where
    normalizeRow index row = map (rowFactor *) row
      where rowFactor = 1 / fromJust (elementAt index row)
    simplifyRow row prevRows i = normalizeRow i zeroedRow
      where
        zeroRowAt targetRow prevRow j = zipWith (-) targetRow scaledPrevRow
          where
            prevRowFactor = fromJust (elementAt j targetRow)
            scaledPrevRow = map (prevRowFactor *) prevRow
        zeroedRow = foldlWithIndex zeroRowAt row prevRows

toGaussianTable :: SimplifiedGaussianTable -> GaussianTable
toGaussianTable (SimplifiedGaussianTable table) = table

getGaussianVariables :: SimplifiedGaussianTable -> GaussianRow
getGaussianVariables (SimplifiedGaussianTable table) =
  foldrWithIndex calculateVariables (zeros $ length (head table) - 1) table
    where
      calculateVariables row vars i = replace i newVar vars
        where
          terms = zipWith (*) vars row
          newVar = foldl' (-) (last row) terms

solveGaussianElimination :: GaussianTable -> GaussianRow
solveGaussianElimination = getGaussianVariables . simplifyGaussianTable

validateGaussianRow :: GaussianRow -> GaussianRow -> Bool
validateGaussianRow vars row = result `compareFloat` last row
  where
    result = foldl' (+) 0 (zipWith (*) vars row)

validateGaussianTable :: GaussianRow -> GaussianTable -> Bool
validateGaussianTable _ [] = True
validateGaussianTable vars (r:rs) = validateGaussianRow vars r && validateGaussianTable vars rs
