module Main where

import System.Console.ANSI (setTitle)

import Gauss (simplifyGaussianTable, getGaussianVariables, toGaussianTable, validateGaussianTable)
import Output (outputGaussianTable, outputGaussianVariables, outputSolutionStatus)
import Input (inputGaussianTable)
import Terminal (noBuffering)

main :: IO ()
main = do
  noBuffering
  setTitle "Gaussian Elimination"

  putStrLn "=== Gaussian Elimination ==="
  putStrLn ""

  table <- inputGaussianTable
  let 
    simplifiedTable = simplifyGaussianTable table
    vars = getGaussianVariables simplifiedTable
    isValid = validateGaussianTable vars table

  putStrLn ""
  putStrLn "Table:"
  outputGaussianTable table

  putStrLn ""
  putStrLn "Simplified Table:"
  outputGaussianTable $ toGaussianTable simplifiedTable

  putStrLn ""
  putStrLn "Variables:"
  outputGaussianVariables vars

  putStrLn ""
  outputSolutionStatus isValid
