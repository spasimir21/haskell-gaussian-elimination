module Output where

import System.Console.ANSI (setSGR, SGR(SetColor, Reset), ConsoleLayer(Foreground), Color(Red, Green), ColorIntensity(Vivid))
import Text.Printf (printf)
import Data.List (foldl')

import Gauss (GaussianTable, GaussianRow)

showFloat :: Float -> String
showFloat 0 = " 0.00"
showFloat x = prefix ++ printf "%.2f" x
  where
    prefix = if x >= 0 then " " else ""

formatGaussianRow :: GaussianRow -> String
formatGaussianRow row = variables ++ "| " ++ showFloat (last row)
  where
    variables = foldl' (\acc x -> acc ++ showFloat x ++ " ") "" (init row)

outputGaussianTable :: GaussianTable -> IO ()
outputGaussianTable = outputInternal
  where
    outputInternal [] = return ()
    outputInternal (r:rs) = do
      putStrLn $ formatGaussianRow r
      outputInternal rs

outputGaussianVariables :: GaussianRow -> IO ()
outputGaussianVariables row = outputInternal row (1 :: Int)
  where
    outputInternal [] _ = return ()
    outputInternal (v:vs) i = do
      putStrLn $ "x" ++ show i ++ " = " ++ showFloat v
      outputInternal vs (i + 1)

outputSolutionStatus :: Bool -> IO ()
outputSolutionStatus isValid = do
  let message = if isValid then "The solution is valid!" else "The solution appears to be incorrect!"
      sgr = if isValid then [SetColor Foreground Vivid Green] else [SetColor Foreground Vivid Red]
  setSGR sgr
  putStrLn message
  setSGR [Reset]
