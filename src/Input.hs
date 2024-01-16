module Input where

import Prelude hiding (error)

import Gauss (GaussianRow, GaussianTable)
import Utils

input :: (Read a) => Int -> String -> (a -> Maybe String) -> IO a
input indents inputName getError = do
  putStr $ indent indents ++ "Enter " ++ inputName ++ ": "
  rawInput <- getLine
  let value = read rawInput
      error = getError value
  maybe (return value) fail error

inputNaturalNumber :: String -> IO Int
inputNaturalNumber inputName = input 0 inputName (\x -> 
  if x > 0 then Nothing
  else Just $ inputName ++ " must be a natural number!")

inputGaussianRow :: Int -> IO GaussianRow
inputGaussianRow variableCount = inputRowInternal 0
  where
    inputRowInternal i = do
      let (inputName, continue) = if i == variableCount then ("Result", False)
                                else (('x' : show (i + 1)) ++ " Term", True)
      var :: Float <- input 2 inputName (const Nothing)
      if continue then do
        other <- inputRowInternal (i + 1)
        return $ var : other
      else return [var]

inputGaussianTable :: IO GaussianTable
inputGaussianTable = do
  variableCount <- inputNaturalNumber "Variable Count"
  equationCount <- inputNaturalNumber "Equation Count"
  inputTableInternal 1 variableCount equationCount
    where
      inputTableInternal :: Int -> Int -> Int -> IO GaussianTable
      inputTableInternal i variableCount equationCount = do
        putStrLn ""
        putStrLn $ "Equation #" ++ show i ++ ":"
        row <- inputGaussianRow variableCount
        if i == equationCount then return [row]
        else do
          table <- inputTableInternal (i + 1) variableCount equationCount
          return $ row : table
