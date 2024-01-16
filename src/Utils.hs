module Utils where

import GHC.Float (powerFloat)

elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt 0 (x:_) = Just x
elementAt i (_:xs) =
  if i < 0 then Nothing else elementAt (i - 1) xs

mapWithPreviousAndIndex :: (a -> [a] -> Int -> a) -> [a] -> [a]
mapWithPreviousAndIndex f l = mapInternal l [] 0
  where
    mapInternal [] _ _ = []
    mapInternal (x:xs) prev i = 
      let z = f x prev i
          xs' = mapInternal xs (prev ++ [z]) (i + 1)
      in z:xs'

foldlWithIndex :: (b -> a -> Int -> b) -> b -> [a] -> b
foldlWithIndex f initialAcc l = foldlInternal initialAcc l 0
  where
    foldlInternal acc [] _ = acc
    foldlInternal acc (x:xs) index =
      let z = f acc x index
      in z `seq` foldlInternal z xs (index + 1)

foldrWithIndex :: (a -> b -> Int -> b) -> b -> [a] -> b
foldrWithIndex f initialAcc l = foldrInternal initialAcc l 0
  where
    foldrInternal acc [] _ = acc
    foldrInternal acc [x] i = f x acc i
    foldrInternal acc (x:xs) index =
      let z = foldrInternal acc xs (index + 1)
      in z `seq` f x z index

replace :: Int -> a -> [a] -> [a]
replace ti v l = replaceInternal l 0
  where
    replaceInternal [] _ = []
    replaceInternal (x:xs) i =
      if i == ti then v : xs
      else x : replaceInternal xs (i + 1)

arr :: a -> Int -> [a]
arr _ 0 = []
arr v l = v : arr v (l - 1)

zeros :: Int -> [Float]
zeros = arr 0

indent :: Int -> String
indent = arr ' '

compareFloatP :: Float -> Float -> Float -> Bool
compareFloatP p a b = abs (a - b) <= t
  where
    t = 10 `powerFloat` (-p)

compareFloat :: Float -> Float -> Bool
compareFloat = compareFloatP 5
