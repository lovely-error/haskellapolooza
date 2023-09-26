import Data.List


infixl ///

class LaxDivision a where
  (///) :: a -> a -> a

instance LaxDivision Int where
  (///) = div
-- instance Fractional t => LaxDivision t where
--   (///) = (/)
instance LaxDivision Float where
  (///) = (/)
instance LaxDivision Double where
  (///) = (/)

-- iteratedMedian :: [Float] -> Float
iteratedMedian i = aux i 1
  where
    aux (v : []) k = v /// k
    aux k ic =
      let len = (length k)
          half = div len 2
          sorted = sort k
          mean = (foldl (+) 0 sorted) /// (fromIntegral $ len)
          (l, r) = splitAt half sorted
          uneven = mod len 2 /= 0
          (l_, r_) = if uneven then (l ++ [0], r) else (l, r)
          zipp = map (\(a, b) -> a + b)(zip l_ r_)
      in aux zipp (ic + 1)


fil i k =
  let len = length i
      fil = sort i
      bound = case k of
        Just n -> (foldl (+) 0 i) /// n
        Nothing -> iteratedMedian i
      (l, r) = splitAt (len `div` 2) fil
      (l_ , r_) = (l, reverse r)
  in aux l r 0 bound [] [] True
  where
    aux r k@(b:bt) lim bound sub res mod
      | lim + b > bound && mod = aux k r lim bound sub res False
      | lim > bound && (not mod) = aux k r 0 bound [] (sub : res) True
      | otherwise = aux r bt (lim + b) bound (b : sub) res mod
    aux r@(_:_) [] lim bound sub res mod = aux [] r lim bound sub res mod
    aux [] [] _ _ sub@(_:_) res _ = sub : res
    aux [] [] _ _ [] res _ = res




main = do
  let example = cycle [4, 7, 4, 12, 3, 4, 17, 21, 4, 4, 8, 7, 3, 21, 8, 29, 2, 17, 10, 35, 18, 9, 10, 8, 18, 16, 18, 36, 5, 30, 9, 78, 18, 32, 23, 2, 18, 8, 27, 24, 35, 6, 3, 17, 17, 36, 16, 2, 6, 5, 3, 14, 9, 8, 4, 13, 17, 9] :: [Int]
  let (a, b) = (take 300 example , take 10000 example)
  -- print a
  putStrLn "\n"
  let res = fil a (Just 4)
  let k =  (map (\ i -> foldl (+) 0 i) res)
  let l = (foldl (+) 0 k) /// fromIntegral (length k)
  print $ (l , res)
  let t = map (foldl (+) 0) res
  print t