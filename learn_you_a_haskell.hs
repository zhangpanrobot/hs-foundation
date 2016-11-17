maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0  = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a==x = True
    | otherwise = elem' a xs

-- quick sort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smallerSort = quickSort [a | a <- xs, a <= x]
      biggerSort = quickSort [a | a <- xs, a > x]
  in smallerSort ++ [x] ++ biggerSort

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

-- add together current item of tow list
sumList :: (Num a) => [a] -> [a] -> [a]
-- TODO: add parttern matching
-- sumList _ [] = _
-- sumList [] _ = _
sumList (x:xs) (y:ys) = map (\(a, b) -> a + b) $ zip (x:xs) (y:ys)
