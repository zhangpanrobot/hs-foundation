let arr = [11, 2, 9, 6, 40]

-- 1
last arr
-- 2
-- last but one element of a list.
last $ take (length arr - 1) arr
-- 3
-- ind the K'th element of a list. The first element in the list is number 1.
elementAt :: (Num a) => a -> [b] -> b
elementAt