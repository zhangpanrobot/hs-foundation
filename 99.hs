-- let arr = [11, 2, 9, 6, 40]
--
-- -- 1
-- -- last arr
-- -- 2
-- -- last but one element of a list.
-- -- last $ init arr
-- -- 3
-- -- ind the K'th element of a list. The first element in the list is number 1.
-- elementAt :: (Num a) => a -> [b] -> b
-- elementAt k > length arr = 0
-- elementAt k arr = arr!!k
add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
 square_cps x $ \x_squared ->
 square_cps y $ \y_squared ->
 add_cps x_squared y_squared $ k

thrice :: (a -> a) -> a -> a
-- js thrice
-- function thrice (func, x) {return func(func(func(x)))}
-- thrice(function (a){return a.slice(1)}, a)
thrice f x = f (f (f x))

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
 f_cps x $ \fx ->
 f_cps fx $ \ffx ->
 f_cps ffx $ k
