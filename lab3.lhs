> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f z [] = z
> myFoldl f z (x:xs) = myFoldl f (f z x) xs

> pairs            :: (a -> b -> a) -> [a] -> [b]
> pairs f (x:y:t)  = f x y : pairs f t
> pairs f t        = t

Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse xs = foldl (\z x -> x : z) [] xs


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr fun acc (y:ys) = fun y (myFoldr fun acc ys)


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)


For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.


Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs (x:xs) = [abs x] ++ map abs xs

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs [x] = x
> sumAbs (x:xs) = x + sumAbs (xs)
