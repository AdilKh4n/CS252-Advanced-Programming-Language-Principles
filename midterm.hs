myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

multall :: [Integer] -> Integer
multall [] = 1
multall (x:xs) = x * multall xs

myLast :: [Integer] -> Integer
myLast (x:xs) = last xs

areaofcircle :: Integer -> Integer
areaofcircle x = do
     i <- x
     Just $ 3 * x * x