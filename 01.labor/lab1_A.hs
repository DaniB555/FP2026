
osszeg :: Num a => a -> a -> a
osszeg a b = a + b
kulonbseg :: Num a => a -> a -> a
kulonbseg a b = (-) a b

hanyados1 :: Fractional a => a -> a -> a
hanyados1 a b = a / b

hanyados2 :: Integral a => a -> a -> a
hanyados2 a b = a `div` b

osztmar :: Integral a => a -> a -> a
osztmar a b = a `mod` b

elsoF :: Fractional a => a -> a -> a
elsoF a b =(-b)/a

abszolut :: (Ord a, Num a) => a -> a
abszolut a= if a < 0 then -a else a

abszolut2 a
    |a<0 = (-a)
    |otherwise = a

elojel2 a
    |a<0 ="negativ"
    |a>0 = "positiv"
    |otherwise = "nulla"

max1 a b = if a > b then a else b

max2 a b
    |a>b =a 
    |otherwise =b
    

min1 a b = if a < b then a else b

min2 a b
    |a<b =a 
    |otherwise =b

