import Text.Printf (printf)
import Language.Haskell.TH (prim)
import Distribution.Simple.Setup (falseArg)
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))

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

negyzetgyokN n = [ sqrt i|i<-[1..n]]

negyzetN n = [i*i| i <- [1..n] ]

kobN n = [i^3 | i <-[1..n]]

nemNegyzetN n=[i|i<-[1..n],(sqrt i)*(sqrt i)/=i]

xhatvanyN x n =[x^i|i<-[1..n]]


osztokparosN n=[i|i<-[1..n],n `mod` i == 0,mod i 2 ==0]

osztokN n=[i|i<-[1..n],mod n i ==0]
primszam n = osztokN n==[1,n]

primszamokN n=[i|i<-[1..n], primszam i]

osszetettN n=[i|i<-[1..n],primszam i == False]
osszetettparosN n=[i|i<-[1..n],primszam i == False,mod i 2 ==0]
osszetettparatlanN n=[i|i<-[1..n],primszam i == False,mod i 2 /=0]
pitagorasz n=[(a,b,c)| c<-[1..n],b<-[1..c],a<-[1..b] , a**2+b**2==c**2]

betuSzam =zip['a'..'z'][0..25]
szamok1 =zip[0..5][5,4..0]
szamok2 n=[(i,n-i)|i<-[0..n]]
tfLs n=take n ls
    where
        ls=[True,False]++ls
main = do
    putStrLn("x hatvany n")
    print(xhatvanyN 5 3) 
    print(primszamokN 10)
    print(osszetettN 10)
    print(osszetettparosN 10)
    print(osszetettparatlanN 10)
    --putStrLn("Pitagoraszi szamharmasok"++ show(pitagorasz 100))
    --print(pitagorasz 100)
    print(szamok2 10)