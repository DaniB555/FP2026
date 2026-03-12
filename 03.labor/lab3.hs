import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
-- atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)


-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] =0
myLength(x:xs) =1+myLength(xs)


myLength3 ls =foldr(\x->(+)1)0 ls
-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum3 ls =foldl1 min ls
-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum3 ls = foldl1 max ls
-- - meghatározza egy lista n-ik elemét (!!),
listaN2 ls n
    |ls ==[] = error "ures lista"
    |n<0 = error "neg index"
    |length ls <= n = error "tul nagy index"
    |otherwise = ls !! n

-- - egymásután fűzi a paraméterként megadott két listát (++),
-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls =ls == reverse ls
-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x
    |x<0 = szjLs (abs(x))
    |x<10 = [x]
    |otherwise = szjLs (div x 10) ++ [mod x 10]
-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso ls = tail ls ++ [head ls]
-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p
    |x<0 =error "neg szam"
    |x<p =[x]
    |otherwise =decP(div x p) p ++ [mod x p]
-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec ls p = foldr (\hatvany x->x+(p*hatvany))0 ls


-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.
ls1=[[1,22,3],[2,3,4,4]]
myLengthLs = map myLength ls1
-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
poli :: Num t => [t] -> t -> t
poli [] x= 0
poli (a : aLs) x =a+x*(poli aLs x)
-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
