import Data.Char
-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
length' ls = foldl (\res x ->res+1) 0 ls
sum' ls = foldl (+) 0 ls

reverse' ls = foldl (\res x -> x : res) [] ls

-- - meghatározza egy lista pozitív elemeinek összegét,

-- - egy lista páros elemeinek szorzatát,
product' ls = foldl (*)1 ls
maximum' ls = foldl1(\x1 x2 -> if x1>x2 then x1 else x2) ls

lsFuz lss = foldl1(++) lss
map' fg ls = foldl(\res x-> res++ [fg x]) [] ls

pozitivOsszead :: (Foldable t, Ord b, Num b) => t b -> b
pozitivOsszead ls = foldl(\res x1 -> if x1>=0 then res+x1 else res) 0 ls

-- - n-ig a négyzetszámokat.
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$
atlag ls = sum ls /fromIntegral (length ls)

-- III.
lsTo = "ez egy PrOBA szoveg. ez egy masik proBa! Tobbfele irasJEL"
-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)

--tokenize = map (irasjelHelyettesit . toLower)

irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

lengthLista ls=map length ls
myMinimum :: (Num b, Enum b, Ord a) => [a] -> (a, [b])
myMinimum ls = (m, map snd $ filter fg $ zip ls [0,1..])
    where
    m = minimum ls
    fg k = fst k == m


-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.

talalat x ls = l1
        where
            zipls= zip ls [0..]
            l1=map snd $ filter(\y->fst y == x) zipls
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
ps ls = sum [t2 | (t1,t2,t3)<-ls]
ps2 :: (Num a1, Eq p) => [(a2, a1, p)] -> p -> a1
ps2 ls r =sum [t2 | (t1,t2,t3)<-ls1]
    where ls1 = filter (\(t1,t2,t3)->t3 == r) ls 
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
main = do
    -- let lista = "ez egy PrOBA szoveg. ez egy masik proBa! Tobbfele irasJEL"
    -- let l1 = tokenize lista
    -- let l2=lengthLista l1
    -- let m1=minimum l2
    -- putStrLn "a szavak hossza"
    -- print l2
    -- let r1 = myMinimum l2
    -- print r1
    -- let l3 = zip l1 l2
    -- print l3




    -- let a=5
    -- let l1 = [3, 13, 5, 6, 7, 12, 5, 8, 5]
    -- let t1= talalat a l1

    -- let t2= talalat 'e' "Bigeri vizeses"
    -- --print t1
    -- let c1=concatMap ((<> " ") . show)t1
    -- putStrLn $ show a <> " talalat pozicioi " <> c1
    -- print t2



    
    let ls = [("golya",120,"ma"),("fecske",85,"cj"),("cinege",132,"ms")]
    let result = ps ls
    let madarLs = concatMap (<> " ")[ t1 | (t1,t2,t3)<-ls]
    putStrLn $ madarLs <> " populacio szama: " <> show result
    let result2 = ps2 ls "ms"
    putStrLn $ madarLs <>" "<>"ms"<> "-populacio szam: " ++ show result2