import Control.Monad.Trans.Cont (reset)
szjSzam n res
    |n<0 = szjSzam (abs n) res
    |n<10 = res + 1
    |otherwise = szjSzam (div n 10) (res + 1)

--szj elofordulas

szjSzamOsszeg n szj
    |szj>9 = error "nem szamjegy"
    |n <10 = if n==szj then szj else 0
    |otherwise = if mod n 10 == szj then szj + szjSzamOsszeg(div n 10 ) szj else szjSzamOsszeg(div n 10) szj


--egy szam paros szamjegyeinek szama
parosSzamSzj n
    | n < 0 = parosSzamSzj (abs n)
    | n< 10 = if even n then 1 else 0
    |otherwise = if even (mod n 10) then 1 + parosSzamSzj (div n 10) else parosSzamSzj(div n 10)


lgSzj n ln
    | n<0 = lgSzj (abs n) ln
    |n<10 = if mod n 10 > ln then n else ln
    |otherwise = if mod n 10 > ln then lgSzj (div n 10) ( mod n 10) else lgSzj (div n 10) ln

bSzamrDSzj n b d
    | n < 0 =bSzamrDSzj (abs n) b d
    |n<b = if n==d then 1 else 0
    |otherwise = if mod n b == d then 1 + bSzamrDSzj (div n b) b d else bSzamrDSzj (div n b) b d

fibo a b res n
    |n==0 = res
    |otherwise = fibo b res (res + b) (n-1)

fiboN :: (Eq t1, Num t1, Num t2) => t1 -> t2
fiboN n = fibo 0 1 0 n

fiboN2 :: (Eq t1, Num t1, Num t2) => t1 -> t2
fiboN2 n = fiboSg 0 1 0 n
    where
        fiboSg _ _ res 0 = res
        fiboSg a b res n = fiboSg b res (res+b)(n-1)

fiboLs n = map(fibo 0 1 0)[0..n]

ls2 :: [(Integer, Integer)]
ls2=[(577723707,7),(423,3),(0,1),(12,2)]
szjSzamOsszegLs ls= map (uncurry szjSzamOsszeg)ls


lgSzjLs ls = map(\x->lgSzj x 0)ls


