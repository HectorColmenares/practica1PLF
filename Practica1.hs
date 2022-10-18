import Data.Data (mkIntegralConstr)

--ej1
average3Numbers :: (Floating a) =>a -> a -> a -> a
average3Numbers x y z = (x + y + z) / 3 


--ej2
isLastDigit3 :: (Integral a )=>a->Bool
isLastDigit3 3=True
isLastDigit3 num 
 |ultimoD num ==3=True
 |otherwise=False
 where ultimoD num= rem num 10


 --ej3
has3Digits :: (Integral a)=>a->Bool
has3Digits x
 | x>99 && x<1000 = True
 | otherwise= False


 --ej4
isNegative :: (Integral a)=>a->Bool
isNegative num
 |num<0=True
 |otherwise=False


 --ej5   
sum2Digits:: Int->Int
sum2Digits x 
    | x>9 && x<100 = (x `mod` 10)+((x `div` 10) `mod` 10) 
    | x>99 = error "El numero no tiene dos digitos "

--ej6
even2Digit:: Int->Bool
even2Digit x 
    | (x>9 && x<100) = even (x `mod` 10) && even((x `div` 10) `mod` 10) 
    | x>99 = error "El numero no es de dos digitos "

--ej7
isPrimeNumber :: Int -> Bool
isPrimeNumber x= elem x[1,2,3,5,7,11,13,17]


--ej8
isEvenAndPrimeNumber :: Int -> Bool
isEvenAndPrimeNumber  x = isPrimeNumber x && even(x)


    --ej9
multiplos :: (Integral a)=> a->a-> Bool
multiplos num mul
 |  mod num mul == 0 = True
 |otherwise = False

    --ej10
equal2digits :: (Integral a)=> a->Bool
equal2digits x
 |x<10 = False
 |x <=99 = div x 10 == mod x 10


 --ej11
higher:: Int->Int->Int->Int
higher a b c 
    | a>b && a>c = a
    | b>a && b>c = b
    | c>a && c>b = c

    --ej12
suma ::(Integral a)=> a ->a ->Bool
suma a b = even (a+b)


--ej13
sum2Digit2Number :: (Integral a) => a -> a -> a
sum2Digit2Number x y
    |x > 9 && x <100 && y > 9 && y <100 = (div x 10 + mod x 10) + (div y 10 + mod y 10)
    |otherwise = error "Algún número no tiene 2 dígitos"


    --ej14 
sum3Digits:: Int->Int
sum3Digits x 
 | x>100 && x<1000 =sum [read [y] | y <- show x]
 | x>999= error "El numero no tiene tres digitos "


 --ej15 
equials3Digits :: (Integral a) => a -> Bool
equials3Digits num 
 |num >= 100 && num <= 999 = dig1 == dig2 || dig2 == dig3 || dig1 == dig3
 |otherwise = error "El numero no tiene 3 digitos"
 where 
    dig1 = div num 100
    dig2 = mod (div num 10)10
    dig3 = mod num 10
    suma =  dig1 + dig2 + dig3 

 --ej16
positionHigher3Digits :: Integer -> String
positionHigher3Digits x
    |(x > 99 && x <1000) && ((max x1 x2 == x1) && (max x1 x3 == x1))= "El mayor se encuentra en la pos 1"
    |(x > 99 && x <1000) && ((max x1 x2 == x2) && (max x2 x3 == x2))= "El mayor se encuentra en la pos 2"
    |(x > 99 && x <1000) && ((max x1 x3 == x3) && (max x2 x3 == x3))= "El mayor se encuentra en la pos 3"
    |otherwise = error "El número no tiene 3 dígitos"
    where
        x1 = div x 100
        x2 = div (mod x 100) 10
        x3 = mod (mod x 100) 10


--EJ17
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

--ej18
safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then error"No es posible dividir entre cero"
 else Just (n `div` m)

 --ej19
xorl :: Bool -> Bool -> Bool
xorl True True = False
xorl True False = True
xorl False True = True
xorl False False = True


--ej20
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
 where
    x' = x1 - x2
    y' = y1 - y2


--ej7
    multripleta :: (Integral a) => [(a,a,a)] -> [a]
    multripleta []  =[]
    multripleta xs = [x*y*z | (x,y,z) <- xs]