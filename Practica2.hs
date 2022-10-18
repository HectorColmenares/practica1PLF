--ej1
firstToEnd :: (Read a)=>[a]->[a]
firstToEnd []=[]
firstToEnd[x]=[x]
firstToEnd (x:xs) =  xs ++ [x]

--ej2
minAndMax :: (Ord a)=>[a]->[a]
minAndMax []= error "Lista vacia"
minAndMax [x]= error "Falta 1 elemento"
minAndMax xs= [minimum xs, maximum xs]

--ej3
minorsFirstElement :: (Integral a)=>[a]->[a]
minorsFirstElement [] = error "Lista vacia"
minorsFirstElement [a] = error "Solo hay un elemento"
minorsFirstElement list = [x | x <- tail list , x<head list]

--ej4
greaterOrEqualFirstElement :: Integral a => [a] -> [a]
greaterOrEqualFirstElement [] = error "Lista vacia"
greaterOrEqualFirstElement [a] = error "Solo hay un elemento"
greaterOrEqualFirstElement list = [x | x <- tail list , x >=head list]

--Ej5
minorsToSumFirstAndSecondElement :: Integral a => [a] -> [a]
minorsToSumFirstAndSecondElement [] = error "Lista vacia"
minorsToSumFirstAndSecondElement [a] = error "Solo hay un elemento"
minorsToSumFirstAndSecondElement list= [x | x <- tail (tail list) , x <head list + head(tail list)]

--Ej6
listSumDuplaToList :: Integral a => [(a,a)] -> [a]
listSumDuplaToList [] = []
listSumDuplaToList list = fst(head list) + snd (head list) : listSumDuplaToList (tail list)

--ej7
multripleta :: (Integral a) => [(a,a,a)] -> [a]
multripleta []  =[]
multripleta xs = [x*y*z | (x,y,z) <- xs]

--ej8
changeFstToSnd :: (Integral a)=>[(a,a)]->[(a,a)]
changeFstToSnd xs = [(y,x) | (x,y)<-xs]

--ej9
sumVectors :: (Integral a)=>[(a,a)]->(a,a)
sumVectors []=(0,0)
sumVectors  [x] = (fst x,snd x)
sumVectors list = (fst (head list) + fst (sumVectors (tail list)),snd (head list) + snd (sumVectors (tail list)))

--ej10
dividers :: Int -> [Int]
dividers x = [y | y <- [1..a], x `mod` y == 0] ++[x]
    where a = x `div` 2

--ej11
esPrimo :: (Integral a)=>a -> Bool
esPrimo n = factores n == [1,n]
 where factores n = [x | x <- [1..n], n `mod` x == 0]
primeNumbers :: (Integral a)=>a->[a]
primeNumbers 0=[]
primeNumbers n = [x | x <- [1..n], esPrimo x]

--ej12
infinitePrimeNumbers :: [Integer]
infinitePrimeNumbers = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]