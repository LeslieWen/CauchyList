module Cauchy where
  data CauchyList = CauchyList Int [Int]
  
  --Misc. helper functions for our overloaded methods

  regularMulti content1 content2 i j = if (i+1==j) then 0 else
    (content1 !! j) * (content2 !! (i-j))+(regularMulti content1 content2 i (j+1))
  

  --Appends zeros to end of listX if its length is shorter than that of listY
  addZeros listX listY = (listX++take(max (length(listY)-length(listX)) 0)(repeat 0))
  
  --Used for CauchyList by CauchyList Multiplication
  --Appends zeros to end of listX so it is the sum of it and the other list -1
  addZerosMulti listX listY = (listX++take(length(listX)+length(listY)-1)(repeat 0))

  instance Eq CauchyList where
    -- Implement Eq type class here
    (CauchyList a contentA) == (CauchyList b contentB) = ( a == b && contentA == contentB)
  
  instance Num CauchyList where
    -- Implement Num type class here   
    (CauchyList a contentA) + (CauchyList b contentB) = CauchyList a (map (`mod` a)(zipWith(+) ( addZeros (contentB) (contentA) ) ( addZeros (contentA) (contentB) ) ))
    (CauchyList a contentA) - (CauchyList b contentB) = CauchyList a (map (`mod` a)(zipWith(-) ( addZeros (contentA) (contentB) ) ( addZeros (contentB) (contentA) ) ))
    --if b is -100, then do 
    (CauchyList a contentA) * (CauchyList b contentB) = if(b == -100) then (CauchyList a (map(*contentB!!0) contentA)) else 
      CauchyList a (map (`mod` a)([regularMulti (addZerosMulti (contentA) (contentB)) (addZerosMulti (contentB) (contentA)) i 0 | i <- [0..(length contentA + length contentB-2)]]))

    

    abs (CauchyList a contentA) = CauchyList (abs a) (map abs(contentA))
    signum (CauchyList a contentA) = CauchyList (signum a) (map signum(contentA))
    -- -100 will act as a flag so that we know when are doing scalar multiplication
    fromInteger n = CauchyList (fromInteger(-100)) [fromInteger n]

  instance Show CauchyList where
    -- Implement Show type class here  
    show (CauchyList a b) =
      "p: " ++ (show a) ++ "\nlength: " ++ show(length(b)) ++ "\ncontent: " ++ (show b)
  main = do
    --print(CauchyList 1 [1,2,3,4,5,6])
    --print(addZeros [1,2,3] [1,2,3,4,3,2,1,3,4])
    --print(addZeros [1,2,3,4,4] [1,2,3,4])
    
    print(CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1] + CauchyList 31 [12, 4, 7, 15, 13, 4])
    print(CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1] - CauchyList 31 [12, 4, 7, 15, 13, 4])
    print((CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1]) * (CauchyList 31 [12, 4, 7, 15, 13, 4]))

    --print((CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1]) * (CauchyList 31 [12, 4, 7, 15, 13, 4]))
    --print(CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1] == CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1])
    --print(abs(CauchyList 1 [-3,-4,-3,-9]))
    --print(signum(CauchyList 1 [-3,-4,-3,-9,9,8,5,4]))
    --print((CauchyList 31 [17, 9, 22, 27, 28, 27, 15, 28, 24, 1])*65)
    --print((CauchyList 1 [4,8,9])*2)
    --print((CauchyList 1 [-3,-4,-3,-9,9,8,5,4])*((CauchyList 1 [-3,-4,-3,-9,9,8,5,4])))
