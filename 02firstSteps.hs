{- Question 1

-}

{- Question 2

2^3*4 = (2^3)*4
     /= 2^(3*4)

2*3 + 4*5 = (2*3) + (4*5)
         /= 2*(3 + (4*5))

2 + 3 * 4 ^ 5 = (2 + (3 * (4 ^ 5)))
-}

{- Question 3
  (i) ticks on infix div are incorrect. 
 (ii) name of expression starts with an uppercase. 
(iii) layout rule not followed with local a, xs 
        a,x need to be in the same column.
-}

{- Question 4
larst :: [a] -> a
larst = head . reverse
-}

{- Question 5 
Here are two ways to get the initial i.e. all 
but the final, elements of a non-empty list. 

init1 :: [a] -> [a]
init1 = reverse . tail . reverse

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs

tail will fail on an empty list, so will init1
init2 does not fail; it returns []
-}


