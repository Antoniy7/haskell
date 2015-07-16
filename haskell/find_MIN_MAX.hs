findMin::[Double]->Double
findMin numbers
 |length numbers==1  =head numbers
 |otherwise = min (head numbers) (findMin (tail numbers))

findMax numbers
 |length numbers == 1  = head numbers
 |otherwise = max(head numbers) (findMax (tail numbers))

findMinAndMax::[Double]->(Double,Double) -- naredenea dvoika vrsuhta 
findMinAndMax numbers = (findMin numbers , findMax numbers)