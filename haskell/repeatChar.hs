repeatChar :: Char->Integer->String
repeatChar ch n 
 |n==0        =""
 |otherwise = ch:(repeatChar ch(n-1))  -- [ch]++(repeatChar ch(n-1))


--ch:    svurzani spisaci demek direkten dostup do opashakata .. ? 

-- head tail init i last :)  rabotqt samo s neprazni spisaci 

--1:[2,3]       stava  [1,2,3]   

--Prelude> [1,2]++[3,4]
--[1,2,3,4]
