isPrefix::String->String->Bool
              --priema 2 stringa i vrusha bool 
isPrefix small big
 |length small > lenght big   = False
 |small==""                   =True    --prazniq niz 
 |(head small) /= (head big) = False  -- ne sa ednakiv ? 
 |otherwise                  =isPrefix(tail small) (tail big)

countSubStr::String->String->Integer
countSubStr small big
 |lenght small > lenght big = 0 
 |isPrefix small big = 1 + (countSubStr small (tail big))
 |otherwise    = countSubStr small (tail big)