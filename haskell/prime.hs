isPrime::Int->Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrimeHelper 2
  where
   isPrimeHelper i 
    |(n`mod`i) ==0   =False
    |i>s              = True    -- s e koren ot n   -- (fromIntegral)i> sqrt (fromIntegral n) pak stava
    |otherwise= isPrimeHelper(i+1)  -- otherwise=true
    where
    s=floor(sqrt (fromIntegral(n)))
   
    