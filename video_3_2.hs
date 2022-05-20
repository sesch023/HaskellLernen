fac n =
 if n <= 1 then
  1
 else
  n * fac (n-1)
  
fac_guard n
 | n <= 1 = 1
 | otherwise = n * fac (n-1)
 
communicator x
 | x == "Hello" = "Hello Lad"
 | x == "Bye" = "Bye you filthy man"
 | x == "Fuck you" = "Are you fokking retarded?"
 | otherwise = "Wanna go to the pub?"
 
fac_acc n = aux n 1
 where
  aux n acc
   | n <= 1 = acc
   | otherwise = aux (n-1) (acc*n)