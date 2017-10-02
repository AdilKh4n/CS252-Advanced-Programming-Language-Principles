import BigNum

main :: IO ()
main = do
  
  --3 * 4
  putStrLn $ show $ bigMultiply [3] [4]
  --1987 * 0
  putStrLn $ show $ bigMultiply [987,1] [0]
  --3001074098 * 4
  putStrLn $ show $ bigMultiply [98,74,1,3] [4]


   --2^8
   putStrLn $ show $ bigPowerOf [2] [8]
   --1832^0
   putStrLn $ show $ bigPowerOf [832,1] [0]
 
 