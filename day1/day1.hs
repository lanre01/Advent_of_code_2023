import Data.Char

conString :: [(String,Int)] 
conString = [("zero", 0), ("one", 1), ("two", 2), ("three", 3), 
             ("four", 4), ("five", 5), ("six", 6),
             ("seven", 7), ("eight", 8), ("nine", 9)]

stringInt :: [String]
stringInt = ["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


fileReader :: String -> IO [String] 
fileReader file = do contents <- readFile file
                     return (lines contents)

takeDigit :: String -> Int 
takeDigit xs | y == 0 = 0  
             | y == 1 = read . concat $ replicate 2 yss  
             | otherwise       = read $  head yss : drop (y-1) yss 
               where yss = [x | x <- xs, isDigit x]
                     y = length yss 

main :: IO ()
main = do lines <- fileReader "day1.txt"
          let x = sum $ map (finally . mainFunction) lines  
          --let x = sum $ map takeDigit lines 
          print x 

getInt :: (String, String) -> [String] -> (Maybe Int, String)
getInt (xs, [])   _   = (Nothing, []) 
getInt (xs, y:ys) str | null word = getInt (xs++[toLower y], ys) str 
                      | otherwise = (Just x, ys) 
                   where word = filter (== (xs++[toLower y])) str
                         x    = removeInt (head word) conString


removeInt :: String -> [(String, Int)] -> Int 
removeInt ys ((xs,x):xss) = if xs == ys then x 
                            else removeInt ys xss  

mainFunction :: String -> [Char]
mainFunction []     = []
mainFunction (y:ys) | isNumber y = y : mainFunction ys
                    | otherwise   = case n of 
                                   Nothing -> mainFunction ys 
                                   Just a  -> head (show a) : mainFunction ys 
                    where (n, _) = getInt ([], y:ys) stringInt 


finally :: String -> Int
finally [y]    = read [y,y]
finally (y:ys) = read ( y : last ys)
             where
             last [x] = [x] 
             last (_:xs) = last xs 
