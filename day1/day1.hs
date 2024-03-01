import Data.Char



fileReader :: String -> IO [String] 
fileReader file = do contents <- readFile file
                     return (lines contents)

takeDigit :: String -> Int 
takeDigit xs = read [x | x <- xs, isDigit x] 

main :: IO ()
main = do lines <- fileReader "day1.txt"
          let x = map takeDigit lines 
          print x 


