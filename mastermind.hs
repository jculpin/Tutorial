-- mastermind.hs
-- attempt to create a version of Master Mind in Haskell
import Data.List 
import System.Random

rollDice :: IO Char
rollDice = getStdRandom (randomR ('A','F'))
                               
exactMatch :: String -> String -> [(Char,Char)]
exactMatch word guess =  zip word guess

matchLetters :: (Char,Char) -> Bool
matchLetters (x,y) = if x==y then True else False

checkExact :: String -> String -> [Bool]
checkExact word1 word2 = map matchLetters (exactMatch word1 word2)

secretWord :: IO [Char]
secretWord  = do
                r1 <- rollDice
                r2 <- rollDice
                r3 <- rollDice
                r4 <- rollDice
                return [r1,r2,r3,r4]                 

checkTrues :: [Bool] -> Int
checkTrues l = length(filter (== True) l)

countColours :: Char -> String -> Int
countColours c lr = length(filter (== c) lr)

correctColours :: Char -> String -> String -> Int
correctColours c secret guess = let
                                  g = countColours c guess
                                  s = countColours c secret
                                  i = if (g/=0 && s/=0) 
                                      then if g > s then s
                                           else g 
                                      else 0
                                 in i
                                                                        
turn :: String -> String -> Int -> IO ()
turn word secret n = do if word == secret 
                           then putStrLn "You win!"
                        else if n == 0 
                             then putStrLn ("You lose!  It was " ++ secret)
                             else mkguess word secret n
      
mkguess :: String -> String -> Int -> IO ()
mkguess word secret n =
            do  putStrLn "Enter your guess (select four letters from A to F):"
                q <- getLine
                putStr ("Exact guesses: ") 
                print (checkTrues(checkExact secret q))
                putStr ( "Correct letters:")
                print (sum (map (\x -> correctColours x secret q) ['A'..'F']) - checkTrues(checkExact secret q))
                putStr ("Guesses left:")
                print (n - 1)
                turn q secret (n-1)

mastermind :: IO ()
mastermind = do 
             secret <- secretWord
             turn "" secret 10  
