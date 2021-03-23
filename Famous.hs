module Famous where

import System.IO
import System.IO.Error

data QA = Question String QA QA | Name String 
            deriving (Show, Read)


defaultTree :: QA
defaultTree = Question "Is she from Europe?" 
                    (Question "Is she i scientist?" 
                            (Name "Marie Curie") (Name "Queen Elizabeth II")) 
                    (Question "Is she an actress?"
                            (Name "Marilyn Monroe") (Name "Hillary Clinton"))

play :: QA -> IO QA
play qa = case qa of
    Question q yes no -> do
        answer <- question (q++" ")
        yesno <- yesNoQuestion answer
        if yesno
            then do 
                newyes <- play yes
                return (Question q newyes no)
            else do
                do 
                newno <- play no
                return (Question q yes newno)
    Name n -> do
        answer <- question ("My guess : is it " ++ n ++ "? ")
        yesno <- yesNoQuestion answer
        if yesno
            then do 
                putStrLn "Hurray! I won!"
                return (Name n)
            else do
                putStrLn "OK - you won this time"
                a1 <- question "Just curious: Who was your famous person? "
                a2 <- question ("Give a question for which the answer for "
                            ++ a1 
                            ++" is \"yes\" and the answer for "
                            ++n
                            ++" is \"no\".\n")
                return (Question a2 (Name a1) (Name n))

question :: String -> IO String
question q = do
    putStr q
    hFlush stdout
    a <- getLine
    return a

yesNoQuestion :: String -> IO Bool
yesNoQuestion a = case a of
    "yes" -> return True
    "no"  -> return False
    _     -> do 
        putStr "Please answer yes or no! "
        hFlush stdout
        answer <- getLine 
        yesNoQuestion answer
        
main :: IO ()
main = do
    putStrLn "Think of a famous person! I will ask you question about her."
    hFlush stdout
    content <- tryIOError (readFile "Famous.qa")
    case content of
        Left _ -> do
                game <- play defaultTree
                writeFile "Famous.qa" (show game)
        Right a -> do
                game <- play (read a)
                writeFile "Famous.qa" (show game)
    putStr "Play again? "
    hFlush stdout
    a <- getLine
    a <- yesNoQuestion a
    if  a then do main
          else putStrLn "Bye!"