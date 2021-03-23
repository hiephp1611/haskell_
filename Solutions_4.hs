module Week4 where

import Test.QuickCheck
import System.Directory
import Data.Maybe

-------------------------------------------------------------------------
-- 1. Properties of the look function

type Table a b = [(a,b)]

look :: Eq a => a -> Table a b -> Maybe b
look x []           = Nothing
look x ((x',y):xys)
  | x == x'         = Just y
  | otherwise       = look x xys

prop_LookJust :: Integer -> [(Integer,Integer)] -> Property
prop_LookJust x xys =
  look_x /= Nothing ==>
    (x,y) `elem` xys
 where
  look_x = look x xys
  Just y = look_x

prop_Look :: Integer -> [(Integer,Integer)] -> Bool
prop_Look x xys =
  examine (look x xys)
 where
  examine Nothing  = x `notElem` [ x | (x,_) <- xys ]
  examine (Just y) = (x,y) `elem` xys

prop_LookSucceed :: Integer -> [(Integer,Integer)] -> Property
prop_LookSucceed k tab =
  k `elem` [k' | (k',_) <- tab] ==> isJust (look k tab)

-------------------------------------------------------------------------
-- 2. Properties of the prefixOf function

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _          = True
prefixOf _  []         = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

prop_prefixOfSelf :: Int -> String -> Bool
prop_prefixOfSelf n s1 = take n s1 `prefixOf` s1

prop_prefixOfAlt :: String -> String -> Property
prop_prefixOfAlt s1 s2 =
    collectLength $
      collect pref $
        pref == (take (length s1) s2 == s1)
  where
    pref = s1 `prefixOf` s2
    collectLength p = if pref then collect (length s1) p else p
  -- The above property shows that (s1 `prefixOf` s2) is false in >90% of the
  -- tests, *and* that whenever it is true, the length of s1 is usually 0!

prop_prefixOfBetter :: String -> String -> Bool
prop_prefixOfBetter s1 s2 = s1 `prefixOf` (s1++s2)

-----------------------------------------------------------------
-------------------------------------------------------------------------
-- 3. The Number Game

game :: IO ()
game =
 do putStrLn "Think of a number 1 -- 100."
    play 1 100
    putStrLn "I won!"

play :: Int -> Int -> IO ()
play a b =
  do putStr ("Is it " ++ show guess ++ "? ")
     s <- getLine
     case s of
       "higher" -> play (guess+1) b
       "lower"  -> play a (guess-1)
       _        -> return ()
 where
  guess = (a+b) `div` 2

-------------------------------------------------------------------------
-- 4. A Backup Script

backup :: IO ()
backup =
  do files <- getDirectoryContents "."
     createDirectory "backup"
     sequence_
       [ copyFile' file ("backup/" ++ file)
       | file <- files
       ]

-- copyFile' file file' copies file to file', if file is a real file
-- (and not a directory)
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' file file' =
  do isFile <- doesFileExist file
     if isFile
       then copyFile file file'
       else return ()
