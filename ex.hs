import System.Info()
import Data.Char
import Data.Either
import Data.List

main :: IO ()
main = do
    print "hello"

-- These functions will help:
--  - toUpper :: Char -> Char   from the module Data.Char
--  - words :: String -> [String]
--  - unwords :: [String] -> String
--
-- Example:
--   capitalize "goodbye cruel world" ==> "Goodbye Cruel World"

capitalize :: String -> String
capitalize s = unwords (map capitalizeFirst (words s))

capitalizeFirst :: String -> String
capitalizeFirst word = toUpper (head word) : tail word