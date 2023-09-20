import System.Info()
import Data.Char
import Data.Either
import Data.List

main :: IO ()
main = do
    print "hello"


data Bin = End | O Bin | I Bin
  deriving (Show, Eq)

-- This function increments a binary number by one.
inc :: Bin -> Bin
inc End   = I End
inc (O b) = I b
inc (I b) = O (inc b)

prettyPrint :: Bin -> String
prettyPrint End = ""
prettyPrint (O b) = prettyPrint b ++ "0"
prettyPrint (I b) = prettyPrint b ++ "1"

-- fromBin :: Bin -> Int
-- fromBin = todo

-- toBin :: Int -> Bin
-- toBin = todo
