import System.Info()

main :: IO ()
main = do
    print "hello"

ilog3 :: Integer -> Integer
ilog3 0 = 0
ilog3 n = 1 + ilog3 (div n 3)