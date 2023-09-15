import System.Info()

main :: IO ()
main = do
    print "hello"


data Tree a = Node a (Tree a) (Tree a) | Empty

-- example :: Tree Int
-- example = (Node 0 (Node 1 (Node 2 Empty Empty)
--                           (Node 3 Empty Empty))
--                   (Node 4 Empty Empty))

