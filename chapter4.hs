zip :: [a] -> [b] -> [(a, b)]    -- two lists to list of pairs
unzip :: [(a, b)] -> ([a], [b])  -- list of pairs to pair of lists
partition :: (a -> Bool) -> [a] -> ([a], [a])    -- elements that satisfy and don't satisfy a predicate
zip [1,2,3] [True,False,True]
  ==> [(1,True),(2,False),(3,True)]
unzip [("Fred",1), ("Jack",10), ("Helen",13)]
  ==> (["Fred","Jack","Helen"],[1,10,13])
partition (>0) [-1,1,-4,3,2,0]
  ==> ([1,3,2],[-1,-4,0])


swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
Here’s an example of pattern matching on tuples and lists at the same time:

-- sum all numbers that are paired with True
sumIf :: [(Bool,Int)] -> Int
sumIf [] = 0
sumIf ((True,x):xs) = x + sumIf xsMap.findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')])
sumIf ((False,_):xs) = sumIf xs
sumIf [(True,1),(False,10),(True,100)]
  ==> 101


-- Note! A type class is a collection of types. It doesn’t have much to do with the classes of 
-- 	object oriented programming! In some situations, type classes can act like interfaces in object oriented programming. 
-- Unfortunately the functions in a type class are often called methods, adding to the confusion.    

compare :: Ord a => a -> a -> Ordering
(<) :: Ord a => a -> a -> Bool
(>) :: Ord a => a -> a -> Bool
(>=) :: Ord a => a -> a -> Bool
(<=) :: Ord a => a -> a -> Bool
max :: Ord a => a -> a -> a
min :: Ord a => a -> a -> a
Prelude> compare 1 1                -- 1 is EQual to 1
EQ
Prelude> compare 1 3                -- 1 is Less Than 3
LT
Prelude> compare 1 0                -- 1 is Greater Than 0
GT
Prelude> min 5 3
3
Prelude> max 5 3
5
Prelude> "aardvark" < "banana"      -- strings are compared alphabetically
True
Prelude> [1,2,3] > [2,5]            -- lists are compared like strings
False
Prelude> [1,2,3] > [1,1]
True




As a last example, let’s sort a list of lists according to length. We’ll need two helper functions:

-- from the module Data.Ord
-- compares two values "through" the function f
comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing f x y = compare (f x) (f y)

-- from the module Data.List
-- sorts a list using the given comparison function
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- Now the implementation of sortByLength is straightforward:

-- sorts lists by their length
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (comparing length)
sortByLength [[1,2,3],[4,5],[4,5,6,7]]   ==>  [[4,5],[1,2,3],[4,5,6,7]]


import qualified Data.Map as Map

-- Create a Map from a list of key-value pairs
Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

-- Insert a value into a map. Overrides any previous value with the same key.
-- Returns a new map. Does not mutate the given map.
Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a

-- Get a value from a map using a key. Returns Nothing if the key was not present in the map.
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

-- An empty map
Map.empty :: Map.Map k a

--The Ord constraint for the key type of the map is needed because maps are implemented as ordered binary search trees.

Prelude> import qualified Data.Map as Map
Prelude Map> values = Map.fromList [("z",3),("w",4)]
Prelude Map> Map.lookup "z" values
Just 3
Prelude Map> Map.lookup "banana" values
Nothing
Prelude Map> Map.insert "x" 7 values
fromList [("w",4),("x",7),("z",3)]
Prelude Map> values                                       -- note immutability!
fromList [("w",4),("z",3)]
Prelude Map> Map.insert "x" 1 (Map.insert "y" 2 values)   -- two insertions
fromList [("w",4),("x",1),("y",2),("z",3)]
Prelude Map>


--Here’s an example of representing a bank as a Map String Int (map from account name to account balance), and withdrawing some money from an account:

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank =
  case Map.lookup account bank of
    Nothing  -> bank                                   -- account not found, no change
    Just sum -> Map.insert account (sum-amount) bank   -- set new balance
--Here’s how you might use the withdraw function in GHCi. Note how the maps get printed as fromList invocations. Also note how calling withdraw ... bank returns a new bank and doesn’t change the existing bank.

GHCi> bank = Map.fromList [("Bob",100),("Mike",50)]
GHCi> withdraw "Bob" 80 bank
fromList [("Bob",20),("Mike",50)]
GHCi> bank                         -- note immutability
fromList [("Bob",100),("Mike",50)]
GHCi> withdraw "Bozo" 1000 bank
fromList [("Bob",100),("Mike",50)]

Data.Map defines all sorts of useful higher-order functions for updating maps. We can rewrite the withdraw function using Data.Map.adjust:

withdraw :: String -> Int -> Map.Map String Int -> Map.Map String Int
withdraw account amount bank = Map.adjust (\x -> x-amount) account bank
--Note! There are separate Data.Map.Strict and Data.Map.Lazy implementations. When you import Data.Map you get Data.Map.Lazy. You can find the documentation for all the Data.Map functions in the docs for Data.Map.Lazy. We won’t go into their differences here, but mostly you should use Data.Map.Strict in real code.


myArray :: Array Int String
myArray = array (7,11) [(7,"seven"), (8,"eight"), (9,"nine"), (10,"ten"), (11,"ELEVEN")]

myArray :: Array Int String
myArray = array (7,11) [(7,"seven"), (8,"eight"), (9,"nine"), (10,"ten"), (11,"ELEVEN")]
Listing all the indices and elements in order can be a bit cumbersome, so there’s also the listArray constructor that just takes a list of elements in order:

listArray :: Ix i => (i, i) -> [e] -> Array i e
myArray :: Array Int String
myArray = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]

-- Arrays are used with two new operators:

-- Array lookup
(!) :: Ix i => Array i e -> i -> e
-- Array update
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
-- Here’s an example GHCi session:

Prelude> import Data.Array
Prelude Data.Array> myArray = listArray (7,11) ["seven", "eight", "nine", "ten", "ELEVEN"]
Prelude Data.Array> myArray
array (7,11) [(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"ELEVEN")]
Prelude Data.Array> myArray ! 8
"eight"
Prelude Data.Array> myArray // [(8,"ocho"),(9,"nueve")]
array (7,11) [(7,"seven"),(8,"ocho"),(9,"nueve"),(10,"ten"),(11,"ELEVEN")]


length (array (7,11) [(7,"seven"),(8,"eight"),(9,"nine"),(10,"ten"),(11,"ELEVEN")])
  ==> 5
foldr (+) 0 (Map.fromList [("banana",3),("egg",7)])
  ==> 10

  

