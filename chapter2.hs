-- head :: [a] -> a            -- returns the first element
-- tail :: [a] -> [a]          -- returns everything except the first element
-- init :: [a] -> [a]          -- returns everything except the last element
-- take :: Int -> [a] -> [a]   -- returns the n first elements
-- drop :: Int -> [a] -> [a]   -- returns everything except the n first elements
-- (++) :: [a] -> [a] -> [a]   -- lists are catenated with the ++ operator
-- (!!) :: [a] -> Int -> a     -- lists are indexed with the !! operator
-- reverse :: [a] -> [a]       -- reverse a list
-- null :: [a] -> Bool         -- is this list empty?
-- length :: [a] -> Int        -- the length of a list

f xs = take 2 xs ++ drop 4 xs
f [1,2,3,4,5,6]  ==>  [1,2,5,6]
f [1,2,3]        ==>  [1,2]

-- Rotating a list by taking the first element and moving it to the end:

g xs = tail xs ++ [head xs]
g [1,2,3]      ==>  [2,3,1]
g (g [1,2,3])  ==>  [3,1,2]

-- Guards
f x y z
  | condition1 = something
  | condition2 = other
  | otherwise  = somethingother

-- Type Inference / Unification
head :: [a] -> a
head [True,False] :: Bool

-- 2.5.1 Sidenote: Some Terminology
-- In a type like [Char] we call Char a type parameter. A type like the list type that needs a type parameter 
-- is called a parameterized type.

-- The fact that a function like head can be used with many different types of arguments
--  is called polymorphism. 
-- The head function is said to be polymorphic. There are many forms of polymorphism, and this Haskell form that uses type variables 
-- is called parametric polymorphism.


-- Multiply an Int with a Maybe Int. Nothing is treated as no multiplication at all.
perhapsMultiply :: Int -> Maybe Int -> Int
perhapsMultiply i Nothing = i
perhapsMultiply i (Just j) = i*j   -- Note how j denotes the value inside the Just

-- given a password, return (Just username) if login succeeds, Nothing otherwise
login :: String -> Maybe String
login "f4bulous!" = Just "unicorn73"
login "swordfish" = Just "megahacker"
login _           = Nothing


intOrZero :: Maybe Int -> Int
intOrZero Nothing = 0
intOrZero (Just i) = i

safeHead :: [a] -> Maybe a
safeHead xs = if null xs then Nothing else Just (head xs)

headOrZero :: [Int] -> Int
headOrZero xs = intOrZero (safeHead xs)
headOrZero []  ==> intOrZero (safeHead [])  ==> intOrZero Nothing  ==> 0
headOrZero [1] ==> intOrZero (safeHead [1]) ==> intOrZero (Just 1) ==> 1


readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)


iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number


lectureParticipants :: [Either String Int]
lectureParticipants = [Right 10, Right 13, Left "easter vacation", Right 17, Left "lecturer was sick", Right 3]