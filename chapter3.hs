onlyPositive xs = filter positive xs
mapBooleans f = map f [False,True]
Prelude> :t onlyPositive
onlyPositive :: [Int] -> [Int]
Prelude> :t mapBooleans
mapBooleans :: (Bool -> b) -> [b]
Prelude> :t mapBooleans not
mapBooleans not :: [Bool]


wrapJust xs = map Just xs
Prelude> :t wrapJust
wrapJust :: [a] -> [Maybe a]
Prelude> wrapJust [1,2,3]
[Just 1,Just 2,Just 3]






-- a predicate that checks if a string is a palindrome
palindrome :: String -> Bool
palindrome str = str == reverse str

-- palindromes n takes all numbers from 1 to n, converts them to strings using show, and keeps only palindromes
palindromes :: Int -> [String]
palindromes n = filter palindrome (map show [1..n])


-- How many words in a string start with “a”? This uses the function words from the module Data.List that splits a string into words.

countAWords :: String -> Int
countAWords string = length (filter startsWithA (words string))
  where startsWithA s = head s == 'a'
countAWords "does anyone want an apple?"
 -- ==> 3


-- Here’s an example where we find what characters come after a given character in a string. 
--First of all, we use tails, map and take to get all substrings of a certain length:

substringsOfLength :: Int -> String -> [String]
substringsOfLength n string = map shorten (tails string)
  where shorten s = take n s
substringsOfLength 3 "hello"
  ==> ["hel","ell","llo","lo","o",""]
--There’s some shorter substrings left at the end (can you see why?), 
-- but they’re fine for our purposes right now. Now that we have substringsOfLength,
-- we can implement the function whatFollows c k s that finds all the occurrences of the character c in the string s, 
-- and outputs the k letters that come after these occurrences.

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
  where match sub = take 1 sub == [c]
whatFollows 'a' 2 "abracadabra"
  ==> ["br","ca","da","br",""]

  reverse (map head (map reverse (["Haskell","pro"] ++ ["dodo","lyric"])))
  (reverse . map head . map reverse) (["Haskell","pro"] ++ ["dodo","lyric"])
  reverse . map head . map reverse $ ["Haskell","pro"] ++ ["dodo","lyric"]

  map ($"string") [reverse, take 2, drop 2]
  ==> [reverse $ "string", take 2 $ "string", drop 2 $ "string"]
  ==> [reverse "string", take 2 "string", drop 2 "string"]
  ==> ["gnirts", "st", "ring"]

  -- https://typeclasses.com/featured/dollar


  substringsOfLength :: Int -> String -> [String]
substringsOfLength n string = map shorten (tails string)
  where shorten s = take n s

whatFollows :: Char -> Int -> String -> [String]
whatFollows c k string = map tail (filter match (substringsOfLength (k+1) string))
  where match sub = take 1 sub == [c]


  whatFollows c k string = map tail (filter match (map shorten (tails string)))
  where shorten s = take (k+1) s
        match sub = take 1 sub == [c]

whatFollows c k string = map tail (filter match (map (take (k+1)) (tails string)))
  where match sub = take 1 sub == [c]

whatFollows c k string = map tail . filter match . map (take (k+1)) $ tails string
  where match sub = take 1 sub == [c]

whatFollows c k string = map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) $ tails string

whatFollows c k = map tail . filter (\sub -> take 1 sub == [c]) . map (take (k+1)) . tails

 \sub -> take 1 sub == [c]
=== \sub -> (==[c]) (take 1 sub)
=== \sub -> (==[c]) ((take 1) sub)
=== \sub -> ((==[c]) . (take 1)) sub
=== ((==[c]) . (take 1))
=== ((==[c]) . take 1)


whatFollows c k = map tail . filter ((==[c]) . take 1) . map (take (k+1)) . tails



-- Using these, we can implement a function findSubstring that finds the earliest and longest substring in a string that consist only of the given characters.
findSubstring :: String -> String -> String
findSubstring chars = takeWhile (\x -> elem x chars)
                      . dropWhile (\x -> not $ elem x chars)

findSubstring "a" "bbaabaaaab"              ==> "aa"
findSubstring "abcd" "xxxyyyzabaaxxabcd"    ==> "abaa"

filter id [True,False,True,True]  ==>  [True,True,True]
dropWhile id [True,True,False,True,False]  ==>  [False,True,False]

map (const 5) [1,2,3,4] ==> [5,5,5,5]
filter (const True) [1,2,3,4] ==> [1,2,3,4]


descend 0 = []
descend n = n : descend (n-1)
descend 4 ==> [4,3,2,1]

iterate f 0 x = [x]
iterate f n x = x : iterate f (n-1) (f x)
iterate (*2) 4 3 ==> [3,6,12,24,48]

let xs = "terve"
in iterate tail (length xs) xs
  ==> ["terve","erve","rve","ve","e",""]


  split :: Char -> String -> [String]
split c [] = []
split c xs = start : split c (drop 1 rest)
  where start = takeWhile (/=c) xs
        rest = dropWhile (/=c) xs
split 'x' "fooxxbarxquux"   ==>   ["foo","","bar","quu"]



myhead :: [Int] -> Int
myhead [] = -1
myhead (first:rest) = first

mytail :: [Int] -> [Int]
mytail [] = []
mytail (first:rest) = rest

describeList :: [Int] -> String
describeList []         = "an empty list"
describeList (x:[])     = "a list with one element"
describeList (x:y:[])   = "a list with two elements"
describeList (x:y:z:xs) = "a list with at least three elements"

describeList :: [Int] -> String
describeList []         = "an empty list"
describeList [x]        = "a list with exactly one element"
describeList [x,y]      = "a list with exactly two elements"
describeList (x:y:z:xs) = "a list with at least three elements"

-- Using pattern matching and recursion, we can recursively process a whole list. Here’s how you sum all the numbers in a list:

sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x + sumNumbers xs

-- Here’s how you compute the largest number in a list, this time using a helper function.
myMaximum :: [Int] -> Int
myMaximum [] = 0       -- actually this should be some sort of error...
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

-- It’s often convenient to use nested patterns while consuming a list. 
-- Here’s an example that counts how many Nothing values occur in a list of Maybes:

countNothings :: [Maybe a] -> Int
countNothings [] = 0
countNothings (Nothing : xs) = 1 + countNothings xs
countNothings (Just _  : xs) = countNothings xs
countNothings [Nothing,Just 1,Nothing]  ==>  2

-- Now that we can build and consume lists, let’s do both of them at the same time. 
-- This function doubles all elements in a list.

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs
It evaluates like this:

doubleList [1,2,3]
=== doubleList (1:(2:(3:[])))
==> 2*1 : doubleList (2:(3:[]))
==> 2*1 : (2*2 : doubleList (3:[]))
==> 2*1 : (2*2 : (2*3 : doubleList []))
==> 2*1 : (2*2 : (2*3 : []))
=== [2*1, 2*2, 2*3]
==> [2,4,6]


map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
and here’s filter:

filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs


However, when you’re returning a list there is a big difference between these two forms. Consider the function doubleList from earlier. Here it is again, implemented first directly, and then via a tail-recursive helper function.

-- Not tail recursive!
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs
-- Tail recursive version
doubleList :: [Int] -> [Int]
doubleList xs = go [] xs
    where go result [] = result
          go result (x:xs) = go (result++[2*x]) xs
-- Here the direct version is much more efficient. The (:) operator works in constant time, whereas the (++) 
-- operator needs to walk the whole list, needing linear time. Thus the direct version uses linear time (O(n))
-- with respect to the length of the list, while the tail-recursive version is quadratic (O(n²))!

-- One might be tempted to fix this by using (:) in the tail-recursive version, 
-- but then the list would get generated in the reverse order. This could be fixed with an application of reverse,
-- but that would make the resulting function quite complicated.  

[f x | x <- lis, p x]
map f (filter p lis)
List comprehensions can do even more. You can iterate over multiple lists:

[ first ++ " " ++ last | first <- ["John", "Mary"], last <- ["Smith","Cooper"] ]
  ==> ["John Smith","John Cooper","Mary Smith","Mary Cooper"]
You can make local defitions:

[ reversed | word <- ["this","is","a","string"], let reversed = reverse word ]
  ==> ["siht","si","a","gnirts"]
You can even do pattern matching in list comprehensions!

firstLetters string = [ char | (char:_) <- words string ]
firstLetters "Hello World!"
  ==> "HW"
  
  (<+>) :: [Int] -> [Int] -> [Int]
xs <+> ys = zipWith (+) xs ys
(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

