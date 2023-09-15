data Report = ConstructReport Int String String
This is how you create a report:

Prelude> :t ConstructReport 1 "Title" "This is the body."
ConstructReport 1 "Title" "This is the body." :: Report
You can access the fields with pattern matching:

reportContents :: Report -> String
reportContents (ConstructReport id title contents) = contents
setReportContents :: String -> Report -> Report
setReportContents contents (ConstructReport id title _contents) = ConstructReport id title contents

Here’s our first own parameterized type Described. The values of type Described a contain a value of type a and a String description.

data Described a = Describe a String

getValue :: Described a -> a
getValue (Describe x _) = x

getDescription :: Described a -> String
getDescription (Describe _ desc) = desc
Prelude> :t Describe
Describe :: a -> String -> Described a
Prelude> :t Describe True "This is true"
Describe True "This is true" :: Described Bool
Prelude> getValue (Describe 3 "a number")
3
Prelude> getDescription (Describe 3 "a number")
"a number"



data IntList = Empty | Node Int IntList
  deriving Show

ihead :: IntList -> Int
ihead (Node i _) = i

itail :: IntList -> IntList
itail (Node _ t) = t

ilength :: IntList -> Int
ilength Empty = 0
ilength (Node _ t) = 1 + ilength t


Prelude> ihead (Node 3 (Node 5 (Node 4 Empty)))
3
Prelude> itail (Node 3 (Node 5 (Node 4 Empty)))
Node 5 (Node 4 Empty)
Prelude> ilength (Node 3 (Node 5 (Node 4 Empty)))
3


data List a = Empty | Node a (List a)
  deriving Show

lhead :: List a -> a
lhead (Node h _) = h

ltail :: List a -> List a
ltail (Node _ t) = t

lnull :: List a -> Bool
lnull Empty = True
lnull _     = False

llength :: List a -> Int
llength Empty = 0
llength (Node _ t) = 1 + llength t
Prelude> lhead (Node True Empty)
True
Prelude> ltail (Node True (Node False Empty))
Node False Empty
Prelude> lnull Empty
True

data Tree a = Node a (Tree a) (Tree a) | Empty

example :: Tree Int
example = (Node 0 (Node 1 (Node 2 Empty Empty)
                          (Node 3 Empty Empty))
                  (Node 4 Empty Empty))


treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)
treeHeight Empty ==> 0
treeHeight (Node 2 Empty Empty)
  ==> 1 + max (treeHeight Empty) (treeHeight Empty)
  ==> 1 + max 0 0
  ==> 1
treeHeight (Node 1 Empty (Node 2 Empty Empty))
  ==> 1 + max (treeHeight Empty) (treeHeight (Node 2 Empty Empty))
  ==> 1 + max 0 1
  ==> 2
treeHeight (Node 0 (Node 1 Empty (Node 2 Empty Empty)) Empty)
  ==> 1 + max (treeHeight (Node 1 Empty (Node 2 Empty Empty))) (treeHeight Empty)
  ==> 1 + max 2 0
  ==> 3


  lookup :: Int -> Tree Int -> Bool
lookup x Empty = False
lookup x (Node y l r)
  | x < y = lookup x l
  | x > y = lookup x r
  | otherwise = True

insert :: Int -> Tree Int -> Tree Int
insert x Empty = Node x Empty Empty
insert x (Node y l r)
  | x < y = Node y (insert x l) r
  | x > y = Node y l (insert x r)
  | otherwise = Node y l r



  data Person = MkPerson String Int String String String deriving Show
-- A list of persons might look like the following:

people :: [Person]
people = [ MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"
         , MkPerson "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"
         , MkPerson "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
         ]
--Suppose that we need to find all engineers from Finland:

query :: [Person] -> [Person]
query [] = []
query (MkPerson name age town state profession):xs
  | state == "Finland" && profession == "Engineer" =
      (MkPerson name age town state profession) : query xs
  | otherwise = query xs
--Thus,

query people ==> [MkPerson "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"]

data Person = MkPerson { name :: String, age :: Int, town :: String, state :: String, profession :: String}
  deriving Show
-- e can still define values of Person normally, but the Show instance prints the field names for us:

Prelude> MkPerson "Jane Doe" 21 "Houston" "Texas" "Engineer"
MkPerson {name = "Jane Doe", age = 21, town = "Houston", state = "Texas", profession = "Engineer"}

MkPerson {name = "Jane Doe", town = "Houston", profession = "Engineer", state = "Texas", age = 21}

query :: [Person] -> [Person]
query []     = []
query (x:xs)
  | state x == "Finland" && profession x == "Engineer" = x : query xs
  | otherwise = query xs

  