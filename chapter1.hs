-- A definition (of a function) can consist of multiple equations. 
-- The equations are matched in order against the arguments until a suitable one is found. 
-- This is called pattern matching.

greet :: String -> String -> String
greet "Finland" name = "Hei, " ++ name
greet "Italy"   name = "Ciao, " ++ name
greet "England" name = "How do you do, " ++ name
greet _         name = "Hello, " ++ name

Prelude> greet "Finland" "Pekka"
"Hei, Pekka"
Prelude> greet "England" "Bob"
"How do you do, Bob"
Prelude> greet "Italy" "Maria"
"Ciao, Maria"
Prelude> greet "Greenland" "Jan"
"Hello, Jan"

-- INDENTATION
i x = let y = x+x+x+x+x+x in div y 5

-- let and in are grouped together, an expression is split
j x = let y = x+x+x
              +x+x+x
      in div y 5

-- the definitions of a and b are grouped together
k = a + b
  where a = 1
        b = 1

l = a + b
  where
    a = 1
    b = 1