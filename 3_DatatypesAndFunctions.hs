{-# LANGUAGE InstanceSigs #-} -- allows to use type signatures in instances for documentation reasons; normally this is not allowed
-- also an example hwo to add LANGUAGE extensions

-- Datatypes and functions - mongolian course 2.1.0
-- and Standard design pattern (catamorphism)
--
--
-- Datatypes
-- google: *module hackage , e.g. prelude hackage
-- :doc *identifier* in ghci gives you doc
import Text.Read
{- define own Bool Datatype
-- use nnot and BBool as Bool and not are defined in Prelude already -}
data BBool = BTrue | BFalse 
    deriving Show
-- Constructors:
--      False :: Bool
--      True :: Bool

-- Functions:                   -- Remember: functions follow the structure of their data
--      fun :: Bool -> ...
--      fun False = ...
--      fun True = ...

--Pattern-Matching:
-- Allowed pattern are:
--  - Saturated constructor applications to other patterns.
--  - Variables (match anything).
--  - Underscore / wildcard pattern (matches anything).
--  - Literals (numbers, characters, strigs, lists).
-- 
-- Patterns are matched in order.
-- In particukarm catch-all casses must come last
-- Best practice: split programming problems by expanding variables into all possible constructors for the type of the variable
-- For non-overlapping cases, the order does not matter

{- define own not -}
nnot :: BBool -> BBool
nnot BFalse = BTrue
nnot BTrue  = BFalse
{- Define a own || (or) function -}
(||-) :: Bool -> Bool -> Bool
(||-) False c = c                   -- prefix version
True ||- _ = True                   -- infix version

{- Define own if-then-else function -} 
ifthenelse :: Bool -> a -> a -> a   -- Polymorphic function so a can be any type
ifthenelse False _t e = e           -- if you want for documentation point out that variable is not evaluate you can put _ in front of the variable (e.g. _t), it is
ifthenelse True t _e = t            -- syntactically the same as just using _ but points out that the variable is not used in this case

{- Guards -}
-- define the same function in form of Guards
ifthenelse' :: Bool -> a -> a -> a
ifthenelse' c t e
    | c = t              -- the | is needed to identify Guards
    | otherwise = e      -- otherwise just represents True

-- Guards are tried one by one
-- Conditions all of type Bool
-- First guard that evaluates to True is chosen


{- Maybe Type -}
{- 
    data Maybe a = Nothing | Just a
Constructors:
    Nothing :: Maybe a
    Just    :: a -> Maybe a

Functions on Maybe:
    fun :: Maybe a -> ...
    fun Nothing  = ...
    fun (Just x) = ...
-}
-- Using a default value
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

-- Chaining optional values; the idea is if the first one contains a value we take the first one, if it doesn't we take the second one
orelse :: Maybe a -> Maybe a -> Maybe a
orelse Nothing y = y
orelse (Just x) _ = Just x

-- Add Maybes
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just(x + y)
addMaybes _  _ = Nothing   

liftMaybe :: (a-> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f (Just a) (Just b) = Just (f a b)
liftMaybe _     _ _ = Nothing

{- Pairs -}
{- 
data (a,b) = (a,b) 

Constructor:
(,) :: a -> b-> -> (a,b)

Functions: 
    fun :: (a,b) -> ...
    fun (a,b) = ...
-}
a :: (Bool, [Char])
a = (True, "Haskell")
a' = fst a 
a'' = snd a

swap :: (a, b) -> (b,a)
swap (a', b') = (b',a')
a''' = swap a

-- curry :: ((a,b)-> c) -> a -> b -> c
-- curry f a b = f (a,b)

foo :: (Int, Int) -> Int
foo (a, b) = a + b

-- uncurry :: (a -> b -> c) -> (a,b) -> c
-- uncurry f (a,b) = f a b

charcons :: Char -> String -> String
charcons a b = a : b



{- Lists -}
{-
data [a] = [] | a : [a] 

Constructors: 
    []  :: [a]
    (:) :: a -> [a] -> [a]

Functions: 
    fun :: [a] -> ...
    fun []       =
    fun (x : xs) = ...fun xs... -- as list is a recursive datatype you often have a recursiv function for the cons(:) case

{- Build in functions for lists: -}

-- length returns the length of list
length :: [a] -> Int
length []       = 0
length (x : xs) = 1 + length xs

-- List generally need to be of one datatype, a trick is to use the datatype Either which has a Left and Right parameter is can be used as follows:
list = [Left True, Right 'x', Left False, Right 'y' , Right 'z']

-- elem checks if the element is contained in the list, this can just be done for lists of types which deriving from Eq
elem :: Eq a => a -> [a] -> Bool
elem x []          = False
elem x (y : ys) = x == y || elem x ys

-- append two lists
(++) :: [a] -> [a] -> [a]
[]          ++ ys = ys
(x : xs) ++ ys =  x : (xs ++ ys)

-- reverse a list (there is a more performant way to do this so in standard library this is defined in another way)
reverse :: [a] -> [a]
reverse []          = []
reverse (x : xs) = reverse xs ++ [x]

-- filter a list
filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x : xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-}

{- Custom Datatypes -}



-- Modelling a look-up table
{-
type Table k v = [(k,v)]        -- the keyword "type" just gives a new name to existing datatypes its not constructing a new datatype
-- interface                    -- Remember: functional data structure are presistent, so tables are not changed they are always newly created
empty :: Table k v
empty = []

insert :: k -> v -> Table k v -> Table k v
insert k v t = (k,v) : t

delete :: Eq k => k -> Table k v -> Table k v
delete _ [] = []
delete k (t : ts)
    | k == fst t    =   delete k ts
    | otherwise     =   t : delete k ts

--Shorter version
delete' :: Eq k => k -> Table k v -> Table k v
delete' k = filter(\kv -> k /= fst kv)

lookup' :: Eq k => k -> Table k v -> Maybe v
lookup' _ [] = Nothing
lookup' k (t : ts)
    | k == fst t = Just (snd t)
    | otherwise = lookup' k ts
-}
{- Make it as own type -}
newtype Table' k v = 
    Table' [(k,v)]                          -- the keyword "newtype" creates a own type which has just one constructor (other than data); !!
         deriving (Show, Eq)                                   -- its also guaranteed to have the same representation as the wrapped type 
-- interface                                -- Remember: functional data structure are presistent, so tables are not changed they are always newly created
empty' :: Table' k v
empty' = Table' []

insert' :: k -> v -> Table' k v -> Table' k v
insert' k v (Table' t) = Table' ((k, v) : t)

delete'' :: Eq k => k -> Table' k v -> Table' k v
delete'' _ (Table' []) = Table' []
delete'' k (Table' (t : ts))
    | k == fst t    =   delete'' k (Table' ts)
    | otherwise     =   uncurry insert' t (delete'' k (Table' ts))

--Shorter version [ x | x <- xs, p x]
delete''' :: Eq k => k -> Table' k v -> Table' k v
delete''' k (Table' t) = Table' (filter (\ kv -> k /= fst kv) t)

lookup'' :: Eq k => k -> Table' k v -> Maybe v
lookup'' _ (Table' []) = Nothing
lookup'' k (Table' (t : ts))
    | k == fst t = Just (snd t)
    | otherwise = lookup'' k (Table' ts)


{- Binary Trees -}
--

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

{--
Constructors:
Leaf :: a -> Tree a
Node :: Tree a -> Tree a -> Tree a

Functions: 
fun :: Tree a -> 
fun (Leaf x) = 
fun(Node l r) = ...funl ... fun r...

Functiosn usually recurse twice

-}

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l r) = flatten l ++ flatten r

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = 1 + max (height l) (height r)

nodeNumber :: Tree a -> Int
nodeNumber (Leaf _) = 0
nodeNumber (Node l r) = 1 + nodeNumber l + nodeNumber r

leafNumber :: Tree a -> Int
leafNumber (Leaf _) = 1
leafNumber (Node l r) = 0 + leafNumber l +  leafNumber r

tree = Node (Node (Leaf "lars") (Leaf "torben")) (Node (Leaf "agata") (Node (Leaf "baloo") (Leaf "mulan")))

{- Expressions -}
data Expr = 
      Lit Int
    | Add Expr Expr
    | Neg Expr
    | IfZero Expr Expr Expr
    deriving Show

expr1 :: Expr
expr1 = IfZero (Add (Lit 3) (Neg(Lit 3))) (Lit 42) (Add (Lit 1) (Lit 2))
{- 
Constructors:
    Lit     :: Int -> Expr                              -- 
    Add     :: Expr -> Expr -> Expr                     -- doble recursive
    Neg     :: Expr -> Expr                             -- recursive
    IfZero  :: Expr -> Expr -> Expr -> Expr             -- triple recursive

Functions: 
    fun :: Expr -> ...
    fun (Lit x) = ...
    fun (Add x y) = ...
    fun (Neg x) =
    fun (If Zero x y z) = ...
 -}

eval:: Expr -> Int
eval (Lit x)        = x
eval (Add x y)      = eval x + eval y
eval (Neg x)        = (-1) * eval x
eval (IfZero x y z)  
                | eval x == 0    = eval y
                | otherwise      = eval z




---------------------------------------------------------------------------------------------------------
-- 3.1 Polymorphism and type classes
---------------------------------------------------------------------------------------------------------
-- Task 1. Implement *any* function 'f' of the type below 
-- Conclusion : There is a virually infinit number of functions of these type
f1 :: (Int,Int) -> (Int,Int)
f1 (n1,n2)= (n2,n1)

f2 :: (Int,Int) -> (Int,Int)
f2 (n1,n2) = (n1 + n2,0)

f3 :: (Int, Int) -> (Int, Int)
f3 (n1,n2) = (12, 2134124)

-----------------------------------------------------------------------
--Task 2. implement *any* function 'g' of the type below. (Not using undefined / loops in any way)

g:: (a,a) -> (a,a)
g  (a1,a2) = (a2,a1)

g1 :: (a,a) -> (a,a)
g1 (a1,a2) = (a1,a2)

g' :: (a,a) -> (a,a)        -- In the end just different implementation but not a different function as g
g' pair = g (g (g pair))

g2 :: (a,a) -> (a,a)
g2 (a1,_a2) = (a1,a1)

g3 :: (a,a) -> (a,a)
g3 (_a1,a2) = (a2,a2)

-- From here on you need two know which types you generally want to use, to be able to do a useful implementations. Without that these are the only four options.
----------------------------------------------------------------------
-- Task 3. Implement *any* functio 'h' of the type bewlow. (Not using undefined / loops in any way)
h :: (a,b) -> (b,a)
h (a1,b1) = (b1,a1)
-- This is the only implementation existing.
---------------------------------------------------------------------------------------------------------------------
-- map: (a->b) -> [a] -> [b]
-- given "i (*2) []" --> What is the result? It has to be the empty list as there is no type information of a's which are needed to produce b's
--
--
-- a "!" forces the compiler to directly evaluate an expression and ignore laziniess
--
--------------------------------------------------------------------------------------------------------------------------------------
-- Overloading  Philip Wadler - Theorems for free
----------------------------

exam :: (Eq a , Num r) => (a , r) -> (a, b) -> r
exam (x1,x2) (y1, y2) = 
    if x1 == y1
        then x2
        else 0

--------- Classes (not used very often in Haskel as classes and types are differnt things (but depend on each other))
class MyEq a where
    (===) :: a -> a -> Bool -- In the classs just a type signature is declared [class are not very often defined in haskel as the most usefull class es already existing]
    (===) x y = not (x =/= y)

    (=/=) :: a -> a -> Bool
    (=/=) x y = not (x === y)       -- Default definition for type signature, but ATTENTION as the definition depends on the first type signature and visa versa, 
    {-# MINIMAL (===) | (=/=) #-}   -- there is the danger to declare not anything at all, you get a deadloop then
                                    -- To prevent that you define a minimal pragma
                                    
                            
                             
{-
(=/=) :: MyEq a => a -> a -> Bool
x =/= y = not (x === y)
-}

instance MyEq Bool where
    (===) True True = True              -- an instance if a class is quite often used for own purpose implementations, this now just works for booleans so its not equl to Eq
    (===) False False = True            -- in the instance you define the implementation of the class's type signature
    (===) _ _ = False

instance MyEq a => MyEq [a] where
    (===) :: MyEq a => [a] -> [a] -> Bool         -- just for documentation we put type signature it is not neccessary (the Language extension in top of the document) and comment above
    (===)  [] [] =  True                          -- here there is our own implementation of Eq for all kinds of lists.
    (===)  [] (_ :_) =  False
    (===)  (_:_) [] =  False
    (===)  (x:xs) (y:ys) = x === y && xs === ys

-- make implementations on your own datatypes
data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
    deriving Eq

instance MyEq Weekday where
    Mo === Mo = True
    Tu === Tu = True
    We === We = True
    Th === Th = True
    Fr === Fr = True
    Sa === Sa = True
    Su === Su = True
    _ ===_ = False
-- or more easy in that case wd1 === wd2 = wd1 == wd2 as Eq is implemented.


{- Ord Class -}
-- :i Ord
-- Eq is a Superclass Ord, what means i cannot declare something to be an Ord if it is not instance of Eq
-- "class Eq a => Ord a where ..." 


{- Show Class -}
-- Parse Datatypes to a String to be able to print it
--
i = show "Ture"
{- Read Class-}
-- Parse Strings back to datatypes (where implemented)
k = read "True" :: Bool
l = read "1" :: Int
-- is not recommended to use as it is a function which crashes better use:
-- Text.Read which need to be imported but does readMaybe
m = readMaybe "True" :: Maybe Bool

--Errorinformation:  Ambigious type variables (in error messages) mean that the context given is not enought to dertermine what typeclass is intended / needed

{- Enum -}
-- :i Enum
n = [1..10]
-- gives: [1,2,3,4,5,6,7,8,9,10]
-- is equa to "enumFromTo 1 10"

{- Bounded -}
-- for types which have a smalles and largest value; sometimes quite usefull
o  = minBound :: Int
p = maxBound  :: Int
-- result: -9223372036854775808 for Int 
-- difference between Int and Integer : Integer is more than a 64 bit type to "-9223372036854775808 - 1" is possible on Integer but not on Int
-- Integer is generally slower as Int so if not needed use Int

{- Num class -}
-- :i Num
-- import Data.Ratio for rational numbers so you can do 1 % 4
-- 2.235256 :: Rational
-- result: 279407 % 125000

-- pi
-- e = exp 1
-- sin
-- cos
-- Integral

-- you cannot multiply int and double when they are bound to variables so you can do 2.5 * 2 but not d = 2.5 , i = 2 , d*i = error
-- Conversionfunctions
-- fromInteger
-- fromIntegral
-- fromDouble
-- floor    := round down to next full number
-- ceiling  := round up to next full number
-- round    := round to the next even number (when you are in the .5 case)

{- Defaulting -}
-- Kicks in for numerical classes and Haskell choose a numercial class which is not 100% declared e.g.
-- 235 can be of Int or Integer it is not declared when you just type it in ghci, when you put on warnings Haskell tells you 
-- :set -Wall := set Warnings

