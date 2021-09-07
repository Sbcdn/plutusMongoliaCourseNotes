{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

import Data.Foldable



{- Higher order Functions -}

-- A function parameterized by another function or returning a function is called a higher-order function
-- Strictly speaking, every curried function in Haskell is a function returning another function:
-- elem :: Eq a => a -> ([a]->Bool)
-- takes two arguments e.g      elem 'a' "Haskell"      and returns     True
-- strictly it takes a function as second argument 
-- :t elem 'a' :: Foldable t => t char -> Bool
-- :t elem 'a' "Haskell" :: Bool
-- Two of the most useful list functions are higher-order, as they each take a function as an argument:
-- filer    :: (a -> Bool) -> [a] -> [a]    -- filter takes a property on a's from a -> Bool and gets a list of [a]'s and 
                                            -- returns all those elements which satisfy the property e.g.:
a = filter even [1..10]
-- result = [2,4,6,8,10]
--
--The use of a function a -> Bool to express a predicate is generally common in Haskell.
--
-- map      :: (a -> b) -> [a] -> [b]       -- map is function which takes a function as first argument to transform a -> b 
                                            -- and as second argument a list of [a]'s and applies the functions to its elemnts to get list of  [b]'s
                                            -- e.g.:
b = map odd [1..4] 
-- result: [True,False,True,False]
c = map (*2) [1..4]
-- result: [2,4,6,8]
--Mapping a function over a data structure is an operation that is not limited to lists.

-- One of the most ubiquitous higher-order functions is function composition, function composition is used infix
-- (.) :: (... -> ...) -> (... -> ...) -> ... -> ... 
-- (f . g) x = f (g x)
-- It is a curried function taking three arguents f,g and x
-- Both f and g are applied to something, so they must be functions
-- No requirements seems to be made about the type of x, except that its passed to g, so letś assume a type variable here, 
-- which then should be the source type of g as well.
-- The target type of g shall match the source type of f, the target type of f is also the type of the overall result.
-- Which leads to: 
-- (.) :: (b -> c) -> (a -> b) -> a -> c  -- or to make the composition of two functions more obvious
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

{-      Pictrue from Lars slides
                f           g
        c <--------- b <--------a
         \                     /  
          \<------<-------<---/
                  f . g
-}
-- e.g.:
d = ((*3) . (*2)) 10
ex :: [Int]
ex = (take 100 . filter odd . map (\x -> x*x))[1 ..] 
-- This creates a list of integers with the first 100 odd square numbers


{- Composition as a design pattern -}

-- -Function composition gives you a way to split one programming problem into serveral, 
--  possibly smaller, programming problems
--
-- -In general, higher-order functions are part of your toolbox for attacking programming problems. 
--  Recognizing something as a map or filter is also useful.
--
-- -Of course, you should never forget the standard design principle 
--  of following the datatype structure as a food way of defining most functions, if applying a higher order function fails 
--
-- Function composition is abit like the functional semicolon. It allows us to decompose larger tasks into smaller ones.
-- Lazy evaluation allows us to seperate the generation of possible results from selecting interesting results. This
-- allows more modular programs in many situations.
-- Partial application and anonymous funstions help to keep such composition chains concise.
-- 
-- In Haskell it is often possible to do an approach which allows you to follow a generation an chekc approach.
-- This means you generate a infinite or big number of solutions and by chekcing /filtering you narrow it down to what you need.
-- This is possible due to the lazieness of Haskell, which would not be possible in strict languages. 
--
-- Also make use of lambda expressions (anonymous functions) to split programing problems into smaller problems and compose them. 


--------------------
{- flip -}

--  Flips the arguments of a two argument curried function
-- flip :: (a-> b -> c) -> (b -> a -> c)
-- flip f x y = f y x
-- Example:
foreach = flip map
e = foreach [1,2,3] (\x -> x*x)



{- zip -}

-- Takes to lists and creates a list of pairs, stops when one of the lists is exhausted
-- :t zip
-- zip :: [a] -> [b] -> [(a,b)]
--
-- Good example for curry / uncurry:
--
-- zip [1..10] [100,110..200]
-- just generate a list of pairs 
-- you want to add the list of pairs for some reason
-- map (+) f 
-- would be nice but will not work as (+) is a -> a -> a
-- we help us with uncurry
f = map (uncurry (+)) (zip [1..10] [100,110..200])



-------
{- ($) -}

-- The $ function is quite usefull and one of Lars favorites :-)
-- :t ($)
-- ($) :: (a -> b) -> a -> b 
-- infixr 0 $

-- ($) f a = f a
-- Infix: f $ a = f a
-- The dollar is nothing else as applicate functions, but why is this usefull?
-- You can use it to get rid of parentesis so instead of writting:
f' = map (uncurry (+)) (zip [1..10] [100,110..200])
-- We can also do
f'' = map (uncurry (+)) $ zip [1..10] [100,110..200]
-- which in the end says apply the next function first, in our case the zip function
--
-- Fot the case of function composition its also very handy, originally we had:
-- ex = (take 100 . filter odd . map (\x -> x*x))[1 ..] 
-- This is the same as:
ex' = take 100 $ filter odd $ map (\x -> x*x) [1 ..] 


{- Capturing design patterns -}

-- One of the strenghts of Haskellś flexibility with functions is that
-- they realy allow to abstract from reoccuring patterns and thereby save code.
-- (And by that support lazy programmers)
--
-- The standard design principle for lists we have been using all the works as follows:
-- fun :: [someType] -> someResult
-- fun []       =  ...     -- code
-- fun (x : xs) =  ...     -- code that can use x and recusrivly call fun xs
--
-- There are two interesting positions (the ...) where we have to fill in situation-specific code. Lets abstract:
-- 
-- At first we give names to the cases that correspond to the constructors.
-- The case 'cons' can use x and a fun xs, so we turn it into a function.
-- 
-- fun :: [someType] -> someResult
-- fun []       = nil
-- fun (x : xs) = cons x (fun xs)
-- 
-- At the moment, this is not a valid function, becuase 'nil' and 'cons' come out of nowhere - but we can turn them into parameters of fun. 
-- We now have to look at the types of 'cons' and 'nil':
-- 'nil' is used as a result, so nil :: someResuls;
-- 'cons' takes a list elements and a result to a a result, so:
-- cons :: someType -> someResult -> someResult.
--
-- fun :: (someType -> someResult -> someResult) 
--        -> someResult 
--        -> [someType] -> someResult
-- fun []       = nil
-- fun (x : xs) = cons x (fun cons nil xs)
--
-- we can give shorter names to 'someType' and 'someResult'
--
-- fun :: (a -> r -> r) -> r -> [a] -> r
-- fun []       = nil
-- fun (x : xs) = cons x (fun cons nil xs)
-- 
-- This function is not called 'fun' but 'foldr' for fold to right. 
-- we can rewrite fun with 'where' to a slight different form:
--
-- foldr :: (a -> r -> r) -> r -> [a] -> r
-- foldr cons nil = go
--      where
--        go []       = nil
--        go (x : xs) = cons x (go xs)
--
-- The arguments 'cons' and 'nil' never change while traversing the list, so we can just refer to them in the local definition go,
-- without explicitly passing them around.
--
-- The 'where' clause allows you to definde local values after the 'where'. Here we defined a local function go.
-- 
-- lets use it on the length function as an example: 
-- classic definition: 
-- length :: [a] -> Int
-- length []            = 0
-- length (x : xs)      = 1 + length xs
--
-- with foldr:
-- length = foldr (\x r -> 1 + r) 0
-- or even shorter with const
-- length = foldr (const (1 +)) 0
-- (To remember: (const: a -> b -> a) 'const' is a function of two arguments which ignores the first argument, 
--  as we don't care about the head of the list (x) we can use it here)
--
-- Second example: (++)
-- (++) :: [a] -> [a] -> [a]
-- (++) xs ys = foldr (:) ys xs
--
-- Third example: 
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p = foldr (\ x r -> if p x then x : r else r) []
--
-- An last but not least:
-- map :: (a -> b) -> [a] -> [b]
-- map f == foldr (\x r -> f x : r)
--
-- As shown foldr is very useful and helps shorten things
-- There are more examples, e.g. 'and' & 'any'
-- and :: [Bool] -> Bool
-- and = foldr (&&) True
--
-- any :: (a -> Bool) -> [a] -> Bool
-- any p = foldr (\ x r -> p x || r) False
--
-- When a list function is easy to express using 'foldr', then you should do it.
-- It makes it immediately recognizable for the reader that it follows the standard design principle.
-- Some functions can be expressed using foldr, but that does not neccessarily make them any clearer. In such cases, aim for clarity.
-- An example for a complicated function to express with 'foldr' is 'take'. 



{- Accumulating parameter pattern -}

-- The idea is to have an accumulator (acc) which is accumulating your intermediate results and then go through the list from left to right 
-- and update the accumulator accordingly. 
-- Example sum :
--
sum :: Num a => [a] -> a
sum = go 0                      -- 0 is the initial value for the accumulator, in the case of an empty list it is returned. 
        where
                go :: Num a => a -> [a] -> a
                go acc []       = acc
                go acc (x : xs) = go (x + acc) xs
-- The function sum takes a list and adds the head of the list to the accumulator if there is the empty list the accumulator is returned.
-- This gives the sum of a list. The constrain is that the list need to contain elements of datatype Num. (Otherwise sum them would make no sense)

--
-- Example reverse:
reverse :: [a] -> [a]
reverse = go []
        where
                go :: [a] -> [a] -> [a]
                go acc []       = acc
                go acc (x : xs) = go (x : acc) xs
-- in this case the accumulator is a list so we start with the empty list [] is initial value for the go function.
-- then we add the head of the list parameter to the accumulator and call go again with the tail of the list.
-- The end result is the reversed list (as we prepend all heads (x) to acc with cons operator(:) )

-- The pattern is easy to recognize, the only difference between sum and reverse are the initial value (0 or []) and the recursive functions
-- (x + acc) or (x : acc). The rest is the same. 
--
-- How we can abstract the general pattern now? 
-- 1. Write the pattern down
-- fun :: [a] -> r
-- fun = go ...
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = go (... acc ... x ...)  xs
--
-- 2. Give the holes names
-- fun :: [a] -> r
-- fun = go e
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = go (op acc x)  xs
--
--3. Figure out the types
-- fun :: (r -> a -> r) -> r -> [a] -> r
-- fun op e = go e
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = go (op acc x)  xs
--
-- This gives us the 'foldl' function
--
-- Compare foldr and foldl on the summation example as this works for both because addition is assoziativ:
--
-- foldr (+) 0 [1,2,3] = 1 + (2 + (3 + 0))
-- foldl (+) 0 [1,2,3] = ((0 + 1) + 2) + 3
--
-- Even when the result is the same there are differences in perfromance:
-- The accumulator is a 'thunk', an unevaluated expression, growing larger and larger with its number of elements due to haskells laziness!
-- Haskell will not do the calculations until the end where you want to see the calculation even if we have accumulator variable,
-- this takes much more memory as just processing the addition step by step.
-- How to solve this? The accumulator shoudl be updated in each step.
-- 
-- The solution is the 'seq' primitiv, a build in function in the Haskell language to achive this behaviour.
-- 
-- It is defined:
-- seq :: a -> b -> b, which evaluates a if seq a b is evaluated and returns b.
-- The definition is similar to 'const' but actually it has an important side effect, whenever you want to evaluate the b it first evaluates the a.
-- By this you can tell Haskell to evaluate stuff directly and not being lazy.
-- 
-- Chech foldl again:
-- we want to execute 'go (op acc x)' directly to be efficient.
-- fun :: (r -> a -> r) -> r -> [a] -> r
-- fun op e = go e
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = go (op acc x)  xs
--
-- first we write foldl with let ... in but don't change functionallity
--
-- fun op e = go e
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = let acc' = op acc x
--                                in go acc' xs
--
-- Now we can use seq:
--
-- fun op e = go e
--      where 
--          go acc []           = acc
--          go acc (x : xs)     = let acc' = op acc x
--                                in acc' `seq` go acc' xs
-- So now acc' will be evaluated before we do the recursive call on go so we do not get a long chain of calculations anymore.
--
-- To shorten this down there is a language extension called 'BangPatterns', using this we can write:
-- activate Language extension on top of document:  {-# LANGUAGE BangPatterns #-}
foldl' :: (r->a->r) -> r -> [a] -> r
foldl' op e = go e
        where
          go acc []     = acc
          go acc (x:xs) = let !acc' = op acc x
                          in go acc' xs

-- it is also possible to write the following and avoid to write the 'e' explicit because it is applied to both sides in front and after the '=' 
--foldl' :: (r->a->r) -> r -> [a] -> r
--foldl' op = go
--        where
--         go acc []     = acc
--          go acc (x:xs) = let !acc' = op acc x
--                          in go acc' xs
--
-- The BangPattern is calles "Pattern" because it is always possible to use it everywhere were you have a pattern of some kind.
-- The ! (Bang) forces in the fold' function the execution of acc' directly. 
-- In the most cases you will use foldl' instead of foldl but there can be a cases where foldl is more efficient as fold' but this is very rarely.
-- The foldl' in Data.List is implemented different and highly optimized.

{- Foldable -}

-- The functions described above are not just applicatable to lists but also make sense in different type classes e.g. in trees.
-- Therefore Haskell implements the 'Foldable' type class.
-- Folable takes type constructors as parameter which is different as the stuff we seen until now:
-- 
-- class Foldable t where
-- foldr    :: (a -> b -> b) -> b -> t a -> b
-- foldl'   :: (b -> a -> b) -> b -> t a -> b
-- toList   :: t a -> [a]
-- null     :: t a -> Bool
-- length   :: t a -> Int
-- elem     :: Eq a => a -> t a -> Bool
-- maximum  :: Ord a => t a -> a
-- product  :: Num a => t a -> a
-- ... and many more
--
-- Foldable abstarcts over a parameterized type t
--
-- You can think of Foldbale t of a type which can be converted to a list.
-- So t is a kind of container. If so a "t a" would be a container of "a's"
-- 
-- Many types are instances of Foldable so also 'Maybe', this also means that all functions working for lists are working on Maybes
-- You can think of a Maybe as a list which contains "Just" one element or no element so "Nothing".
--
-- Pitfalls on Foldables:
-- Attention a pitfall is on pairs which are also an instance of Foldable but just the second argument of the pair!
-- So e.g. sum (3,4) is actually not returning 7 but 4 and toList (3, 4) returns [4]
-- 
-- 'Either' is similar to pairs but it just sees the Right argument.
-- length (Right 3) returns 1
-- length (left 3) returns 0


-- {- Functor -}

-- Take tree type from earlier as example: 
-- data Tree a = Leaf a | Node (Tree a) (Node Tree a)
-- mapTree :: (a -> b) -> Tree a -> Tree b 
-- mapTree f (Leaf x) = Leaf (f x)
-- maptree f (Node l r) = Node (mapTree f l) (mapTree f l) 
--
-- for Maybe:
-- data Maybe a = Nothing | Just a
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapMaybe f Nothing = Nothing
-- mapMaybe f (Just x) = Just (f x)
--
-- A we see here is a pattern again!
-- This pattern for mapping is called 'Functor'
-- 
-- The Functor class also abstracts over a parameterizerd type as Fodlable does.
--
-- class Functor f where
-- fmap :: (a -> b) -> f a -> f b
--
-- Instances of this class are:
-- Lists:
-- instance Functor [] where
--    fmap = map
--
-- our example Tree:
-- instance Functor Tree where
--    fmap = mapTree
--
-- Maybe:
-- instance Functor Maybe where
--     fmap = mapMaybe
--
-- There is also an infix operator defined for the Functor and is just a different name:
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- f <$> x = fmap f x 
--
-- Functor laws:
-- Instances of the Functor class should obey the following laws:
--  1.    fmap id == id
--  2.    fmap (f . g) == fmap f . fmap g
--  
-- The Haskell type system is not strong enough to actually enforce these laws, 
-- but you should nevertheless avoid wrtiting an instance where the laws do not hold.
-- Such an instance would confuse users of your instance.
-- 
-- {- Deriving from Functor and Foldable -}
--
-- Class instances for Functor and Foldable (and a few other classes) can be derived via language extension
-- {-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
-- e.g.:
data Tree' a = Leaf a | Node (Tree' a) (Tree' a)
      deriving (Show, Eq, Functor, Foldable)

-- now the following is possible:
i = length (Node (Leaf 3) (Leaf 4))
-- returns: 2
j = (+1) <$> Node (Leaf 3) (Leaf 4)
-- returns: Node (Leaf 4) (Leaf 5)


{- More Catamorphism (Standard Design Pattern) -}

--
-- 'if ... then ... else...' is the catamorphism for bool
--
-- function 'maybe' (defined in Data.Maybe) is the catamorphism for Maybe
--
-- Catamorphism for  Tree:
-- fun :: (a->r) -> (r -> r -> r)) -> Tree a -> r
-- fun leaf noce = go
--      where 
--      go (Leaf x)   = leaf x
--      go (Node l r) = node (go l) (go r)
--
-- By now, you are hopefully convinced that every (algebraic) datatype comes with its own "standard design pattern function" or 'recursive scheme'.
-- Functions defined this way are called catamorphisms. 
-- For the most common Haskell types, these higher order functions are often known under different name e.g.(foldr, maybe,...)
-- It is actually possible (using higher rank polymorphism) to define algebraic datatypes by their recursion schemes,
-- so in a sense, the type [] is completely determined by foldr, Maybe by maybe etc.
-- 
--  
-- {- Packaging and Tooling -}
-- look video
-- module <Module-Name> (<export-list>) where
-- import Prelude hiding (<hiding list>)
-- ...
--
-- import / export lists
-- 
-- {- Sort -}
-- Tree Merge Sort
--
-- sort :: Ord a => Tree a -> [a]
-- sort (Leaf x)     = [x]
-- sort (Node l r)   = merge (sort l) (sort r)

-- merge :: Ord a => [a] -> [a] -> [a]
-- merge []        ys = ys
-- merge (x : xs)  [] = x : xs
-- merge (x : xs) (y : ys)
--      | x <= y =      = x : merge      xs (y : ys)
--      | otherwise     = y : merge (x : xs)

------------
--
-- {- Cabal -}
-- 
-- cabal --help
-- 
