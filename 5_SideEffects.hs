module IO where

import Control.Monad    (liftM, liftM2, mapM)
import Data.Char        (toUpper)
--import System.Directory ()
import System.IO        (Handle,IOMode (ReadMode), hGetLine, hIsEOF, withFile)
import System.IO.Error  (catchIOError)

{-{- Side Effects -}-}

{- Explicit effects 

Given lazy evaluation as a strategy, the moment of evaluation is not easy to predict and hence not a good trigger for side-effecting actions.
Even worse, it may be difficult to predict whether a term is evaluated at all.
We would like to keep equational reasoning, and allow compiler optimisations such as

    - strictness analyses := evaluating things earlier than needed if they will definitely be needed
        or
    - speculative evaluation := evaluating things even if they might not be needed at all

Explicit effects are a good idea:
We can see via the type of a program whether it is guaranteed to have no side effects, or whether it is allowed to use effects.
In principle, we can even make more fine-grained statements than just yes or no, by allowing just specific classes or effects.
Encourages a programming style that keeps as much as possible effect-free.
Makes it easier to test programs, or to tun them in a different context.

Evaluation vs. execution

data IO a --abstract type
The type of plans that perform some effects that in the end ultimately yield an 'a'.

    - Evaluation does not trigger the actual effects. It will at most evaluate the plan.
    - Execution triggers the actual effects. Executing a plan is not possible from within a Haskell program.

There is just one point in Haskell what actually executes something and this is:

main :: IO ()

This is the only thing when you run the programm will be executed. 
    - The entry point into the program is a plan to perform effects (a possibly rather complex one).
    - This is the one and only plan that actually gets executed.

-- unit type:
-- data () = () --special syntax
-- Constructor:
-- () == ()
-- A type with a single value (nullary tuple).
-- Often used to parameterize other types.
-- A plan for actions with no interesting result: IO ().

e.g. Hello Worl in Haskell:
module Main where

main :: IO ()
main = putStrLn "Hello World!"

putStrLn :: String -> IO () 
This functio has no interessting result it just produces a side effect, this is why it hase type IO ().

print :: Show a => a -> IO() -- for other types as long they implement Show

getLine :: IO String -- gets a String from the console

writeFile "test.txt" "Hello File!"
readFile "test,txt"

(>>) :: IO a -> IO b -> IO b
Function that takes to plans and constructs a plan that first executes the first plan, discard its result,
then executes the second plan and returns its result. 

E.g.:-}
getTwoLines :: IO String
getTwoLines = getLine >> getLine

--liftM :: (a -> b) -> IO a -> IO b
-- Takes a function and a plan. Cnstructs a plan that executes the given plan, but before returning the result, applies the function.
 --e.g.:
duplicateLine :: IO String
duplicateLine = fmap (\x -> x ++ x) getLine



-- toUpper 'a'
-- to convert a complete string we map toUpper over a string
shout :: IO String
shout = liftM (map toUpper) getLine

joinTwoLines1 :: IO String
joinTwoLines1 = liftM2 (flip(++)) getLine getLine

joinTwoLines2 :: IO String
joinTwoLines2 = (\x -> liftM2 (++) x x) getLine

{- Actions that depend on the results of earlier actions 
Bind:
(>>=)              :: IO a (a -> IO b) -> IO b
shout              :: IO String
putStrLn           :: String -> IO ()
shout >>= putStrLn :: IO()
-}
shoutBack :: IO ()
--shoutBack = shout >>= (\s -> putStrLn s)        -- here eta reduction can be done to
shoutBack = shout >>= putStrLn                    -- \ x -> f x === f       "this is called eta reduction"

shoutBackTwice :: IO ()
shoutBackTwice = shout >>= \s  -> putStrLn  s >> putStrLn s


{-
return :: a -> IO a
A plan that when executed, perform no effects and returns the given result.
    Intuitively, IO a says that we may use effects to obtain an a. We are not required to.
    On the other hand, a says that we must not use effects to obtain an a.

There is no function:
runIO :: IO a -> a -- this is forbidden in haskell if you have an IO then you have an IO.

(There is actually one, calles unsafePerformIO, but its use is generally not justified.)
-}

{-
But:
(>>=) :: IO a -> (a -> IO b) -> IO b

Bind gives us an option to temporerly have access to an a out of an IO a
It gives us access to the a that results from the first action.
But wraps it all immediatly up in another IO action. SO no chance to get it out.  


-}
liftM2' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2' f ioa iob = 
    ioa >>= \a -> 
    iob >>= \b -> 
    return (f a b)

 -- instead of writting this you can write more easily: 

liftM2'' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2'' f ioa iob = do 
    a <- ioa  
    b <- iob 
    return (f a b)

-- or even shorter:
liftM2''' :: (a -> b -> c) -> IO a -> IO b -> IO c
liftM2''' f ioa iob = do 
    a <- ioa  
    f a <$> iob 
    
-- Do notations: 
duplicateLine' :: IO String
duplicateLine' = do 
    x <- getLine
    return (x ++ x)

joinTwoLines2' :: IO String
joinTwoLines2' = do 
    x <- getLine
    y <- getLine
    return (y ++ x)  

{- Greeting program -}
greeting :: IO ()
greeting =
    putStrLn "What is your name?"       >>
    getLine                             >>= \ name ->
    putStrLn ("Hello, " ++ name ++ "!") >>
    putStrLn "Where do you live?"       >>
    getLine                             >>= \ loc ->
    let
        answer
            | loc == "Mongolia" = "Fantastic!"
            | loc == "Mexico"   = "Outstanding!"
            | otherwise         = "Sorry, don't know that."
    in
        putStrLn answer

{- Greeting in do notation: -}
greeting' :: IO ()
greeting' = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")
    putStrLn "Where do you live?"
    loc <- getLine
    let
        answer
            | loc == "Mongolia" = "Fantastic!"
            | loc == "Mexico"   = "Outstanding!"
            | otherwise         = "Sorry, I don't know that."
    
    putStrLn answer

{- In do notation you can leave the bind sequence and in away -}


{- Functional Programming with IO -}

-- Ask a question
ask :: String -> IO String
ask q = do
    putStrLn q
    getLine

askMany :: [String] -> IO [String]
askMany []  = return []
askMany (x : xs) = do
    a <- ask x
    as <- askMany xs
    return (a : as)

-- we cannot use map here as we would get the wrong result
-- askMany' :: [String] -> [IO String]
-- askMany' = map ask
-- we would get a list of actions not a list of strings 
--
-- But we can sequence a list of plans (defined in Prelude):
-- sequence :: [IO a] -> IO [a]
-- sequence []         = return []
-- sequence (x : xs)   = do
--    a <- x
--    as <- sequence xs
--    return $ a : as
--
-- We want to perform a list of actions collect all the results.
-- Possible with sequence:
askMany' :: [String] -> IO [String]
askMany' = sequence . map ask

-- or with mapM
askMany'' :: [String] -> IO [String]
askMany'' = mapM ask

--
--
{- Traversing a tree interactivley -}
data Interaction = 
        Question String Interaction Interaction
      | Result String

pick :: Interaction
pick = Question
        "Do you like FP?"
         (Question "Do you like static types?"
            (Result "Try haskell!")
            (Result "Try Clojure!"))
        (Question "Do you like dynamic types?"
            (Result "Try Python!")
            (Result "Try Rust!"))

ford :: Interaction
ford = Question 
        "Do you want a car?"
        (Question "Do you like it in black?"
            (Result "Good then!")
            ford)
        (Result "Never mind then!")


askQuestion :: String -> IO Bool
askQuestion q = do
    putStrLn (q ++ "[y/n]")
    yn <- getChar
    putStrLn ""
    return (yn `elem` "yY")

interaction :: Interaction -> IO String
interaction (Result r) = return r
interaction (Question q y n) = do
    ans <- askQuestion q
    if ans then interaction y
    else interaction n
    -- nicer: interaction $ if ans then y else n

-- Lets test our function in unit test without any IO
simulate :: Interaction -> [Bool] -> Maybe String
simulate (Result r) [] = Just r
simulate (Question _ y n) (b : bs) = simulate (if b then y else n) bs
simulate _                 _        = Nothing                                

-- Some more useful IO functions
-- hGetLine  :: Handle -> IO String
-- hPutStrLn :: Handle -> String -> IO ()
-- hIsEOF    :: Handle -> IO Bool

-- withFile :: FilePath -> IOMode 
--              -> (Hande -> IO r)  -- continuation (aka callback)
--              -> IO r

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- withFile is very useful it close the handle automatically and also has build in error handling

readFileLineByLine :: FilePath -> IO [String]
readFileLineByLine file = 
    withFile file ReadMode readFileHandle

readFileHandle :: Handle -> IO [String]
readFileHandle h = do
        eof <- hIsEOF h
        if eof
            then return []
            else do
                line <- hGetLine h
                lines <- readFileHandle h
                return (line : lines)

--
-- Warning: Both readFile and readFileLineByLine are actually problematic for dofferent reasons.
-- We will learn about better ways to process files later.
--
--
--
--
{- Exceptions -}
-- Exceptions in pure code (via error, missing patterns,..) are bad:
-- It is unclear when exactly, of if, they will be triggered,
-- It is therefore also unclear where or when to best handle them,
-- Explicitly handling failure via Maybe, Either or similar is almost always the better solution
--
-- Exceptions in effecful (IO) code are different: 
-- Execution order is explicit, and handling is easier.
-- There are many things that go wrong.
--
-- From System.IO.Error:
-- catchIOError :: IO a -> (IOError-> a) -> IO a
--
readFileLineByLine' :: FilePath -> IO (Maybe [String])
readFileLineByLine' file = 
    catchIOError 
        (Just <$> withFile file ReadMode readFileHandle) -- (fmap Just $ withFile file ReadMode readFileHandle)
        (const (return Nothing))
--
readFileLineByLine'' :: FilePath -> IO (Either IOError [String])
readFileLineByLine'' file = 
    catchIOError 
        (Right <$> withFile file ReadMode readFileHandle) -- (fmap Just $ withFile file ReadMode readFileHandle)
        (return . Left)
--
--
-- 