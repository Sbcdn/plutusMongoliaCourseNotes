-- 1.2 Overview about Haskell

data Chain' = GenesisBlock' | Block' Chain' Txs'
type Txs' = Int 

--this a chain definition with an arbritary data type txs
data Chain txs = 
    GenesisBlock |          -- GenesisBlock :: Chain txs 
    Block (Chain txs) txs   -- Block :: Chain txs -> txs -> Chain txs
        deriving (Eq, Show) --, Foldable)   -- inherit basic datatypes to derive functionallity given by the bassic datatypes; 
                                            --Foldable needs a condition to work (not explained yet)

chain1 :: Chain Int
chain1 = Block GenesisBlock 2 -- "=" is a binding

chain2 :: Chain Int
chain2 = Block chain1 4

chain2' :: Chain Int
chain2' = Block (Block GenesisBlock 2) 4

chainLength :: Chain txs -> Int
chainLength GenesisBlock = 0
chainLength (Block c _) = chainLength c +1

hasBlock :: Txs' -> Chain' -> Bool
hasBlock x GenesisBlock' = False
hasBlock x (Block' c t)  = 
    x == t || hasBlock x c

-- Defining an own infix operator 
(|>) :: Chain txs -> txs -> Chain txs
(|>) = Block
infixl 5 |>

-- Defining chain with own operator
chain2'' :: Chain Int
chain2'' = GenesisBlock |> 2 |> 4 

-- haskell is type inference so it can determine many types on its own
chain3 = Block (Block ( Block (Block GenesisBlock 2)  4) 5) 6
chain3' = GenesisBlock |> 2 |> 4 |> 5 |> 6

---- 1.3 Higher order Functions ---

hasBlockProp :: (txs -> Bool) -> Chain txs -> Bool
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t) = 
    prop t || hasBlockProp prop c

-- same implementation as above just different style above style is "normal"
hasBlockProp' :: (txs -> Bool) -> Chain txs -> Bool
hasBlockProp' = \prop chain ->
    case chain of 
        GenesisBlock -> False
        Block c t -> prop t || hasBlockProp' prop c


-- \x -> x > 10 is a lambda function identified by \x; it is a inline function and can be interpretated for each x test x > 10 
-- has chain2 a block where the integer is greater than 10 
a = hasBlockProp (\x -> x > 10) chain2
-- this is egal to line 46
a' = hasBlockProp (>10) chain2
--another property check: is there even number in chain2
a'' = hasBlockProp even chain2
-- has Chain2 a value equal to 4
a''' = hasBlockProp (\x -> x == 4) chain2
--or
a'''' = hasBlockProp (4 ==) chain2

-- Explainations
-- A central point in Haskell is that functions follow the structure of their data
--
-- :: means "is of type"
--
-- [] empty list 
-- (:) is called cons, (:) :: a -> [a] -> [a]
-- Examples for lists: 
aList :: [Int]
aList   = 2: (1 : [])
--  all the same defintion:
aList'  = 2 : 1 : []
aList'' = [2,1]
--
-- A list of characters is called String
type Sting = [Char]
-- Single qoutes 'x' : 'y' : [] == ['x','y'] == "xy" are always a character and double quotes "xyz" are always a sequence of characters so a String
--
--  constrains => 
--
--Basic Type classes:
-- Eq           for types that support an equality test
-- Ord          for types that can be compared
-- Num          for numeric types
-- Fractional   for fractional numeric types
-- Show         for types that have a representation as string
-- Read         for types that can be parsed from strings
-- Enum         for types that can be enumerated
-- Bounded      for types that have a smallest and largest value
-- Foldable     for container types (Lists)
--
-- Some usefull functions:
-- elem :: (Eq a, Foldable t) => a -> t a -> Bool       is element a existing in list t a
-- any  :: Foldable t => (a->Bool) -> t a -> Bool       is any element existing in list t a which fullfills a particular property ( a -> Bool ) 
-- reverse :: [a] -> [a]                                reverses a list
-- (++) :: [a] -> [a] -> [a]                            concatonait two lists
-- filer :: (a -> Bool) -> [a] -> [a]                   filter on a given condition (a -> Bool) in list [a] and return a list fullfilling these condition
-- map :: (a -> b) -> [a] -> [b]                        takes a function from (a -> b) on a list [a] and applies it to all elements in the list and 
--                                                      returns the list [b] with the applied changes  e.g. map (*2) [1,2,3] = [2,4,6]; 
--                                                      map length [[1,2,3],[4,5,6,7],[8,9,10]] = [3,4,3]; 
--                                                      map reverse [[1,2,3],[4,5,6,7],[8,9,10]] = [[3,2,1],[7,6,5,4],[10,9,8]]
--id :: a -> a                                          identity funcition returns what evver is feeded in
--const :: a -> b -> a                                  const function takes to arguments and always return its first argument e.g.
--                                                       map (const 3) [[1,2,3],[4,5,6,7],[8,9,10]] = [3,3,3]
--show :: Show a => a -> String                         turns everything which is in a show class (or inherits) to a String
--
-- readFile :: FilePath -> IO String                    reads a file from disc by paarameter file (which is a String) and returns the content of a file 
-- type FilePath = String
-- As readFile does return an IO String it has side effects and so the fileLength can not be determined by: 
--      lengthOfFile file = 
--          length (readFile file) -- this gives a type error
-- But you can do: 
--      lengthOfFile :: FilePath -> IO Int      --  type of result will be IO Int as IO "remarks side effects" once you are an IO you stay and IO 
--      lengthOfFile file = 
--          length = <$> readFile file
-- 
-- IO Operators
-- (<$>)    :: (a -> b) -> IO a -> IO b                     is the map type for IO
-- (>>)     :: IO a -> IO b -> IO b                         
-- (<*>)    :: IO (a -> b) -> IO a -> IO b                  
-- (>>=)    :: IO a -> (a -> IO b) -> IO b                  bind
--
-- Function results depend only on their inputs == Functions have no side effects
-- Side effects are expressed by the IO type:
-- f :: Int -> Int    -- result depends only on argument, the function is a "pure" means the function is completly deterministic
-- g :: Int -> IO Int   -- result is not an Int, but an action
--
-- Lazy evaluation:
-- build is function which takes n as an argument and builds an chain of the length n putting n in the current block and recursivly calling build with n-1
-- 
build :: Int -> Chain Int
build n = 
    if n <= 0
        then GenesisBlock
        else Block (build (n - 1)) n
--
-- chainLength (build 10000000)
-- 10000000                             -- the answer is obvious but it will take while until execution of build ends as the whole recusrion has to be done
--
-- hasBlockProp even (build 10000000) 
-- True                                 -- This results in True nearly imidiatly as of lazy evaluation, 
--                                         the first block which contains 10000000 is checked directly as this is a even number the execution stops and
--                                         and the result "True" is returned without checking for the other values. This is lazy evaluation.
--                                         Haskell will always try to evaluate the outter most expression first if possible. 

--
--

 