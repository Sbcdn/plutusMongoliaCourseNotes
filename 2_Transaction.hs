
type Table k v = [(k,v)]          -- the keyword "type" just gives a new name to existing datatypes its not constructing a new datatype

-- interface                            -- Remember: functional data structure are presistent, so tables are not changed they are always newly created
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
delete' k = filter (\kv -> k /= fst kv)

lookup' :: Eq k => k -> Table k v -> Maybe v
lookup' _ [] = Nothing
lookup' k (t : ts)
    | k == fst t = Just (snd t)
    | otherwise = lookup' k ts

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x

data Transaction = 
    Transaction 
    { trAmount  ::  Amount, 
      trFrom    ::  Account,
      trTo      ::  Account
    }
    deriving (Eq, Show)
type Amount = Int
type Account = String

{- Constructor -}
-- Transaction :: Amount -> Account -> Account -> Transaction

{- Functions on Transaction -} -- can be constructed automatically see above type definition
--fun :: Transaction -> ...
--fun (Transaction amount from to) = ...
{-
trAmount :: Transaction -> Amount
trAmount (Transaction amount _ _) = amount

trFrom :: Transaction -> Account
trFrom (Transaction _ from _) = from

trTo:: Transaction -> Account
trTo (Transaction _ _ to) = to
-}

type Accounts = Table Account Amount
processTransaction ::
    Transaction -> Accounts -> Accounts
processTransaction (Transaction amount f t) as = 
    let 
        fOld = fromMaybe 0 (lookup f  as)
        tOld = fromMaybe 0 (lookup t as)
    in
        insert f (fOld - amount)
            (insert t (tOld + amount) as) 
 