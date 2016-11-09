module Redis where
-- I was exporting only some functions but some weren't found in Main...?
import Data.Char (toLower)
import qualified Data.Map as M

data RType = RSString String | RBString String | RError String | RInt Int | RBSNull | RArr [RType] deriving (Show, Eq)
type SKey = String
type Store = M.Map [Char] RType
data State = State {status :: String, store :: Store } deriving Show


initialState :: State
initialState = State {status="", store=M.fromList []}

-- TODO should avoid the use of `error`

-- helpers functions
delimiter = "\r\n"

splitFirst :: [Char] -> ([Char], [Char])
splitFirst s = (takeWhile (\x -> x /= '\r') s, dropUntil (\x -> x == '\n') s)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f (x:xs)
    | f x = xs
    | otherwise = dropUntil f xs


lower :: String -> String
lower  =  map toLower
-- end of helpers functions

toRType :: String -> (RType, String)
toRType ('+':xs) = toRSString xs
toRType ('-':xs) = toRError xs
toRType ('$':xs) = toRBString xs
toRType (':':xs) = toRInt xs
toRType ('*':xs) = toRArr xs
toRType _ = error "bad type"

toRSString :: String -> (RType, String)
toRSString s = fn $ splitFirst s
    where fn t = (RSString (fst t), (snd t))

toRBString :: String -> (RType, String)
toRBString s = let t = splitFirst s
                   l = read (fst t) :: Int
               in if (l < 0) then (RBSNull, snd t) else (RBString (take l $ snd t), drop (l + length delimiter) $ snd t)

toRError :: String -> (RType, String)
toRError s = fn $ splitFirst s
    where fn t = (RError (fst t), (snd t))

toRInt :: String -> (RType, String)
toRInt s = fn $ splitFirst s
    where fn t = (RInt (read (fst t) :: Int), (snd t))

toRArr :: String -> (RType, String)
toRArr s = let t = splitFirst s
               fn xs = let y = toRType xs
                       in fst y : if null $ snd y then [] else fn $ snd y
           in if (read (fst t) :: Int) == 0 then (RArr [], "") else (RArr (fn $ snd t), "")

fromRType :: RType -> String
fromRType s@(RSString _) = "+" ++ fromRSString s
fromRType s@(RError _) = "-" ++ fromRError s
fromRType s@(RBString _) = "$" ++ fromRBString s
fromRType s@(RInt _) = ":" ++ fromRInt s
fromRType (RArr xs) = "*" ++ (show $ length xs) ++ fromRArr xs
fromRType _ = error "bad type"

fromRSString :: RType -> String
fromRSString (RSString x) =  x ++ delimiter
fromRSString _ = error "type should be RSString"

fromRError :: RType -> String
fromRError (RError x) =  x ++ delimiter
fromRError _ = error "type should be RError"

fromRInt :: RType -> String
fromRInt (RInt x) =  show x ++ delimiter
fromRInt _ = error "type should be RInt"

fromRBString :: RType -> String
fromRBString (RBString x) =  (show $ length x) ++ delimiter ++ x ++ delimiter
fromRBString RBSNull = "-1" ++ delimiter
fromRBString _ = error "type should be RBString"

fromRArr :: [RType] -> String
fromRArr (x:xs) =  fromRType x ++ fromRArr xs
fromRArr [] =  []

processCommand :: String -> State -> IO State
processCommand str st = return $ checkCommand (fst (toRType str)) st

checkCommand :: RType -> State -> State
checkCommand c@(RArr xs) s = run xs s
checkCommand _ s = s {status="-command should be an array "}

run :: [RType] -> State -> State
run c@(RBString(y):ys) s
    | lower y == "set" = set c s
    | lower y == "get" = get c s
    | lower y == "del" = del c s
    | lower y == "exists" = exists c s
    | otherwise = s {status="-unknow command "}

exists :: [RType] -> State -> State
exists c s
    | length c < 2 = s {status="-(error) wrong number of arguments (given 0, expected 1)"}
    | otherwise = fn c
    where fn (x:RBString(k):_) = case (M.lookup k (store s)) of
                        Nothing -> s {status=":0"}
                        Just(_) -> s {status=":1"}

set :: [RType] -> State -> State
set c s
    | length c < 3 = s {status="+OK"}
    | otherwise = fn c
    where fn (x:RBString(k):v:_) = s {status="+OK", store=(M.insert k v (store s))}

get :: [RType] -> State -> State
get c s
    | length c < 2 = s {status="-(error) wrong number of arguments (given 0, expected 1)"}
    | otherwise = fn c
    where fn (x:RBString(k):_) = case (M.lookup k (store s)) of
                        Nothing -> s {status="+(nil)"}
                        Just(x) -> s {status=fromRType x}


del :: [RType] -> State -> State
del c s
    | length c < 2 = s {status="-(error) ERR wrong number of arguments for 'del' command"}
    | otherwise = fn c
    where fn (x:RBString(k):_) = case (M.lookup k (store s) >>= (\x -> return (M.delete k (store s)))) of
                                    Nothing -> s {status=":0"}
                                    Just(x) -> s {status=":1", store=x}

