module Redis (
    RType(..)
    , toRType
    , toRSString
    , toRBString
    , toRError
    , toRInt
    , toRArr
    , fromRType
    , fromRSString
    , fromRError
    , fromRInt
    , fromRBString
    , fromRArr
    , processCommand
) where

data RType = RSString String | RBString String | RError String | RInt Int | RBSNull | RArr [RType] deriving (Show, Eq)

-- TODO should avoid the use of `error`

delimiter = "\r\n"

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

splitFirst :: [Char] -> ([Char], [Char])
splitFirst s = (takeWhile (\x -> x /= '\r') s, dropUntil (\x -> x == '\n') s)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f (x:xs)
    | f x = xs
    | otherwise = dropUntil f xs


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

processCommand :: String -> String
processCommand s = interpretCommand $ (fst (toRType s))


interpretCommand :: RType -> String
interpretCommand c@(RArr xs) = "receive an arr" ++ delimiter
interpretCommand _ = "command should be an array " ++ delimiter

