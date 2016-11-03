module Redis (
    RType(..)
    , toRType
    , toRSString
    , toRBString
    , toRError
    , toRInt
    , fromRType
    , fromRSString
    , fromRError
    , fromRInt
    , fromRBString
    , fromRArr
) where

import qualified Data.List.Split as Sp

data RType = RSString String | RBString String | RError String | RInt Int | RBSNull | RArr [RType] deriving (Show, Eq)

delimiter = "\r\n"

toRType :: String -> RType
toRType ('+':xs) = toRSString xs
toRType ('-':xs) = toRError xs
toRType ('$':xs) = toRBString xs
toRType (':':xs) = toRInt xs
toRType ('*':xs) = toRArr xs
toRType _ = error "bad type"

toRSString :: String -> RType
toRSString s = RSString (head $ Sp.splitOn delimiter s)

toRBString :: String -> RType
toRBString s = let t = splitFirst s
                   l = read (fst t) :: Int
               in if (l < 0) then RBSNull else RBString (take l $ snd t)

toRError :: String -> RType
toRError s = RError (head $ Sp.splitOn delimiter s)

toRInt :: String -> RType
toRInt s = RInt (read (head $ Sp.splitOn delimiter s) :: Int)

toRArr :: String -> RType
toRArr s = RArr []

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

