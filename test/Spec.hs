import Test.Hspec
--import Test.QuickCheck
import Control.Exception (evaluate)
import Redis

-- TODO group by from and to + idempotent
-- TODO test errors
main :: IO ()
main = hspec $ do
  describe "Redis.toRType" $ do
    describe "Simple Strings" $ do

      it "toRType returns RSString for a Simple String" $ do
        toRType "+Hello\r\nWorld" `shouldBe` (RSString "Hello", "World")

      it "toRSString returns RSString" $ do
        toRSString "Hello\r\nWorld" `shouldBe` (RSString "Hello", "World")

      it "fromRType returns RSString as String for a Simple String" $ do
        fromRType (RSString "Hello") `shouldBe` "+Hello\r\n"

      it "fromRSString returns RSString as String" $ do
        fromRSString (RSString "Hello") `shouldBe` "Hello\r\n"

    describe "Errors" $ do

      it "toRType returns RError for Error" $ do
        toRType "-Error message\r\nleftover" `shouldBe` (RError "Error message", "leftover")

      it "toRSString returns RError" $ do
        toRError "ERR unknown command 'foobar'\r\nleftover" `shouldBe` (RError "ERR unknown command 'foobar'", "leftover")

      it "fromRType returns RError as String for Error" $ do
        fromRType (RError "Error message") `shouldBe` "-Error message\r\n"

      it "fromRSString returns RError as String" $ do
        fromRError (RError "Error message") `shouldBe` "Error message\r\n"

    describe "Integers" $ do

      it "toRType returns RSString for a Simple String" $ do
        toRType ":5\r\nWorld" `shouldBe` (RInt 5, "World")

      it "toRSString returns RSString" $ do
        toRInt "10\r\nWorld" `shouldBe` (RInt 10, "World")

      it "fromRType returns RInt as String for Int" $ do
        fromRType (RInt 10) `shouldBe` ":10\r\n"

      it "fromRSString returns RInt as String" $ do
        fromRInt (RInt 10) `shouldBe` "10\r\n"

    describe "Bulk Strings" $ do

      it "toRType returns RBString for a Bulk String" $ do
        toRType "$6\r\nfoobar\r\ntest" `shouldBe` (RBString "foobar", "test")

      it "toRSString returns RSString" $ do
        toRBString "6\r\nfoobar\r\ntest" `shouldBe` (RBString "foobar", "test")

      it "toRSString with 0 length returns empty RSString" $ do
        toRBString "0\r\n\r\ntest" `shouldBe` (RBString "", "test")

      it "toRSString with -1 length returns RBSNull" $ do
        toRBString "-1\r\n" `shouldBe` (RBSNull, "")

      it "fromRType returns RBString as String for Bulk String" $ do
        fromRType (RBString "foobar") `shouldBe` "$6\r\nfoobar\r\n"

      it "fromRBString returns RBString as String for Bulk String" $ do
        fromRBString RBSNull `shouldBe` "-1\r\n"

    describe "Arrays" $ do
      it "toRType returns RInt as String for Int with 0 elements" $ do
        toRType "*0\r\n" `shouldBe` (RArr [], "")

      it "toRArr RArr returns list a String with 0 elements" $ do
        toRArr "0\r\n" `shouldBe` (RArr [], "")

      it "toRType returns RInt as String for Int" $ do
        toRType "*3\r\n$5\r\nhello\r\n+world\r\n:1\r\n" `shouldBe` (RArr [RBString "hello", RSString "world", RInt 1 ], "")

      it "toRArr RArr returns list a String" $ do
        toRArr "3\r\n$5\r\nhello\r\n+world\r\n:1\r\n" `shouldBe` (RArr [RBString "hello", RSString "world", RInt 1 ], "")

      it "fromRType RArr returns RArr as a String" $ do
        fromRType (RArr [RBString "hello", RSString "world", RInt 1 ]) `shouldBe` "*3$5\r\nhello\r\n+world\r\n:1\r\n"

      it "fromRArr RArr returns list a String" $ do
        fromRArr [RBString "hello", RSString "world", RInt 1 ] `shouldBe` "$5\r\nhello\r\n+world\r\n:1\r\n"

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)

--    it "throws an exception if used with an empty list" $ do
--      evaluate (head []) `shouldThrow` anyException