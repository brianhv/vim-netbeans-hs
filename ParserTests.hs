import Test.HUnit
import Data.ByteString.Char8 as BS
import Parser
import Data

test1 :: Test
test1 = TestCase (assertEqual "File open event should parse correctly" expected actual) where
    expected = EventMessage $ VimEvent bufid seqno $ FileOpened "/tmp/foo.txt" True False
    --actual   = case parseVimLine (BS.pack "0:fileOpened=0 \"/tmp/foo.txt\" T F") of
    actual   = case parseVimLine test1line of
        Left _  -> undefined
        Right v -> v
    bufid = BufferID 1
    seqno = SequenceNum 1

test1line :: ByteString
test1line = BS.pack "0:startupDone=1"

main :: IO ()
main = do
    print test1line
    _ <- runTestTT test1
    return ()
