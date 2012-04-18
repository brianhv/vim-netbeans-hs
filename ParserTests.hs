import Control.Monad
import Test.HUnit
import Data.ByteString.Char8 as BS
import Parser
import Data

parserTest :: String -> VimMessage -> Test
parserTest line expected = TestCase t where
    t = case parseVimLine (BS.pack line) of
        Left e  -> assertFailure $ show e
        Right v -> assertEqual "asdf" v expected

testFileOpen :: Test
testFileOpen = parserTest line expected where
    line     = "0:fileOpened=1 \"/tmp/foo.txt\" T F"
    expected = EventMessage $ VimEvent bufid seqno $ FileOpened "/tmp/foo.txt" True False
    bufid    = BufferID 0
    seqno    = SequenceNum 1

testStartupDone :: Test
testStartupDone = parserTest line expected where
    line     = "1:startupDone=2"
    expected = EventMessage $ VimEvent bufid seqno StartupDone
    bufid    = BufferID 1
    seqno    = SequenceNum 2

testBareResponse :: Test
testBareResponse = parserTest line expected where
    line     = "1"
    expected = ReplyMessage $ VimReply (SequenceNum 1)

tests :: Test
tests = TestList
    [ TestLabel "FileOpened" testFileOpen
    , TestLabel "StartupDone" testStartupDone
    , TestLabel "Bare response" testBareResponse
    ]

main :: IO ()
main = void $ runTestTT tests 
