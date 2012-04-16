import Control.Monad.IO.Class
import Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit
import qualified Data.ByteString.Char8 as BS
import Data
import Parser
import Serializer (encodeCommand)

socketHandler :: (MonadIO m) => Application m
socketHandler src snk = src
                     $= CB.lines
                     $= lineToEvent
                     $= logItem
                     $= CL.map pickResponses
                     $= CL.map encodeResponses
                     $$ snk

pickResponses :: VimEventType -> [VimCommandType]
pickResponses StartupDone = [EditFile "/tmp/test.txt"]
pickResponses _ = []

encodeResponses :: [VimCommandType] -> BS.ByteString
encodeResponses = BS.concat . map mkCommand where
    mkCommand = encodeCommand (BufferID 1) (SequenceNum 1)

lineToEvent :: (Monad i) => Conduit BS.ByteString i VimEventType
lineToEvent = CL.map f where
    f line = case parseVimLine line of
        Left e                                -> InvalidEvent (show e)
        Right (EventMessage (VimEvent _ _ t)) -> t
        Right (ReplyMessage (VimReply _))     -> InvalidEvent "Actually, just got a reply..."

logItem :: (MonadIO m, Show a) => Conduit a m a
logItem = CL.mapM printAndReturn where printAndReturn ev = liftIO $ print ev >> return ev

main :: IO ()
main = do
    Prelude.putStrLn "Starting server"
    runTCPServer (ServerSettings 3219 HostAny) socketHandler
