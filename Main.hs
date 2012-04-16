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
                     $= flatten
                     $= encodeResponse
                     $$ snk

pickResponses :: VimEventType -> [VimCommandType]
pickResponses StartupDone = [EditFile "/tmp/test.txt"]
pickResponses _ = []

flatten :: (Monad m) => Conduit [a] m a
flatten = conduitState () push close where
    push _ xs = return $ StateProducing () xs
    close _   = return []

encodeResponse :: (Monad m) => Conduit VimCommandType m BS.ByteString
encodeResponse = conduitState 1 push close where
    push state input = return $ StateProducing (state + 1) [encodeCommand (BufferID 1) (SequenceNum state) input]
    close _ = return []

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
