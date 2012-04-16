import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Monoid (mempty)
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit
import Data.ByteString.Char8 as BS
import Data
import Parser
import Serializer (encodeCommand)

socketHandler :: (MonadIO m) => Application m
socketHandler src snk = src $= CB.lines $= lineToEvent $= logEvent $= CL.map pickResponse $$ snk -- $= lineToEvent $$ undefined

pickResponse :: VimEventType -> ByteString
pickResponse StartupDone = encodeCommand (BufferID 1) (SequenceNum 1) $ EditFile "/tmp/test.txt"
pickResponse _ = BS.pack ""

lineToEvent :: (Monad i) => Conduit ByteString i VimEventType
lineToEvent = CL.map f where
    f line = case parseVimLine line of
        Left e                                -> InvalidEvent (show e)
        Right (EventMessage (VimEvent _ _ t)) -> t
        Right (ReplyMessage (VimReply _))     -> InvalidEvent "Actually, just got a reply..."

-- eventToByteString :: (Monad i) => Conduit VimEventType i ByteString
-- eventToByteString = CL.map (BS.pack . show)

--printAndDiscard :: Conduit ByteString IO ByteString
--printAndDiscard = NeedInput (\i -> liftIO (void $ BS.putStrLn i) >> printAndDiscard) mempty

logEvent :: (MonadIO m) => Conduit VimEventType m VimEventType
logEvent = NeedInput push close where
    push i = HaveOutput (liftIO (void $ print i) >> NeedInput push close) (return ()) i
    close  = mempty

--unlines' :: Monad m => Conduit ByteString m ByteString
--unlines' = CL.map $ \t -> t `BS.append` BS.pack "\n"

main :: IO ()
main = do
    Prelude.putStrLn "Starting server"
    runTCPServer (ServerSettings 3219 HostAny) socketHandler
