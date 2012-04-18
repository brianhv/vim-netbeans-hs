module Conduit
    ( vimApplication
    , flatten
    , fromList
    , logItem
) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data
import Serializer
import Parser

vimApplication :: (MonadIO m) => Conduit VimEventType m IdeMessage -> Application m
vimApplication responder src snk = src
                     $= CB.lines
                     $= logItem "Raw event"
                     $= lineToEvent
                     $= logItem "Parsed event"
                     $= responder
                     $= encodeResponse
                     -- $= logItem
                     $$ snk

flatten :: (Monad m) => Conduit [a] m a
flatten = NeedInput push close where
    push = fromList
    close = Done Nothing ()

fromList :: Monad m => [a] -> Pipe i a m ()
fromList []     = Done Nothing ()
fromList (x:xs) = HaveOutput (fromList xs) (return ()) x

encodeResponse :: (Monad m) => Conduit IdeMessage m BS.ByteString
encodeResponse       = encode 1 where
    encode seqno     = NeedInput (push seqno) close
    push seqno input = HaveOutput (encode (seqno+1)) (return ()) $
                        encodeCommand (BufferID 1) (SequenceNum seqno) input 
    close            = Done Nothing ()

lineToEvent :: (Monad i) => Conduit BS.ByteString i VimEventType
lineToEvent = CL.map f where
    f line = case parseVimLine line of
        Left e                                -> InvalidEvent (show e)
        Right (EventMessage (VimEvent _ _ t)) -> t
        Right (ReplyMessage (VimReply _))     -> InvalidEvent "Actually, just got a reply..."

logItem :: (MonadIO m, Show a) => String -> Conduit a m a
logItem description = CL.mapM printAndReturn where
    printAndReturn ev = liftIO $ putStrLn (lineToPrint ev) >> return ev
    lineToPrint ev    = case description of
        "" -> show ev
        _  -> description ++ ": " ++ show ev
