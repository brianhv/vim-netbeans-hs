module Conduit
    ( vimApplication
    , flatten
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
                     $= lineToEvent
                     $= logItem
                     $= responder
                     $= encodeResponse
                     $= logItem
                     $$ snk

flatten :: (Monad m) => Conduit [a] m a
flatten = conduitState () push close where
    push _ xs = return $ StateProducing () xs
    close _   = return []

encodeResponse :: (Monad m) => Conduit IdeMessage m BS.ByteString
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
