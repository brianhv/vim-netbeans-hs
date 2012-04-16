module Serializer (encodeCommand) where

import Data

encodeCommand :: BufferID -> SequenceNum -> VimCommandType -> String
encodeCommand bufferID sequenceNumber command = buf ++ ":" ++ cmd ++ "!" ++ seqno ++ args ++ "\n" where
    buf   = show bufferID
    cmd   = commandTypeString command
    seqno = show sequenceNumber
    args  = encodeCommandArgs command

encodeStringArg :: String -> String
encodeStringArg arg = " \"" ++ arg ++ "\""

encodeIntArg :: Int -> String
encodeIntArg arg = ' ' : show arg

encodeBoolArg :: Bool -> String
encodeBoolArg True  = " T"
encodeBoolArg False = " F"

-- Can this be refactored using Data.Data?
encodeCommandArgs :: VimCommandType -> String
encodeCommandArgs (AddAnno serNum typeNum off len)  = concat
                   [encodeIntArg serNum, encodeIntArg typeNum, encodeIntArg off,
                    encodeIntArg len]
encodeCommandArgs (BalloonResult text)              = encodeStringArg text
encodeCommandArgs (DefineAnnoType typeNum typeName tooltip glyphFile fg bg) =
    concat [encodeIntArg typeNum, encodeStringArg typeName,
                    encodeStringArg tooltip, encodeStringArg glyphFile,
                    encodeStringArg fg, encodeStringArg bg]
encodeCommandArgs (EditFile path)                   = encodeStringArg path
encodeCommandArgs (Guard off len)                   = encodeIntArg off ++ encodeIntArg len
encodeCommandArgs (MoveAnnoToFront serNum)          = encodeIntArg serNum
encodeCommandArgs (NetbeansBuffer isNetbeansBuffer) = encodeBoolArg isNetbeansBuffer
encodeCommandArgs (PutBufferNumber path)            = encodeStringArg path
encodeCommandArgs (RemoveAnno serNum)               = encodeIntArg serNum
encodeCommandArgs (SetBufferNumber path)            = encodeStringArg path
encodeCommandArgs (SetDot off)                      = encodeIntArg off
encodeCommandArgs (SetExitDelay seconds)            = encodeIntArg seconds
encodeCommandArgs (SetFullName pathname)            = encodeStringArg pathname
encodeCommandArgs (SetModified modified)            = encodeBoolArg modified
encodeCommandArgs (SetModTime time)                 = encodeIntArg time
encodeCommandArgs (SetTitle name)                   = encodeStringArg name
encodeCommandArgs (SetVisible visible)              = encodeBoolArg visible
encodeCommandArgs (ShowBalloon text)                = encodeStringArg text
encodeCommandArgs (Unguard off len)                 = encodeIntArg off ++ encodeIntArg len
encodeCommandArgs _ = ""
