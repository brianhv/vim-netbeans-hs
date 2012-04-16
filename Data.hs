module Data
    ( VimCommandType(..)
    , VimFunctionType(..)
    , VimEventType(..)
    , VimEvent(..)
    , VimReply(..)
    , VimMessage(..)
    , IdeMessage(..)
    , SequenceNum(..)
    , BufferID(..)
    , messageTypeString
) where

newtype BufferID = BufferID Int deriving (Read)
newtype SequenceNum = SequenceNum Int deriving (Read)

instance Show BufferID where
    show (BufferID i) = show i

instance Show SequenceNum where
    show (SequenceNum i) = show i

data VimCommandType = ActionMenuItem
                    | ActionSensitivity
                    | AddAnno Int Int Int Int
                    | BalloonResult String
                    | Close
                    | Create
                    | DefineAnnoType Int String String String String String
                    | EditFile String
                    | EnableBalloonEval
                    | EndAtomic
                    | Guard Int Int
                    | InitDone
                    | InsertDone
                    | MoveAnnoToFront Int
                    | NetbeansBuffer Bool
                    | PutBufferNumber String
                    | Raise
                    | RemoveAnno Int
                    | Save
                    | SaveDone
                    | SetAsUser
                    | SetBufferNumber String
                    | SetContentType
                    | SetDot Int
                    | SetExitDelay Int
                    | SetFullName String
                    | SetLocAndSize
                    | SetMark
                    | SetModified Bool
                    | SetModTime Int
                    | SetReadOnly
                    | SetStyle
                    | SetTitle String
                    | SetVisible Bool
                    | ShowBalloon String
                    | SpecialKeys
                    | StartAtomic
                    | StartCaretListen
                    | StartDocumentListen
                    | StopCaretListen
                    | StopDocumentListen
                    | Unguard Int Int
                    | Version
    deriving (Show)

data VimFunctionType = GetDot
                     | GetCursor
                     | GetLength
                     | GetMark
                     | GetAnno Int
                     | GetModified
                     | GetText
                     | Insert Int String
                     | Remove Int Int
                     | SaveAndExit

data VimEventType = BalloonEval Int Int String
                  | BalloonText String
                  | ButtonRelease Int Int Int
                  | Disconnect
                  | FileClosed
                  | FileModified
                  | FileOpened String Bool Bool
                  | Geometry Int Int Int Int
                  | InsertEvent Int String
                  | InvalidEvent String
                  | InvokeAction
                  | KeyCommand String
                  | KeyAtPos String Int Int
                  | Killed
                  | NewDotAndMark Int Int
                  | RemoveEvent Int Int
                  | Quit
                  | Revert
                  | SaveEvent
                  | StartupDone
                  | Unmodified
                  | VersionEvent String
     deriving (Show)

data VimEvent = VimEvent BufferID SequenceNum VimEventType

data VimReply = VimReply SequenceNum

data VimMessage = EventMessage VimEvent | ReplyMessage VimReply
data IdeMessage = CommandMessage VimCommandType | FunctionMessage VimFunctionType

commandTypeString :: VimCommandType -> String
commandTypeString (ActionMenuItem)      = "actionMenuItem"
commandTypeString (ActionSensitivity)   = "actionSensitivity"
commandTypeString (AddAnno {})          = "addAnno"
commandTypeString (BalloonResult _)     = "balloonResult"
commandTypeString (Close)               = "close"
commandTypeString (Create)              = "create"
commandTypeString (DefineAnnoType {})   = "defineAnnoType"
commandTypeString (EditFile _)          = "editFile"
commandTypeString (EnableBalloonEval)   = "enableBalloonEval"
commandTypeString (EndAtomic)           = "endAtomic"
commandTypeString (Guard _ _)           = "guard"
commandTypeString (InitDone)            = "initDone"
commandTypeString (InsertDone)          = "insertDone"
commandTypeString (MoveAnnoToFront _)   = "moveAnnoToFront"
commandTypeString (NetbeansBuffer _)    = "netbeansBuffer"
commandTypeString (PutBufferNumber _)   = "putBufferNumber"
commandTypeString (Raise)               = "raise"
commandTypeString (RemoveAnno _)        = "removeAnno"
commandTypeString (Save)                = "save"
commandTypeString (SaveDone)            = "saveDone"
commandTypeString (SetAsUser)           = "setAsUser"
commandTypeString (SetBufferNumber _)   = "setBufferNumber"
commandTypeString (SetContentType)      = "setContentType"
commandTypeString (SetDot _)            = "setDot"
commandTypeString (SetExitDelay _)      = "setExitDelay"
commandTypeString (SetFullName _)       = "setFullName"
commandTypeString (SetLocAndSize)       = "setLocAndSize"
commandTypeString (SetMark)             = "setMark"
commandTypeString (SetModified _)       = "setModified"
commandTypeString (SetModTime _)        = "setModTime"
commandTypeString (SetReadOnly)         = "setReadOnly"
commandTypeString (SetStyle)            = "setStyle"
commandTypeString (SetTitle _)          = "setTitle"
commandTypeString (SetVisible _)        = "setVisible"
commandTypeString (ShowBalloon _)       = "showBalloon"
commandTypeString (SpecialKeys)         = "specialKeys"
commandTypeString (StartAtomic)         = "startAtomic"
commandTypeString (StartCaretListen)    = "startCaretListen"
commandTypeString (StartDocumentListen) = "startDocumentListen"
commandTypeString (StopCaretListen)     = "stopCaretListen"
commandTypeString (StopDocumentListen)  = "stopDocumentListen"
commandTypeString (Unguard _ _)         = "unguard"
commandTypeString (Version)             = "version"

functionTypeString :: VimFunctionType -> String
functionTypeString (GetDot)      = "getDot"
functionTypeString (GetCursor)   = "getCursor"
functionTypeString (GetLength)   = "getLength"
functionTypeString (GetMark)     = "getMark"
functionTypeString (GetAnno _)   = "getAnno"
functionTypeString (GetModified) = "getModified"
functionTypeString (GetText)     = "getText"
functionTypeString (Insert _ _)  = "insert"
functionTypeString (Remove _ _)  = "remove"
functionTypeString (SaveAndExit) = "saveAndExit"

messageTypeString :: IdeMessage -> String
messageTypeString (CommandMessage m)  = commandTypeString m
messageTypeString (FunctionMessage f) = functionTypeString f
