module Data where

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


data VimEventType = BalloonEval Int Int String
                  | BalloonText String
                  | ButtonRelease Int Int Int
                  | Disconnect
                  | FileClosed
                  | FileModified
                  | FileOpened String Bool Bool
                  | Geometry Int Int Int Int
                  | Insert Int String
                  | InvalidEvent String
                  | InvokeAction
                  | KeyCommand String
                  | KeyAtPos String Int Int
                  | Killed
                  | NewDotAndMark Int Int
                  | Remove Int Int
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

commandTypeToByteString :: VimCommandType -> String
commandTypeToByteString (ActionMenuItem)      = "actionMenuItem"
commandTypeToByteString (ActionSensitivity)   = "actionSensitivity"
commandTypeToByteString (AddAnno {})          = "addAnno"
commandTypeToByteString (BalloonResult _)     = "balloonResult"
commandTypeToByteString (Close)               = "close"
commandTypeToByteString (Create)              = "create"
commandTypeToByteString (DefineAnnoType {})   = "defineAnnoType"
commandTypeToByteString (EditFile _)          = "editFile"
commandTypeToByteString (EnableBalloonEval)   = "enableBalloonEval"
commandTypeToByteString (EndAtomic)           = "endAtomic"
commandTypeToByteString (Guard _ _)           = "guard"
commandTypeToByteString (InitDone)            = "initDone"
commandTypeToByteString (InsertDone)          = "insertDone"
commandTypeToByteString (MoveAnnoToFront _)   = "moveAnnoToFront"
commandTypeToByteString (NetbeansBuffer _)    = "netbeansBuffer"
commandTypeToByteString (PutBufferNumber _)   = "putBufferNumber"
commandTypeToByteString (Raise)               = "raise"
commandTypeToByteString (RemoveAnno _)        = "removeAnno"
commandTypeToByteString (Save)                = "save"
commandTypeToByteString (SaveDone)            = "saveDone"
commandTypeToByteString (SetAsUser)           = "setAsUser"
commandTypeToByteString (SetBufferNumber _)   = "setBufferNumber"
commandTypeToByteString (SetContentType)      = "setContentType"
commandTypeToByteString (SetDot _)            = "setDot"
commandTypeToByteString (SetExitDelay _)      = "setExitDelay"
commandTypeToByteString (SetFullName _)       = "setFullName"
commandTypeToByteString (SetLocAndSize)       = "setLocAndSize"
commandTypeToByteString (SetMark)             = "setMark"
commandTypeToByteString (SetModified _)       = "setModified"
commandTypeToByteString (SetModTime _)        = "setModTime"
commandTypeToByteString (SetReadOnly)         = "setReadOnly"
commandTypeToByteString (SetStyle)            = "setStyle"
commandTypeToByteString (SetTitle _)          = "setTitle"
commandTypeToByteString (SetVisible _)        = "setVisible"
commandTypeToByteString (ShowBalloon _)       = "showBalloon"
commandTypeToByteString (SpecialKeys)         = "specialKeys"
commandTypeToByteString (StartAtomic)         = "startAtomic"
commandTypeToByteString (StartCaretListen)    = "startCaretListen"
commandTypeToByteString (StartDocumentListen) = "startDocumentListen"
commandTypeToByteString (StopCaretListen)     = "stopCaretListen"
commandTypeToByteString (StopDocumentListen)  = "stopDocumentListen"
commandTypeToByteString (Unguard _ _)         = "unguard"
commandTypeToByteString (Version)             = "version"
