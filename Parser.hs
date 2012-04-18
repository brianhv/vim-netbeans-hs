module Parser (parseVimLine) where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec hiding (Parser)
import Data
import Data.ByteString

parseVimLine :: ByteString -> Either ParseError VimMessage
parseVimLine = parse parseLine ""

parseLine :: Parser VimMessage
parseLine = parseEvent <|> parseReply

parseReply :: Parser VimMessage
parseReply = do
    seqno <- liftM read $ many1 digit
    return $ ReplyMessage $ VimReply seqno

parseNumber :: (Num a, Read a) => Parser a
parseNumber = read <$> many1 digit

parseWrappedNumber :: (Num a, Read a) => (a -> b) -> Parser b
parseWrappedNumber ctor = ctor <$> parseNumber

parseNumberArg :: (Num a, Read a) => Parser a
parseNumberArg = whitespace >> parseNumber

parseStringArg :: Parser String
parseStringArg = do
    whitespace
    _ <- char '"'
    result <- many (noneOf "\"")
    _ <- char '"'
    return result

parseBoolArg :: Parser Bool
parseBoolArg = do
    whitespace
    tf <- oneOf "TF"
    return (tf == 'T')

whitespace :: Parser ()
whitespace = void $ many (char ' ')

parseEvent :: Parser VimMessage
parseEvent = do
    num <- parseNumber
    parseAsReply (SequenceNum num) <|> parseRestOfEvent (BufferID num)
    where
    parseAsReply seqno     = eof >> return (ReplyMessage $ VimReply seqno)
    parseRestOfEvent bufID = do
        _     <- char ':'
        name  <- many1 letter
        _     <- char '='
        seqno <- parseWrappedNumber SequenceNum

        ev <- case name of
            "balloonEval"   -> BalloonEval <$> parseNumberArg <*> parseNumberArg <*> parseStringArg
            "balloonText"   -> BalloonText <$> parseStringArg
            "buttonRelease" -> ButtonRelease <$> parseNumberArg <*> parseNumberArg <*> parseNumberArg
            "disconnect"    -> pure Disconnect
            "fileClosed"    -> pure FileClosed
            "fileModified"  -> pure FileModified
            "fileOpened"    -> FileOpened <$> parseStringArg <*> parseBoolArg <*> parseBoolArg
            "geometry"      -> Geometry <$> parseNumberArg <*> parseNumberArg <*> parseNumberArg <*> parseNumberArg
            "insert"        -> InsertEvent <$> parseNumberArg <*> parseStringArg
            "invokeAction"  -> pure InvokeAction
            "keyCommand"    -> KeyCommand <$> parseStringArg
            "keyAtPos"      -> KeyAtPos <$> parseStringArg <*> parseNumberArg <*> parseNumberArg
            "killed"        -> pure Killed
            "newDotAndMark" -> NewDotAndMark <$> parseNumberArg <*> parseNumberArg
            "remove"        -> RemoveEvent <$> parseNumberArg <*> parseNumberArg
            "quit"          -> pure Quit
            "revert"        -> pure Revert
            "save"          -> pure SaveEvent
            "startupDone"   -> pure StartupDone
            "unmodified"    -> pure Unmodified
            "version"       -> VersionEvent <$> parseStringArg
            unknown         -> pure $ InvalidEvent ("Unknown command: " ++ unknown)
        return $ EventMessage $ VimEvent bufID seqno ev
