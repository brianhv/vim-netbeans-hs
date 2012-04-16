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

parseNumber :: (Read a) => Parser a
parseNumber = liftM read (many1 digit)

parseNumberArg :: (Read a) => Parser a
parseNumberArg = many (char ' ') >> parseNumber

parseStringArg :: Parser String
parseStringArg = do
    _ <- many (char ' ')
    _ <- char '"'
    result <- many (noneOf "\"")
    _ <- char '"'
    return result

parseBoolArg :: Parser Bool
parseBoolArg = do
    tf <- oneOf "TF"
    return (tf == 'T')

parseEvent :: Parser VimMessage
parseEvent = do
    bufID <- parseNumber
    _     <- char ':'
    name  <- many1 letter
    _     <- char '='
    seqno <- parseNumber

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
