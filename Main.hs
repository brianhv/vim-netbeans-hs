import Data.Conduit.Network
import Data.Conduit
import Data
import Conduit

vimEventResponder :: (Monad a) => Conduit VimEventType a IdeMessage
vimEventResponder = conduitState () push close where
    push _ StartupDone = return $ StateProducing () [ CommandMessage $ EditFile "/tmp/test.txt"
                                                    , FunctionMessage $ Insert 0 "Hello, world!"]
    push _ _           = return $ StateProducing () []
    close _            = return []

main :: IO ()
main = do
    Prelude.putStrLn "Starting server"
    runTCPServer (ServerSettings 3219 (Host "127.0.0.1")) (vimApplication vimEventResponder)
