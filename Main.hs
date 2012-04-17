import Data.Conduit.Network
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data
import Conduit

vimEventResponder :: (Monad a) => Conduit VimEventType a IdeMessage
vimEventResponder = CL.map responses =$= flatten where
    responses StartupDone = [ CommandMessage $ EditFile "/tmp/test.txt"
                            , FunctionMessage $ Insert 0 "Hello, world!"]
    responses _           = []

main :: IO ()
main = do
    Prelude.putStrLn "Starting server"
    runTCPServer (ServerSettings 3219 (Host "127.0.0.1")) (vimApplication vimEventResponder)
