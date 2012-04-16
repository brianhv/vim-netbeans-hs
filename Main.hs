import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit
import Data
import Conduit

vimEventResponder :: (Monad a) => Conduit VimEventType a IdeMessage
vimEventResponder = CL.map pickResponses =$= flatten

pickResponses :: VimEventType -> [IdeMessage]
pickResponses StartupDone = [ CommandMessage $ EditFile "/tmp/test.txt"
                            , FunctionMessage $ Insert 0 "Hello, world!"]
pickResponses _           = []

main :: IO ()
main = do
    Prelude.putStrLn "Starting server"
    runTCPServer (ServerSettings 3219 HostAny) (vimApplication vimEventResponder)
