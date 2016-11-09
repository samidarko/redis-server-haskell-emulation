module Main where

import Redis
import System.IO
import Network.Socket hiding (recv, send)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C


mainLoop :: Socket -> State -> IO ()
mainLoop sock state = do
    conn <- accept sock     -- accept a connection and handle it
    runConn conn state           -- run our server's logic
    mainLoop sock state         -- repeat

runConn :: (Socket, SockAddr) -> State -> IO State
runConn (sock, _) s = do
    msg <- recv sock 1024
    print msg
    newState <- processCommand (C.unpack msg) s
    sendAll sock $ C.pack ((status newState) ++ delimiter)
    print newState
    close sock
    return newState


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 6388 iNADDR_ANY)   -- listen on TCP port 6388.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock (initialState)
