{-# LANGUAGE RecordWildCards #-}

module Main where

import Library
import Data.Word
import Data.Unique
import Data.Serialize
import Control.Monad
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomIO)
import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = do replicateM_ 10 $ forkIO $ do
            mbSkt <- launchSocket (undefined :: Client) "localhost" "39000"
            case mbSkt of
              Nothing -> return ()
              Just (skt, _) -> forever $ do
                                      seqno <- newUnique >>= return . fromIntegral . hashUnique
                                      newMClientId <- randomIO
                                      newMQty      <- randomIO
                                      newMPrice    <- randomIO

                                      let
                                          newMsg :: NewMsg
                                          newMsg = NewMsg {..}

                                          newMsgStr :: C8.ByteString
                                          newMsgStr = runPut . put $ newMsg

                                          newMsgStrLen :: Word32
                                          newMsgStrLen = fromIntegral . C8.length $ newMsgStr

                                          hdr :: Header
                                          hdr = Header New seqno 1 1 newMsgStrLen

                                          hdrStr :: C8.ByteString
                                          hdrStr = runPut . put $ hdr

                                          hdrStrLen :: Word32
                                          hdrStrLen = fromIntegral . C8.length $ hdrStr

                                          hdrStrLenStr :: C8.ByteString
                                          hdrStrLenStr = runPut . put $ hdrStrLen

                                      -- todo : smunix : code below needs more clenanup
                                      print (hdr, newMsg)
                                      safeSend skt hdrStrLenStr
                                      safeSend skt hdrStr
                                      safeSend skt newMsgStr

                                      -- threadDelay $ 1*1000000
          threadDelay $ 10*1000000
