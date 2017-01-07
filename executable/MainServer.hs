{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Main where

import Library
import Data.Word
import Data.Unique
import Data.Serialize
import Control.Monad
import Control.Concurrent (forkIO)
import Foreign.Storable
import Data.ByteString.Char8 as C8

main :: IO ()
main = do
  mbSkt <- launchSocket (undefined :: Server) "39000"
  case mbSkt of
    Nothing -> return ()
    Just (skt, _) -> forever loop
      where
        loop = do
          (conn, _) <- accept skt
          forkIO $ do
            print conn
            forever $ do
              eHdr <- runGet (get :: Get Header) <$> recvWithLen conn 4
              case eHdr of
                Left _ -> print "bad header recvd"
                Right (hdr@Header{..}) -> do
                  print (hdr, hdrPayloadLen)
                  ploadStr <- safeRecv conn (fromIntegral hdrPayloadLen)
                  case hdrType of
                    New -> print $ runGet (get :: Get NewMsg) ploadStr
                    Modify -> print $ runGet (get :: Get ModifyMsg) ploadStr
                    Cancel -> print $ runGet (get :: Get CancelMsg) ploadStr
