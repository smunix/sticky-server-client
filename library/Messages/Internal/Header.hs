{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass, RecordWildCards #-}

module Messages.Internal.Header ( Header(..)
                                ) where

import Messages.Internal.Types
import GHC.Generics
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import Data.Serialize

data Header = Header { hdrType       :: Type
                     , hdrSeqNo      :: Word32
                     , hdrSrcId      :: Word32
                     , hdrDstId      :: Word32
                     , hdrPayloadLen :: Word32
                     } deriving (Eq, Show, Generic)

instance Serialize Header

instance Storable Header where
  sizeOf Header{..} = sizeOf hdrType
                      + sizeOf hdrSeqNo
                      + sizeOf hdrSrcId
                      + sizeOf hdrDstId
                      + sizeOf hdrPayloadLen

  alignment _ = alignment (0 :: Word32)

  peek ptr = do
    hdrType <- peek (castPtr ptr :: Ptr Type)
    hdrSeqNo <- peek (castPtr $ ptr `plusPtr` hdrSeqNoOff :: Ptr Word32)
    hdrSrcId <- peek (castPtr $ ptr `plusPtr` hdrSrcIdOff :: Ptr Word32)
    hdrDstId <- peek (castPtr $ ptr `plusPtr` hdrDstIdOff :: Ptr Word32)
    return Header {..}
      where
        hdrSeqNoOff = sizeOf (undefined :: Type)
        hdrSrcIdOff = hdrSeqNoOff + sizeOf (undefined :: Word32)
        hdrDstIdOff = hdrSrcIdOff + sizeOf (undefined :: Word32)

  poke ptr Header{..} = do
    poke (castPtr ptr :: Ptr Type) hdrType
    poke (castPtr $ ptr `plusPtr` fromIntegral hdrSeqNoOff :: Ptr Word32) hdrSeqNo
    poke (castPtr $ ptr `plusPtr` hdrSrcIdOff :: Ptr Word32) hdrSrcId
    poke (castPtr $ ptr `plusPtr` hdrDstIdOff :: Ptr Word32) hdrDstId
      where
        hdrSeqNoOff = sizeOf (undefined :: Type)
        hdrSrcIdOff = hdrSeqNoOff + sizeOf (undefined :: Word32)
        hdrDstIdOff = hdrSrcIdOff + sizeOf (undefined :: Word32)
