{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v1 #-}

module Messages.Internal.Types ( Type(..)
                               , NewMsg(..)
                               , ModifyMsg(..)
                               , CancelMsg(..)
                               ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import GHC.Generics
import Data.Word
import Data.Serialize
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic (GStorable)

data Type
  = New
  | Modify
  | Cancel
  deriving (Enum, Bounded, Show, Eq, Generic)

instance Serialize Type

instance Storable Type where
  sizeOf = (sizeOf :: Word8 -> Int) . fromIntegral . fromEnum
  alignment = (alignment :: Word8 -> Int)  . fromIntegral . fromEnum
  peek ptr = do
    w8 <- peek (castPtr ptr :: Ptr Word8)
    return . toEnum . fromIntegral $ w8
  poke ptr a = poke (castPtr ptr :: Ptr Word8) (fromIntegral . fromEnum $ a)

data NewMsg = NewMsg { newMClientId :: Word32
                     , newMQty :: Word32
                     , newMPrice :: Word32
                     } deriving (Eq, Show, Generic, GStorable)
instance Serialize NewMsg

data ModifyMsg = ModifyMsg { modMClientId :: Word32
                           , modMQty :: Word32
                           , modMPrice :: Word32
                           } deriving (Eq, Show, Generic, GStorable)
instance Serialize ModifyMsg

data CancelMsg = CancelMsg { canMClientId :: Word32 } deriving (Eq, Show, Generic, GStorable)
instance Serialize CancelMsg
