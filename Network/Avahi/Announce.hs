{-# LANGUAGE OverloadedStrings #-}
module Network.Avahi.Announce where

import Data.Int
import Data.Word
import Data.Char
import Data.Maybe
import qualified DBus.Client as C
import DBus.Client.Simple

import Network.Avahi.Common

-- | Announce network service
announce :: String -- ^ Service name
         -> String -- ^ Service type
         -> String -- ^ Domain
         -> String -- ^ Host
         -> Word16 -- ^ Port number
         -> String -- ^ Text
         -> IO ()
announce name stype domain host port text = do
  bus <- connectSystem
  server <- proxy bus avahiBus "/"
  [newGroup] <- call server serverInterface "EntryGroupNew" []
  new <- proxy bus avahiBus (fromJust $ fromVariant newGroup)
  let text' = [map (fromIntegral . ord) text] :: [[Word8]]
  call new entryGroupInterface "AddService" [toVariant (-1 :: Int32), -- IF_UNSPEC
                                             toVariant (0 :: Int32),  -- PROTO_INET
                                             toVariant (0 :: Word32),  -- 
                                             toVariant name,
                                             toVariant stype,
                                             toVariant domain,
                                             toVariant host,
                                             toVariant port,
                                             toVariant text']
  call new entryGroupInterface "Commit" []
  return ()

