{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Network.Avahi.Announce where

import Data.Int
import Data.Word
import Data.Char
import Data.Maybe
import qualified DBus.Client as C
import DBus.Client.Simple

import Network.Avahi.Common

-- | Announce network service
announce :: Service      -- ^ Service to announce
         -> IO ()
announce (Service {..}) = do
  bus <- connectSystem
  server <- proxy bus avahiBus "/"
  [newGroup] <- call server serverInterface "EntryGroupNew" []
  new <- proxy bus avahiBus (fromJust $ fromVariant newGroup)
  let text' = [map (fromIntegral . ord) serviceText] :: [[Word8]]
  call new entryGroupInterface "AddService" [toVariant (-1 :: Int32), -- IF_UNSPEC
                                             proto2variant serviceProtocol,
                                             flags_empty,
                                             toVariant serviceName,
                                             toVariant serviceType,
                                             toVariant serviceDomain,
                                             toVariant serviceHost,
                                             toVariant servicePort,
                                             toVariant text']
  call new entryGroupInterface "Commit" []
  return ()

