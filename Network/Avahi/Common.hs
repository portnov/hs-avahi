{-# LANGUAGE OverloadedStrings #-}
module Network.Avahi.Common where

import Data.Int
import Data.Word
import DBus.Client.Simple

-- | Service specification
data Service = Service {
  serviceProtocol :: InetProtocol,
  serviceName :: String,
  serviceType :: String,
  serviceDomain :: String,
  serviceHost :: String,
  serviceAddress :: Maybe String,
  servicePort :: Word16,
  serviceText :: String }
  deriving (Eq,Show)

-- | Service browsing query
data BrowseQuery = BrowseQuery {
  lookupProtocol :: InetProtocol, -- ^ Protocol to be used for lookup
  lookupServiceName :: String,    -- ^ Service name to find
  lookupDomain :: String,         -- ^ Domain to search in (usually `local')
  lookupCallback :: Service -> IO () -- ^ Function to be called on found service
  }

-- | Internet protocol specification
data InetProtocol =
    PROTO_UNSPEC  -- ^ Unspecified (any) protocol (-1)
  | PROTO_INET    -- ^ IPv4 protocol (0)
  | PROTO_INET6   -- ^ IPv6 protocol (1)
  deriving (Eq,Show)

proto2variant :: InetProtocol -> Variant
proto2variant PROTO_UNSPEC = toVariant (-1 :: Int32)
proto2variant PROTO_INET   = toVariant (0 :: Int32)
proto2variant PROTO_INET6  = toVariant (1 :: Int32)

variant2proto :: Variant -> InetProtocol
variant2proto x =
  case fromVariant x :: Maybe Int32 of
    Nothing -> error $ "Erroneus PROTO: " ++ show x
    Just (-1) -> PROTO_UNSPEC
    Just 0 -> PROTO_INET
    Just 1 -> PROTO_INET6
    Just y -> error $ "Erroneus PROTO: " ++ show y

forceMaybe :: String -> Maybe a -> a
forceMaybe msg Nothing = error msg
forceMaybe _ (Just x)  = x

fromVariant_ :: (IsVariant a) => String -> Variant -> a
fromVariant_ msg x = forceMaybe msg (fromVariant x)

iface_unspec :: Variant
iface_unspec = toVariant (-1 :: Int32)

flags_empty :: Variant
flags_empty = toVariant (0 :: Word32)

avahiBus :: BusName
avahiBus = busName_ "org.freedesktop.Avahi"

hostNameResolver :: BusName
hostNameResolver = busName_ "org.freedesktop.Avahi.HostNameResolver"

serviceResolver :: BusName
serviceResolver = busName_ "org.freedesktop.Avahi.ServiceResolver"

serverInterface :: InterfaceName
serverInterface = interfaceName_ "org.freedesktop.Avahi.Server"

avahiBrowser :: InterfaceName
avahiBrowser = interfaceName_ "org.freedesktop.Avahi.ServiceBrowser"

entryGroupInterface :: InterfaceName
entryGroupInterface = interfaceName_ "org.freedesktop.Avahi.EntryGroup"

