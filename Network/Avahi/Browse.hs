{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Network.Avahi.Browse
  (browse,
   dispatch
  ) where

import Control.Monad
import Control.Concurrent
import Data.Text (Text)
import Data.Word
import Data.Int
import Data.Char
import DBus.Client as C
import DBus.Internal.Types
import DBus.Internal.Message

import Network.Avahi.Common

listenAvahi ::  Maybe BusName -> C.MatchRule
listenAvahi name = matchAny { matchSender = name }

-- | Browse for specified service
browse :: BrowseQuery -> IO ()
browse (BrowseQuery {..}) = do
  client <- connectSystem
  [sb] <- call' client "/" serverInterface "ServiceBrowserNew" [iface_unspec,
                                                               proto2variant lookupProtocol,
                                                               toVariant lookupServiceName,
                                                               toVariant lookupDomain,
                                                               flags_empty ]
  addMatch client (listenAvahi $ fromVariant sb) (handler client lookupCallback)
  addMatch client (listenAvahi $ Just serviceResolver) (handler client lookupCallback)
  return ()

-- | Dispatch signal and call corresponding function.
dispatch ::  [(String, Signal -> IO b)] -> Signal -> IO ()
dispatch pairs signal = do
  let signame = signalMember signal
  let good = [callback | (name, callback) <- pairs, memberName_ name == signame]
  forM_ good $ \callback ->
      callback signal

handler ::  Client -> (Service -> IO ()) -> Signal -> IO ()
handler client callback signal = do
  dispatch [("ItemNew", on_new_item client),
            ("Found",   on_service_found callback) ] signal

on_new_item ::  Client -> Signal -> IO ()
on_new_item client signal = do
  let body = signalBody signal
      [iface,proto,name,stype,domain,flags] = body
  call' client "/" serverInterface "ServiceResolverNew" [iface,
                                                         proto,
                                                         name,
                                                         stype,
                                                         domain, 
                                                         proto2variant PROTO_UNSPEC,
                                                         flags_empty ]
  return ()

on_service_found :: (Service -> IO ()) -> Signal -> IO ()
on_service_found callback signal = do
  let body = signalBody signal
      [iface, proto, name, stype, domain, host, aproto, addr, port, text, flags] = body
      service = Service {
                  serviceProtocol = variant2proto proto,
                  serviceName = fromVariant_ "service name" name,
                  serviceType = fromVariant_ "service type" stype,
                  serviceDomain = fromVariant_ "domain" domain,
                  serviceHost = fromVariant_ "service host" host,
                  serviceAddress = fromVariant addr,
                  servicePort = fromVariant_ "service port" port,
                  serviceText = maybe "" toString (fromVariant text :: Maybe [[Word8]]) }
  callback service

toString :: [[Word8]] -> String
toString list = concatMap (map (chr . fromIntegral)) list

