{-# LANGUAGE OverloadedStrings #-}
module Network.Avahi.Common where

import DBus.Client.Simple

avahiBus :: BusName
avahiBus = busName_ "org.freedesktop.Avahi"

serverInterface :: InterfaceName
serverInterface = interfaceName_ "org.freedesktop.Avahi.Server"

entryGroupInterface :: InterfaceName
entryGroupInterface = interfaceName_ "org.freedesktop.Avahi.EntryGroup"

