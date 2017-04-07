{-# LANGUAGE OverloadedStrings #-}
module Network.Avahi.Browse

import DBus.Client

listenNameOwnerChanged = MatchRule {
  matchSender = busName "org.freedesktop.Avahi",
  matchDestination = Nothing,
  matchPath = Nothing,
  matchInterface = Nothing,
  matchMember = Nothing }

browse = do
  bus <- connectSystem
  listen bus listenNameOwnerChanged nameOwnerChanged
          self.system_bus.add_signal_receiver(self.avahi_dbus_connect_cb, "NameOwnerChanged", "org.freedesktop.DBus", arg0="org.freedesktop.Avahi")
