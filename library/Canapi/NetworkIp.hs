module Canapi.NetworkIp
where

import Canapi.Prelude
import Network.IP.Addr
import qualified Network.Socket as Network
import qualified System.Endian as Cpu


sockAddrInetAddr :: Network.SockAddr -> Maybe (InetAddr IP)
sockAddrInetAddr = \ case
  Network.SockAddrInet a b -> Just (InetAddr (IPv4 (hostAddressIp4 b)) (portNumberInetPort a))
  Network.SockAddrInet6 a _ b _ -> Just (InetAddr (IPv6 (hostAddress6Ip6 b)) (portNumberInetPort a))
  Network.SockAddrUnix _ -> Nothing

sockAddrIP :: Network.SockAddr -> Maybe IP
sockAddrIP = \ case
  Network.SockAddrInet _ a -> Just (IPv4 (hostAddressIp4 a))
  Network.SockAddrInet6 _ _ a _ -> Just (IPv6 (hostAddress6Ip6 a))
  Network.SockAddrUnix _ -> Nothing

hostAddressIp4 :: Network.HostAddress -> IP4
hostAddressIp4 = IP4 . Cpu.fromBE32

hostAddress6Ip6 :: Network.HostAddress6 -> IP6
hostAddress6Ip6 = uncurryN ip6FromWords . Network.hostAddress6ToTuple

portNumberInetPort :: Network.PortNumber -> InetPort
portNumberInetPort = fromIntegral
