package rl
package tests

import org.specs2.Specification
import org.specs2.matcher.DataTables


class IPv6AddressSpec extends Specification with DataTables { def is =

  "The IPv6 regular expression should only work with valid ip6 addresses" ! ipv6test

  def ipv6test = {
    "address" || "expected result" |
    "" !! false |
    "::1" !! true |
    "::" !! true |
    "0:0:0:0:0:0:0:1" !! true |
    "0:0:0:0:0:0:0:0" !! true |
    "2001:DB8:0:0:8:800:200C:417A" !! true |
    "FF01:0:0:0:0:0:0:101" !! true |
    "2001:DB8::8:800:200C:417A" !! true |
    "FF01::101" !! true |
    "2001:DB8:0:0:8:800:200C:417A:221" !! false |
    "FF01::101::2" !! false |
    "fe80::217:f2ff:fe07:ed62" !! true |
    "2001:0000:1234:0000:0000:C1C0:ABCD:0876" !! true |
    "3ffe:0b00:0000:0000:0001:0000:0000:000a" !! true |
    "FF02:0000:0000:0000:0000:0000:0000:0001" !! true |
    "0000:0000:0000:0000:0000:0000:0000:0001" !! true |
    "0000:0000:0000:0000:0000:0000:0000:0000" !! true |
    "02001:0000:1234:0000:0000:C1C0:ABCD:0876" !! false |
    "2001:0000:1234:0000:00001:C1C0:ABCD:0876" !! false |
    "2001:0000:1234:0000:0000:C1C0:ABCD:0876  0" !! false |
    "2001:0000:1234: 0000:0000:C1C0:ABCD:0876" !! false |
    "3ffe:0b00:0000:0001:0000:0000:000a" !! false |
    "FF02:0000:0000:0000:0000:0000:0000:0000:0001" !! false |
    "3ffe:b00::1::a" !! false |
    "::1111:2222:3333:4444:5555:6666::" !! false |
    "2::10" !! true |
    "ff02::1" !! true |
    "fe80::" !! true |
    "2002::" !! true |
    "2001:db8::" !! true |
    "2001:0db8:1234::" !! true |
    "::ffff:0:0" !! true |
    "1:2:3:4:5:6:7:8" !! true |
    "1:2:3:4:5:6::8" !! true |
    "1:2:3:4:5:6::8" !! true |
    "1:2:3:4:5::8" !! true |
    "1:2:3:4::8" !! true |
    "1:2:3::8" !! true |
    "1:2::8" !! true |
    "1::8" !! true |
    "1::2:3:4:5:6:7" !! true |
    "1::2:3:4:5:6" !! true |
    "1::2:3:4:5" !! true |
    "1::2:3:4" !! true |
    "1::2:3" !! true |
    "1::8" !! true |
    "::2:3:4:5:6:7:8" !! true |
    "::2:3:4:5:6:7" !! true |
    "::2:3:4:5:6" !! true |
    "::2:3:4:5" !! true |
    "::2:3:4" !! true |
    "::2:3" !! true |
    "::8" !! true |
    "1:2:3:4:5:6::" !! true |
    "1:2:3:4:5::" !! true |
    "1:2:3:4::" !! true |
    "1:2:3::" !! true |
    "1:2::" !! true |
    "1::" !! true |
    "1:2:3:4:5::7:8" !! true |
    "1:2:3::4:5::7:8" !! false |
    "12345::6:7:8" !! false |
    "1:2:3:4::7:8" !! true |
    "1:2:3::7:8" !! true |
    "1:2::7:8" !! true |
    "1::7:8" !! true |> { (candidate, isValid) =>
        IPv6Address.findFirstIn(candidate).isDefined must_== isValid
    }
  }

/*

 */
}