package rl
package tests

import org.specs2.Specification
import rl.Uri._

object TestParser extends Uri.UriParser {
  def parseFragment(possible: String) = {
    parseAll(fragmentOpt, possible) match {
      case Success(Some(result), _) => result
      case _ => EmptyFragmentNode
    }
  }

  def parseQuery(possible: String) = {
    parseAll(queryOpt, possible) match {
      case Success(Some(result), _) => result
      case _ => EmptyQueryStringNode
    }
  }

  def parseIPv4(possible: String) = {
    parseAll(ipv4Address, possible ) match {
      case Success(address, _) => address
      case _ => null
    }
  }

  def parseIPv6(possible: String) = {
    parseAll(ipv6Address, possible ) match {
      case Success(address, _) => address
      case _ => null
    }
  }

  def parseIPvFuture(possible: String) = {
    parseAll(ipvFuture, possible ) match {
      case Success(address, _) => address
      case _ => null
    }
  }

  def parseIPLiteral(possible: String) = {
    val res = parseAll(ipLiteral, possible )
    println("The result: " + res)
    res match {
      case Success(address, _) => address
      case _ => null
    }
  }
}

class UriParserSpec extends Specification { def is =

  "A UriParser should" ^
    "parse a fragment" ! { TestParser.parseFragment("#i-m-a-fragment") must_== FragmentNode("i-m-a-fragment") } ^
    "parse a querystring" ! { TestParser.parseQuery("?id=6") must_== QueryStringNode("id=6") } ^
    "parse an ipv4 address" ! { TestParser.parseIPv4("123.23.34.56") must_== IPv4AddressNode("123", "23", "34", "56") } ^
    "parse an ipv6 address" ! {
      TestParser.parseIPv6("2001:0000:1234:0000:0000:C1C0:ABCD:0876") must_== IPv6AddressNode("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
    } ^
    "parse an ipvFuture address" ! {
      TestParser.parseIPvFuture("v2A.dd") must_== IPvFutureAddressNode("v2A.dd")
    } ^
    "parse an ip Future literal" ! {
      TestParser.parseIPLiteral("[v2A.dd]") must_== IPvFutureAddressNode("v2A.dd")
    } ^
    "parse an ip v6 literal" ! {
      TestParser.parseIPLiteral("[2001:0000:1234:0000:0000:C1C0:ABCD:0876]") must_== IPv6AddressNode("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
    } ^ end

}