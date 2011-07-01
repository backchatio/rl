package rl
package tests

import org.specs2.Specification
import rl.Uri._

object TestParser extends Uri.UriParser {
  def parseFragment(possible: String) = {
    parseAll(fragmentOpt, possible) match {
      case Success(Some(result), _) => result
      case Success(None, _) => None
      case Failure(msg, _) => msg
    }
  }

  def parseQuery(possible: String) = {
    parseAll(queryOpt, possible) match {
      case Success(Some(result), _) => result
      case Success(None, _) => None
      case Failure(msg, _) => msg
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
    parseAll(ipLiteral, possible ) match {
      case Success(address, _) => address
      case _ => null
    }
  }
}

class UriParserSpec extends Specification { def is =

  "A UriParser should" ^
    "when parsing a fragment" ^
      "get the fragment value" ! { TestParser.parseFragment("#i-m-a-fragment") must_== FragmentNode("i-m-a-fragment") } ^
      "get none when empty fragment" ! { TestParser.parseFragment("#") must_== None } ^
      "get none when no fragment found" ! { TestParser.parseFragment("") must_== None } ^ p^
    "when parsing a query string" ^
      "get the query string value" ! { TestParser.parseQuery("?id=6") must_== QueryStringNode("id=6") } ^
      "get none when empty query string" ! { TestParser.parseQuery("?") must_== None } ^
      "get none when no querystring found" ! { TestParser.parseQuery("") must_== None } ^ p^
    "when parsing ip addresses" ^
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