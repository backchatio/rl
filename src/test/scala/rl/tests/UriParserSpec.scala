package rl
package tests

import org.specs2.Specification
import Uri._

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

  def parsePath(possible: String) = {
    parseAll(path, possible) match {
      case Success(path, _) => path
      case _ => null
    }
  }

  def parseAuthority(possible: String) = {
    parseAll(authority, possible) match {
      case Success(auth, _) => auth
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
      } ^ p^
    "when parsing paths" ^
      "parse a relative path" ! {
        val seg = PathSegmentNode("..") :: PathSegmentNode("..") :: PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser.parsePath("../../hello/world.txt") must_== RelativePathNode(seg)
      } ^
      "parse an absolute path" ! {
        val seg = PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser.parsePath("/hello/world.txt") must_== AbsolutePathNode(seg)
      } ^ p^
    "when parsing the authority" ^
      "parse www.example.org" ! {
        TestParser.parseAuthority("www.example.org") must_== AuthorityNode(None, HostNode("www.example.org"), None)
      } ^
      "parse www.example.org:8080" ! {
        TestParser.parseAuthority("www.example.org:8080") must_== AuthorityNode(None, HostNode("www.example.org"), Some(PortNode(8080)))
      } ^
      "parse tom:tim@www.example.org:8080" ! {
        TestParser.parseAuthority("tom:tim@www.example.org:8080") must_== AuthorityNode(Some(UserInfoNode("tom:tim")), HostNode("www.example.org"), Some(PortNode(8080)))
      } ^
      "parse tom@www.example.org:8080" ! {
        TestParser.parseAuthority("tom@www.example.org:8080") must_== AuthorityNode(Some(UserInfoNode("tom")), HostNode("www.example.org"), Some(PortNode(8080)))
      } ^
      "parse tom:tim@www.example.org" ! {
        TestParser.parseAuthority("tom:tim@www.example.org") must_== AuthorityNode(Some(UserInfoNode("tom:tim")), HostNode("www.example.org"), None)
      } ^
      "parse tom@www.example.org" ! {
        TestParser.parseAuthority("tom@www.example.org") must_== AuthorityNode(Some(UserInfoNode("tom")), HostNode("www.example.org"), None)
      } ^ p^
    "when parsing a full uri" ^
      "return a failure for 'http://www.exa mple.org'" ! {
        val res = TestParser("http://www.exa mple.org")
        res must beAnInstanceOf[FailedUri]
      } ^
      "absolute uri 'http://www.example.org:8080'" ! {
        TestParser("http://www.example.org:8080") must_== AbsoluteUriNode(
          SchemeNode("http"),
          Some(PathWithAuthorityNode(AuthorityNode(None, HostNode("www.example.org"), Some(PortNode(8080))), Nil)),
          None,
          None
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt'" ! {
        val seg = PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser("http://www.example.org/hello/world.txt") must_== AbsoluteUriNode(
          SchemeNode("http"),
          Some(PathWithAuthorityNode(AuthorityNode(None, HostNode("www.example.org"), None), seg)),
          None,
          None
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three'" ! {
        val seg = PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser("http://www.example.org/hello/world.txt/?id=5&part=three") must_== AbsoluteUriNode(
          SchemeNode("http"),
          Some(PathWithAuthorityNode(AuthorityNode(None, HostNode("www.example.org"), None), seg)),
          Some(QueryStringNode("id=5&part=three")),
          None
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go'" ! {
        val seg = PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser("http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go") must_== AbsoluteUriNode(
          SchemeNode("http"),
          Some(PathWithAuthorityNode(AuthorityNode(None, HostNode("www.example.org"), None), seg)),
          Some(QueryStringNode("id=5&part=three")),
          Some(FragmentNode("there-you-go"))
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/#here-we-are'" ! {
        val seg = PathSegmentNode("hello") :: PathSegmentNode("world.txt") :: Nil
        TestParser("http://www.example.org/hello/world.txt/#here-we-are") must_== AbsoluteUriNode(
          SchemeNode("http"),
          Some(PathWithAuthorityNode(AuthorityNode(None, HostNode("www.example.org"), None), seg)),
          None,
          Some(FragmentNode("here-we-are"))
        )
      } ^ end



}