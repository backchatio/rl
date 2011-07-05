package rl
package tests

import org.specs2.Specification
import Uri._

object TestParser extends Uri.UriParser {
  def parseFragment(possible: String) = {
    parseAll(fragmentOpt, possible) match {
      case Success(result, _) => result
      case Failure(msg, _) => msg
    }
  }

  def parseQuery(possible: String) = {
    parseAll(queryOpt, possible) match {
      case Success(result, _) => result
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
      "get the fragment value" ! { TestParser.parseFragment("#i-m-a-fragment") must_== StringFragment("i-m-a-fragment") } ^
      "get none when empty fragment" ! { TestParser.parseFragment("#") must_== EmptyFragment } ^
      "get none when no fragment found" ! { TestParser.parseFragment("") must_== EmptyFragment } ^ p^
    "when parsing a query string" ^
      "get the query string value" ! { TestParser.parseQuery("?id=6") must_== MapQueryString("id=6") } ^
      "get none when empty query string" ! { TestParser.parseQuery("?") must_== EmptyQueryString } ^
      "get none when no querystring found" ! { TestParser.parseQuery("") must_== EmptyQueryString } ^ p^
    "when parsing ip addresses" ^
      "parse an ipv4 address" ! { TestParser.parseIPv4("123.23.34.56") must_== IPv4Address("123.23.34.56") } ^
      "parse an ipv6 address" ! {
        TestParser.parseIPv6("2001:0000:1234:0000:0000:C1C0:ABCD:0876") must_== IPv6Address("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
      } ^
      "parse an ipvFuture address" ! {
        TestParser.parseIPvFuture("v2A.dd") must_== IPvFutureAddress("v2A.dd")
      } ^
      "parse an ip Future literal" ! {
        TestParser.parseIPLiteral("[v2A.dd]") must_== IPvFutureAddress("v2A.dd")
      } ^
      "parse an ip v6 literal" ! {
        TestParser.parseIPLiteral("[2001:0000:1234:0000:0000:C1C0:ABCD:0876]") must_== IPv6Address("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
      } ^ p^
    "when parsing paths" ^
      "parse a relative path" ! {
        val seg = ".." :: ".." :: "hello" :: "world.txt" :: Nil
        TestParser.parsePath("../../hello/world.txt") must_== RelativePath(seg)
      } ^
      "parse an absolute path" ! {
        val seg = "hello" :: "world.txt" :: Nil
        TestParser.parsePath("/hello/world.txt") must_== AbsolutePath(seg)
      } ^ p^
    "when parsing the authority" ^
      "parse www.example.org" ! {
        TestParser.parseAuthority("www.example.org") must_== Authority(None, HostName("www.example.org"), None)
      } ^
      "parse www.example.org:8080" ! {
        TestParser.parseAuthority("www.example.org:8080") must_== Authority(None, HostName("www.example.org"), Some(8080))
      } ^
      "parse tom:tim@www.example.org:8080" ! {
        TestParser.parseAuthority("tom:tim@www.example.org:8080") must_== Authority(Some(new UserInfo("tom", "tim")), HostName("www.example.org"), Some(8080))
      } ^
      "parse tom@www.example.org:8080" ! {
        TestParser.parseAuthority("tom@www.example.org:8080") must_== Authority(Some(new UserInfo("tom","")), HostName("www.example.org"), Some(8080))
      } ^
      "parse tom:tim@www.example.org" ! {
        TestParser.parseAuthority("tom:tim@www.example.org") must_== Authority(Some(new UserInfo("tom", "tim")), HostName("www.example.org"), None)
      } ^
      "parse tom@www.example.org" ! {
        TestParser.parseAuthority("tom@www.example.org") must_== Authority(Some(new UserInfo("tom", "")), HostName("www.example.org"), None)
      } ^ p^
    "when parsing a full uri" ^
      "return a failure for 'http://www.exa mple.org'" ! {
        val res = TestParser("http://www.exa mple.org", "http://www.exa mple.org")
        res must beAnInstanceOf[FailedUri]
        res.originalUri must_== "http://www.exa mple.org"
      } ^
      "absolute uri 'http://www.example.org:8080'" ! {
        TestParser("http://www.example.org:8080", "http://www.example.org:8080") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), Some(8080))),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org:8080"
        )
      } ^
      "absolute uri 'http://www.example.org/'" ! {
        TestParser("http://www.example.org/", "http://www.example.org/") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org/"
        )
      } ^
      "absolute uri 'http://www.詹姆斯.org/'" ! {
        Uri("http://www.詹姆斯.org/") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.xn--8ws00zhy3a.org"), None)),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.詹姆斯.org/"
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt'" ! {
        TestParser("http://www.example.org/hello/world.txt", "http://www.example.org/hello/world.txt") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" ::"world.txt" :: Nil),
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org/hello/world.txt"
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three'" ! {
        TestParser("http://www.example.org/hello/world.txt/?id=5&part=three", "http://www.example.org/hello/world.txt/?id=5&part=three") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" ::"world.txt" :: Nil),
          MapQueryString("id=5&part=three"),
          EmptyFragment,
          "http://www.example.org/hello/world.txt/?id=5&part=three"
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go'" ! {
        TestParser("http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go","http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" ::"world.txt" :: Nil),
          MapQueryString("id=5&part=three"),
          StringFragment("there-you-go"),
          "http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go"
        )
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/#here-we-are'" ! {
        TestParser("http://www.example.org/hello/world.txt/#here-we-are", "http://www.example.org/hello/world.txt/#here-we-are") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" ::"world.txt" :: Nil),
          EmptyQueryString,
          StringFragment("here-we-are"),
          "http://www.example.org/hello/world.txt/#here-we-are"
        )
      } ^ end



}