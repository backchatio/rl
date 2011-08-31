package rl
package tests

import collection.GenSeq

import org.specs2.Specification
import Uri._
import java.net.{URI,URISyntaxException}

class NotImplementedException(msg: String) extends RuntimeException(msg)
object TestParser {
  def parseFragment(possible: String): Uri = {
    return notImplemented
  }

  def parseQuery(possible: String): Uri = {
    return notImplemented
  }

  def parseIPv4(possible: String): Uri = {
    return notImplemented
  }

  def parseIPv6(possible: String): Uri = {
    return notImplemented
  }

  def parseIPvFuture(possible: String): Uri = {
    return notImplemented
  }

  def parseIPLiteral(possible: String): Uri = {
    return notImplemented
  }

  def parsePath(possible: String): Uri = {
    return notImplemented
  }

  def parseAuthority(possible: String): Uri = {
    return notImplemented
  }

  def apply(toParse: String, originalUri: String): Uri = {
    return notImplemented
  }

  private def notImplemented: Uri = {
    return new FailedUri(new NotImplementedException("This implementation is not complete"), "http://kang.jazz.net")
  }
}

object UriParser {
  def parse(text : String) : Uri = {
    var theUri : URI = null

    try {
      theUri = new URI(text)
    } catch {
      case parseFail : URISyntaxException => return new FailedUri(parseFail, text)
    }
    
    new AbsoluteUri(
      Scheme(theUri.getScheme), 
      Some(Authority(theUri.getAuthority)), 
      parsePath(theUri.getPath),
      parseQueryString(theUri.getQuery),
      parseFragment(theUri.getFragment),
      text
    )
  }

  private def parsePath(text : String) : UriPath = { 
    val isEmptyPath = null == text || text.trim.size == 0 || text == "/"
    if (isEmptyPath) EmptyPath else AbsolutePath(text.split("/").drop(1).toList)  
  }

  private def parseQueryString(text : String) : QueryString = {
    if (null == text || text.trim.size == 0) return EmptyQueryString
    MapQueryString(text)
  }

  private def parseFragment(text : String) : UriFragment = {
    if (null == text || text.trim.size == 0) return EmptyFragment
    StringFragment(text)
  }
}

class UriParserSpec extends Specification {
  def is =

    "A UriParser should" ^
      "when parsing a fragment" ^
      "get the fragment value" ! { TestParser.parseFragment("#i-m-a-fragment") must_== StringFragment("i-m-a-fragment") }.pendingUntilFixed ^
      "get none when empty fragment" ! { TestParser.parseFragment("#") must_== EmptyFragment }.pendingUntilFixed ^
      "get none when no fragment found" ! { TestParser.parseFragment("") must_== EmptyFragment }.pendingUntilFixed ^ p ^
      "when parsing a query string" ^
      "get the query string value" ! { TestParser.parseQuery("?id=6") must_== MapQueryString("id=6") }.pendingUntilFixed ^
      "get none when empty query string" ! { TestParser.parseQuery("?") must_== EmptyQueryString }.pendingUntilFixed ^
      "get none when no querystring found" ! { TestParser.parseQuery("") must_== EmptyQueryString }.pendingUntilFixed ^ p ^
      "when parsing ip addresses" ^
      "parse an ipv4 address" ! { TestParser.parseIPv4("123.23.34.56") must_== IPv4Address("123.23.34.56") }.pendingUntilFixed ^
      "parse an ipv6 address" ! {
        TestParser.parseIPv6("2001:0000:1234:0000:0000:C1C0:ABCD:0876") must_== IPv6Address("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
      }.pendingUntilFixed ^
      "parse an ipvFuture address" ! {
        TestParser.parseIPvFuture("v2A.dd") must_== IPvFutureAddress("v2A.dd")
      }.pendingUntilFixed ^
      "parse an ip Future literal" ! {
        TestParser.parseIPLiteral("[v2A.dd]") must_== IPvFutureAddress("v2A.dd")
      }.pendingUntilFixed ^
      "parse an ip v6 literal" ! {
        TestParser.parseIPLiteral("[2001:0000:1234:0000:0000:C1C0:ABCD:0876]") must_== IPv6Address("2001:0000:1234:0000:0000:C1C0:ABCD:0876")
      }.pendingUntilFixed ^ p ^
      "when parsing paths" ^
      "parse a relative path" ! {
        val seg = ".." :: ".." :: "hello" :: "world.txt" :: Nil
        TestParser.parsePath("../../hello/world.txt") must_== RelativePath(seg)
      }.pendingUntilFixed ^
      "parse an absolute path" ! {
        val seg = "hello" :: "world.txt" :: Nil
        TestParser.parsePath("/hello/world.txt") must_== AbsolutePath(seg)
      }.pendingUntilFixed ^ p ^
      "when parsing the authority" ^
      "parse www.example.org" ! {
        TestParser.parseAuthority("www.example.org") must_== Authority(None, HostName("www.example.org"), None)
      }.pendingUntilFixed ^
      "parse www.example.org:8080" ! {
        TestParser.parseAuthority("www.example.org:8080") must_== Authority(None, HostName("www.example.org"), Some(8080))
      }.pendingUntilFixed ^
      "parse tom:tim@www.example.org:8080" ! {
        TestParser.parseAuthority("tom:tim@www.example.org:8080") must_== Authority(Some(new UserInfo("tom", "tim")), HostName("www.example.org"), Some(8080))
      }.pendingUntilFixed ^
      "parse tom@www.example.org:8080" ! {
        TestParser.parseAuthority("tom@www.example.org:8080") must_== Authority(Some(new UserInfo("tom", "")), HostName("www.example.org"), Some(8080))
      }.pendingUntilFixed ^
      "parse tom:tim@www.example.org" ! {
        TestParser.parseAuthority("tom:tim@www.example.org") must_== Authority(Some(new UserInfo("tom", "tim")), HostName("www.example.org"), None)
      }.pendingUntilFixed ^
      "parse tom@www.example.org" ! {
        TestParser.parseAuthority("tom@www.example.org") must_== Authority(Some(new UserInfo("tom", "")), HostName("www.example.org"), None)
      }.pendingUntilFixed ^ p ^
      "when parsing a full uri" ^
      "return a failure for 'http://www.exa mple.org'" ! {
        val res = UriParser.parse("http://www.exa mple.org")
        res must beAnInstanceOf[FailedUri]
        res.originalUri must_== "http://www.exa mple.org"
      } ^
      "absolute uri 'http://www.example.org:8080'" ! {
        UriParser.parse("http://www.example.org:8080") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), Some(8080))),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org:8080")
      } ^
      "absolute uri 'http://www.example.org/'" ! {
        UriParser.parse("http://www.example.org/") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org/")
      } ^
      "absolute uri 'http://www.詹姆斯.org/'" ! {
        UriParser.parse("http://www.詹姆斯.org/") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.xn--8ws00zhy3a.org"), None)),
          EmptyPath,
          EmptyQueryString,
          EmptyFragment,
          "http://www.詹姆斯.org/")
      }.pendingUntilFixed ^
      "absolute uri 'http://www.example.org/hello/world.txt'" ! {
        UriParser.parse("http://www.example.org/hello/world.txt") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" :: "world.txt" :: Nil),
          EmptyQueryString,
          EmptyFragment,
          "http://www.example.org/hello/world.txt")
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three'" ! {
        UriParser.parse("http://www.example.org/hello/world.txt/?id=5&part=three") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" :: "world.txt" :: Nil),
          MapQueryString("id=5&part=three"),
          EmptyFragment,
          "http://www.example.org/hello/world.txt/?id=5&part=three")
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go'" ! {
        UriParser.parse("http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" :: "world.txt" :: Nil),
          MapQueryString("id=5&part=three"),
          StringFragment("there-you-go"),
          "http://www.example.org/hello/world.txt/?id=5&part=three#there-you-go")
      } ^
      "absolute uri 'http://www.example.org/hello/world.txt/#here-we-are'" ! {
        UriParser.parse("http://www.example.org/hello/world.txt/#here-we-are") must_== AbsoluteUri(
          Scheme("http"),
          Some(Authority(None, HostName("www.example.org"), None)),
          AbsolutePath("hello" :: "world.txt" :: Nil),
          EmptyQueryString,
          StringFragment("here-we-are"),
          "http://www.example.org/hello/world.txt/#here-we-are")
      } ^ end

}
