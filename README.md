# The Mojolly/BackChat.io RL library

This is a url utility library that parses URL's conforming to [RFC-3986](http://tools.ietf.org/html/rfc3986).

## Why?
While the java platform gives a lot, many things aren't relevant or outdated. This library tries to update the understanding of a url or uri to a deeper level. We process a bunch of feeds that get added arbitrarily. We use this library to canonicalize and disambiguate between all the urls pointing to the same domain.

## What?
The library implements a uri parser defined by the [ABNF grammar in RFC-3986](http://tools.ietf.org/html/rfc3986#appendix-A).
In addition to parsing a url, it also normalizes and canonicalizes public domains with the list found at: [public suffix list](http://publicsuffix.org/).
So how does this library improve on the current URL and URI implementations?  

*  url encoding conforms to [RFC-3986](http://tools.ietf.org/html/rfc3986)  
*  normalizes urls along the following [guidelines](http://en.wikipedia.org/wiki/URL_normalization) cfr. [RFC-3986](http://tools.ietf.org/html/rfc3986)  
*  canonicalizes urls by stripping common query string parameters and reordering the remaining querystrings in alphabetical order.

## Running the tests
To run the tests with sbt-0.10 you have to increase the thread stack size (ie. -Xss4m).

## Patches
Patches are gladly accepted from their original author. Along with any patches, please state that the patch is your original work and that you license the work to the *rl* project under the MIT License.

## License
MIT licensed. check the [LICENSE](https://github.com/mojolly/rl/blob/master/LICENSE) file

## TODO
Make the parsers pluggable so that the scala parser combinator based one can be replaced as it's slow as.

```
git:(next) ✗ » java -server -Dfile.encoding=UTF-8 -cp ~/projects/mojolly/local/scala/lib/scala-library.jar:. rl.Benchmark
string to encode: I'm a very long $tring with twitter://casualjim in, it
url to parse: http://www.詹姆斯.org/path/to/somewhere/?id=45&dskafd=safla&sdkfa=sd#dksd$sdl
java encoded: I%27m+a+very+long+%24tring+with+twitter%3A%2F%2Fcasualjim+in%2C+it
java parsed: http://www.%E8%A9%B9%E5%A7%86%E6%96%AF.org/path/to/somewhere/?id=45&dskafd=safla&sdkfa=sd#dksd$sdl
rl encoded: I'm%20a%20very%20long%20$tring%20with%20twitter://casualjim%20in,%20it
rl parsed: http://www.xn--8ws00zhy3a.org//path/to/somewheresdkfa=sd&dskafd=safla&id=45#dksd$sdl
Start url encoding bench
Start url decoding bench
Starting uri parsing bench

Started: Tue Jul 05 13:35:34 BST 2011
To encode 1000000 uri's
Java took: 2445 millis
RL took: 1882 millis

To decode 1000000 uri's
Java took: 1745 millis
RL took: 669 millis

To parse 100000 uri's
java took: 898 millis
rl took: 22198 millis



git:(next) ✗ » scala -J-server -Dfile.encoding=UTF-8 rl.Benchmark
string to encode: I'm a very long $tring with twitter://casualjim in, it
url to parse: http://www.詹姆斯.org/path/to/somewhere/?id=45&dskafd=safla&sdkfa=sd#dksd$sdl
java encoded: I%27m+a+very+long+%24tring+with+twitter%3A%2F%2Fcasualjim+in%2C+it
java parsed: http://www.%E8%A9%B9%E5%A7%86%E6%96%AF.org/path/to/somewhere/?id=45&dskafd=safla&sdkfa=sd#dksd$sdl
rl encoded: I'm%20a%20very%20long%20$tring%20with%20twitter://casualjim%20in,%20it
rl parsed: http://www.xn--8ws00zhy3a.org//path/to/somewheresdkfa=sd&dskafd=safla&id=45#dksd$sdl
Start url encoding bench
Start url decoding bench
Starting uri parsing bench

Started: Tue Jul 05 13:37:13 BST 2011
To encode 1000000 uri's
Java took: 2388 millis
RL took: 2129 millis

To decode 1000000 uri's
Java took: 1793 millis
RL took: 667 millis

To parse 100000 uri's
java took: 971 millis
rl took: 21548 millis
```

So in the decoding and encoding part we're either on par with or faster than then java.net.URLEncoder and java.net.URLDecoder

## Thanks

to the following projects for leading the way:  

*  [ipv6-testcases](http://forums.dartware.com/viewtopic.php?t=452), [perl script](http://download.dartware.com/thirdparty/test-ipv6-regex.pl)
*  [postrank-uri](https://github.com/postrank-labs/postrank-uri)  
*  [domainatrix](https://github.com/pauldix/domainatrix)  
*  [google-guava](http://code.google.com/p/guava-libraries/)  
