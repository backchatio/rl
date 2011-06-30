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
at this moment the ipv6 address parser still needs some work as there are about 18% failures in the test cases.

## Patches
Patches are gladly accepted from their original author. Along with any patches, please state that the patch is your original work and that you license the work to the *rl* project under the MIT License.

## License
MIT licensed. check the [LICENSE](https://github.com/mojolly/rl/blob/master/LICENSE) file

## Thanks

to the following projects for leading the way:  

*  [ipv6-testcases](http://forums.dartware.com/viewtopic.php?t=452), [perl script](http://download.dartware.com/thirdparty/test-ipv6-regex.pl)
*  [postrank-uri](https://github.com/postrank-labs/postrank-uri)  
*  [domainatrix](https://github.com/pauldix/domainatrix)  
*  [google-guava](http://code.google.com/p/guava-libraries/)  
