# The Mojolly/BackChat.io RL library

This is a url utility library that parses URL's conforming to [RFC-3986](http://tools.ietf.org/html/rfc3986).

## Why?
While the java platform gives a lot, many things aren't relevant or outdated. This library tries to update the understanding of a url or uri to a deeper level. We process a bunch of feeds that get added arbitrarily. We use this library to canonicalize and disambiguate between all the urls pointing to the same domain.

## What?
In addition to parsing a url (for which it partially leans on [java.net.URI](http://download.oracle.com/javase/6/docs/api/java/net/URI.html) and [java.net.URL](http://download.oracle.com/javase/6/docs/api/java/net/URL.html)), it also normalizes and canonicalizes public domains with the list found at: [public suffix list](http://publicsuffix.org/).
So how does this library improve on the current URL and URI implementations?  

*  url encoding conforms to [RFC-3986](http://tools.ietf.org/html/rfc3986)  
*  normalizes urls along the following [guidelines](http://en.wikipedia.org/wiki/URL_normalization) cfr. [RFC-3986](http://tools.ietf.org/html/rfc3986)  
*  canonicalizes urls by stripping common query string parameters and reordering the remaining querystrings in alphabetical order.


## Patches
Patches are gladly accepted from their original author. Along with any patches, please state that the patch is your original work and that you license the work to the *rl* project under the MIT License.

## License
MIT licensed. check the [LICENSE](https://github.com/mojolly/rl/blob/master/LICENSE) file

## Thanks

to the following projects for leading the way:  

*  [postrank-uri](https://github.com/postrank-labs/postrank-uri)  
*  [domainatrix](https://github.com/pauldix/domainatrix)  
*  [google-guava](http://code.google.com/p/guava-libraries/)  
