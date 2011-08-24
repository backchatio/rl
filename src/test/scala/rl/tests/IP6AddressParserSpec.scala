//package rl
//package tests
//
//import org.specs2.Specification
//
//
//class IP6AddressParserSpec extends Specification { def is = {
//
//  "Parsing an IP Address should" ^
//    "when matching ABNF: 6( h16 \":\" ) ls32" ^
//      "should parse with an IPv4 address attached" ! {
//        TestIPAddressParser.parseAddress("8:39AF:888:1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached" ! {
//        TestIPAddressParser.parseAddress("8:39AF:888:1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^ p ^
//    "when matching ABNF: \"::\" 5( h16 \":\" ) ls32" ^
//      "should parse with an IPv4 address attached" ! {
//        TestIPAddressParser.parseAddress("::39AF:888:1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached" ! {
//        TestIPAddressParser.parseAddress("::39AF:888:1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^ p ^
//    "when matching ABNF: [               h16 ] \"::\" 4( h16 \":\" ) ls32" ^
//      "should parse with an IPv4 address attached with optional h16 present" ! {
//        TestIPAddressParser.parseAddress("475::39AF:1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with optional h16 present" ! {
//        TestIPAddressParser.parseAddress("485::39AF:1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with optional h16 absent" ! {
//        TestIPAddressParser.parseAddress("::39AF:1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with optional h16 absent" ! {
//        TestIPAddressParser.parseAddress("::39AF:1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^ p ^
//    "when matching ABNF: [ *1( h16 \":\" ) h16 ] \"::\" 3( h16 \":\" ) ls32" ^
//      "should parse with an IPv4 address attached with both optional parts present" ! {
//        TestIPAddressParser.parseAddress("4852:475::1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with both optional parts present" ! {
//        TestIPAddressParser.parseAddress("4851:485::1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("475::1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("485::1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:3333:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:3333:123:123", "valid") must_== "valid"
//      } ^ p ^
//    "when matching ABNF: [ *2( h16 \":\" ) h16 ] \"::\" 2( h16 \":\" ) ls32" ^
//      "should parse with an IPv4 address attached with all optional parts present" ! {
//        TestIPAddressParser.parseAddress("3333:4852:475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with all optional parts present" ! {
//        TestIPAddressParser.parseAddress("3333:4851:485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with two optional parts present" ! {
//        TestIPAddressParser.parseAddress("4852:475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with two optional parts present" ! {
//        TestIPAddressParser.parseAddress("4851:485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:123:123", "valid") must_== "valid"
//      } ^ p ^
//    "when matching ABNF: [ *3( h16 \":\" ) h16 ] \"::\"    h16 \":\"   ls32" ^
//      "should parse with an IPv4 address attached with all optional parts present" ! {
//        TestIPAddressParser.parseAddress("3333:4852:475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with all optional parts present" ! {
//        TestIPAddressParser.parseAddress("3333:4851:485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with two optional parts present" ! {
//        TestIPAddressParser.parseAddress("4852:475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with two optional parts present" ! {
//        TestIPAddressParser.parseAddress("4851:485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("475::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with one optional part present" ! {
//        TestIPAddressParser.parseAddress("485::1111:2222:123:123", "valid") must_== "valid"
//      } ^
//      "should parse with an IPv4 address attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:123.123.123.123", "valid") must_== "valid"
//      } ^
//      "should parse with an h16:h16 attached with no optional parts present" ! {
//        TestIPAddressParser.parseAddress("::1111:2222:123:123", "valid") must_== "valid"
//      } ^ end
//  }
//}