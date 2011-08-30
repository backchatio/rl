package rl.tests

import org.specs2.Specification
import rl.DomainParser

class DomainParserSpec extends Specification {
  def is =

    "A domain parser should" ^
      "reading the dat file" ^
        "create the first level of the tree" ! { DomainParser.publicSuffixes.contains("com") must beTrue } ^
        "create the first level of the tree even when the first doesn't appear on a line on its own" ! {
          DomainParser.publicSuffixes.contains("uk") must beTrue
        } ^
        "create the lower levels of the tree" ! {
          (DomainParser.publicSuffixes("jp") contains "ac" must beTrue) and
            (DomainParser.publicSuffixes("jp")("aichi") contains "*" must beTrue)
        } ^ end

}