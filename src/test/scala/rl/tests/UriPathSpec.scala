package rl
package tests

import org.specs2.Specification

class UriPathSpec extends Specification {
  def is =

    "Normalizing windows paths should" ^
      "convert a local path 'C:\\Windows\\Temp\\foo.txt'" ! normalizeLocalWindowsPath ^
      "convert a relative path 'Windows\\Temp\\foo.txt'" ! normalizeRelativeWindowsPath ^
      "convert a UNC path '\\\\theserver\\theshare\\thefile.txt" ! normalizeUncWindowsPath ^
      "Encode the spaces in a path" ! normalizeSpacesInPath ^ end

  def normalizeLocalWindowsPath = {
    UriPath.windowsToUnixPath("C:\\Windows\\Temp\\foo.txt") must_== "file:///C:/Windows/Temp/foo.txt"
  }

  def normalizeRelativeWindowsPath = {
    UriPath.windowsToUnixPath("Windows\\Temp\\foo.txt") must_== "Windows/Temp/foo.txt"
  }

  def normalizeUncWindowsPath = {
    UriPath.windowsToUnixPath("\\\\theserver\\theshare\\thefile.txt") must_== "file://theserver/theshare/thefile.txt"
  }

  def normalizeSpacesInPath = {
    UriPath.windowsToUnixPath("C:\\Windows\\Program Files\\blah.txt") must_== "file:///C:/Windows/Program%20Files/blah.txt"
  }
}