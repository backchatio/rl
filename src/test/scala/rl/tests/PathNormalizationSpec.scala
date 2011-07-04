package rl
package tests

import org.specs2.Specification

class PathNormalizationSpec extends Specification { def is =

  "Normalizing windows paths should" ^
    "convert a local path 'C:\\Windows\\Temp\\foo.txt'" ! normalizeLocalWindowsPath ^
    "convert a relative path 'Windows\\Temp\\foo.txt'" ! normalizeRelativeWindowsPath ^
    "convert a UNC path '\\\\theserver\\theshare\\thefile.txt" ! normalizeUncWindowsPath  ^ end

  def normalizeLocalWindowsPath = {
    PathUtils.windowsToUnixPath("C:\\Windows\\Temp\\foo.txt") must_== "file:///C:/Windows/Temp/foo.txt"
  }

  def normalizeRelativeWindowsPath = {
    PathUtils.windowsToUnixPath("Windows\\Temp\\foo.txt") must_== "Windows/Temp/foo.txt"
  }

  def normalizeUncWindowsPath = {
    PathUtils.windowsToUnixPath("\\\\theserver\\theshare\\thefile.txt") must_== "file://theserver/theshare/thefile.txt"
  }

  def normalizeSpacesInPath = {
    PathUtils.windowsToUnixPath("C:\\Windows\\Program Files\\blah.txt") must_== "file:///C:/Windows/Program%20Files/blah.txt"
  }
}