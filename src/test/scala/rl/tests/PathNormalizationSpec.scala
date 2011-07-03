package rl
package tests

import org.specs2.Specification

class PathNormalizationSpec extends Specification { def is =

  "Normalizing windows paths should" ^
    "convert a local path 'C:\\Windows\\Temp\\foo.txt'" ! normalizeLocalWindowsPath ^
    "convert a relative path 'Windows\\Temp\\foo.txt'" ! normalizeRelativeWindowsPath ^
    "convert a UNC path '\\\\theserver\\theshare\\thefile.txt" ! normalizeUncWindowsPath ^ end

  def normalizeLocalWindowsPath = {
    Uri.normalizeWindowsPath("C:\\Windows\\Temp\\foo.txt") must_== "file:///C:/Windows/Temp/foo.txt"
  }

  def normalizeRelativeWindowsPath = {
    Uri.normalizeWindowsPath("Windows\\Temp\\foo.txt") must_== "Windows/Temp/foo.txt"
  }

  def normalizeUncWindowsPath = {
    Uri.normalizeWindowsPath("\\\\theserver\\theshare\\thefile.txt") must_== "file://theserver/theshare/thefile.txt"
  }
}