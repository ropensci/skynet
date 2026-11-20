## Resubmission

This is a resubmission of a package archived on 2024-10-20 because the
maintainer email was undeliverable. The maintainer email has been updated
to a working address (Filipeamteixeira@gmail.com).

In this version I have, following the review of 2026-11-20:

* Rewritten the Description so it no longer starts with "A ... tool".
* Added \value to the Rd files of all exported functions, describing the
  class, structure and meaning of the output (or that the function is called
  for its side effects).
* Replaced \dontrun{} with \donttest{}. Examples that run on the bundled data
  in under 5 seconds are now unwrapped. The download_*() examples stay in
  \donttest{} wrapped in if(interactive()), because they download large files
  from the BTS website.
* Removed top-level calls to options(). download_db1b() now saves the timeout
  option and restores it with on.exit() immediately after changing it.

## Test environments

* local macOS install, R release
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / Package was archived on CRAN: see above.
* Possibly misspelled words in DESCRIPTION: 'BTS' is the acronym for the
  Bureau of Transportation Statistics, spelled out in the Description.

## Reverse dependencies

There are no reverse dependencies.
