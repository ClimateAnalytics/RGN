rhub::check_for_cran() gives 6 NOTES (some are repeats) - see below.

All seem acceptable 
- 'misspelled' words are correct, possible invalid url is valid
- 'non-standard things in the check directory' and 'detritus in the temp directory' notes seem to be bugs (based on web searches)

---------------------------------------------------------------

> results = rhub::check_for_cran()
> results$cran_summary()

For a CRAN submission we recommend that you fix all NOTEs, WARNINGs and ERRORs.
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub linux-x86_64-rocker-gcc-san (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'David McInerney <dmcinern@gmail.com>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Qin (19:63)
    RGN (3:29, 17:57)
    al (19:71)
    et (19:67)
  
  Found the following (possibly) invalid URLs:
    URL: http://www.bom.gov.au/water/hrs/
      From: man/BassRiverData.Rd
      Status: 403
      Message: Forbidden

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
    'lastMiKTeXException'
  Found the following files/directories:

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [5s/14s] NOTE
  Maintainer: ‘David McInerney <dmcinern@gmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    al (19:71)
    et (19:67)
    Qin (19:63)
    RGN (3:29, 17:57)
  
  Found the following (possibly) invalid URLs:
    URL: http://www.bom.gov.au/water/hrs/
      From: man/BassRiverData.Rd
      Status: 403
      Message: Forbidden

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/21s] NOTE
  Maintainer: ‘David McInerney <dmcinern@gmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Qin (19:63)
    RGN (3:29, 17:57)
    al (19:71)
    et (19:67)
  
  Found the following (possibly) invalid URLs:
    URL: http://www.bom.gov.au/water/hrs/
      From: man/BassRiverData.Rd
      Status: 403
      Message: Forbidden

0 errors ✔ | 0 warnings ✔ | 6 notes ✖
