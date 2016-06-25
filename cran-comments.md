# Version 1.0.0

## Round 1

### Test environments

* Windows 7, R 3.2.2 (local)
* ubuntu 12.04, R 3.2.0 (on travis-ci)
* ubuntu 14.04, R 3.1.3 (on my DigitalOckean droplet)

### Submission comments

2016-02-18

R CMD check passed with 0 warnings or errors, and notes about the provided sample data being large (4MB) and an invalid URL that will be valid once the package is on CRAN (http://cran.r-project.org/package=ddpcr)

### Reviewer comments

2016-02-18 Kurt Hornik:

```
You mis-use file LICENSE: for MIT this should only be the completed
template, see <https://www.r-project.org/Licenses/MIT>.

Then:

Unndefined global functions or variables:
  abline capture.output dbinom density head kmeans median modifyList
  packageVersion pbinom points quantile read.csv sd setNames tail
Consider adding
  importFrom("graphics", "abline", "points")
  importFrom("stats", "dbinom", "density", "kmeans", "median", "pbinom",
             "quantile", "sd", "setNames")
  importFrom("utils", "capture.output", "head", "modifyList",
             "packageVersion", "read.csv", "tail")
to your NAMESPACE file.

Pls fix
```

## Round 2

### Submission comments

2016-02-19

Addressed all previous comments (used conventional LICENSE file and namespaced all functions from default packages)

### Reviewer comments

2016-02-18 Kurt Hornik:

```
Now

* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘cran-comments.md’

Pls fix

Best
-k
```

## Round 3

### Submission comments

2016-02-19

Added cran-comments to buildignore

### Reviewer comments

2016-02-18 Kurt Hornik:

```
We also see

Found the following (possibly) invalid URLs:
  URL: http://cran.r-project.org/package=ddpcr
    From: inst/doc/overview.html
    Status: 404
    Message: Not Found

This cannot work yet, but should be https:// instead.

Pls fix

Best
-k
```

## Round 4

### Submission comments

2016-02-19

Changed CRAN package URL from http to https

### Reviewer comments

2016-02-18 Kurt Hornik:

```
Thanks, on CRAN now.

Best
-k
```

# Version 1.1.2

## Round 1

### Test environments

* Windows 7, R 3.2.4 (local)
* ubuntu 12.04, R 3.2.3 (on travis-ci)
* ubuntu 14.04, R 3.1.3 (on my DigitalOcean droplet)

### Submission comments

2016-03-16

R CMD check passed with no errors/warnings/notes

### Reviewer comments

2016-03-17 Kurt Hornik:

Thanks, on CRAN now

---

# Version 1.3

## Round 1

### Submission comments

2016-06-03 

No errors or warnings. 1 note telling me that the package was "Archived on 2016-05-02 as check problems were not corrected despite reminders." but I never got any notification of any checks not passing

## Reviewer comments

2016-06-04 Kurt Hornik:

```
This fails on win-builder with

* checking tests ...
** running tests for arch 'i386' ... ERROR
Running the tests in 'tests/testthat.R' failed.
Last 13 lines of output:
  1. Failure: is_dir is TRUE when passed a directory (@test-utils.R#6) -----------
  is_dir("../../tests") isn't true.


  2. Failure: is_dir is TRUE when passed a directory (@test-utils.R#7) -----------
  is_dir("../../tests/") isn't true.
```

## Round 2

### Submission comments

2016-06-04 

No errors or warnings, one note telling me the package was archived recently because of failing tests

## Reviewer comments

2016-06-04 Uwe Ligges

Thanks, on CRAN now.

---

# Version 1.4

## Round 1

### Submission comments

2016-06-24 

No errors, warnings, or notes. Need to update only 3 weeks after previous submission in order to comply with Hadley's new dplyr package that was released today

## Reviewer comments

2016-06-25

