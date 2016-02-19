# Version 1.0.0

## Round 1

### Test environments

* Windows 7, R 3.2.2 (local)
* ubuntu 12.04, R 3.2.0 (on travis-ci)
* ubuntu 14.04, R 3.1.3 (on my DigitalOcean droplet)

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