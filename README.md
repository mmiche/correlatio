# Info
The development version of the 'correlatio' package can be downloaded from GitHub (see below). The stable version can be downloaded from CRAN.

## From GitHub
1. If not yet installed, install the R package `devtools`, so that the `correlatio` package can be downloaded from GitHub.
```R
install.packages("devtools")
```
2. The `correlatio` package requires 2 other packages to be installed, which are available on CRAN:
```R
install.packages(pkgs = c("ggplot2", "tibble"), dependencies = TRUE)
```
3. Install the `correlatio`package:
```R
devtools::install_github(repo = "https://github.com/mmiche/correlatio",
                         build_vignettes = TRUE)
```

## From CRAN (as soon as available)
```R
install.packages(pkgs = c("correlatio"), dependencies = TRUE)
```
