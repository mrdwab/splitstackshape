# splitstackshape

R functions to split concatenated data, conveniently stack columns of `data.frame`s, and conveniently reshape `data.frame`s.

## Core Functions

* `concat.split`: A set of functions to split strings where data have been concatenated into a single value, as is common when getting data collected with tools like Google Forms.
* `Stacked`: A function to create a list of `stack`ed sets of variables. Similar to `melt` from "reshape2", but doesn't put everything into one very long `data.frame`.
* `Reshape`: A function to allow base R's `reshape` function to work with "unbalanced" datasets.

## Utilities

* `getanID`: A function for creating a secondary ID when duplicated "id" variables are present.

## Install

The package [is on CRAN](http://cran.r-project.org/web/packages/splitstackshape/index.html). You can install it using:

```r
install.packages("splitstackshape")
```

To install the development version, use:

```r
library(devtools)
install_github("splitstackshape", "mrdwab", ref = "devel")
```

<<<<<<< HEAD
Current version: 1.2.1
=======
Current version: 1.2.0
>>>>>>> master




