# splitstackshape

R functions to split concatenated data, conveniently stack columns of `data.frame`s, and conveniently reshape `data.frame`s.

## Core Functions

* `concat.split`: A set of functions to split strings where data have been concatenated into a single value, as is common when getting data collected with tools like Google Forms.
* `Stacked`: A function to create a list of `stack`ed sets of variables. Similar to `melt` from "reshape2", but doesn't put everything into one very long `data.frame`.
* `Reshape`: A function to allow base R's `reshape` function to work with "unbalanced" datasets.

## Install

The package [is on CRAN](http://cran.r-project.org/web/packages/splitstackshape/index.html) (though the mirrors have yet to be populated and Windows binaries have yet to be built). Once that is good, you should be able to use:

```r
install.packages("splitstackshape")
```

To install the development version, use:

```r
library(devtools)
install_github("splitstackshape", "mrdwab")
```





