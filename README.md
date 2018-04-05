# splitstackshape

[![Build Status](https://travis-ci.org/mrdwab/splitstackshape.svg?branch=v2.0)](https://travis-ci.org/mrdwab/splitstackshape)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mrdwab/splitstackshape?branch=v2.0&svg=true)](https://ci.appveyor.com/project/mrdwab/splitstackshape)
[![Coverage Status](https://img.shields.io/codecov/c/github/mrdwab/splitstackshape/v2.0.svg)](https://codecov.io/gh/mrdwab/splitstackshape/branch/v2.0)

R functions to split concatenated data, stack columns of your datasets, and convert your data into different shapes.

## Core Functions

* `cSplit`: A core function that collects the functionality of several of the  `concat.split` family of functions.
* `concat.split`: A set of functions to split strings where data have been concatenated into a single value, as is common when getting data collected with tools like Google Forms. (`cSplit_l` to return a `list`; and `cSplit_e` to return an "expanded" view of the input data.)
* `Stacked`: A function to create a list of `stack`ed sets of variables. Similar to `melt` from "reshape2", but doesn't put everything into one very long `data.frame`.
* `reshape_long`: A function to reshape data into a long form, even if measurements are unbalanced. This replaces the `Reshape` and `merged.stack` functions.
* `stratified`: A function to take random row samples by groups, similar to `sample_n` and `sample_frac` from "dplyr".

## Utilities

* `getanID`: A function for creating a secondary ID when duplicated "id" variables are present.
* `expandRows`: "Expands" the rows of a dataset.
* `listCol_l`, `listCol_w`, and `unlist_cols`: Unlists (long) or flattens (wide) a column in a `data.frame` or a `data.table` stored as a `list`. 

See the [**NEWS**](NEWS.md) file for more function changes.

## Install

The package [is on CRAN](https://CRAN.R-project.org/package=splitstackshape). You can install it using:

```r
install.packages("splitstackshape")
```

To install the V2 pre-release version, use:

```r
library(devtools)
install_github("mrdwab/splitstackshape", ref = "v2.0")
```

To install the V1 development version, use:

```r
library(devtools)
install_github("mrdwab/splitstackshape", ref = "v1_development")
```

Current CRAN version: 1.4.4
