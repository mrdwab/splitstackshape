# splitstackshape NEWS

---------------------------------------------------------------------

Author/Maintainer : Ananda Mahto
Email             : ananda@mahto.info
URL               : http://github.com/mrdwab/splitstackshape
BugReports        : http://github.com/mrdwab/splitstackshape/issues

---------------------------------------------------------------------

## 1.4.4

29 March 2018

* `Reshape()` bugfix. Reported at [https://stackoverflow.com/q/49281838/1270695](https://stackoverflow.com/q/49281838/1270695).
* `listCol_w()` bugfix. Thanks to @jazzurro.
* `cSplit_e()` bugfix. Reported at [https://stackoverflow.com/q/48576331/1270695](https://stackoverflow.com/q/48576331/1270695)

## 1.4.3

20 March 2018

* Updated the NAMESPACE to address the NOTES when building 
  the package

## 1.4.2

23 October 2014

* `listCol_l` and `listCol_w` added as utilities for unlisting 
  or flattening columns stored as `list`s in `data.frame`s and
  `data.table`s.

## 1.4.1

18 October 2014

Bug in `:::.stripWhite` when using `"|"` as a delimiter fixed.

## 1.4.0

13 October 2014

See 1.3.0 -- 1.3.8 for details of changes.

`cSplit` now replaces `concat.split.compact` and 
`concat.split.multiple` in `concat.split`; `cSplit_f` has been
introduced as a related function. Other new functions are
`stratified` and `expandRows`.

## 1.3.8

12 October 2014

### Added functions

* `cSplit_f`

  The "_f" is both representative of `fread`, which this
  function uses to split the concatenated cells, and "fixed"",
  which is indicative of the fact that this function would
  only work if the number of resulting columns is the same
  for each row in the input.

* `expandRows`

  "Expand" the rows of a `data.frame` or a `data.table` either
  by values specified in a column of the input dataset or by
  a vector specifying the number of times to repeat each row.

### Enhancement

* `Reshape`, `Stacked`, and `merged.stack` now try to guess
  the "`id.vars`" values based on the values in "`var.stubs`".
  The values can still be specified manually.

## 1.3.1 -- 1.3.7

08/10 October 2014

Incremental cleanups and additions to get ready for V1.4.0

### Enhancement

* `concat.split.compact` and `concat.split.multiple` are now
  simply wrappers for `cSplit` and no longer use 
  `:::read.concat` to split up the values.
* `concat.split.expanded` and `concat.split.list` now made
  `data.table` compatable. 
* `concat.split.list` and `concat.split.expanded` given short
  name forms (`cSplit_l` and `cSplit_e`).

Added functions:

* `cSplit`

  Before the release of 1.4.0, the basic `concat.split*` 
  functions would become simple wrappers for `cSplit`, which
  is much more efficient than the previous implementations.
  The earlier functions will remain for compatability purposes.
  Since `cSplit` is already in use, it will be an exported
  function.

* `stratified`

  A function to take fixed or proportional samples by group
  from a `data.frame` or `data.table.

Non-exported additions:

* `:::.collapseMe` 
* `:::.stripWhite`
* `:::Names`
* `:::trim`
* `:::vGrep`

## 1.3.0

27 October 2013

### Feature Changes

* Due to changes resulting from the introduction of `numMat` and
  `charMat`, `concat.split.expanded` and `concat.split` now have 
  an additional argument, `type`, which takes a value of either
  `"numeric"` or `"character"`. It is set to a default of 
  `type = "numeric"` in the case of `concat.split.expanded` and 
  `type = NULL` in the case of `concat.split`.

Added functions:

* :::numMat

  `numMat` replaces `binaryMat` and `valueMat` for numeric data.

* :::charMat

  `charMat` replaces `charBinaryMat` for string data.

Dropped functions:

Due to changes introduced after recommendations by @flodel, 
the following functions have been rewritten as `numMat` and 
`charMat`

* :::binaryMat
* :::valueMat
* :::charBinaryMat

## 1.2.1

20 October 2013

### Feature Changes

New function added:

* :::charBinaryMat

  `concat.split.expanded` did not previously support expanding 
  "character" data. Due to prompting by @juba, `charBinaryMat` 
  has been included to handle such cases.

## 1.2.0

27 August 2013

### Enhancement

* Further refinement of `Stacked` and `merge.stack`. `merge.stack`
  is now faster than `Reshape`, at least for large datasets.

## 1.1.0

18 August 2013

### Enhancement

* `Stacked` and `merge.stack` now made MUCH faster using almost
  a pure `data.table` solution.

## 1.0.2

17 August 2013

### Bugfixes

* When `Stacked` results in a list of length 1, it is "unlisted"
  before being returned.
* `Reshape` (and as a result, `concat.split.multiple(..., 
  direction = "long")`) has been enhanced by the addition of a
  feature to automatically add an ID variable if the present 
  "IDs" are not unique.

## Feature Changes

New functions added:

* getanID
* :::Names

## 1.0.1

16 August 2013

### Bugfixes

* `read.concat` updated to use `count.fields` to determine the 
  correct number of columns that the resulting `data.frame` 
  should have.

### Feature change

* `Reshape` now has an option to remove the `rownames` from the
  output, set to `TRUE` by default.

## 1.0

12 August 2013

Initial commit of splitstacshape with the following main functions:

* concat.split (plus: concat.split.compact, concat.split.expanded, 
  concat.split.list, and concat.split.multiple) -- To split 
  concatenated data into more manageable data formats.
* Reshape -- To help base R's reshape function handle unbalanced 
  data and simplify the reshape syntax (wide to long only).
* Stacked -- To selectively stack columns of a data.frame.

### Full list of functions

*Non-exported functions are indicated with ::: before their names.*

* concat.split.compact
* concat.split.expanded
* concat.split.list
* concat.split.multiple
* concat.split
* merged.stack
* Reshape
* Stacked
* :::binaryMat
* :::FacsToChars
* :::NoSep
* :::othernames
* :::read.concat
* :::valueMat
