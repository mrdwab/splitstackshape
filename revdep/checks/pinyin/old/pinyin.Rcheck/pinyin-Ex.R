pkgname <- "pinyin"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pinyin')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bookdown2py")
### * bookdown2py

flush(stderr()); flush(stdout())

### Name: bookdown2py
### Title: Convert the Chinese headers of bookdown .Rmd files into Pinyin
### Aliases: bookdown2py

### ** Examples

bookdown2py(dic = NA)



cleanEx()
nameEx("file.rename2py")
### * file.rename2py

flush(stderr()); flush(stdout())

### Name: file.rename2py
### Title: Rename files according to a given dictionary
### Aliases: file.rename2py

### ** Examples

file.rename2py(dic = NA)



cleanEx()
nameEx("file2py")
### * file2py

flush(stderr()); flush(stdout())

### Name: file2py
### Title: Convert the characters in an entire files according to a given
###   dictionary
### Aliases: file2py

### ** Examples

file2py(dic = NA)



cleanEx()
nameEx("load_dic")
### * load_dic

flush(stderr()); flush(stdout())

### Name: load_dic
### Title: Load a customized dictionary.
### Aliases: load_dic

### ** Examples

load_dic()



cleanEx()
nameEx("pinyin")
### * pinyin

flush(stderr()); flush(stdout())

### Name: pinyin
### Title: Former version of py()
### Aliases: pinyin

### ** Examples

pinyin()



cleanEx()
nameEx("py")
### * py

flush(stderr()); flush(stdout())

### Name: py
### Title: Convert strings of Chinese characters into Pinyin.
### Aliases: py

### ** Examples

py(dic = NA)



cleanEx()
nameEx("pydic")
### * pydic

flush(stderr()); flush(stdout())

### Name: pydic
### Title: Load a Pinyin library
### Aliases: pydic

### ** Examples

pydic()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
