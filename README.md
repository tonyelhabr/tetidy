
tetidy <img src="man/figures/logo.png" align="right"/>
======================================================

Introduction
------------

This package contains functions that I use often in projects.

### Installation

`devtools::install_github("tonyelhabr/tetidy")`.

Notes
-----

### Inspiration

The original purpose of this package was to provide a convenient interface to the `readr` and `rio` functions for importing and exporting data to various file formats. Because I often found myself using the variable name as the name of the file, I figured it would be nice to implement "lazy" methods for doing so. Under the hood, this packages's functions use "Non-Standared Evaluation" (NSE) (using techniques that may or may not be up to date with the latest `tidyverse` methods of NSE 😄).

Additionally, while implementing these `import`/`export` functions, I realized a couple of other functions that I often use in projects (e.g. `render_proj_io()` could be added. (I don't necessarily recommend using these "other" functions, however, because they are tailored towards my use cases.)

### Syntax [1]

-   `import_ext*()` functions - Implement `rio`-like interface (note the use of the verb "import") for reading data from files. The main function is `import_ext()`, which requires that the file extension be specified with the `ext` paramater. Shorcuts for the extensions that I find myself using most often are implemented via the syntax `import_ext_[ext]()` (e.g. `import_ext_csv()`). Under the hood, preference is given to `readr` methods if they exist and can be recognized. (This is because it reads in `data.frame`s as `tibble`s by default. Otherwise, an attempt is made to use a `rio` method (with subsequent coercion to a `tibble`).

-   `export_ext*()` functions - Counterpart to the `import_ext_*()` functions. Notably, exporting of plots is supported. However, the support is not robust-- it is intended only for use with `ggplot2` plots, and only to the `png` file format. The `units`, `width`, and `height` are set to package option values (see `options("tetidy.ggsave.units"`, etc.) if they are not all specified explicitly together.

-   `*_proj_io()` functions - "Meta" functions for an RStudio project. `parse_proj_io()` attempts to create a `tibble` with information regarding the input/output files in each script in an RStudio project. The output could be used in a number of ways, such as in a node-dependency graph visualizing how the project's files relate to one another. `render_proj_io()` (a wrapper for `rmarkdwon::render()`) converts .R scripts formatted with Roxygen2 style comments to .Rmd files. This is intended to be used by users (like me) who prefer the "feel" of an R script when working with data that will eventually be presented in a .Rmd file/report and don't want to work directly with a .Rmd file. [2] **NOTE:** These functions are fairly experimental and should not be relied upon for anything that should be reliable.

-   `set_pkg_*_opts()` functions - Functions to facilitate setting this package's options (This is preferable to calling `options()` directly, which requires knowing exactly what the name of the option is).

Here is a list of all functions in the package.

    #>  [1] "create_dir"          "do_call_with"        "export_ext"         
    #>  [4] "export_ext_csv"      "export_ext_png"      "export_ext_rda"     
    #>  [7] "export_ext_rdata"    "export_ext_RData"    "export_ext_rds"     
    #> [10] "export_ext_xlsx"     "export_gg"           "export_path"        
    #> [13] "get_path_lazily"     "get_path_safely"     "import_ext"         
    #> [16] "import_ext_csv"      "import_ext_rda"      "import_ext_rdata"   
    #> [19] "import_ext_RData"    "import_ext_rds"      "import_ext_xlsx"    
    #> [22] "import_path"         "import_path_cleanly" "render_proj_io"     
    #> [25] "set_pkg_render_opts" "sort_named_list"     "warningf"

Examples
--------

Unfortunately, none (right now).

[1] The `fundManageR` package inspired some of the format of this section. See its [README file](https://github.com/abresler/fundManageR/blob/master/readme.Rmd).

[2] See [this blog post](http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/) for more discussion.
