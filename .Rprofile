
if(interactive()) {
  library("base")
  library("methods")
  library("datasets")
  library("utils")
  library("grDevices")
  library("graphics")
  library("stats")

  path_r_profile <- "~/.Rprofile"
  if(file.exists(path_r_profile)) {
    source(path_r_profile)
  }
  rm("path_r_profile")

  suppressWarnings(suppressPackageStartupMessages(library("devtools")))
  suppressWarnings(suppressPackageStartupMessages(library("usethis")))
  suppressWarnings(suppressPackageStartupMessages(library("testthat")))
  suppressWarnings(suppressPackageStartupMessages(library("travis")))
  suppressWarnings(suppressPackageStartupMessages(library("pkgdown")))
  devtools::load_all()
}
