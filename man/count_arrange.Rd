% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-combos.R
\name{count_arrange}
\alias{count_arrange}
\title{\code{dplyr::count()} + \code{dplyr::arrange()}}
\usage{
count_arrange(data, ..., sort = FALSE, .by_group = FALSE)
}
\arguments{
\item{data}{A \code{data.frame}.}

\item{...}{Optional variables to use when determining uniqueness. If there
are multiple rows for a given combination of inputs, only the first
row will be preserved. If omitted, will use all variables.}

\item{sort}{if \code{TRUE} will sort output in descending order of \code{n}}

\item{.by_group}{If \code{TRUE}, will sort first by grouping variable. Applies to
grouped data frames only.}
}
\value{
A \link[tibble:tibble-package]{tibble}.
}
\description{
Shorthand for \code{{dplyr}} functions called consecutively.
}
\details{
Note that arranging is performed on the column used to compute
the value for \code{dplyr::count()}, which is different than simply
calling \code{dplyr::count(x, ..., sort = TRUE)} (which sorts by the output column \code{n}).
For this reason, no \code{sort} option is provided (to prevent the user from
mistakenly using this function in such a manner).
}
\section{Borrowing from the \code{{tidyverse}}}{

The logic for handling \code{...} is borrowed from a review of the internals for
\code{tidyr::unite()} and \code{tidyr::gather()}.
}

\seealso{
Other dplyr-combos: \code{\link{arrange_distinctly}},
  \code{\link{pull_distinctly}}
}
\concept{dplyr-combos}
