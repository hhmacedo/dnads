% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pnad_get.R
\name{pnad_get}
\alias{pnad_get}
\title{Download PNAD files and read them.}
\usage{
pnad_get(year, design = TRUE, vars = NULL, hh_only = FALSE)
}
\arguments{
\item{year}{A year must be provided.}

\item{design}{TRUE if you want a survey object, FALSE if you want a dataframe.}

\item{vars}{Selected variables}

\item{hh_only}{If TRUE, will skip person data, using just household data.}
}
\value{
A survey object or a tibble.
}
\description{
Download PNAD files and read them.
}
\examples{
\dontrun{
# Download PNAD 2015
pnad2015 <- pnad_get(year = 2015)

# Select just the variable V8005 for PNAD 2008
pnad2008 <- pnad_get(year = 2008, vars = c("V8005"))
}

}
