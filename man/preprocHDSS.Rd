% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/preprocHDSS.R
\name{preprocHDSS}
\alias{preprocHDSS}
\title{Pre-process raw HDSS data}
\usage{
preprocHDSS(x, checkVA = FALSE)
}
\arguments{
\item{x}{a data frame with raw HDSS data}

\item{checkVA}{A logical flag indicating whether to test for the presence and
validity of verbal autopsy data.}
}
\value{
A data frame with modified columns.
}
\description{
This function cleans up some common data variants that appear in raw
HDSS data files.
}
\details{
The function undertakes the following modifications:
\itemize{
    \item empty dates are set to \code{NA};
    \item dates get stripped of trailing time stamps and converted to
          \code{Date}-variables, regardless of the separator used;
    \item \code{Sex} is coded as an f/m factor;
    \item for verbal autopsy variables, blanks are replaced with NAs;
    \item cause of death codes are matched against the canonical list of
          approved causes (based on WHO standard).
}
}
\seealso{
\code{\link{readRawHDSS}} \code{\link{coreTests}}
}

