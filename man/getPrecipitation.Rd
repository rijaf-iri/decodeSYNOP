% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_precipititation_data.R
\name{getPrecipitation}
\alias{getPrecipitation}
\title{Get precipitation data.}
\usage{
getPrecipitation(synop)
}
\arguments{
\item{synop}{a synop data object, output of the function \code{getSynopSections}.}
}
\value{
A named list of numeric vector of length 2,\cr
the first element of the list is a vector of length 2: c(duration[in hour], amount [in mm]) from section 1,
the second element is a vector length 3: c(duration[in hour], amount [in mm], amount past 24 hour [in mm]) from section 3.
}
\description{
Get precipitation data, group \code{6RRRt_R} from section 1 and 3, and group \code{7R_{24}R_{24}R_{24}R_{24}} from section 3.
}
\examples{

\dontrun{
aaxx <- "AAXX 07181 33837 11583 83102 10039 29072 30049 40101 52035 60012="
synop <- getSynopSections(aaxx)
precip <- getPrecipitation(synop)
}

}
