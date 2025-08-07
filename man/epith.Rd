#' epith
#' Thresholds for Monitoring Surveillance of Selected Diseases in the PIDSR
#' An R Package that creates a plot showing the Alert and Epidemic Thresholds for Disease Surveillance.
#'
#' @param data A data frame - this should contain only 1 column named donset (a date variable) for the date of onset.  The data frame should also contain AT LEAST 6 YEARS DATA to be able to generate the thresholds.
#' @param disease Disease name
#' @param latest_week Latest morbidity week
#' @param latest_year Latest year
#' @param area Area to show in the title of the plot (e.g. "Albay Province")
#' @param alert_sd (Optional) Standard deviation for alert threshold - default is 1 SD.
#' @param epidemic_sd (Optional)Standard deviation for epidemic threshold - default is 1.65 SD.
#' @param onset_column Column name for date of onset (string)
#' @export
#'
#' @examples
#' your_function(data=1, 2)
your_function <- function(data, disease, latest_week, latest_year, area, onset_column) {
  # function body
}

\name{epith}
\alias{epith}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thresholds for Monitoring Surveillance of Selected Diseases in the PIDSR
}
\description{
An R Package that creates a plot showing the Alert and Epidemic Thresholds for Disease Surveillance
}
\usage{
epith(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{current_year}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\author{
%%  ~~who you are~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
