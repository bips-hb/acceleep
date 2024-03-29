% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert-data.R
\name{aggregate_spiro}
\alias{aggregate_spiro}
\title{Take raw spirotmetry data and collapse it into approrpiate intervals
TODO: Add demographic data to include REE + AEE units}
\usage{
aggregate_spiro(
  input_file_spiro,
  file_demo = here::here("data/input/demo.csv"),
  t0,
  spiro_interval = 30
)
}
\arguments{
\item{input_file_spiro}{File path to spirometry data.}

\item{file_demo}{\verb{[here::here("data/input/demo.csv")]}: Path to \code{demo.csv}.}

\item{t0}{Value of class \code{hms::hms()} to calibrate interval sequence to.}

\item{spiro_interval}{\verb{[30]} Interval in seconds to calculate mean METs from}
}
\value{
A tibble
}
\description{
Take raw spirotmetry data and collapse it into approrpiate intervals
TODO: Add demographic data to include REE + AEE units
}
\note{
Since \code{t0} will vary by accelerometer model, placement, and SID,
this function needs to be called for all combinations thereof,
as this is the most robust way to ensure measurements align correctly.
}
\examples{
\dontrun{
aggregate_spiro("data/input/spiro/ID_001_spiro.csv", t0 = hms::as_hms("09:00:00"))
}
}
