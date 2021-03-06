% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tk_detect.R
\name{tk_detect}
\alias{tk_detect}
\title{Detect Thermokarst from Elevation}
\usage{
tk_detect(elev, radii = 15, fun = "median", cutoff = 0, n.cores = 1)
}
\arguments{
\item{elev}{A raster of elevation.}

\item{radii}{A number or numeric vector of neighborhood sizes (in units of
the crs).}

\item{fun}{The function to use to determine average elevation. Either median
or mean.}

\item{cutoff}{The cut-off value to use when reclassifying microtopography as
thermokarst.}

\item{n.cores}{The number of cores to use.}
}
\value{
A list containing the cutoff value used, the radii used, elev.crop,
avg.elev, microtopography, and thermokarst. Elev.crop is an elevation
raster cropped to the final output size. avg.elev is a raster of the median
elevation values. Microtopography is the cropped elevation minus the median
elevation, such that negative values indicate a low spot on the landscape
which may be thermokarst. Thermokarst is a raster of the thermokarst
classification: thermokarst (1) or non-thermokarst (0).
}
\description{
Detect thermokarst features by comparing the elevation in each cell of a
digital terrain model to the average elevation in a circular neighborhood.
}
\examples{
tk_detect(tk_ponds)
}
