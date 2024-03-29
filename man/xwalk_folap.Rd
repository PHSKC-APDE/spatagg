% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xwalk.R
\name{xwalk_folap}
\alias{xwalk_folap}
\title{Create an xwalk matrix based on fractional overlap}
\usage{
xwalk_folap(source, target, source_id = "id", target_id = "id", ...)
}
\arguments{
\item{source}{sf data.frame. Sf object containing the source geographies}

\item{target}{sf data.frame. sf object containng the target geographies}

\item{source_id}{character. Column that has the unique ids in source}

\item{target_id}{character. Column that has the unique ids in target}

\item{...}{options passed to \code{sf::st_intersection}}
}
\value{
data.frame with columns from target, source, and a \code{fraction}
denoting the fraction of source that intersects with target spatially.
Depending on how the two geography sets overlap, it may be that source doesn't
fully cover target
}
\description{
Create an xwalk matrix based on fractional overlap
}
