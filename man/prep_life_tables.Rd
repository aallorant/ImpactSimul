% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ltPrep.R
\name{prep_life_tables}
\alias{prep_life_tables}
\title{Preparing life-tables for simulation}
\usage{
prep_life_tables(lt_male, lt_female, lt_out)
}
\arguments{
\item{lt_male}{Life table for males. Should include Age for Age group and nMx for group specific mortality}

\item{lt_female}{Life table for females. Should include Age for Age group and nMx for group specific mortality}

\item{lt_out}{The rds file in which to write the resulting life table.}
}
\value{
A life table to be
}
\description{
Preparing life-tables for simulation
}
