% Generated by roxygen2 (4.0.2): do not edit by hand
\name{parseParams}
\alias{parseParams}
\title{parseParams - utility function for WFG}
\usage{
parseParams(strList, i)
}
\arguments{
\item{strList}{the parameter list - containing refs to functions and parameters for them}

\item{i}{is the index of the current function.}
}
\description{
This is only exported for the tests \cr
When reading a spec there is always a current-function, this decides how many of the entries following it are parameters to it as opposed to a next-function
}

