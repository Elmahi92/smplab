\name{euclidean}
\alias{euclidean}
\title{Euclidian algorithm 
}
\description{
Euclidian algorithm to find the greatest common divisor of two numbers
}
\usage{
euclidean(a, b)
}
\arguments{
  \item{a}{
  Numeric scalarsor integers
}
  \item{b}{
  Numeric scalars or integers
}
}
\details{Euclidian algorithm to find the greatest common divisor of two
numbers,the algorithm with pseudocode
function gcd(a, b)
    while a ≠ b 
        if a > b−
            a := a − b
        else
            b := b − a
    return a
}
\references{
\url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
}
\author{
Simon & Mohamed}
\examples{
euclidean(123612, 13892347912)
}

