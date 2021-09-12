\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Euclidian algorithm 
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
  Numeric scalars
or integers
%%     ~~Describe \code{a} here~~
}
  \item{b}{
  Numeric scalars
or integers
%%     ~~Describe \code{b} here~~
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Greatest common divisor of two numbers
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
%% ~put references to the literature/web site here ~
}
\author{
Simon & Mohamed
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (a, b) 
{
    while (b != 0) {
        t <- b
        b <- a\%\%b
        a <- t
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
