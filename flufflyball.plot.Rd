\name{flufflyball.plot}
\alias{flufflyball.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot beautiful sun graphs like a fluffly ball.
}
\description{
Produce a stylized sun-like plot of the given (grouped) values. The length of the line corresponds to normalized values sweeped around clockwise according to coordinates provided by user.
}

\usage{
flufflyball.plot (id,group,coord,values,...)
}
%- maybe also 'usage' for other objects documented here. filter=0, cex.circle=6, cex.legend=0.5, exclude.na=TRUE
\arguments{
  \item{id}{A character vector where ids corresponds to the id name of each observation.}
  \item{group}{A numeric or integer vector where group corresponds to the group of each observation }
  \item{coord}{A numeric or integer vector where coord corresponds to coordinates of each observation}
  \item{values}{A numeric or integer vector where values corresponds to the values of each observation to be represented in the plot}
  \item{filter}{A natural number for a threshold of the minimal value to be represented in the plot. Default: filter=0}
  \item{cex.circle}{A numerical value giving the amount by which the centered circle should be magnified for . Default:cex.circle=6}
  \item{plot.legend}{Logical. If TRUE plots a legend. Default:plot.legend=TRUE}
  \item{cex.legend}{A numerical value giving the amount by which the legend should be magnified for . Default:cex.legend=6}
  \item{exclude.na}{Logical. If TRUE exclude all observations with any NA. Default:exclude.na=TRUE}
}
\details{
}
\value{
Returns a vector for unique "ids" ('id' levels) filtered by parameter 'filter' and plotted.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tiago A. de Souza (tiagoantonio@gmail.com) &
github.com/tiagoantonio/flufflyball
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

flufflyball.plot(id=sample(LETTERS,5000,replace = TRUE), group=sample(1:4,5000, replace=TRUE), coord=sample(1:1000,5000, replace=TRUE), values=sample (0:100,5000, replace=TRUE))

flufflyball.plot(id=sample(letters,2000,replace = TRUE), group=sample(1:4,2000, replace=TRUE), coord=seq(1:2000), values=seq(1:2000), cex.circle = 9, cex.legend=1.1)

flufflyball.plot(id=sample(LETTERS,50,replace = TRUE), group=rep(1,50), coord=sample(1:100,50), values=rnorm(50,mean=100,sd=50), cex.circle = 20, cex.legend = 1.1)

flufflyball.plot(id=sample(letters,5000,replace = TRUE), group=sample(1:4,5000, replace=TRUE), coord=sample(1:1000,5000, replace=TRUE), values=sample (0:100,5000, replace=TRUE), cex.circle=15, plot.legend=FALSE, filter=80)

flufflyball.plot(id=sample(LETTERS,1000,replace = TRUE), group=sample(1:2,1000, replace=TRUE), coord=seq(1:1000), values=sort(rnorm(1000,mean=100,sd=7), decreasing = TRUE))


## The function is currently defined as
fluffy.ball.plot=function(id,group,coord,values, filter=0, plot.legend=FALSE, cex.circle=6, cex.legend=0.5, exclude.na=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~plot }% __ONLY ONE__ keyword per line