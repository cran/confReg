\name{loadObject-methods}
\docType{methods}
\alias{loadObject-methods}
\alias{loadObject,character-method}
\alias{loadObject}
\title{Load object from binary file}
\description{
 Load a \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator} object from a binary file.
}
\usage{
 loadObject(filename)
}
\arguments{
  \item{filename}{
    Character string containing the filename of the object file requested to load.
  }
}
\details{
 The \code{loadObject} method loads a \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator}
 from a file if it has been saved with method \link{saveObject}.
}

\value{
 For \code{loadObject} a \code{"character"} string.
}


\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}

\seealso{
See also \code{saveObject}.
}


\examples{
## Create an example data matrix
dataMatrix <- matrix(runif(10*100),100,10)
y<- (10*sin(pi*dataMatrix[,1]*dataMatrix[,2])+20*(dataMatrix[,3]-0.5)^2+10*dataMatrix[,4]+5*dataMatrix[,5]) + rnorm(100,0,0.1)
dataMatrix <- cbind(dataMatrix,y)

## Create Dataset object with example data matrix
data <- Dataset(dataMatrix)

## Save object
saveObject(data,"data")

## Load object
trainingData <- loadObject("data")

}

\keyword{methods}

