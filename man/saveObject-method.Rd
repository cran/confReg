\name{saveObject-methods}
\docType{methods}
\alias{saveObject-methods}
\alias{saveObject,ConfidenceEstimator,character-method}
\alias{saveObject,Dataset,character-method}
\alias{saveObject,Regression,character-method}
\alias{saveObject,FastRegression,character-method}
\alias{saveObject}
\title{Save object in binary file}
\description{
Saves a \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator} object in a binary file.
}

\usage{
\S4method{saveObject}{Dataset,character}(.Object, filename)
\S4method{saveObject}{Regression,character}(.Object, filename)
\S4method{saveObject}{ConfidenceEstimator,character}(.Object, filename)
}
\arguments{
	\item{.Object}{
    An object of type \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator}.
  }

  \item{filename}{
    Character string containing the filename of the object file requested to load.
  }
}
\details{
 The \code{saveObject} method saves a \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator} object
 to a file.
}

\value{
 For \code{saveObject} an object of type \code{Dataset} object, a \code{Regression} object, or a \code{ConfidenceEstimator} and a \code{"character"} string.
}


\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}

\seealso{
See also \code{loadObject}.
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


}

\keyword{methods}



