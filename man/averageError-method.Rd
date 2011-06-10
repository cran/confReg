\name{averageError-methods}
\docType{methods}
\alias{averageError-methods}
\alias{averageError}
\alias{averageError,vector,vector-method}
\title{ Average squared error}
\description{
Implements the mean squared error given two
vectors of numerics.
}

\usage{
averageError(realValues, predictedValues)
}
\arguments{
	\item{realValues}{
    A vector containing the correct and real values.
  }

  \item{predictedValues}{
   A vector containing predicted and possible erroneous values.
  }
}
\details{
 The \code{averageError} method calculates the mean squared error of a prediction.
}

\value{
 For \code{averageError} two vectors of numerics are required.
}


\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}



\examples{
## Two example vectors
real <- c(1,2,3,4,5);
predicted <- c(1.1,1.8,2.75,4.5,5.3);

## calculate average error
averageError(real, predicted)
}

\keyword{methods}

