\name{evaluateConfidenceEstimates-methods}
\docType{methods}
\alias{evaluateConfidenceEstimates-methods}
\alias{evaluateConfidenceEstimates}
\alias{evaluateConfidenceEstimates,vector,vector,matrix-method}
\title{Evaluate confidences estimates}
\description{
Evaluate confidence estimates in terms of the  correlation of the prediction error and the confidence interval width.
}


\usage{
evaluateConfidenceEstimates(realValues, predictedValues, confidenceValues, qualityFunction = averageError)
}
\arguments{
  \item{realValues}{
    A vector containing the correct and real values.
  }

  \item{predictedValues}{
   A vector containing predicted and possible erroneous values.
  }
  
  \item{confidenceValues}{
  A matrix of three columns containing the confidence scores and the interval borders of the corresponding instances.
  }
  
  \item{qualityFunction}{
  A function that determins the prediction quality by returning a quality measure given a vector with original values and a vector of predictions.
  }
  
}
\details{
 The \code{evaluateConfidenceEstimates} calculates the pearson correlation of the prediction error and the width of the predicted
 confidence/error interval.
}

\value{
 For \code{evaluateConfidenceEstimates} two vectors containing the correct and the predicted values and one matrix containing
 the confidence information for each instance.
}


\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}

\seealso{
See also \code{evaluateConfidenceEstimates}.
}


\keyword{methods}

