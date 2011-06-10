\name{ConfidenceEstimatorLocalVariance-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorLocalVariance-class}
\alias{ConfidenceEstimatorLocalVariance-method}
\alias{ConfidenceEstimatorLocalVariance}
\alias{ConfidenceEstimatorLocalVariance,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorLocalVariance,logical,vector-method}
\alias{estimate,ConfidenceEstimatorLocalVariance,Dataset,vector-method}

\title{Class "ConfidenceEstimatorLocalVariance"}
\description{
This class implements confidence estimation by calculating the variance introduced
by a novel instance (sensitivity analysis). For every novel instance, the training dataset is updated
with this instance, labeling it with its predicted response plus and minus some errors.
The variance of the responses of the updated models is used as a measure of confidence, where
a lower variance corresponds to a higher confidence. This model requires no
optimization but is computationally very expensive.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorLocalVariance(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorLocalVariance", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{	
	\item{\code{regressionModel}:}{An \code{"Regression"} object necessary for predictions in the sensitivity analysis.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
    \item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. Not used by the estimator. }
    \item{\code{predictionsOfTrainingData}:}{A \code{"vector"} containing the predicted responses from a model trained on the training data. }
    \item{\code{confidenceIntervals}:}{A \code{"matrix"} containing the confidence intervals of the training data sorted by their confidence score. Each row contains the confidence score, the lower interval bound, and the upper interval bound of one instance in the training set.}
	\item{\code{overallInterval}:}{A \code{"vector"} of two elements containing the interval borders of a confidence interval estimated on all training errors, regardless their confidence score. }	
	
  }
}
\section{Extends}{
Class \code{"\linkS4class{ConfidenceEstimator}"}, directly.
}
\section{Methods}{
  \describe{
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorLocalVariance", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr The confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorLocalVariance", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by calculating the local variance obtained by a sensitivity analysis. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
	 }
}
\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}
\seealso{
See also \code{"\linkS4class{ConfidenceEstimator}"}.
}
\examples{
showClass("ConfidenceEstimatorLocalVariance")
}
\keyword{classes}
