\name{ConfidenceEstimatorPredictiveVariance-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorPredictiveVariance-class}
\alias{ConfidenceEstimatorPredictiveVariance-method}
\alias{ConfidenceEstimatorPredictiveVariance}
\alias{ConfidenceEstimatorPredictiveVariance,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorPredictiveVariance,logical,vector-method}
\alias{estimate,ConfidenceEstimatorPredictiveVariance,Dataset,vector-method}

\title{Class "ConfidenceEstimatorPredictiveVariance"}
\description{
This class implements confidence estimation by the predictive variance of 
a linear regression model. The design matrix of a regression model
trained on the training data is used to calculate the predictive variance
for novel instances. Larger variances will lead to smaller confidence
scores. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorPredictiveVariance(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorPredictiveVariance", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{regressionMatrix}:}{The design \code{"matrix"} of the underlying regression model. }
    \item{\code{regressionModel}:}{An \code{"Regression"} object necessary for the cross-validation in the optimization procedure.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
    \item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. Not used by this estimator. }
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorPredictiveVariance", optimize = "logical", predictionsOfTrainingData = "vector")}: Creates the confidence estimator by calculating the design matrix of a linear regression. }
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorPredictiveVariance", testData = "Dataset", predictionsOfTestData = "vector")}: Predicts the confidences of the given test data by calculating the predictive variance of the instances using the saved design matrix. The optimization option is ignored since no optimization is possible. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorPredictiveVariance")
}
\keyword{classes}
