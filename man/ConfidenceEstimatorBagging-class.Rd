\name{ConfidenceEstimatorBagging-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorBagging-class}
\alias{ConfidenceEstimatorBagging-method}
\alias{ConfidenceEstimatorBagging}
\alias{ConfidenceEstimatorBagging,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorBagging,logical,vector-method}
\alias{estimate,ConfidenceEstimatorBagging,Dataset,vector-method}

\title{Class "ConfidenceEstimatorBagging"}
\description{
This class implements confidence estimation by calculating the variance of
50 regression models that have been created via bootstrap aggregation or bagging.
A larger variance of the predicted responses implies a lower confidence.
This model requires no optimization.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorBagging(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorBagging", regressionModel, trainingData, qualityFunction)}. 
}
\section{Slots}{
  \describe{
    \item{\code{modelList}:}{A \code{"list"} of 50 trained regression model.}
    \item{\code{regressionModel}:}{An \code{"Regression"} object that need to be trained on 50 different datasets.}
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorBagging", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by training 50 regression models on 50 different uniformly sampled datasets of the training dataset. Then the confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval. }
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorBagging", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by predicting every instance with each of the 50 regression models and calculating the variance of their predicted responses.	The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorBagging")
}
\keyword{classes}
