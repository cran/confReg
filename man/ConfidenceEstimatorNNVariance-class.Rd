\name{ConfidenceEstimatorNNVariance-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorNNVariance-class}
\alias{ConfidenceEstimatorNNVariance-method}
\alias{ConfidenceEstimatorNNVariance}
\alias{ConfidenceEstimatorNNVariance,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorNNVariance,logical,vector-method}
\alias{estimate,ConfidenceEstimatorNNVariance,Dataset,vector-method}

\title{Class "ConfidenceEstimatorNNVariance"}
\description{
This class implements confidence estimation by calculating the variance of the responses of the nearest neighbors.
For every novel instance, one minus the variance of the responses of the m sourrounding neighbors
is calculated. 
The optimal number of neighbors m needs to be optimized in a cross-validation. As an
alternative, a kernel estimate can be used to put more weight on the variance of closer
instances.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorNNVariance(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorNNVariance", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{regressionModel}:}{An \code{"Regression"} object necessary for the cross-validation in the optimization procedure.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
    \item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. In this case, the number of neighbors m. }
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorNNVariance", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by determine the optimal evironmental parameter m using 5 2-fold cross-validations, skipped if no optimization is requested. Then the confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorNNVariance", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by calculating the variance of the responsesn of the m nearest neighbors in the training dataset. If the model was not optimized, a kernel estimate is utilized instead. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorNNVariance")
}
\keyword{classes}
