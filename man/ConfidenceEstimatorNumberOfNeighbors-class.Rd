\name{ConfidenceEstimatorNumberOfNeighbors-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorNumberOfNeighbors-class}
\alias{ConfidenceEstimatorNumberOfNeighbors-method}
\alias{ConfidenceEstimatorNumberOfNeighbors}
\alias{ConfidenceEstimatorNumberOfNeighbors,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorNumberOfNeighbors,logical,vector-method}
\alias{estimate,ConfidenceEstimatorNumberOfNeighbors,Dataset,vector-method}

\title{Class "ConfidenceEstimatorNumberOfNeighbors"}
\description{
This class implements confidence estimation by calculating the number of
neighbors in a local environment. For every novel instance, the number of
neighbors within a distance m in the training dataset are calculated.
The optimal environmental distance m can be optimized in a cross-validation.
As an alternative, a kernel density estimate can be used to calculate
the density of neighbors in the local environment.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorNumberOfNeighbors(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorNumberOfNeighbors", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{
	\item{\code{regressionModel}:}{An \code{"Regression"} object necessary for the cross-validation in the optimization procedure.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
    \item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. In this case, a distance value which defines the local environment used by the estimator. }
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorNumberOfNeighbors", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by determine the optimal evironmental distance m using 5 2-fold cross-validations, skipped if no optimization is requested. Then the confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorNumberOfNeighbors", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by counting the number of neighbors within a distance range of m in the training dataset. If the model was not optimized, a kernel densitiy estimate is utilized instead. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorNumberOfNeighbors")
}
\keyword{classes}
