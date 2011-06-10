\name{ConfidenceEstimatorDifferenceToNeighbors-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorDifferenceToNeighbors-class}
\alias{ConfidenceEstimatorDifferenceToNeighbors-method}
\alias{ConfidenceEstimatorDifferenceToNeighbors}
\alias{ConfidenceEstimatorDifferenceToNeighbors,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorDifferenceToNeighbors,logical,vector-method}
\alias{estimate,ConfidenceEstimatorDifferenceToNeighbors,Dataset,vector-method}

\title{Class "ConfidenceEstimatorDifferenceToNeighbors"}
\description{
This class implements confidence estimation by calculating the difference to a 
simple nearest neighbor prediction. For every novel instance, the absolute difference
of the predicted response and the average responses of the m nearest neighbors
in the training data is utilized for measuring the confidence. A large difference
implies low confidence in the prediction. 
The optimal number of neighbors m needs to be optimized in a cross-validation. As an
alternative, the number of neighbors can simply be set to five.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorDifferenceToNeighbors(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorDifferenceToNeighbors", regressionModel, trainingData, qualityFunction)}.
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorDifferenceToNeighbors", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by determine the optimal evironmental parameter m using 5 2-fold cross-validations, skipped if no optimization is requested. Then the confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorDifferenceToNeighbors", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by calculating the difference of the predictd response and the average responses of the m nearest neighbors in the training dataset. If the model was not optimized, only the five nearest neighbors are considered. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row.}
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
showClass("ConfidenceEstimatorDifferenceToNeighbors")
}
\keyword{classes}
