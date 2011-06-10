\name{ConfidenceEstimatorSVM-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorSVM-class}
\alias{ConfidenceEstimatorSVM-method}
\alias{ConfidenceEstimatorSVM}
\alias{ConfidenceEstimatorSVM,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorSVM,logical,vector-method}
\alias{estimate,ConfidenceEstimatorSVM,Dataset,vector-method}

\title{Class "ConfidenceEstimatorSVM"}
\description{
This class implements confidence estimation by utilizing the decision value
of a one-class support vector machine (SVM) with a Gaussian kernel (RBF).
For every novel instance, the decision value reflects the similarity to
the training dataset. The optimal parameters of the SVM can be found via
a cross-validation on the training data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorSVM(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorSVM", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{svmModel}:}{Object of class \code{"ANY"} The trained SVM model for confidence estimation. }	
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorSVM", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by training a one-class SVM on the training data using an RBF kernel. If optimization is requested, the parameters of the SVM and the RBF kernel are optimized in cross-validation.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorSVM", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidenecs of the given test data by calculating the decision values of the previously trained one-class SVM. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorSVM")
}
\keyword{classes}
