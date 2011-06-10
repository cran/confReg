\name{ConfidenceEstimatorAverageBiasedDistance-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimatorAverageBiasedDistance-class}
\alias{ConfidenceEstimatorAverageBiasedDistance-method}
\alias{ConfidenceEstimatorAverageBiasedDistance}
\alias{ConfidenceEstimatorAverageBiasedDistance,Regression,Dataset-method}
\alias{create,ConfidenceEstimatorAverageBiasedDistance,logical,vector-method}
\alias{estimate,ConfidenceEstimatorAverageBiasedDistance,Dataset,vector-method}

\title{Class "ConfidenceEstimatorAverageBiasedDistance"}
\description{
This class implements confidence estimation by calculating the average distance
to instances in the training data by putting a bias on closer instances.
Instances with a large biased average distance are weighted
as less reliable. The outlier-free version of this estimator calculates
the average biased distance to the instances in the training data with a maximum error
of m, where the errors on the training data need to be provided at the moment of creation. 
The optimal error threshold for an outlier needs to be optimized in a cross-validation. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ConfidenceEstimatorAverageBiasedDistance(regressionModel, trainingData, qualityFunction)}
or by calling \code{new("ConfidenceEstimatorAverageBiasedDistance", regressionModel, trainingData, qualityFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{regressionModel}:}{An \code{"Regression"} object necessary for the cross-validation in the optimization procedure.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
    \item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. In this case, the maximum error to be not categorized as an outlier. }
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
    \item{create}{\code{signature(.Object = "ConfidenceEstimatorAverageBiasedDistance", optimize = "logical", predictionsOfTrainingData = "vector")}: \cr Creates the confidence estimator by determine the optimal error threshold m using 5 2-fold cross-validations, skipped if no optimization is requested. Then the confidence intervals based on the confidence score are obtained by estimating the confidences of the training instances. This information will be used in the estimation process to transform a confidence value into a confidence score and a confidence interval.}
    \item{estimate}{\code{signature(.Object = "ConfidenceEstimatorAverageBiasedDistance", testData = "Dataset", predictionsOfTestData = "vector")}: \cr Predicts the confidences of the given test data by calculating the average biased distance of each instances to the instances in the training data with a maximum error of m. If the model was not optimized, the average biased distance to any instance in the training data is calculated. The function returns a matrix containing the confidence score, the lower bound, and the upper bound of every test instances in each row. }
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
showClass("ConfidenceEstimatorAverageBiasedDistance")
}
\keyword{classes}
