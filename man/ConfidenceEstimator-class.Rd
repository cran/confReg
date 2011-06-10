\name{ConfidenceEstimator-class}
\Rdversion{1.1}
\docType{class}
\alias{ConfidenceEstimator-class}
\alias{getEstimatedPerformance,ConfidenceEstimator-method}
\alias{getEstimatedPerformance-method}
\alias{getEstimatedPerformance}
\alias{create,ConfidenceEstimator,logical,vector-method}
\alias{estimate,ConfidenceEstimator,Dataset,vector-method}
\alias{create-method}
\alias{estimate-method}
\alias{create}
\alias{estimate}

%\alias{saveObject,ConfidenceEstimator,character-method}
\alias{plotConfidenceCurve,ConfidenceEstimator,vector,vector,vector-method}
\alias{plotConfidenceIntervals,ConfidenceEstimator,vector,vector,vector-method}
\alias{plotConfidenceCurve-method}
\alias{plotConfidenceIntervals-method}
\alias{plotConfidenceCurve}
\alias{plotConfidenceIntervals}


\title{Class "ConfidenceEstimator"}
\description{
Virtual class for a confidence estimator. The confidence estimator is initialized
with an regression object and the corresponding training data. Given this data,
the confidence estimator need be created before it can estimate the
confidence of individual predictions. The estimation methods differ from
estimator to estimator but all methods return a confidence score between
zero and one for every prediction, with zero beeing a low-confidence prediction
and one beeing a reliable one. Moreover, confidence scores are translated into
confidence intervals. 
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
     \item{\code{regressionModel}:}{An \code{"Regression"} object necessary for the cross-validation in the optimization procedure.}
    \item{\code{trainingData}:}{A \code{"Dataset"} object containing the training data of the prediction model.}
    \item{\code{qualityFunction}:}{A \code{"function"} measuring the quality of a prediction by comparing a vector of real and predicted values.}
	\item{\code{estimatedPerformance}:}{A \code{"numeric"} that saves the performance of the estimator on the training data in terms of the correlation of the prediction error and the confidence interval width.}
    \item{\code{environmentalParameter}:}{A \code{"numeric"} that defines a property of the estimator. }
    \item{\code{predictionsOfTrainingData}:}{A \code{"vector"} containing the predicted responses from a model trained on the training data. }
    \item{\code{confidenceIntervals}:}{A \code{"matrix"} containing the confidence intervals of the training data sorted by their confidence score. Each row contains the confidence score, the lower interval bound, and the upper interval bound of one instance in the training set.}
	\item{\code{overallInterval}:}{A \code{"vector"} of two elements containing the interval borders of a confidence interval estimated on all training errors, regardless their confidence score. }	
	
  }
}
\section{Methods}{
  \describe{
    \item{getEstimatedPerformance}{\code{signature(.Object = "ConfidenceEstimator")}: \cr Returns the estimation quality of the trained estimator obtained on the training dataset.}
	
	\item{saveObject}{\code{signature(.Object = "ConfidenceEstimator", filename = "character")}: \cr Save object in binary file. }
	
	\item{plotConfidenceIntervals}{\code{signature(.Object = "ConfidenceEstimator", realValues = "vector", predictedValues = "vector", confidenceValues = "vector"))}: \cr Plots the estimated score-based confidence intervals with the respect to the confidence score. In addition, the predicted instances are plotted according to their confidence score and prediction error.}
	
  }
}
\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}




\seealso{
See also the different implementations of confidence estimators: \code{"\linkS4class{ConfidenceEstimatorNNErrors}"}, \code{"\linkS4class{ConfidenceEstimatorNNVariance}"},\code{"\linkS4class{ConfidenceEstimatorLocalCV}"},\code{"\linkS4class{ConfidenceEstimatorLocalVariance}"},\code{"\linkS4class{ConfidenceEstimatorLocalBias}"},\code{"\linkS4class{ConfidenceEstimatorSVM}"},\code{"\linkS4class{ConfidenceEstimatorPredictiveVariance}"},\code{"\linkS4class{ConfidenceEstimatorDifferenceToNeighbors}"},\code{"\linkS4class{ConfidenceEstimatorNumberOfNeighbors}"},\code{"\linkS4class{ConfidenceEstimatorBagging}"},\code{"\linkS4class{ConfidenceEstimatorMinimumDistance}"},\code{"\linkS4class{ConfidenceEstimatorAverageDistance}"}, and \code{"\linkS4class{ConfidenceEstimatorAverageBiasedDistance}"}
}


\examples{
showClass("ConfidenceEstimator")
}
\keyword{classes}
