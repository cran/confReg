\name{TwinScan-class}
\Rdversion{1.1}
\docType{class}
\alias{TwinScan-class}
\alias{selectFeatures,TwinScan,Dataset,ModelEvaluation,logical,Regression-method}
\alias{TwinScan-method}
\alias{TwinScan,ANY-method}
\alias{TwinScan}

\title{Class "TwinScan"}
\description{
A class for a TwinScan feature selection. First, the features are ranked according to their
individual quality. In a second step, features are iteratively added to the best feature
if the prediction quality will be increased. Prediction quality is accessed by a \code{ModelEvaluation}
object, which evaluates a \code{Regression} object on the selected features in a
cross-validation scheme. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{TwinScan(qualityFunction, compareFunction)} or
or by calling \code{new("TwinScan", qualityFunction, compareFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{qualityFunction}:}{A \code{"function"} to compare two vectors, for example \code{"averageError"} or \code{cor}.  }
    \item{\code{compareFunction}:}{A \code{"function"} to compare two qualities from the quality function, for example \code{"min"} and \code{"max"}, respectively to the two examples of quality functions. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{FeatureSelection}"}, directly.
}
\section{Methods}{
  \describe{
    \item{selectFeatures}{\code{signature(.Object = "TwinScan", trainingData = "Dataset", modelEvaluation = "ModelEvaluation", randomize = "logical", regressionModel = "Regression")}: \cr Selects features from a given \code{"Dataset"} by evaluating it using a given \code{"ModelEvaluation"} with a \code{"Regression"} method. First, the features are ranked according to their individual prediction quality assessed in a cross-validation with the given model. In a second step, the feature set is iteratively extended by the next best feature if the added feature increased the prediction quality. Thus, the selection method will iterate over the feature set only twice. The function wil return the selected features as a vector of feature indices.}
  }
}
\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}


\examples{

## Create an example data matrix
dataMatrix <- matrix(runif(10*100),100,10)
y<- (10*sin(pi*dataMatrix[,1]*dataMatrix[,2])+20*(dataMatrix[,3]-0.5)^2+10*dataMatrix[,4]+5*dataMatrix[,5]) + rnorm(100,0,0.1)
dataMatrix <- cbind(dataMatrix,y)

## Create Dataset object with example data matrix
data <- Dataset(dataMatrix);

## Create a Regression object with linear regression of lm and predict
rModel <- Regression(lm, predict);

## create ModelEvaluation object with average squared error as quality function
me <- ModelEvaluation();

## create TwinScan object that minimizes the average squared error
fs <- TwinScan(averageError, min);

## select features from the training data
featureList <- selectFeatures(fs, data, me, randomize=TRUE, rModel)

## assign feature set to dataset
featureSet(data) <- featureList;

}
\keyword{classes}
