\name{Dataset-class}
\Rdversion{1.1}
\docType{class}
\alias{Dataset-class}
\alias{calculateFolds,Dataset,numeric-method}
\alias{calculateFolds-method}
\alias{calculateFolds}
\alias{dim,Dataset-method}
\alias{dim-method}
\alias{featureSet<-,Dataset-method}
\alias{featureSet<--method}
\alias{featureSet<-}
\alias{getData,Dataset-method}
\alias{getData-method}
\alias{getData}
\alias{getFeatureSet,Dataset-method}
\alias{getFeatureSet-methods}
\alias{getFeatureSet}
\alias{getNumberOfInstances,Dataset-method}
\alias{getNumberOfInstances-method}
\alias{getNumberOfInstances}
\alias{getResponses,Dataset-method}
\alias{getResponses-method}
\alias{getResponses}
\alias{normalize,Dataset-method}
\alias{normalize-method}
\alias{normalize}
\alias{normalizeBy,Dataset,Dataset-method}
\alias{normalizeBy-method}
\alias{normalizeBy}
\alias{splitByFolds,Dataset,vector,numeric-method}
\alias{splitByFolds-method}
\alias{splitByFolds}
\alias{unscaleVector,Dataset,vector-method}
\alias{unscaleVector-method}
\alias{unscaleVector}
%\alias{saveObject,Dataset,character-method}
\alias{Dataset-method}
\alias{Dataset,matrix-method}
\alias{Dataset}

\title{Class "Dataset"}
\description{
  Contains the dataset in form of a matrix with the last column being the
  response value. The dataset can be normalized and features can be selected
  by a given feature set. Folds for a cross-validation can be calculated and
  a dataset can be splitted in a training set and test set according to these
  folds.  
}
\section{Objects from the Class}{
Objects can be created by with a data matrix by \code{Dataset(dataMatrix)} or by calling \code{new("Dataset", dataMatrix)}.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Data given as \code{"matrix"}  }
    \item{\code{means}:}{Means of all features stored in a\code{"vector"} }
    \item{\code{sds}:}{Standard deviations of all features stored in a \code{"vector"}  }
    \item{\code{featureSet}:}{Current feature set given by a \code{"vector"}  }
    \item{\code{numberOfInstances}:}{Size of the dataset as \code{"numeric"}  }
    \item{\code{response}:}{Index of the response value as \code{"numeric"}, assumed to be the last column in data.   }
    \item{\code{scaled}:}{Saves whether data was normalized as \code{"logical"}  }
  }
}
\section{Methods}{
  \describe{
	\item{getData}{\code{signature(.Object = "Dataset")}: \cr Returns the whole saved data matrix. It does not consider whether features are selected or not. }
	\item{getFeatureSet}{\code{signature(.Object = "Dataset")}: \cr Returns currently selected features as vector of feature indices. }
	\item{featureSet<-}{\code{signature(.Object = "Dataset")}: \cr Set selected features by assigning a vector of feature indices. }  
	\item{getNumberOfInstances}{\code{signature(.Object = "Dataset")}: \cr Returns size of the dataset. }
	\item{getResponses}{\code{signature(.Object = "Dataset")}: \cr Returns vector of response values, which equal the last column of the internal data matrix. }	
    \item{dim}{\code{signature(x = "Dataset")}: \cr Returns vector with the two dimensions of the data set \code{"c(number of instances, number of features)"}. }	    
    
    \item{normalize}{\code{signature(.Object = "Dataset")}: \cr Normalize data such that every feature has zero mean and variance equal to one. }
    \item{normalizeBy}{\code{signature(.Object2 = "Dataset", .Object = "Dataset")}: \cr Normalize a \code{"Dataset"} object by using the same normalization that has been applied to the \code{".Object2"} \code{"Dataset"} object.}    
    \item{unscaleVector}{\code{signature(.Object = "Dataset", vector = "vector")}: \cr Returns an unscaled version of the given vector by reversing the normalization that has been applied to the responses. Usually used to un-normalize the predicted response values. }
	
	\item{calculateFolds}{\code{signature(.Object = "Dataset", numberOfFolds = "numeric")}: \cr Returns a vector of  fold assignments for each instance in \code{"Dataset"} object. }
	\item{splitByFolds}{\code{signature(.Object = "Dataset", vectorOfFolds = "vector", extraFold = "numeric")}: \cr Splits the data set into two by using a given fold assignment. Instances with a fold assignment equal to \code{extraFold} are assigned to the test data. Returns a list of two new \code{"Dataset"} objects \code{"list(training data, test data)"}. }       
	
	\item{saveObject}{\code{signature(.Object = "Dataset", filename = "character")}: \cr Save object in binary file. }
	
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

## Split dataset such that one fifth is the test set and the rest is the training set
folds <- calculateFolds(data, 5, random=TRUE);
datasets <- splitByFolds(data, folds, 5);

## Normalize training dataset
trainingData <- normalize(datasets[[1]]);

## Normalize test dataset by scaling it the same way the training dataset was scaled
testData <- normalizeBy(datasets[[2]], trainingData);


}
\keyword{classes}
