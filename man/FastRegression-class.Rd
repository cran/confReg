\name{FastRegression-class}
\Rdversion{1.1}
\docType{class}
\alias{FastRegression-class}
\alias{learnFromDataset,FastRegression,Dataset-method}
\alias{predictDataset,FastRegression,Dataset-method}
\alias{FastRegression-method}
\alias{FastRegression,ANY-method}
\alias{FastRegression}

\title{Class "FastRegression"}
\description{
A fast implementation of linear ridge regression with a lambda of 10E-10.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{FastRegression()} or by calling
\code{new("FastRegression")}.
}

\section{Slots}{
  \describe{
    \item{\code{regressionFunction}:}{A regression \code{"function"} is inherited from \code{Regression} but not used.}
    \item{\code{predictionFunction}:}{A prediction \code{"function"} is inherited from \code{Regression} but not used}
    \item{\code{model}:}{Optionally a learned model as a vector of coefficients can be provided. }
  }
}
\section{Methods}{
  \describe{
    
    \item{learnFromDataset}{\code{signature(.Object = "Regression", dataset = "Dataset")}: Learns from the provided \code{Dataset} object by linear ridge regression and returns the updated object.}
    \item{predictDataset}{\code{signature(.Object = "Regression", dataset = "Dataset")}: Predicts the responses of the provided \code{Dataset} by using learned regression model. It returns a vector of predictions.}
    
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

## Create a FastRegression object
rModel <- FastRegression();

## Learn regression model on training dataset
rModel <- learnFromDataset(rModel, data);

## Predict responses of training data
predictDataset(rModel, data);

}
\keyword{classes}