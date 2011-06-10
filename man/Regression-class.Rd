\name{Regression-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression-class}
\alias{learnFromDataset}
\alias{predictDataset}
\alias{learnFromDataset,Regression,Dataset-method}
\alias{predictDataset,Regression,Dataset-method}
\alias{Regression-method}
\alias{Regression,function,function-method}
\alias{Regression,function,function,ANY-method}
\alias{Regression,ANY-method}
\alias{Regression}

\title{Class "Regression"}
\description{
General regression objects that can encapsulate other regression schemes such as lm or svm.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{Regression(regressionFunction, predictionFunction)} or by calling
\code{new("Regression", regressionFunction, predictionFunction)}.
}

\section{Slots}{
  \describe{
    \item{\code{regressionFunction}:}{A regression \code{"function"} that can learn from the provided data and can be called via formulas i.e. method(y~.,data). Examples are lm or svm.}
    \item{\code{predictionFunction}:}{A prediction \code{"function"} that can predict with the model created by the provided regression function i.e. predict(model, data), for example predict from the stats package.}
    \item{\code{model}:}{Optionally a learned model of \code{"ANY"} type can be provided. }
  }
}
\section{Methods}{
  \describe{
    
    \item{learnFromDataset}{\code{signature(.Object = "Regression", dataset = "Dataset")}: Learns from the provided \code{Dataset} object by applying the provided \code{regressionFunction} to the data. It returns the updated object. }
    \item{predictDataset}{\code{signature(.Object = "Regression", dataset = "Dataset")}: Predicts the responses of the provided \code{Dataset} by using the provided \code{predictionFunction}. It returns a vector of predictions.}
    
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

## Learn regression model on training dataset
rModel <- learnFromDataset(rModel, data);

## Predict responses of training data
predictDataset(rModel, data);

}
\keyword{classes}