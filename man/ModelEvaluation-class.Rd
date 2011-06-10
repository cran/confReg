\name{ModelEvaluation-class}
\Rdversion{1.1}
\docType{class}
\alias{ModelEvaluation-class}
\alias{evaluateInCV,ModelEvaluation,Dataset,Regression,numeric-method}
\alias{evaluateInCV-method}
\alias{evaluateInCV}
\alias{getPredictionsOfCV,ModelEvaluation,Dataset,Regression,numeric-method}
\alias{getPredictionsOfCV-method}
\alias{getPredictionsOfCV}
\alias{ModelEvaluation-method}
\alias{ModelEvaluation,ANY-method}
\alias{ModelEvaluation}

\title{Class "ModelEvaluation"}
\description{
Object to evaluate the quality of regression method on a dataset with a
given feature set. Currently, only a cross-validation evaluation is implemented.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ModelEvaluation(qualityFunction)} or
or by calling \code{new("ModelEvaluation", qualityFunction)}.
}
\section{Slots}{
  \describe{
    \item{\code{qualityFunction}:}{A \code{"function"} to compare two vectors, for example \code{"averageError"} or \code{cor}.}
  }
}
\section{Methods}{
  \describe{
    \item{evaluateInCV}{\code{signature(.Object = "ModelEvaluation", data = "Dataset", regressionModel = "Regression", numberOfFolds = "numeric")}: \cr Given a \code{"Dataset"} with a defined feature set and a \code{"Regression"} model object it evaluates the quality of this model on the data by performing a random n-fold cross-validation. It returns the average quality value returned by the quality function applied to the predicted and real response values.}  
	
	\item{getPredictionsOfCV}{\code{signature(.Object = "ModelEvaluation", data = "Dataset", regressionModel = "Regression", numberOfFolds = "numeric")}: \cr Given a \code{"Dataset"} with a defined feature set and a \code{"Regression"} model object it performs a random n-fold cross-validation. It returns the predicted values obtained in the cross-validation of all instances in the given dataset in a vector.}  
	
	
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
me <- ModelEvaluation(qualityFunction = averageError);

## perform a five-fold cross-validation 
evaluateInCV(me, data, rModel, 5, random=TRUE);

}
\keyword{classes}
