\name{FeatureSelection-class}
\Rdversion{1.1}
\docType{class}
\alias{FeatureSelection-class}
\alias{selectFeatures,FeatureSelection,Dataset,ModelEvaluation,logical,Regression-method}
\alias{selectFeatures-method}
\alias{selectFeatures}

\title{Class "FeatureSelection"}
\description{
A virtual class for a feature selection scheme.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{qualityFunction}:}{A \code{"function"} to compare two vectors, for example \code{averageError} or \code{cor}.  }
    \item{\code{compareFunction}:}{A \code{"function"} to compare two qualities from the quality function, for example \code{min} and \code{max}, respectively to the two examples of quality functions. }
  }
}
\section{Methods}{
  \describe{
    \item{selectFeatures}{\code{signature(.Object = "FeatureSelection", trainingData = "Dataset", modelEvaluation = "ModelEvaluation", randomize = "logical", regressionModel = "Regression")}: \cr Start a feature selection routine and return a vector of features as a list of feature indices.}
  }
}
\references{
Briesemeister et al., No longer confidential: Estimating Confidences of Individual Regression Predictions, to be submitted
}
\author{
Sebastian Briesemeister
}

\seealso{
See also the implementations of this class: \code{"\linkS4class{TwinScan}"}.
}
\examples{
showClass("FeatureSelection")
}
\keyword{classes}
