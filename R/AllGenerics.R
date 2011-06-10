

# generic for saving an object
setGeneric(name="saveObject", function(.Object, filename) standardGeneric("saveObject"));

# function for loading an object
setGeneric(name="loadObject", function(filename) standardGeneric("loadObject"));

setGeneric("averageError", function(realValues, predictedValues) standardGeneric("averageError"));


### Feature Selection ###

setGeneric("TwinScan", function(qualityFunction = averageError, compareFunction = min, ...) standardGeneric("TwinScan"));

setGeneric("selectFeatures", function(.Object, trainingData, modelEvaluation, randomize, regressionModel, ...) standardGeneric("selectFeatures"));


### Model Evaluation ###

setGeneric("ModelEvaluation", function(qualityFunction = averageError, ...) standardGeneric("ModelEvaluation"));

setGeneric("getPredictionsOfCV", function(.Object, data, regressionModel, numberOfFolds, randomize = TRUE, featureSelection = FALSE,...) standardGeneric("getPredictionsOfCV"));

setGeneric("evaluateInCV", function(.Object, data, regressionModel, numberOfFolds, randomize = TRUE, featureSelection = FALSE,...) standardGeneric("evaluateInCV"));

### Regression ###

setGeneric("Regression", function(regressionFunction, predictionFunction, ...) standardGeneric("Regression"));

setGeneric("FastRegression", function(regressionFunction, predictionFunction, ...) standardGeneric("FastRegression"));

setGeneric("learnFromDataset", function(.Object, dataset, subset = c()) standardGeneric("learnFromDataset"));

setGeneric("predictDataset", function(.Object, dataset, subset = c()) standardGeneric("predictDataset"));

### Dataset ###

setGeneric("Dataset", function(dataMatrix, ...) standardGeneric("Dataset"));

setGeneric("getData", function(.Object, ...) standardGeneric("getData"));

setGeneric("getFeatureSet", function(.Object, ...) standardGeneric("getFeatureSet"));

setGeneric("featureSet<-", function(.Object, value) standardGeneric("featureSet<-"));

setGeneric("getResponses", function(.Object, ...) standardGeneric("getResponses"));

setGeneric("getNumberOfInstances", function(.Object, ...) standardGeneric("getNumberOfInstances"));

setGeneric("normalize", function(.Object, ...) standardGeneric("normalize"));

setGeneric("distTo", function(.Object, .Object2, ...) standardGeneric("distTo"));

setGeneric("normalizeBy", function(.Object, .Object2, ...) standardGeneric("normalizeBy"));

setGeneric("calculateFolds", function(.Object, numberOfFolds, random = TRUE, ...) standardGeneric("calculateFolds"));

setGeneric("splitByFolds", function(.Object, vectorOfFolds, extraFold, ...) standardGeneric("splitByFolds"));

setGeneric("selectInstances", function(.Object, vectorOfInstances, ...) standardGeneric("selectInstances"));

setGeneric("addInstance", function(.Object, instance, ...) standardGeneric("addInstance"));

setGeneric("unscaleVector", function(.Object, vector, ...) standardGeneric("unscaleVector"));

### Confidence Estimator

setGeneric("create", function(.Object, optimize = FALSE, predictionsOfTrainingData = c(), ...) standardGeneric("create"));

setGeneric(name="estimate", function(.Object, testData, predictionsOfTestData, ...) standardGeneric("estimate"));

setGeneric("getEstimatedPerformance", function(.Object, ...) standardGeneric("getEstimatedPerformance"));

setGeneric("evaluateConfidenceEstimates", function(realValues, predictedValues, confidenceValues, qualityFunction = averageError) standardGeneric("evaluateConfidenceEstimates"));

setGeneric("optimize", function(.Object, optimize, ...) standardGeneric("optimize"));

setGeneric("predictConfidenceIntervals", function( confidenceIntervals, predictedValues, confidenceValues, ...) standardGeneric("predictConfidenceIntervals"));

setGeneric("estimateConfidenceIntervals", function(.Object, realValues, predictedValues, confidenceValues, ...) standardGeneric("estimateConfidenceIntervals"));

setGeneric("plotConfidenceIntervals", function(.Object, realValues, predictedValues, confidenceValues, ...) standardGeneric("plotConfidenceIntervals"));

### Confidence Estimator Implementations

setGeneric("ConfidenceEstimatorSVM", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorSVM"));

setGeneric("ConfidenceEstimatorAverageBiasedDistance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorAverageBiasedDistance"));

setGeneric("ConfidenceEstimatorNNErrors", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorNNErrors"));

setGeneric("ConfidenceEstimatorAverageDistance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorAverageDistance"));

setGeneric("ConfidenceEstimatorBagging", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorBagging"));

setGeneric("ConfidenceEstimatorDifferenceToNeighbors", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorDifferenceToNeighbors"));

setGeneric("ConfidenceEstimatorLocalBias", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorLocalBias"));

setGeneric("ConfidenceEstimatorLocalCV", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorLocalCV"));

setGeneric("ConfidenceEstimatorLocalVariance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorLocalVariance"));

setGeneric("ConfidenceEstimatorMinimumDistance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorMinimumDistance"));

setGeneric("ConfidenceEstimatorNNVariance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorNNVariance"));

setGeneric("ConfidenceEstimatorNumberOfNeighbors", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorNumberOfNeighbors"));

setGeneric("ConfidenceEstimatorPredictiveVariance", function(regressionModel, trainingData, qualityFunction = averageError, ...) standardGeneric("ConfidenceEstimatorPredictiveVariance"));
