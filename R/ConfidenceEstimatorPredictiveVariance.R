########################################## CONFIDENCE PREDICTIVE VARIANCE ################################

setMethod("ConfidenceEstimatorPredictiveVariance", signature("Regression", "Dataset"), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorPredictiveVariance", regressionModel, trainingData, qualityFunction);
})

setMethod("initialize", "ConfidenceEstimatorPredictiveVariance",  function(.Object, regressionModel, trainingData, qualityFunction = averageError, regressionMatrix = matrix()) {
	.Object <- callNextMethod(.Object, regressionModel, trainingData, qualityFunction )
	.Object@regressionMatrix <- regressionMatrix
	.Object
})



setMethod("create", signature("ConfidenceEstimatorPredictiveVariance", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {

	# calculate regression matrix
	instances <- c(1:getNumberOfInstances(.Object@trainingData));
	featureSet <- getFeatureSet(.Object@trainingData);
	dims <- c(length(instances), length(featureSet));
	data <- getData(.Object@trainingData);
	data <- data[instances,featureSet];
	data <- matrix(unlist(data),dims[1],dims[2]);	

	a <- as.matrix(data)
	h <- t(a)%*%a;
	i <- diag(dim(h)[2]) * 1e-10;
	regressionMatrix <- solve(h+i);

	# save parameters
	.Object@regressionMatrix <- regressionMatrix;
	.Object@estimatedPerformance <- 0;
	.Object@environmentalParameter <- 0;
	.Object@predictionsOfTrainingData <- c(1);		
		
	# predict confidences of training data	
	responses <- getResponses(.Object@trainingData);
	confidencesTrain <- estimate(.Object, .Object@trainingData, predictionsOfTrainingData)[,1]; 

	# use predicted confidences to estimate confidence intervals
	.Object <- estimateConfidenceIntervals(.Object, responses, predictionsOfTrainingData, confidencesTrain);	
	.Object;
})

setMethod("estimate", signature("ConfidenceEstimatorPredictiveVariance", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	numberOfTestInstances <- getNumberOfInstances(testData);
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;
	featureSet <- getFeatureSet(.Object@trainingData);
	
	# predict with trained model set
	testMatrix <- getData(testData);
	testMatrix = as.matrix(testMatrix[,featureSet]);	
	confidences =  1 - diag(testMatrix %*% .Object@regressionMatrix %*% t(testMatrix));	
	
	# save scores in value matrix
	predictedConfidenceValues <- matrix(c(1:3*numberOfTestInstances),numberOfTestInstances,3);
	predictedConfidenceValues[,1] <- unlist(confidences);
	
	# if confidence intervals given, calculate the predicted intervals
	if (length(.Object@confidenceIntervals) > 1)
	{
		predictedConfidenceValues <- predictConfidenceIntervals(.Object@confidenceIntervals, predictionsOfTestData, predictedConfidenceValues[,1]);
		
	} else {
		# otherwise use overall interval
		predictedConfidenceValues[,2] <- .Object@overallInterval[1];
		predictedConfidenceValues[,3] <- .Object@overallInterval[2];
	}

	return(predictedConfidenceValues);	
})
