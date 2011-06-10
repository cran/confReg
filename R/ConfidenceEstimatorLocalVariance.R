########################################## CONFIDENCE LOCAL VARIANCE ################################

setMethod("ConfidenceEstimatorLocalVariance", signature("Regression", "Dataset"), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorLocalVariance", regressionModel, trainingData, qualityFunction);
})


setMethod("create", signature("ConfidenceEstimatorLocalVariance", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
	callNextMethod();
	.Object@confidenceIntervals <- matrix()
	
	# no optimization necessary, so only save void parameters
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

setMethod("estimate", signature("ConfidenceEstimatorLocalVariance", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	callNextMethod();

	numberOfTestInstances <- getNumberOfInstances(testData);
	numberOfInstances <- getNumberOfInstances(.Object@trainingData);
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;
	featureSet <- getFeatureSet(.Object@trainingData);

	# predict by introducing errors in the instance	
	addedErrors = c(0.01,0.1,0.5,1,2);
	minResponse = min(responses);
	maxResponse = max(responses);
	testDataMatrix = getData(testData);
	confidences = mat.or.vec(length(numberOfTestInstances),1);
	
	newData <- getData(.Object@trainingData);
	for (instance in c(1:numberOfTestInstances))
	{		
		for (error in addedErrors)
		{
			newInstancePositive <- testDataMatrix[instance, ];
			newInstanceNegative <- testDataMatrix[instance, ];
			newInstancePositive[dim(testDataMatrix)[2]] <- predictionsOfTestData[instance] + error*(maxResponse-minResponse);
			newInstanceNegative[dim(testDataMatrix)[2]] <- predictionsOfTestData[instance] - error*(maxResponse-minResponse);			

			
			newData <- rbind(newData,newInstancePositive);
			newData <- rbind(newData,newInstanceNegative);
		}
	}

	newData <- Dataset(newData);
	featureSet(newData) <- featureSet;
	tryCatch({	
		for (instance in c(1:numberOfTestInstances))
		{		
			addedVariance = 0;
			for (errorIndex in c(1:length(addedErrors)))
			{
				i = numberOfInstances+ (instance-1)*length(addedErrors)*2 +(errorIndex-1)*2 + 1;
								
				regressionModelPositive <- learnFromDataset(.Object@regressionModel,newData,c(c(1:numberOfInstances), i));
				regressionModelNegative <- learnFromDataset(.Object@regressionModel,newData,c(c(1:numberOfInstances), i+1));
				
				predictionPositive <- predictDataset(regressionModelPositive,newData,c(i));
				predictionNegative <- predictDataset(regressionModelNegative,newData,c(i+1));			
				
				addedVariance = addedVariance + (predictionPositive - predictionNegative);			
			}
			confidences[instance] =  1- addedVariance / length(addedErrors);
		}
	}, error = function(ex) {
		message(paste("ERROR in ConfidenceEstimatorLocalVariance: Splits of the feature set might contain colinear features. Either the dataset is too small or no feature selection has been applied: \n ",ex));
		stop();
	})
		
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
