########################################## CONFIDENCE BAGGING ################################

setMethod("ConfidenceEstimatorBagging", signature("Regression", "Dataset"), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorBagging", regressionModel, trainingData, qualityFunction);
})

setMethod("initialize", "ConfidenceEstimatorBagging",  function(.Object, regressionModel, trainingData, qualityFunction = averageError, modelList = list()) {
	.Object <- callNextMethod(.Object, regressionModel, trainingData, qualityFunction )
	.Object@modelList <- modelList;
	.Object
})



setMethod("create", signature("ConfidenceEstimatorBagging", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
	callNextMethod();
	.Object@confidenceIntervals <- matrix()
	
	numberOfModels <- 50;
	numberOfInstances <- getNumberOfInstances(.Object@trainingData);
	subset <- c(1:numberOfInstances)
	allFeatures <- getFeatureSet(.Object@trainingData);	
	dataMatrix <- getData(.Object@trainingData);
	
	modelList <- rep( list(list()), numberOfModels)
	
	tryCatch({	
		for (i in c(1:numberOfModels))
		{
			#sampledInstances <- c(matrix(c((i%/%1)+1,(i%/%2)+1,(i%/%3)+1,(i%/%4)+1,(i%/%5)+1,(i%/%6)+1,(i%/%7)+1,(i%/%8)+1,(i%/%8)+1,(i%/%10)+1),10,40))[1:numberOfInstances];	
			sampledInstances <- round(runif(numberOfInstances,min=1,max=numberOfInstances));
			
			trainingData <-	dataMatrix[sampledInstances,];
			workData <- Dataset(trainingData);
			featureSet(workData) <- allFeatures;		
					
			model <- learnFromDataset(.Object@regressionModel,workData);		
			modelList[[i]] <- model;
		}
	}, error = function(ex) {
		message(paste("ERROR in ConfidenceEstimatorBagging: Splits of the feature set migtht contain colinear features. Either the dataset is too small or no feature selection has been applied: \n ",ex));
		stop();
	})
	
	.Object@modelList <- modelList;
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

setMethod("estimate", signature("ConfidenceEstimatorBagging", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	callNextMethod();

	numberOfTestInstances <- getNumberOfInstances(testData);
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;	
	
	# predict with trained model set
	predictionResults <- lapply(c(1:length(.Object@modelList)), function(x)  predictDataset(.Object@modelList[[x]],testData) )	
	predsInMatrix <- do.call(cbind,predictionResults);
	confidences <- 1- apply(predsInMatrix, 1, var );	
	
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
