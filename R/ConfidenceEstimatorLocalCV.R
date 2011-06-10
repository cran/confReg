########################################## CONFIDENCE LOCAL CROSSVALIDATION ################################

setMethod("ConfidenceEstimatorLocalCV", signature("Regression", "Dataset"), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorLocalCV", regressionModel, trainingData, qualityFunction);
})


setMethod("create", signature("ConfidenceEstimatorLocalCV", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
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

setMethod("estimate", signature("ConfidenceEstimatorLocalCV", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	callNextMethod();

	numberOfTestInstances <- getNumberOfInstances(testData);
	numberOfTrainingInstances <- getNumberOfInstances(.Object@trainingData);
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;
	featureSet <- getFeatureSet(.Object@trainingData);
	
	numberOfNeighbors = numberOfTrainingInstances / 20;
	if (numberOfNeighbors < 3)
	{
		numberOfNeighbors <- 3;
	}
	if (numberOfNeighbors > 50)
	{
		numberOfNeighbors <- 50;
	}
	
	# calculate distance matrix from test to training dataset	
	distMatrix <- distTo(testData, .Object@trainingData);	
	neighbors <- apply(distMatrix,1,function(x) return(order(x)[1:numberOfNeighbors]) );
	
	# do CV for every instance and calculate confidences as absolute error in nn one-leave-out CV
	confidences <- mat.or.vec(numberOfTestInstances,1);
	
	tryCatch({
		for (instance in c(1:numberOfTestInstances))
		{
			addedError <- 0;
			sumDistances <- 0;

			newTrainingData <- selectInstances(.Object@trainingData,  neighbors[,instance]);
			for (n in c(1:numberOfNeighbors))
			{
			
				model <- learnFromDataset(.Object@regressionModel, newTrainingData,setdiff((1:numberOfNeighbors),n));
				predictionOfInstance <- predictDataset(model, newTrainingData, c(n));			
				
				realIndex <- neighbors[,instance][n];
				addedError <- addedError + abs(predictionOfInstance - responses[realIndex] ) * dnorm(distMatrix[instance,realIndex]);
				sumDistances <- sumDistances + dnorm(distMatrix[instance,realIndex]);				
			}
					
			confidences[instance] <-  1 - addedError / sumDistances;		
		}
	}, error = function(ex) {
		message(paste("ERROR in ConfidenceEstimatorLocalCV: Splits of the feature set of the local environment might contain colinear features. Either the dataset is too small or no feature selection has been applied: \n ",ex));
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
