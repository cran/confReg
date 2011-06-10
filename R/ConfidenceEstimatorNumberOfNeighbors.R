########################################## CONFIDENCE ESTIMATOR NUMBER OF NEIGHBORS ################################

setMethod("ConfidenceEstimatorNumberOfNeighbors", signature("Regression", "Dataset" ), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorNumberOfNeighbors", regressionModel, trainingData, qualityFunction);
})


setMethod("create", signature("ConfidenceEstimatorNumberOfNeighbors","logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
	callNextMethod();
	.Object@confidenceIntervals <- matrix()

	if (optimize)
	{
		numberOfFolds <- 5;	
		foldResults <- c(0,0,0);
		for (i in 1:numberOfFolds)
		{
			foldResults <- foldResults + optimize(.Object, optimize = "distance");
		}
		foldResults <- foldResults / numberOfFolds;
		
		# save obtained results in object
		.Object@estimatedPerformance <- foldResults[3];
		.Object@environmentalParameter <- foldResults[1];	
	} else {
		# if no optimization is done, save -1 to use a kernel densitiy estimate in the estimation
		.Object@estimatedPerformance <- 0;
		.Object@environmentalParameter <- -1;	
	}
	.Object@predictionsOfTrainingData <- predictionsOfTrainingData;
	
	# predict confidences of training data	
	responses <- getResponses(.Object@trainingData);
	confidencesTrain <- estimate(.Object, .Object@trainingData, predictionsOfTrainingData)[,1]; 
	
	# use predicted confidences to estimate confidence intervals
	.Object <- estimateConfidenceIntervals(.Object, responses, predictionsOfTrainingData, confidencesTrain);	
	.Object;
})


setMethod("estimate", signature("ConfidenceEstimatorNumberOfNeighbors", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {
	callNextMethod();

	distMatrix <- distTo(testData, .Object@trainingData);
	numberOfTestInstances <- dim(distMatrix)[1];
	distance <- .Object@environmentalParameter
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;	
	
	confidences <- mat.or.vec(dim(distMatrix)[1],1) - 500;
	if (distance != -1)
	{
		#  predict confidence scores by considering nns next neighbors
		if (min(distMatrix[ distMatrix>0 ]) <= distance)
		{
			neighbors <- apply(distMatrix,1,function(x) which(x<=distance & x>0) );
			confidences <- lapply(neighbors,function(x) length(x) );
		}
	} else {
		# predict confidence scores by kernel density estimate
		confidences <- apply(distMatrix,1,function(x) {
				x <- x[x!=0];
				mean(exp(-0.5*x^2))
			});
	}

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