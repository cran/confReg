########################################## CONFIDENCE ESTIMATOR ################################


setMethod("initialize", "ConfidenceEstimator",  function(.Object, regressionModel, trainingData, qualityFunction = averageError) {
	.Object@regressionModel <- regressionModel;
	.Object@estimatedPerformance <- 0;
	.Object@qualityFunction <- qualityFunction;
	.Object@trainingData <- trainingData;
	.Object@environmentalParameter <- 0;
	.Object@predictionsOfTrainingData <- c(1);
	.Object@confidenceIntervals <- matrix();
	.Object@overallInterval <- c(1,1);
	.Object
})


setMethod("getEstimatedPerformance", signature("ConfidenceEstimator"),  function(.Object) {
	.Object@estimatedPerformance;
})

setMethod("create", signature("ConfidenceEstimator", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
	if (!is.vector(predictionsOfTrainingData) || !is.numeric(predictionsOfTrainingData) ) stop("ERROR in create of ConfidenceEstimator: Requires a numeric vector as input.");
	if (length(predictionsOfTrainingData) != getNumberOfInstances(.Object@trainingData) ) stop("ERROR in create of ConfidenceEstimator: Number of predicted values does not equal the training dataset size.");
})

setMethod("estimate", signature("ConfidenceEstimator", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	if (!is.vector(predictionsOfTestData) || !is.numeric(predictionsOfTestData) ) stop("ERROR in estimate of ConfidenceEstimator: Requires a numeric vector as input.");
	if (length(predictionsOfTestData) != getNumberOfInstances(testData) ) stop("ERROR in estimate of ConfidenceEstimator: Number of predicted values does not equal the test dataset size.");
})



# evaluate a set of confidence estimates by checking the correlation of their errors with the confidence interval width
setMethod("evaluateConfidenceEstimates", signature("vector","vector","matrix"),  function(realValues, predictedValues, confidenceValues, qualityFunction = averageError) {
	if (!is.vector(realValues) || !is.vector(predictedValues) || !is.matrix(confidenceValues)) stop("ERROR in evaluateConfidenceEstimates: Requires two vectors and a matrix as input.");
	if (!is.numeric(realValues) || !is.numeric(predictedValues) || !is.numeric(confidenceValues)) stop("ERROR in evaluateConfidenceEstimates: Requires numerics as input.");
	if (length(realValues)!=length(predictedValues) || length(predictedValues)!=dim(confidenceValues)[1] ) stop("ERROR in evaluateConfidenceEstimates: Requires two vectors of equal length and a matrix of equal row length as input.");

	# reject if interval width is equal for all
	if (any(is.na(confidenceValues[,3]-confidenceValues[,2])) || sd((confidenceValues[,3]-confidenceValues[,2])) < 0.000001)
	{
		return(NA);
	}
	
	options(warn=-1)
	# correlation of width and errors
	r <- cor((confidenceValues[,3]-confidenceValues[,2]), abs(realValues-predictedValues))
	
	# sort errors and interval width to get perfect estimation
	e <- sort(abs(realValues-predictedValues));
	pc <- sort((confidenceValues[,3]-confidenceValues[,2]));	
	
	# normalize correlation by correlation of perfect estimator
	r <- r / cor(pc,e)
	options(warn=1)

	return(r);
})


# optimize the environmental parameter of a confidence estimator
setMethod("optimize", signature("ConfidenceEstimator"), function(.Object, optimize) {
	if (!optimize %in% c("distance","neighbors","errors")) stop("ERROR in optimize of ConfidenceEstimator: Valid optimization options are 1,2, and 3.");

	numberOfFolds <- 2;
	qualityFunction <- averageError;		
	randomize <- TRUE;  ##### CHANGE TO TRUE!!!!!!!
	data <- .Object@trainingData;
	numberOfInstances <- dim(data)[1];

	#calculate folds
	folds <- calculateFolds(data, numberOfFolds, randomize);
	
	# keep track of the following values
	distMatrix <- matrix(1000000000, numberOfInstances, numberOfInstances);
	preds <- c(1:numberOfInstances);
	reals <- getResponses(data)
	averageDistance <- 0;
	numberOfDistances <- 0;
	averageError <- 0;

	tryCatch({	
		for (fold in c(1:numberOfFolds))
		{	
			# get train and test set		
			datasets <- splitByFolds(data, folds, fold);
			trainingData <- datasets[[1]];
			testData <- datasets[[2]];

			# define test and train set
			testsetInstances <- which(folds==fold);
			trainingSetInstances <- which(folds!=fold);
			
			# predict values on test data
			model <- learnFromDataset(.Object@regressionModel,trainingData);
			preds[testsetInstances] <- predictDataset(model,testData);

			helpMatrix <- distTo(testData, trainingData);
			distMatrix[testsetInstances, trainingSetInstances] <- helpMatrix;
			
			averageDistance <- averageDistance + sum(helpMatrix);
			numberOfDistances <- numberOfDistances+length(helpMatrix)				
			
			averageError <- averageError + sum( (preds[testsetInstances] - getResponses(testData))^2 ) / length(testsetInstances);
		}	
	}, error = function(ex) {
		message(paste("ERROR in ConfidenceEstimator: Splits of the feature set migtht contain colinear features. Either the dataset is too small or no feature selection has been applied: \n ",ex));
		stop();
	})
	
	# update saved values
	averageDistance <- averageDistance / numberOfDistances;
	averageError <- averageError / numberOfFolds;
	
	# optimize
	bestQuality <- -1000000;
	bestParameter <- 0;
	
	# set optimize interval depending on the parameter to optimize
	
	# optimize == 1 => distance is optimized
	if (optimize == "distance")
	{
		testParameters <- seq(averageDistance/2,averageDistance,averageDistance/16);
	 	
	# optimize == 2 => number of neighbors is optimized
	} else if(optimize == "neighbors")
	{		
		steps <- floor(numberOfInstances/20);
		testParameters <- seq(5,numberOfInstances/2,steps);
		
	#optimize == 3 => maximal error of neighbors is optimized
	} else if (optimize == "errors") {
		testParameters <- seq(averageError/32,averageError,averageError/16);
	} else {
		return(c(0,0,0));
	}

	# iterate over different parameters and calc AOC to find best parameter
	for (p in testParameters)	
	{
		#calculate confidences for each fold and calc aoc		
		confidences <- mat.or.vec(numberOfInstances,1);
		summedQuality <- 0;		
		# save if non NA results where obtained in cv, if not break loop
		nonNA <- FALSE;
		
		.Object@environmentalParameter  <- p;

		for (fold in c(1:numberOfFolds))
		{
			# get train and test set		
			datasets <- splitByFolds(data, folds, fold);
			trainingData <- datasets[[1]];
			testData <- datasets[[2]];

			# define test and train set
			testsetInstances <- which(folds==fold);
			trainingSetInstances <- which(folds!=fold);
			
			# set training data and predicts for this fold
			.Object@trainingData <- trainingData;
			.Object@predictionsOfTrainingData <- preds[trainingSetInstances];	
	
			# estimate confidences of training data and corresponding confidence interval borders
			confidencesTrain <- estimate(.Object, trainingData, preds[trainingSetInstances])[,1];  			
			.Object <- estimateConfidenceIntervals(.Object, getResponses(trainingData), preds[trainingSetInstances], confidencesTrain);	

			# estimate confidence scores and intervals
			estimatedIntervals <- estimate(.Object, testData, preds[testsetInstances]);

			# eval estimation quality
			quality <- evaluateConfidenceEstimates(reals[testsetInstances], preds[testsetInstances], estimatedIntervals, .Object@qualityFunction);
			summedQuality <- sum(summedQuality, quality, na.rm=TRUE);

			if (!is.na(quality)) nonNA <- TRUE		

		}
		if (optimize == "neighbors" && !nonNA)
		{
			break;
		}
		
		summedQuality <- summedQuality  / numberOfFolds;
		
		if (summedQuality  > bestQuality)
		{
			bestQuality <- summedQuality ;
			bestParameter <- p;
		}	
	}

	
	return(c(bestParameter, averageDistance, bestQuality));	
	
})



# predict confidence intervals using the pre-calculated intervals, the predicted values and the corresponding confidence scores
setMethod("predictConfidenceIntervals", signature("matrix", "vector", "vector"), function(confidenceIntervals, predictedValues, confidenceValues) {
	if (!is.matrix(confidenceIntervals) || !is.vector(predictedValues) || !is.vector(confidenceValues)) stop("ERROR in predictConfidenceIntervals: Requires a matrix and two vectors as input.");
	if (!is.numeric(confidenceIntervals) || !is.numeric(predictedValues) || !is.numeric(confidenceValues)) stop("ERROR in predictConfidenceIntervals: Requires three numerics as input.");
	if (dim(confidenceIntervals)[2]!=3 || length(predictedValues)!=length(confidenceValues) ) stop("ERROR in predictConfidenceIntervals: Requires two vectors and a matrix of equal length as input.");
	
	numberOfTestInstances <- length(predictedValues)
	numberOfTrainingInstances <- dim(confidenceIntervals)[1];
	predictedConfidenceIntervals <- matrix(c(1:3*numberOfTestInstances),numberOfTestInstances,3);

	for (i in c(1:numberOfTestInstances))
	{
		csInterval <- length(which(confidenceIntervals[,1] <= confidenceValues[i]));
		if (csInterval == 0)
		{
			csInterval <- 1;
		}
		
		predictedConfidenceIntervals[i,1] <- csInterval / numberOfTrainingInstances;
		predictedConfidenceIntervals[i,2] <- predictedValues[i] - confidenceIntervals[csInterval,3];
		predictedConfidenceIntervals[i,3] <- predictedValues[i] - confidenceIntervals[csInterval,2];
	}
	return(predictedConfidenceIntervals)
})

# estimate confidence intervals based on a set of errors and corresponding confidence scores
setMethod("estimateConfidenceIntervals", signature("ConfidenceEstimator", "vector", "vector", "vector"), function(.Object, realValues, predictedValues, confidenceValues) {
	if (!is.vector(realValues) || !is.vector(predictedValues) || !is.vector(confidenceValues)) stop("ERROR in estimateConfidenceIntervals of ConfidenceEstimator: Requires three vectors as input.");
	if (!is.numeric(realValues) || !is.numeric(predictedValues) || !is.numeric(confidenceValues)) stop("ERROR in estimateConfidenceIntervals of ConfidenceEstimator: Requires three vectors of numerics as input.");
	if (length(realValues)!=length(predictedValues) || length(predictedValues)!=length(confidenceValues) ) stop("ERROR in estimateConfidenceIntervals of ConfidenceEstimator: Requires three vectors of equal length as input.");

	sortedConfidences <- sort(confidenceValues);
	numberOfInstances <- length(realValues);
	instances <-  c(1:numberOfInstances);
	
	# define range of instances with similar confidence scores to be considered for interval estimation
	range <- 50;
	if (numberOfInstances/2 < 50)
	{
		range <- numberOfInstances/4;
	}
	confidenceIntervals <- matrix(c(1:3*numberOfInstances),numberOfInstances,3);
	
	for (i in c(1:numberOfInstances))
	{
		instMin <- i-range;
		if (i-range < 1) {
			instMin <- 1;
		}
		instMax <- i+range;
		if (i+range > numberOfInstances)
		{
			instMax <- numberOfInstances;
		}
		csMin <- sortedConfidences[instMin];
		csMax <- sortedConfidences[instMax];
		
		confidentInstances <- instances[ which(confidenceValues >= csMin & confidenceValues <= csMax) ];		
		errors <- predictedValues[confidentInstances]-realValues[confidentInstances];

		confidenceIntervals[i,1] <- sortedConfidences[i];
		confidenceIntervals[i,2] <- quantile(errors,0.1);
		confidenceIntervals[i,3] <- quantile(errors,0.9);
	}
	errors <- predictedValues-realValues;
	.Object@overallInterval <- c(quantile(errors,0.1),quantile(errors,0.9));
	
	.Object@confidenceIntervals <- confidenceIntervals;
	.Object
})


# save confidence estimator in binary file
setMethod("saveObject", signature("ConfidenceEstimator","character"), function(.Object, filename) {
	helpIntern = .Object;
	save(helpIntern, file=filename);	
})



# create plot of confidence intervals
setMethod("plotConfidenceIntervals", signature("ConfidenceEstimator", "vector", "vector", "vector"), function(.Object, realValues, predictedValues, confidenceValues) {
	if (!is.vector(realValues) || !is.vector(predictedValues) || !is.vector(confidenceValues)) stop("ERROR in plotConfidenceIntervals of ConfidenceEstimator: Requires three vectors as input.");
	if (!is.numeric(realValues) || !is.numeric(predictedValues) || !is.numeric(confidenceValues)) stop("ERROR in plotConfidenceIntervals of ConfidenceEstimator: Requires three vectors of numerics as input.");
	if (length(realValues)!=length(predictedValues) || length(predictedValues)!=length(confidenceValues) ) stop("ERROR in plotConfidenceIntervals of ConfidenceEstimator: Requires three vectors of equal length as input.");
	
	numberOfTrainingInstances <- dim(.Object@confidenceIntervals)[1];

	# smooth confidence curve by averaging over 10 scores
	smoothingParameter <- 10;
	upperBound <- .Object@confidenceIntervals[,3];
	lowerBound <- .Object@confidenceIntervals[,2];	
	for (i in c(1:numberOfTrainingInstances))
	{
		a <- i - smoothingParameter/2;
		b <- i + smoothingParameter/2;
		if (a < 1) a <- 1;
		if (b > numberOfTrainingInstances) b <- numberOfTrainingInstances;

		upperBound[i] <- mean(.Object@confidenceIntervals[c(a:b),3]);
		lowerBound[i] <- mean(.Object@confidenceIntervals[c(a:b),2]);
	}
	
	# plot bounds
	plot( c(1:numberOfTrainingInstances), upperBound, type="l",lty=1, ylim=c(min(lowerBound)-0.5,max(upperBound)+0.5), xaxt="n", xlab="confidence score", ylab="prediction error" );
	lines( c(1:numberOfTrainingInstances), lowerBound, type="l",lty=1 );
	lines( c(1,numberOfTrainingInstances), c(0,0), type="l",lty=3 );
	lines( c(1,numberOfTrainingInstances), c(.Object@overallInterval[1],.Object@overallInterval[1]), type="l",lty=2 );
	lines( c(1,numberOfTrainingInstances), c(.Object@overallInterval[2],.Object@overallInterval[2]), type="l",lty=2 );
	
	#plot instances
	points( confidenceValues*numberOfTrainingInstances, predictedValues-realValues, col="red");
		
	axis(1,at=seq(1,numberOfTrainingInstances+1,numberOfTrainingInstances/10),labels=seq(0,1,0.1));
	legend(numberOfTrainingInstances*0.65, min(lowerBound)-0.2, cex=0.6, lty=c(1,2,3), legend=c("score-based confidence interval", "overall confidence interval","zero error"));

})