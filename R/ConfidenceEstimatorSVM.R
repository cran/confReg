########################################## CONFIDENCE ESTIMATOR 1-CLASS SVM ################################

setMethod("ConfidenceEstimatorSVM", signature("Regression", "Dataset"), function(regressionModel, trainingData, qualityFunction = averageError) {
	new("ConfidenceEstimatorSVM", regressionModel, trainingData, qualityFunction);
})

setMethod("initialize", "ConfidenceEstimatorSVM",  function(.Object, regressionModel, trainingData, qualityFunction = averageError, svmModel = NULL) {
	.Object <- callNextMethod(.Object, regressionModel, trainingData, qualityFunction )
	.Object@svmModel <- svmModel
	.Object
})



setMethod("create", signature("ConfidenceEstimatorSVM", "logical", "vector"), function(.Object, optimize, predictionsOfTrainingData) {
	callNextMethod();
	.Object@confidenceIntervals <- matrix()

	numberOfFolds <- 2;	
	data <- .Object@trainingData;
	dataMatrix <- getData(data);
	numberOfInstances <- dim(data)[1];
	featureSet <-getFeatureSet(data);
	randomize = TRUE;
	
	#calculate folds
	folds <- calculateFolds(data, numberOfFolds, randomize);
	
	preds <- c(1:numberOfInstances);
	reals <- getResponses(data)

	for (fold in c(1:numberOfFolds))
	{	
		# define test and train set
		testsetInstances <- which(folds==fold);
		trainingSetInstances <- which(folds!=fold);

		# predict values on test data
		tryCatch({	
			model <- learnFromDataset(.Object@regressionModel,data,trainingSetInstances);
			preds[testsetInstances] <- predictDataset(model,data,testsetInstances);
		}, error = function(ex) {
			message(paste("ERROR in ConfidenceEstimatorSVM: Splits of the feature set migtht contain colinear features. Either the dataset is too small or no feature selection has been applied: \n ",ex));
			stop();
		})
	}		
	
	bestNu <- 0.5;
	bestGamma <- 0.5;
	bestCost <- 0.5;
	
	# since the SVM returns values of a different scale for test data, we save the mean and sd of the test confidences in the cv to normalize the confidence score of the training data to this scale
	normalizationParams <- c(0,0)
	
	if (optimize)
	{	
		bestQuality <- -100;

		for (nu in seq(0.1,0.9,0.1))
		{
			for (gamma in 2^(-1:2))
			{
				for (cost in 2^(-1:4))
				{				
					summedQuality <- 0;
					p1 <- 0;
					p2 <- 0;
					
					for (fold in c(1:numberOfFolds))
					{
						# define test and train set
						testsetInstances <- which(folds==fold);
						trainingSetInstances<- which(folds!=fold);
						
						# train an svm model and estimate confidence interval borders
						m <- e1071::svm(x=dataMatrix[trainingSetInstances,featureSet], scale=TRUE, type="one-classification", kernel="radial", cost = cost, gamma=gamma, nu=nu);
						confidencesTrain <- unlist(attr(stats::predict(m, dataMatrix[trainingSetInstances,featureSet], decision.values=TRUE),"decision.values"))	
						
						#  estimate confidences 
						confidences <- unlist(attr(stats::predict(m, dataMatrix[testsetInstances,featureSet], decision.values=TRUE),"decision.values")) 
						
						# normalize training confidences to same range as test confidences
						confidencesTrain <- (confidencesTrain-mean(confidencesTrain))/sd(confidencesTrain)*sd(confidences)+mean(confidences)
						p1 <- p1+mean(confidences)
						p2 <- p2+sd(confidences)

						# estimate confidence interval border and predict intervals for test data		
						.Object <- estimateConfidenceIntervals(.Object, getResponses(data)[trainingSetInstances], preds[trainingSetInstances], confidencesTrain);	
						predictedConfidenceValues <- predictConfidenceIntervals(.Object@confidenceIntervals,  preds[testsetInstances], confidences);

						# get quality		
						quality <- evaluateConfidenceEstimates(reals[testsetInstances], preds[testsetInstances], predictedConfidenceValues, .Object@qualityFunction);
						summedQuality <- sum(summedQuality, quality, na.rm=TRUE);												
					}
					p1 <- p1 /numberOfFolds;
					p2 <- p2 /numberOfFolds;

					if (summedQuality/numberOfFolds > bestQuality)
					{
						bestQuality <- summedQuality/numberOfFolds;
						bestCost <- cost;
						bestNu <- nu;
						bestGamma <- gamma;
						normalizationParams <- c(p1,p2)
					}
				}
			}
		}
		.Object@estimatedPerformance <- bestQuality;
		
	} else {
		# if no optimization is request, perform a cross-validation to yield normalization ratio for test data
		summedQuality <- 0;
		p1 <- 0;
		p2 <- 0;
		for (fold in c(1:numberOfFolds))
		{
			# define test and train set
			testsetInstances <- which(folds==fold);
			trainingSetInstances<- which(folds!=fold);
						
			# train an svm model and estimate confidence interval borders
			m <- e1071::svm(x=dataMatrix[trainingSetInstances,featureSet], scale=TRUE, type="one-classification", kernel="radial", cost = bestCost, gamma=bestGamma, nu=bestNu);
			confidencesTrain <- unlist(attr(stats::predict(m, dataMatrix[trainingSetInstances,featureSet], decision.values=TRUE),"decision.values"))	
					
			# estimate confidences 
			confidences <- unlist(attr(stats::predict(m, dataMatrix[testsetInstances,featureSet], decision.values=TRUE),"decision.values")) 
			
			# normalize training confidences to same range as test confidences
			confidencesTrain <- (confidencesTrain-mean(confidencesTrain))/sd(confidencesTrain)*sd(confidences)+mean(confidences)
			p1 <- p1+mean(confidences)
			p2 <- p2+sd(confidences)
						
			# estimate confidence interval border and predict intervals for test data						
			.Object <- estimateConfidenceIntervals(.Object, getResponses(data)[trainingSetInstances], preds[trainingSetInstances], confidencesTrain);							
			predictedConfidenceValues <- predictConfidenceIntervals(.Object@confidenceIntervals,  preds[testsetInstances], confidences);

			# get quality		
			quality <- evaluateConfidenceEstimates(reals[testsetInstances], preds[testsetInstances], predictedConfidenceValues, .Object@qualityFunction);
			summedQuality <- sum(summedQuality, quality, na.rm=TRUE);												
		}
		normalizationParams <- c(p1 /numberOfFolds, p2 /numberOfFolds);
					
		.Object@estimatedPerformance <- summedQuality/numberOfFolds;
					
	}
	.Object@environmentalParameter <- -1;
	.Object@predictionsOfTrainingData <- predictionsOfTrainingData;	
	
	# create and save best SVM model
	.Object@svmModel <- e1071::svm(x=dataMatrix[,featureSet], scale=TRUE, type="one-classification", kernel="radial", cost = bestCost, gamma=bestGamma, nu=bestNu);	
	.Object@trainingData <- data;

	# predict confidences of training data and normalize to test data interval	
	confidencesTrain <- unlist(attr(stats::predict(.Object@svmModel, dataMatrix[,featureSet], decision.values=TRUE),"decision.values")) 	
	confidencesTrain <- (confidencesTrain-mean(confidencesTrain))/sd(confidencesTrain)*normalizationParams[2]+normalizationParams[1]

	# use predicted confidences to estimate confidence interval borders
	.Object <- estimateConfidenceIntervals(.Object, getResponses(data), predictionsOfTrainingData, confidencesTrain);	
	.Object;
})

setMethod("estimate", signature("ConfidenceEstimatorSVM", "Dataset", "vector"), function(.Object, testData, predictionsOfTestData) {	
	callNextMethod();

	featureSet <- getFeatureSet(testData);
	testDataMatrix <- getData(testData);
	numberOfTestInstances <- getNumberOfInstances(testData);
	responses <- getResponses(.Object@trainingData);	
	errors <- .Object@predictionsOfTrainingData - responses;	
	
	# predict with trained svmModel
	confidences = attr(stats::predict(.Object@svmModel, testDataMatrix[,featureSet], decision.values=TRUE),"decision.values")	
	
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
