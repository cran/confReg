########################################## TWIN SCAN ####################################


setMethod("TwinScan", signature(), function(qualityFunction = averageError, compareFunction = min) {
	new("TwinScan", qualityFunction, compareFunction);
})


setMethod("selectFeatures", signature("TwinScan", "Dataset", "ModelEvaluation", "logical", "Regression"), function(.Object, trainingData, modelEvaluation, randomize, regressionModel) {

	featureStartset <- getFeatureSet(trainingData);
	#	print(featureStartset);
	numberOfFeatures <- length(featureStartset);
	#data <- getData(trainingData);
	
	#z <- proc.time()[3];
	
	# calculate qual of single features
	qualityOfSingleFeatures <- unlist(lapply(c(1:numberOfFeatures), function(x)	
	#for (feature in featureStartset)
	{
		feature = featureStartset[x];
		#qual <- evaluateInCV(eval, data, m, 5);
		featureSet(trainingData) <- feature;
		
		qual <- evaluateInCV(modelEvaluation, trainingData, regressionModel, 5, randomize); 		
		if (randomize)
		{			
			for (cvTimes in c(1:(3-1)))
			{
				qual <- qual + evaluateInCV(modelEvaluation, trainingData, regressionModel, 5, randomize); 
			}
			qual <- qual / 3;			
		} 
		#print("overall avg");
		#print(feature);
		#print(qual);
		qual	
	}))

	#print(qualityOfSingleFeatures)
	#print(qualityOfSingleFeatures)
	#print(proc.time()[3]-z);

	# set best feature as start feature set
	featureOrder <- order(qualityOfSingleFeatures);
	if (.Object@compareFunction(1,2) == 2)
	{
		featureOrder <- rev(featureOrder);
	}
	#print(qualityOfSingleFeatures);
	#print(order(qualityOfSingleFeatures));
	#print(featureOrder);
	#print(featureStartset);
	#print(max(qualityOfSingleFeatures));
	bestQuality <- qualityOfSingleFeatures[featureOrder[1]];
	#print(featureOrder[1]);
	featureSet <- c(featureStartset[featureOrder[1]]);
	#print(bestQuality)
	z <- proc.time()[3];
	# iterate over remaining feature in order of their previous quality and add greedily
	#lapply(featureOrder[2:length(featureOrder)], function(feature)	
	for (feature in featureOrder[2:length(featureOrder)])
	{
		featureSet(trainingData) <- c(featureStartset[feature],featureSet);
		#print(c(featureStartset[feature],featureSet))
		
		qual <- evaluateInCV(modelEvaluation, trainingData, regressionModel, 5, randomize); 		
		if (randomize)
		{			
			for (cvTimes in c(1:(3-1)))
			{
				qual <- qual + evaluateInCV(modelEvaluation, trainingData, regressionModel, 5, randomize); 
			}
			qual <- qual / 3;			
		} 		
			
		
		
		if (.Object@compareFunction(qual,bestQuality+0.001,bestQuality-0.001) == qual)
		{
			#print("add");
			#print(feature);
			#print(qual);
			#print("keep");
			bestQuality <- qual;
			featureSet <- sort(append(featureSet,featureStartset[feature]));
			if (length(featureSet) >= 50)
			{
				break;
			}
		}

	}
	
	#print(bestQuality)
	#print(proc.time()[3]-z);
	return(sort(featureSet));	

})

