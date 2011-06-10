
########################################## MODEL EVALUATION ####################################


setMethod("ModelEvaluation", signature(), function(qualityFunction = averageError) {
	new("ModelEvaluation", qualityFunction);
})

# setValidity
setMethod("initialize", "ModelEvaluation",  function(.Object, qualityFunction = averageError) {
	.Object@qualityFunction <- qualityFunction;
	.Object
})


setMethod("getPredictionsOfCV", signature("ModelEvaluation","Dataset", "Regression", "numeric"), function(.Object, data, regressionModel, numberOfFolds, randomize, featureSelection) {
	folds <- calculateFolds(data, numberOfFolds, randomize);

	predictions <- mat.or.vec(getNumberOfInstances(data),1);
	
	for(fold in c(1:numberOfFolds))
	{		
		tryCatch({	
			if (is.object(featureSelection))
			{
				workdata <- Dataset( (getData(data))[which(folds!=fold),] );
				featureSetOfFold <- select(featureSelection, workdata, new("ModelEvaluation", averageError), randomize=FALSE, regressionModel)
				workdata <- data;
				featureSet(workdata) <- featureSetOfFold;				
				
				regressionModel <- learnFromDataset(regressionModel,workdata,which(folds!=fold));
				predictions[which(folds==fold)] <- predictDataset(regressionModel,workdata,which(folds==fold));							
			}  else {			
				regressionModel <- learnFromDataset(regressionModel,data,which(folds!=fold));
				predictions[which(folds==fold)] <- predictDataset(regressionModel,data,which(folds==fold));											
			}
						
		}, error = function(ex) {		
		})		

	}
	
	return(predictions);

})


#define evaluation method
setMethod("evaluateInCV", signature("ModelEvaluation","Dataset", "Regression", "numeric"), function(.Object, data, regressionModel, numberOfFolds, randomize, featureSelection) {
	folds <- calculateFolds(data, numberOfFolds, randomize);
	badValue = Inf;
	if (.Object@qualityFunction(c(1,2),c(1,2)) > 0.9999)
	{
		badValue = 0;
	}
	
	qual <- unlist(lapply(c(1:numberOfFolds), function(fold)	
	{		
		qualityOfFold <- badValue;
		tryCatch({	
			if (is.object(featureSelection))
			{
				workdata <- Dataset( (getData(data))[which(folds!=fold),] );
				featureSetOfFold <- selectFeatures(featureSelection, workdata, new("ModelEvaluation", averageError), randomize=FALSE, regressionModel)
				workdata <- data;
				featureSet(workdata) <- featureSetOfFold;
				
				regressionModel <- learnFromDataset(regressionModel,workdata,which(folds!=fold));
				preds <- predictDataset(regressionModel,workdata,which(folds==fold));				
				reals <- getResponses(workdata)[which(folds==fold)];

			}  else {		
			
				regressionModel <- learnFromDataset(regressionModel,data,which(folds!=fold));
				preds <- predictDataset(regressionModel,data,which(folds==fold));
				reals <- getResponses(data)[which(folds==fold)];			
			}
			
			
			qualityOfFold <- .Object@qualityFunction(reals, preds);		

			if (is.na(qualityOfFold))
			{
				qualityOfFold <- badValue;
			}		
		}, error = function(ex) {	
			print(ex)
		})		

		qualityOfFold;
	}))

	return(mean(qual));	
})
