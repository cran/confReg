#######################################  REGRESSION ##########################################

setMethod("Regression", signature("function", "function"), function(regressionFunction = lm, predictionFunction = predict) {
	new("Regression", regressionFunction, predictionFunction);
})


setMethod("initialize", "Regression",  function(.Object, regressionFunction = lm, predictionFunction = predict, model = NULL) {
	.Object@regressionFunction <- regressionFunction;
	.Object@predictionFunction <- predictionFunction;
	.Object@model <- model;
	.Object
})


setMethod("learnFromDataset", signature("Regression", "Dataset"), function(.Object, dataset, subset = c()) {
	if (is.null(subset))
	{
		subset <- c(1:getNumberOfInstances(dataset));
	}	
	
	data <- getData(dataset);
	y <- dim(data)[2];
	data <- data[subset,c(getFeatureSet(dataset),y)];

	options(warn=-1)
	input <- data.frame(data);	
	options(warn=1)
	names(input) <- c(getFeatureSet(dataset),"y");

	.Object@model <- .Object@regressionFunction(y~.,data=input, tol=1e-10);
	
	.Object
})


setMethod("predictDataset", signature("Regression","Dataset"), function(.Object, dataset, subset = c()) {
	if (is.null(subset))
	{
		subset <- c(1:getNumberOfInstances(dataset));
	}	
	data <- getData(dataset);
	data <- matrix(unlist(data[subset,c(getFeatureSet(dataset))]),length(subset),length(getFeatureSet(dataset)));

	input <- data.frame(data);
	names(input) <- getFeatureSet(dataset);
	
	preds <- .Object@predictionFunction(.Object@model, input);

	return(preds);
})


setMethod("saveObject", signature("Regression","character"), function(.Object, filename) {
	helpIntern = .Object;
	save(helpIntern, file=filename);	
})

