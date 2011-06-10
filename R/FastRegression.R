#######################################  REGRESSION ##########################################

setMethod("FastRegression", signature(), function() {
	new("FastRegression");
})



setMethod("learnFromDataset", signature("FastRegression", "Dataset"), function(.Object, dataset, subset = c()) {
	lambda = 1e-10;

	if (is.null(subset))
	{
		subset <- c(1:getNumberOfInstances(dataset));
	}		
	
	featureSet <- getFeatureSet(dataset);
	dims <- c(length(subset), length(featureSet)+1);
	data <- getData(dataset);
	data <- matrix(unlist(data[subset,c(featureSet,dim(data)[2])]),dims[1],dims[2]);
	
	if (dim(data)[2] < 2)
	{
		stop("ERROR in learnFromDataset of FastRegression: Regression needs at least two dimensions, one x-dimension and one y-dimension.");
	} 
	
	a <- as.matrix(data[ ,-1*dim(data)[2]])
	b <- as.matrix(data[ ,dim(data)[2]])
	a <- cbind(1,a)
	
	h <- t(a)%*%a;
	i <- diag(dim(h)[2]) * lambda;
	h <- h+i;

	coef <- solve(h) %*% t(a) %*% b;	

	.Object@model <- coef;
	
	.Object
})


setMethod("predictDataset", signature("FastRegression","Dataset"), function(.Object, dataset, subset = c()) {
	if (is.null(subset))
	{
		subset <- c(1:getNumberOfInstances(dataset));
	}	
	
	featureSet <- getFeatureSet(dataset);	

	if (!length(featureSet)+1 == length(.Object@model))
	{
		stop("ERROR in predictDataset of FastRegression: Feature set of test set does not match the feature set of the training data:");
	}
	
	dims <- c(length(subset), length(featureSet));
	data <- getData(dataset)[subset,featureSet];
	data <- matrix(unlist(data),dims[1],dims[2]);
	
	coef <- .Object@model
	data<-cbind(1,data)  
	preds <- data %*% coef
	return(preds);
})


setMethod("saveObject", signature("FastRegression","character"), function(.Object, filename) {
	helpIntern = .Object;
	save(helpIntern, file=filename);	
})

