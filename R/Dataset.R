
#######################################  DATASET ##########################################

setMethod("Dataset", signature("matrix"), function(dataMatrix) {
	new("Dataset", dataMatrix);
})


setMethod("initialize", "Dataset",  function(.Object, data) {
	if (!is.numeric(data)) stop("ERROR in constructor of Dataset: No numeric features given.");
	if (!is.matrix(data)) stop("ERROR in constructor of Dataset: No matrix given.");
	if (dim(data)[2] < 2) stop("ERROR in constructor of Dataset: No response given.");

	.Object@data <- data;	
	.Object@numberOfInstances <- dim(.Object@data)[1];
	.Object@means <-apply(data,2,mean);
	.Object@sds <-apply(data,2,sd);
	featureSet <- c();
	for (feature in c(1:(dim(data)[2]-1)))
	{	
		if ((!is.na(.Object@sds[feature]) & .Object@sds[feature]>0.01) | .Object@numberOfInstances == 1)
		{
			featureSet <- append(featureSet,feature);
		}
	}
	.Object@featureSet <- featureSet;
	.Object@response <- dim(.Object@data)[2];  
	.Object@scaled <- FALSE;
	.Object
})


setMethod("getData", signature("Dataset"), function(.Object) {
	.Object@data;
})

setMethod("getFeatureSet", signature("Dataset"), function(.Object) {
	.Object@featureSet;
})

setReplaceMethod("featureSet", signature("Dataset"), function(.Object, value) {
	if (!is.numeric(value) || !is.vector(value)) stop("ERROR in featureSet<- of Dataset: No numeric feature vector given.");
	.Object@featureSet <- value;
	.Object
})

setMethod("getResponses", signature("Dataset"), function(.Object) {
	.Object@data[ ,.Object@response];
})


setMethod("getNumberOfInstances", signature("Dataset"), function(.Object) {
	.Object@numberOfInstances;
})


setMethod("normalize", signature("Dataset"), function(.Object) {
	.Object@means <-apply(.Object@data,2,mean);
	.Object@sds <-apply(.Object@data,2,sd);
	.Object@data <- as.matrix(scale(.Object@data));
	.Object@scaled <- TRUE;
	.Object	
})

setMethod("dim", signature("Dataset"), function(x) {
	dim(x@data)
})


setMethod("distTo", signature("Dataset", "Dataset"), function(.Object, .Object2) {
	#Object = test, Object2 = train
	
	if(!all(.Object@featureSet == .Object2@featureSet)) stop("Error in distTo of Dataset: Two given datasets differ in their feature set.");

	numberOfTrainingInstances <- .Object2@numberOfInstances;
	trainingMatrix <- t(matrix(as.matrix(.Object2@data[ ,.Object@featureSet]),numberOfTrainingInstances))

	distMatrix <- apply(.Object@data,1,function(x) sqrt(colSums( (c(as.matrix(x[.Object@featureSet])) - trainingMatrix)^2 )) );
	
	return(t(distMatrix));
})

setMethod("normalizeBy", signature("Dataset","Dataset"), function(.Object, .Object2) {
	if (dim(.Object@data)[2] != dim(.Object2@data)[2]) stop("Error in normalizeBy of Dataset: Two given datasets differ in their number of features.");

	.Object@data <- t((t(.Object@data)-c(as.matrix(.Object2@means,length(.Object2@means))))/c(as.matrix(.Object2@sds,length(.Object2@sds))))
	.Object@means <- .Object2@means;
	.Object@sds <- .Object2@sds;
	.Object@featureSet <- .Object2@featureSet;
	.Object@scaled <- TRUE;
	.Object;
})

setMethod("calculateFolds", signature("Dataset","numeric"), function(.Object, numberOfFolds, random) {
	if (.Object@numberOfInstances < numberOfFolds) stop("Error in calcualteFolds of Dataset: Given dataset consists of less instances than requested folds.");

	options(warn=-1)
	folds <- c(matrix( c(1:numberOfFolds), 1, .Object@numberOfInstances ));
	options(warn=1)
	
	if (random)
	{
		folds <- sample(folds)
	}
	return(folds);
})

setMethod("splitByFolds", signature("Dataset","vector","numeric"), function(.Object, vectorOfFolds, extraFold) {
	
	if (!is.numeric(vectorOfFolds) || !is.vector(vectorOfFolds)) stop("ERROR in splitByFolds of Dataset: No numeric vector given.");
	if (.Object@numberOfInstances != length(vectorOfFolds)) stop("Error in splitByFolds of Dataset: Given dataset consists of less or more instances than requested fold assignment.");
	
	testData <- new("Dataset", .Object@data[ vectorOfFolds == extraFold, ]);
	trainingData <- new("Dataset", .Object@data[ vectorOfFolds != extraFold, ]);
	if (.Object@scaled)
	{
		trainingData@means <- .Object@means;
		testData@means <- .Object@means;
		trainingData@sds <- .Object@sds;
		testData@sds <- .Object@sds;
		trainingData@featureSet <- .Object@featureSet;
		testData@featureSet <- .Object@featureSet;
		trainingData@scaled <- .Object@scaled;
		testData@scaled <- .Object@scaled;		
	}
	
	list(trainingData, testData);
})

setMethod("selectInstances", signature("Dataset","vector"), function(.Object, vectorOfInstances) {

	if (!is.numeric(vectorOfInstances) || !is.vector(vectorOfInstances)) stop("ERROR in selectInstances of Dataset: No numeric vector given.");

	newData <- new("Dataset", .Object@data);
	newData@data = matrix(unlist(.Object@data[ vectorOfInstances, ]),length(vectorOfInstances),dim(.Object@data)[2]);
	newData@featureSet <- .Object@featureSet;
	newData@means <- .Object@means;
	newData@sds <- .Object@sds;
	newData@scaled <- .Object@scaled;	
	newData@numberOfInstances <- length(vectorOfInstances);
	
	return(newData);
})

setMethod("addInstance", signature("Dataset","vector"), function(.Object, instance) {
	if (!is.numeric(instance) || !is.vector(instance)) stop("ERROR in addInstance of Dataset: No numeric vector given.");
	if (length(instance) != dim(.Object@data)[2]) stop("ERROR in addInstance of Dataset: Provided instances has contains a different number of features than the given dataset.");
	
	.Object@data <- rbind(.Object@data, instance);
	.Object@numberOfInstances <- .Object@numberOfInstances +1;
	.Object;
})

setMethod("unscaleVector", signature("Dataset","vector"), function(.Object, vector) {
	if (.Object@scaled)
	{
		vector <- vector*.Object@sds[.Object@response] + .Object@means[.Object@response];
	}
	return(vector);
})


setMethod("saveObject", signature("Dataset","character"), function(.Object, filename) {
	helpIntern = .Object;
	save(helpIntern, file=filename);
})

