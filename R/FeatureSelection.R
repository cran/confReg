########################################## FEATURE SELECTION ####################################

# setValidity
setMethod("initialize", "FeatureSelection",  function(.Object,qualityFunction = averageError, compareFunction = min) {
	.Object@qualityFunction <- qualityFunction;
	.Object@compareFunction <- compareFunction;
	.Object
})

#define selection method
setMethod("selectFeatures", signature("FeatureSelection", "Dataset", "ModelEvaluation", "logical", "Regression"), function(.Object, trainingData, modelEvaluation, randomize, regressionModel) {
})
