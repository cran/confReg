# define dataset class based on numeric matrix
setClass("Dataset", representation(data = "matrix", means = "vector", sds = "vector", featureSet = "vector", numberOfInstances = "numeric", response = "numeric", scaled="logical"))

# define regression model as class
setClass("Regression", representation(regressionFunction = "function", predictionFunction = "function", model = "ANY"), prototype(regressionFunction = lm, predictionFunction = predict))

# define a class for fast regression
setClass("FastRegression", contains = "Regression")

# define feature selection class
setClass("FeatureSelection", representation(qualityFunction = "function", compareFunction = "function", "VIRTUAL"))

# define twin scan selection class
setClass("TwinScan", contains = "FeatureSelection");

# define model evaluation class
setClass("ModelEvaluation", representation(qualityFunction = "function"))

# define confidence estimator virtual class
setClass("ConfidenceEstimator", representation(regressionModel = "Regression",  trainingData = "Dataset", qualityFunction = "function", estimatedPerformance = "numeric", environmentalParameter = "numeric", predictionsOfTrainingData = "vector", confidenceIntervals = "matrix",  overallInterval = "vector", "VIRTUAL"))

# Confidence estimation via variance of nearest-neighbors
setClass("ConfidenceEstimatorNNVariance", contains = "ConfidenceEstimator");

# Confidence estimation via prediction errors of nearest-neighbors
setClass("ConfidenceEstimatorNNErrors", contains = "ConfidenceEstimator");

# Confidence estimation via prediction of number of neighbors in environment
setClass("ConfidenceEstimatorNumberOfNeighbors", contains = "ConfidenceEstimator");

# Confidence estimation as difference to nearest neighors prediction
setClass("ConfidenceEstimatorDifferenceToNeighbors", contains = "ConfidenceEstimator");

# Confidence estimation as minimum distance to an instance in the training set
setClass("ConfidenceEstimatorMinimumDistance", contains = "ConfidenceEstimator");

# Confidence estimation as average distance to an instance in the training set
setClass("ConfidenceEstimatorAverageDistance", contains = "ConfidenceEstimator");

# Confidence estimation as average biased distance to an instance in the training set
setClass("ConfidenceEstimatorAverageBiasedDistance", contains = "ConfidenceEstimator");

# Confidence estimation as score of a one-class SVM
setClass("ConfidenceEstimatorSVM", contains = "ConfidenceEstimator", representation(svmModel = "ANY"));

# Confidence estimation based on variance of bagged predictor
setClass("ConfidenceEstimatorBagging", contains = "ConfidenceEstimator", representation(modelList = "list"));

# Confidence estimation based on the predictive variance of ridge regression
setClass("ConfidenceEstimatorPredictiveVariance", contains = "ConfidenceEstimator", representation(regressionMatrix = "matrix"));

# Confidence estimation based on the senstitvity analysis of local variance
setClass("ConfidenceEstimatorLocalVariance", contains = "ConfidenceEstimator");

# Confidence estimation based on the senstitvity analysis of local bias
setClass("ConfidenceEstimatorLocalBias", contains = "ConfidenceEstimator");

# Confidence estimation based on a local cross validation
setClass("ConfidenceEstimatorLocalCV", contains = "ConfidenceEstimator");
