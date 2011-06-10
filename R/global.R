##################################################
###                Global.R

setMethod("loadObject", signature("character"), function(filename) {
	load(file=filename);
	helpIntern
})

setMethod("averageError", signature("vector", "vector"), function(realValues, predictedValues) {
	errors <- (realValues-predictedValues)^2;
	sse <- mean(errors);
	return(sse);
})

