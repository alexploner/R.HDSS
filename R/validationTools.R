#' Tools for checking variables in a data frame
#'
#' These functions check for existence and properties of a specified variable
#' name in a given data frame.
#'
#' @param v a character expression giving the full variable name
#' @param x the data frame where to check
#' @param val a vector of permissible values for the variable
#'
#' @return a list consisting of a logical value \code{flag} that indicates whether
#'         or not the test was passed, and a character string \code{text} that
#'         contains message giving information on what happened
#'
#' @name var-check-tools
NULL

#' @rdname var-check-tools
varExists = function(v, x)
{
	flag = TRUE
	text = "ok"
	if ( !( v %in% colnames(x) ) ) {
		flag = FALSE
		text = paste("Required variable '", v, "' not found", sep="")
	}
	list(flag=flag, text=text)
}

#' @rdname var-check-tools
varNoMissing = function(v, x)
{
	flag = TRUE
	text = "ok"
	if ( any(is.na(x[,v])) ) {
		flag = FALSE
		text = paste("Variable '", v, "' has missing values", sep="")
	}
	list(flag=flag, text=text)
}

#' @rdname var-check-tools
varNoDuplicate = function(v, x)
{
	flag = TRUE
	text = "ok"
	if ( any(duplicated(x[,v])) ) {
		flag = FALSE
		text = paste("Variable '", v, "' has non-unique values", sep="")
	}
	list(flag=flag, text=text)
}

#' @rdname var-check-tools
varIsSequential = function(v, x)
{
	flag = TRUE
	text = "ok"
	if ( !all(diff(x[,v]) == 1) ) {
		flag = FALSE
		text = paste("Variable '", v, "' is not sequential", sep="")
	}
	list(flag=flag, text=text)
}

#' @rdname var-check-tools
varExistsNoMissing = function(v, x)
{
	test = varExists(v, x)
	if (test$flag) {
		test2 = varNoMissing(v, x)
		test$flag = test$flag & test2$flag
		test$text = c(test$text, test2$text)
	}
	test
}

#' @rdname var-check-tools
varValidValues = function(v, x, val)
{
	flag = TRUE
	text = "ok"
	vv  = x[, v]
	vv  = vv[!is.na(vv)]
	if ( !all(vv %in% val) ) {
		flag = FALSE		
		text = paste("Variable '", v, "' has non-valid values", sep="")
	}
	list(flag=flag, text=text)
}

