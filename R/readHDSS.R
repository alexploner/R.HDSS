#' Read raw HDSS data
#'
#' Read one or several data files, or a whole directory of data files, as
#' provided by the INDEPTH project, into R.
#'
#' @param path The directory from which to read the data files
#' @param fn A vector of file names to be read; if missing, all files with the
#' specified extension in the specified directory will be read.
#' @param zipped A logical flag, indicating whether to read from zipped files or
#' from unzipped files (i.e. \code{.csv} files with the same basename).
#' @param verb A logical flag, indicating whether to report during reading
#' @param ... Extra arguments, passed to \code{\link{read.csv}}
#'
#' @return A list of data frames, each containing the raw data of one of the
#' specified files; the names of the list entries are the basenames (without
#' extensions) of the files read in.
#' @export
readRawHDSS = function(path=".", fn, zipped=TRUE, verb=TRUE, ...)
{
	## What files are supposed to look like
	pattern = if(zipped) ".zip$" else ".csv$"
	
	## Get all files of the specified type, if none are given
	if (missing(fn)) {
		fn = dir(path, pattern, full.names=FALSE)
		if (length(fn) == 0) stop("No files of type ", pattern, " found")
	}

	## Build the full file name, as well as the wrapped filename for .zip
	fn1 = file.path(path, fn)
	fn2 = sub(".zip", ".csv", fn)

	## Loop
	raw = list()	
	nf = length(fn)
	for (i in 1:nf) {
		con = if(zipped) unz(fn1[i], fn2[i]) else file(fn1[i])
		if (verb) cat("Reading ", fn[i], "...\n", sep="")
		raw[[i]] = read.csv(con, ...)
	}
	names(raw) = sub(pattern, "", fn)
	raw
}
		
	
	
	

