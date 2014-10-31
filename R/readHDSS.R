#' Read raw HDSS data
#'
#' Read one or several data files, or a whole directory of data files, as
#' provided by the INDEPTH project, into R.
#'
#' @param file A file name or a vector of filenames to be read; if missing, all
#' files with the specified extension in the specified directory will be read.
#' @param path The directory from which to read the data files
#' @param zipped A logical flag, indicating whether to read from zipped files or
#' from unzipped files (i.e. \code{.csv} files with the same basename).
#' @param verb A logical flag, indicating whether to report during reading
#' @param forceList A logical flag indicating whether to return a list, see
#' Details
#' @param ... Extra arguments, passed to \code{\link{read.csv}}
#'
#' @details The data files are assumed to be CSV text files with standard coulmn
#' names as outlined in the HDSS specifications, see Examples below.
#' Alternatively, the data files can also be zipped versions of such a CSV file.
#' In this case, it is assumed that the name of the zipped file (without path)
#' is the same as the name of the zipped file, but with extension \code{.zip}
#' instead of \code{.csv}. It is no possible to mix zipped and unzipped files
#' in the same function call.
#'
#' Note that by default, this function returns a data frame if one file is found
#' and a list of data frames if to or more files are found. Use argument
#' \code{forceList} if consistent behavior is required.
#'
#' @seealso \code{\link{coreRecordTests}}, \code{\link{preprocHDSS}}
#' 
#' @return If only one filename is specified and \code{forceList} is FALSE, a
#' single data frame. If multiple filenames are specified or implied (by
#' specifying only the path to the data), a list of data frames, each containing
#' the raw data of one of the specified files; the names of the list entries are
#' the basenames (without extensions) of the files read in.
#' @export
#' @examples
#' file = system.file("extdata/testHDSS.csv", package="R.HDSS")
#' test1 = readRawHDSS(file)
#' head(test1)
readRawHDSS = function(file, path, zipped=FALSE, verb=TRUE, forceList=FALSE, ...)
{
	
	## What files are supposed to look like
	pattern = if(zipped) ".zip$" else ".csv$"
	
	## Get all files of the specified type, if none are given
	if (missing(file)) {
		if (missing(path)) stop("Path must be specified if no file is given")
		file = dir(path, pattern, full.names=FALSE)
		if (length(file) == 0) stop("No files of type ", pattern, " found")
	}

	## Build the full file name, as well as the wrapped filename for .zip
	fn1 = if (!missing(path)) file.path(path, file) else file
	fn2 = basename(sub(".zip", ".csv", file))

	## Loop
	raw = list()	
	nf = length(file)
	for (i in 1:nf) {
		con = if(zipped) unz(fn1[i], fn2[i]) else file(fn1[i])
		if (verb) cat("Reading ", file[i], "...\n", sep="")
		raw[[i]] = read.csv(con, ...)
	}
	names(raw) = sub(pattern, "", file)
	if (forceList) {
		return(raw)
	} else if (length(raw)==1) {
		return(raw[[1]])
	} else {
		warning("Multiple data sets read, ignoring forceList")
		return(raw)
	}
}
		
	
	
	

