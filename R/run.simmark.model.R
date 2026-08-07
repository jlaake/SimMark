#' Runs MARK.EXE to simulate data
#' 
#' Passes input file from model (\code{model$input}) to MARK, runs MARK
#' to create simulation data file.
#' 
#' 
#' @param model MARK model created by \code{\link{make.simmark.model}}
#' @param invisible if TRUE, execution of MARK.EXE is hidden from view
#' @param threads number of cpus to use with mark.exe if positive or number of cpus to remain idle if negative
#' @param ignore.stderr If set TRUE, messages from mark.exe are suppressed; they are automatically suppressed with Rterm
#' @return model: MARK model object with the base filename stored in
#' \code{output} and the extracted \code{results} from the output file appended
#' onto list; see \code{\link{mark}} for a detailed description of a
#' \code{mark} object.
#' @author Jeff Laake
#' @export
#' @seealso \code{\link{make.simmark.model}},
#' \code{\link{simmark}}
#' @keywords model
run.simmark.model <-
function(model,invisible=FALSE,filename=NULL,threads=-1,ignore.stderr=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
  os=R.Version()$os
# Run mark.exe to simulate the data; assumed to be in normal location in which Mark is installed unless
# the variable MarkPath has been defined
# 24 Aug 05; save all files and give names mark###.*
#  9 Jan 06; use specified name if given
#
  if(!is.null(filename))
  {
	  basefile=filename
	  outfile = paste(basefile, ".out", sep = "")
#
#     If outfile already exists, ask user if mark object should be created with
#     existing file
#
	  RunMark=TRUE
	  if(file.exists(outfile))
	  {
		  if(toupper(substr(readline("Create mark model with existing file (Y/N)?"),1,1))=="Y")
		   RunMark=FALSE
	  }
  }else
  {
	   RunMark=TRUE
	   prefix="simmark"
     basefile=paste(prefix,"001",sep="")
	   i = 1
       while (file.exists(paste(basefile, ".out", sep = ""))) 
	   {
          i = i + 1
          basefile = paste(prefix, formatC(as.integer(i), flag = "0",width=3),sep = "")
       }
  }
  outfile = paste(basefile, ".out", sep = "")
  inputfile = paste(basefile, ".inp", sep = "")
#
# Write input file to temp file 
#
  writeLines(model$input,inputfile)
# Windows operating system
  if(os=="mingw32")
  {
  	 markpath=RMark:::create_markpath()
	 if(is.null(markpath))
	 {
		 cat("mark.exe, mark32.exe or mark64.exe cannot be found. Add to system path or specify MarkPath object (e.g., MarkPath='C:/Programme/Mark'")
		 return(NULL)
	 }
	 if(RunMark)
		 if(.Platform$GUI[1]=="RTerm")
		 {
			 if(invisible)
				 system(paste(markpath, " i=",inputfile," o=", outfile," threads=", threads,sep = ""),
				        ignore.stdout=TRUE,ignore.stderr=TRUE)
			 else
				 system(paste(markpath, " i=",inputfile," o=", outfile,
								 "threads=", threads,sep = ""),ignore.stderr=ignore.stderr)
			 
		 }else
		 {
			 system(paste(markpath, " i=",inputfile," o=", outfile,
								 " threads=", threads,sep = ""),invisible=TRUE,ignore.stderr=ignore.stderr)
			 if(file.exists("fort.0"))unlink("fort.0")
		 }
  } else
# Non Windows operating systems
  {
    if(!exists("MarkPath"))MarkPath=""
    if(RunMark)
       system(paste("mark i=",inputfile," o=", outfile,
            " threads=", threads,sep = ""),ignore.stderr=ignore.stderr)
  }
  model$output=basefile
  model$input=NULL
 return(model)
}


