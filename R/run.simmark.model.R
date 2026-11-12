#' Runs MARK.EXE to simulate data
#' 
#' Passes input file from model (\code{model$input}) to MARK, runs MARK
#' to create simulation data file.
#' 
#' 
#' @param model MARK model created by \code{\link{make.simmark.model}}
#' @param invisible if TRUE, execution of MARK.EXE is hidden from view
#' @param prefix base portion of filename with numeric value added for a series of model (eg simmark001,simmark002 etc)
#' @param threads number of cpus to use with mark.exe if positive or number of cpus to remain idle if negative
#' @param ignore.stderr If set TRUE, messages from mark.exe are suppressed; they are automatically suppressed with Rterm
#' @param keep.input if TRUE, keeps input field in the model
#' @param simfile simulation binary filename
#' @param delete if TRUE, deletes input and output files
#' @return model: mark model object 
#' \code{output} and the extracted simulation results in \code{simresults} from the output file
#' onto list; see mark in RMark for a detailed description of many (but not all) values
#' \code{mark} object created here.
#' @author Jeff Laake
#' @export
#' @seealso \code{\link{make.simmark.model}},
#' \code{\link{simmark}}
#' @keywords model
run.simmark.model <-
function(model,invisible=FALSE,prefix="simmark",threads=-1,ignore.stderr=FALSE,keep.input=FALSE,simfile,delete=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
  os=R.Version()$os
  RunMark=TRUE
  basefile=paste(prefix,"001",sep="")
  i = 1
  while (file.exists(paste(basefile, ".out", sep = ""))) 
  {
      i = i + 1
      basefile = paste(prefix, formatC(as.integer(i), flag = "0",width=3),sep = "")
  }
  outfile = paste(basefile, ".out", sep = "")
  inputfile = paste(basefile, ".inp", sep = "")
#
# Write input file to temp file 
#
  writeLines(model$input,inputfile)

  markpath=RMark:::create_markpath()
	 
  if(is.null(markpath))
  {
    if(os == "mingw32")
      stop("mark.exe, mark32.exe or mark64.exe cannot be found. Add to system path or specify MarkPath object (e.g., MarkPath='C:/Programme/Mark'")
    else
      stop("MARK executable could not be found.")
  }
  if(RunMark){
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
  }
  
  model$output=basefile
  if(!keep.input)model$input=NULL
  model=summarize.simmark(model,simfile)
  if(delete)
  {
    unlink(paste(model$output,".out",sep=""))
    unlink(paste(model$output,".inp",sep=""))
  }
 return(model)
}


