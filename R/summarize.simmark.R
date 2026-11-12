# Summarize results from  proc simulate in program MARK.  
#' 
#' Passes arguments from MARK output file to read in binary results file and return summary
#' 
#' @param model model result
#' @param filename name of binary file containing simulation values
#' @return results: summarized simulation results
#' @author Gary White,Jeff Laake
#' @export
#' @seealso \code{\link{simmark}}
summarize.simmark <- function(model,filename)
{
read_fortran_records_to_array <- function(file, nestimates, endian = "little") {
  con <- file(file, "rb")
  on.exit(close(con))
  records <- list()
  repeat {
    # Read leading record marker
    marker <- readBin(con, integer(), size = 4, n = 1, endian = endian)
    # EOF reached
    if (length(marker) == 0) break
    # Expected number of bytes in the record
    expected_bytes <- nestimates * 8
    if (marker != expected_bytes) {
      stop("Record length mismatch: expected ", expected_bytes,
           " bytes but record marker says ", marker)
    }
    # Read the data block
    vals <- readBin(con, numeric(), size = 8, n = nestimates, endian = endian)
    # Skip trailing marker
    readBin(con, integer(), size = 4, n = 1, endian = endian)
    # Store
    records[[length(records) + 1]] <- vals
  }
  # Convert list ? matrix
    out <- do.call(rbind, records)
  #
  # create column names depending on values of arguments
  #
    cnames=c("-2logL","Deviance","df.data","df.dev","df.model",paste("BetaTrue",1:ncovs,sep=""),paste("BetaEst",1:ncovs,sep=""),paste("BetaSE",1:ncovs,sep=""))
    if(nlogit>0)  cnames=c(cnames,paste("RealTrue",1:nlogit,sep=""),paste("RealEst",1:nlogit,sep=""),paste("RealSE",1:nlogit,sep=""))
    if(nderived>0)cnames=c(cnames,paste("DerivedEst",1:nderived,sep=""),paste("DerivedSE",1:nderived,sep=""))  
    colnames(out) <- cnames
  return(out)
}

#
# read in outfile and search for R Variable Values:
# read in next 3 lines until ; and execute them and then call function to summarize
# and store summary in model results
#
# ncovs,nlogit and nderived are set with eval below they are assigned a value NULL to
# prevent check note
ncovs=NULL
nlogit=NULL
nderived=NULL
outfile=paste(model$output,".out",sep="")
out=readLines(outfile)  
x=grep("R Variable Values:",out,fixed=TRUE)
if(length(x)!=0)
{
  eval(parse(text=out[x+1]))
  eval(parse(text=out[x+2]))
  eval(parse(text=out[x+3]))
  # First 5 values are -2logL, deviance, ndf, ndfdev, and estimated number of parameters
  nestimates=5+(ncovs*3+nlogit*3+nderived*2)
  model$simresults = read_fortran_records_to_array(filename,nestimates)
} else
  model$simresults=NULL
return(model)
}

