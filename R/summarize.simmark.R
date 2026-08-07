# Summarize results from  proc simulate in program MARK.  
#' 
#' Passes arguments from MARK output file to read in binary results file and return summary
#' 
#' @param ncovs number of beta estimates
#' @param nlogit number of real estimates
#' @param nderived number of derived estimates
#' @param filename name of binary file containing simulation values
#' @return results: summarized simulation results
#' @author Gary White,Jeff Laake
#' @export
#' @seealso \code{\link{simmark}}
summarize.simmark <- function(ncovs,nlogit,nderived,filename)
{

nestimates=(ncovs*3+nlogit*3+nderived*2);

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

  # Convert list → matrix
  out <- do.call(rbind, records)
#
# create column names depending on values of arguments
#
   cnames=c(paste("BetaTrue",1:ncovs,sep=""),paste("BetaEst",1:ncovs,sep=""),paste("BetaSE",1:ncovs,sep=""))
   if(nlogit>0)  cnames=c(cnames,paste("RealTrue",1:nlogit,sep=""),paste("RealEst",1:nlogit,sep=""),paste("RealSE",1:nlogit,sep=""))
   if(nderived>0)cnames=c(cnames,paste("DerivedEst",1:nderived,sep=""),paste("DerivedSE",1:nderived,sep=""))
   colnames(out) <- cnames
  return(out)
}

results = read_fortran_records_to_array(filename,nestimates)
return(results)
}

