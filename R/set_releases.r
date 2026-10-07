output_releases=function(outfile,releases,nocc,number.of.groups,nstrata,nevents)
{
if(length(releases)==1)
  releases=array(releases,dim=c(1,nstrata+nevents,number.of.groups))
if(is.matrix(releases))
  dim(releases)=c(nrow(releases),1,ncol(releases))
if(nstrata<=1)
{
  for (i in 1:number.of.groups)
  {
    write(paste("releases group=",i,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(releases[,1,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
} else
{
  for (i in 1:number.of.groups)
  for(j in 1:(nstrata+nevents))
  {
    if(j<=nstrata)
       write(paste("releases group=",i," strata=",j,";",sep=""),file=outfile,append=TRUE)
    else
      write(paste("releases group=",i," event=",j-nstrata,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(releases[,j,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
}
}

test_releases=function(releases,nocc,number.of.groups,nstrata,nevents)
{
  if(is.null(releases))
    stop("\nMust specify releases")
  if(!is.array(releases)&!length(releases)==1)
    stop("\nreleases must be an array or single constant")
  if(is.array(releases))
  {
    len=dim(releases)
    len=len[length(len)]
    if(!len==number.of.groups)
      stop("number of rows must be number of groups")
    len=dim(releases)
    len=len[1]
    if(len>nocc)
      stop("number of releases must be no greater than number of occasions")
    len=dim(releases)
    if(length(len)>2)
    {
      if(len[2]!=(nstrata+nevents))
        stop("number of columns must be number of strata + number of events")
    }
  }
  return(NULL)
}
  
# code for testing releases that has been removed from make.simmark.model; may want at some point
# Output proc simulate statement 
#test_releases(releases,nocc=nocc,number.of.groups=number.of.groups,nstrata=nstrata,nevents=0)
# if(is.null(releases))
#   stop("\nMust specify releases")
# if(!is.array(releases))
#   stop("\nreleases must be an array")
# len=dim(releases)
# len=len[length(len)]
# if(!len==number.of.groups)
#   stop("number of rows must be number of groups")
# len=dim(releases)
# len=len[1]
# if(len!=nocc-1)
#   stop("number of releases must be # of occasions -1")
# len=dim(releases)
# if(length(len)>2)
# {
#    if(len[2]!=nstrata)
#      stop("number of columns must be number of strata")
# }
