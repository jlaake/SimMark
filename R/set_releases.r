output_releases=function(outfile,releases,nocc,number.of.groups,nstrata,nevents)
{
if(length(releases)==1)
{
  #releases=c(releases,rep(0,nocc-1))
  releases=array(releases,dim=c(1,nstrata+nevents,number.of.groups))
}
if(nstrata<=1)
{
  for (i in 1:number.of.groups)
  {
    write(paste("releases group=",i,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(releases[,,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
} else
{
  for (i in 1:number.of.groups)
  for(j in 1:nstrata)
  {
    write(paste("releases group=",i," strata=",j,";",sep=""),file=outfile,append=TRUE)
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
  