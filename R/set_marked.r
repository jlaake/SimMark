output_marked=function(outfile,marked,nocc,number.of.groups,nstrata=1,nevents=0)
{
if(nstrata>1)stop("At this time, nstrata>1 is not supported")
if(length(marked)==1)
    marked=array(marked,dim=c(1,1,number.of.groups))
if(is.matrix(marked))
  dim(marked)=c(nrow(marked),1,ncol(marked))
if(nstrata<=1)
{
  for (i in 1:number.of.groups)
  {
    write(paste("marked group=",i,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(marked[,1,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
} else
{
  for (i in 1:number.of.groups)
  for(j in 1:nstrata)
  {
    write(paste("marked group=",i," strata=",j,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(marked[,j,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
}
}

test_marked=function(marked,nocc,number.of.groups,nstrata,nevents)
{
  if(is.null(marked))
    stop("\nMust specify marked")
  if(!is.array(marked)&!length(marked)==1)
    stop("\nmarked must be an array or single constant")
  if(is.array(marked))
  {
    len=dim(marked)
    len=len[length(len)]
    if(!len==number.of.groups)
      stop("number of rows must be number of groups")
    len=dim(marked)
    len=len[1]
    if(len>nocc)
      stop("number of marked must be no greater than number of occasions")
    len=dim(marked)
    if(length(len)>2)
    {
      if(len[2]!=(nstrata+nevents))
        stop("number of columns must be number of strata + number of events")
    }
  }
  return(NULL)
}
  