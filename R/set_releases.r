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
#' Get releases from a processed data list
#' 
#' Extracts values of releases from a processed data list from a model that has been fitted
#' 
#' @param model fitted mark model 
#' @param abund if TRUE, use abundance estimates
#' @param occupancy if TRUE, uses number of capture histories per group
#' @param multistate if TRUE, use multistate releases
#' @param LD if TRUE has a live-dead format
#' @return matrix of release numbers per group
#' @author Jeff Laake
#' @export

get_releases=function(model,abund=FALSE,occupancy=FALSE,multistate=FALSE,LD=FALSE)
{
  if(class(model)[1]!="mark") stop("\nmodel must be a fitted mark model\n")
  if(!abund&!occupancy&!multistate)
  {
    chsummary=summary_ch(model$data)
    if(any(chsummary=="STOP")) return("STOP")
    x=sapply(chsummary,function(x)x$Released)
    return(x)
  } else
  {
    if(occupancy)
    {
      numch=sapply(model$data$freq,sum)
      if(length(numch)==1) 
        return(numch)
      else
        return(matrix(numch,nrow=1))
    } else
      if(abund)
      {
         if(model$model=="POPAN")
         {
           Nhat=get.real(model,"N")
           if(length(Nhat)>1)
             return(matrix(Nhat,nrow=1))
           else
             return(Nhat)
         } else
         {
           Nhat=round(model$results$derived[["N Population Size"]]$estimate)
           if(length(Nhat)>1)
             return(matrix(Nhat,nrow=1))
           else
             return(Nhat)
         }
      } else
      {
        return(get.MS.releases(model$data,LD=LD))
      }
  }
}
#' Get multistate releases from a processed data list
#' 
#' Extracts values of releases from a processed data list from a multistate model that has been fitted
#' 
#' @param dp processed data list from fitted mark model 
#' @return array of release numbers per occasion, state, group
#' @author Jeff Laake
#' @export


get.MS.releases=function(dp,LD)
{
  release.values=array(0,dim=c(nocc-1,length(dp$strata.labels),ncol(dp$freq)))
  x=dp$data
  if (LD) {
    chmat = t(sapply(strsplit(x$ch, split = ""), function(x) rbind(x)))
    chmat = chmat[, seq(1, ncol(chmat), 2)]
    x$ch = apply(chmat, 1, paste, collapse = "")
  }
  pc=process.ch(x$ch)
  nocc=nchar(x$ch[1])
  nch=length(x$ch)
  cseq=cbind(1:length(x$ch),pc$first)
  pcmat=matrix((unlist(strsplit(x$ch,""))),byrow=TRUE,ncol=nocc,nrow=nch)
  whichrows=split(1:nch,list(pcmat[cseq],pc$first))
  if(ncol(dp$freq)>1)
  {
    sums=sapply(whichrows,function(i) colSums(dp$freq[i,]))
    strata.occ=strsplit(colnames(sums),".",fixed=TRUE)
  }
  else 
  {
    sums=sapply(whichrows,function(i) sum(dp$freq[i]))
    strata.occ=strsplit(names(sums),".",fixed=TRUE)
  }
  for(i in 1:length(strata.occ))
  {
    strata=which(dp$strata.labels==strata.occ[[i]][1])
    occ=as.numeric(strata.occ[[i]][2])
    if(occ<nocc)
    {
      if(ncol(dp$freq)>1)
          release.values[occ,strata,]=sums[,i]
      else
        release.values[occ,strata,]=sums[i]
    }
  }
  return(release.values)
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
  