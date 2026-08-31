#' Create labels for derived data
#'
#' For a specific capture-recapture model, create the dlabels which are
#' used to add labels to the derived parameters.
#' 
#' @param ddf dataframe of derived parameters and their attributes
#' @param nocc number of occasions (number of sessions if robust design)
#' @param secnocc number of occasions if robust design
#' @param ngroups number of groups
#' @param nstates number of states in multistate model
#' @return vector of dlabels to be added to simmark input file for mark to use to label derived parameter values
#' @author Jeff Laake
#' @export
create_dlabels=function(ddf,nocc,secnocc,ngroups,nstates)
{
  dlabels=NULL
  labelnum=1
  for(i in 1:nrow(ddf))
  {
    setgroups=ifelse(!is.na(ddf$Group[i]) & ddf$Group[i],TRUE,FALSE)
    setstrata=ifelse(!is.na(ddf$Strata[i]) & ddf$Strata[i],TRUE,FALSE) 
    nstr=ifelse(setstrata,nstates,1)
    if(!is.na(ddf$Occasion[i]) & ddf$Occasion[i])
    {
      setoccasion=TRUE
      setsession=FALSE
      numocc=eval(parse(text=ddf$OccNum[i]))
    } else
    {
      numocc=1
      setoccasion=FALSE
      setsession=FALSE
      if(!is.na(ddf$Session[i]) & ddf$Session[i]) 
      {
        setsession=TRUE
        numsess=eval(parse(text=ddf$SessNum[i]))
      } else
        numsess=1
    }
    if(setsession)
    {
      for(ng in 1:ngroups)
        for(nst in 1:nstr)
          for(ns in 1:numsess)
          {
            labelstring=paste("dlabel(",labelnum,")=",sep="")
            labelstring=paste(labelstring,strsplit(ddf$dpar_label[i]," ")[[1]][1],sep="")
            if(setgroups)
              labelstring=paste(labelstring," Grp ",ng,sep="")
            if(setstrata)
              labelstring=paste(labelstring," Str ",nst,sep="")
            if(setsession)
              labelstring=paste(labelstring," Ses ",ns,sep="")
            labelnum=labelnum+1
            labelstring=paste(labelstring,";",sep="")
            dlabels=c(dlabels,labelstring)
          }
   }else
   {
      for(ng in 1:ngroups)
        for(nst in 1:nstr)
          for(noc in 1:numocc)
          {
            labelstring=paste("dlabel(",labelnum,")=",sep="")
            labelstring=paste(labelstring,strsplit(ddf$dpar_label[i]," ")[[1]][1],sep="")
            if(setgroups)
              labelstring=paste(labelstring," Grp ",ng,sep="")
            if(setstrata)
              labelstring=paste(labelstring," Str ",nst,sep="")
            if(setoccasion)
              labelstring=paste(labelstring," Occ ",noc,sep="")
            labelnum=labelnum+1
            labelstring=paste(labelstring,";",sep="")
            dlabels=c(dlabels,labelstring)
          }
   }
  }
  return(dlabels)
}


