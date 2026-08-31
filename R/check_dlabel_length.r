check_dlabel_length=function(ngroups=2,nstrata=3)
{
  #read in and loop through models
  fdir=system.file(package="SimMark")	
  fdir=file.path(fdir,"models.txt")	
  model_definitions=read.delim(fdir,header=TRUE,
                               colClasses=c("numeric","character","character",rep("logical",4),rep("numeric",3),rep("logical",3),"character"))
  for(i in 1:nrow(model_definitions))
  {
     if(model_definitions$simulate[i])
     {
      cat("\nchecking ",model_definitions$model[i])
      model=model_definitions$model[i]
      nderived=model_definitions$nderived[i]
      if(!model_definitions$robust[i])
      {
        nocc=5
        numsession=0
      } else
      {
        numsession=4
        nocc=40
        secnocc=nocc
      }
      model_list=sim.setup.model(model,nocc)
      if(model_list$nDerived!="0")
      {
        compute_derived_length=function(nderived,noccas,ngrps,nprimy,nstrta)
          return(eval(parse(text=nderived)))
        cdl=compute_derived_length(nderived=model_list$nDerived,noccas=nocc,ngrps=ngroups,nprimy=numsession,nstrta=nstrata)
        if(!model_list$robust)
          dlabels=create_dlabels(model,model_list$derived_labels,nocc,secnocc=40,ngroups,nstates=nstrata)      else
            dlabels=create_dlabels(model,model_list$derived_labels,nocc=numsession,secnocc=40,ngroups,nstates=nstrata)
        if(length(dlabels)!=cdl)
        {
            cat("\ncdl= ",cdl)
            cat("\ndl= ",length(dlabels))
            cat("\ndlabels= ",dlabels)
            stop("Invalid dlabel structure for model",model)
        } else
          cat(" ok")
      }
    }
  }
}
