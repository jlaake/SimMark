#' Dummy data creation for simulation
#'
#' For a specific capture-recapture model, create some dummy data for code in SimMark to
#' develop design data and input file for simulation with MARK.
#' 
#' @param model supported c-r model name (eg "CJS","multistrata")
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @param nstates number of states in multistate model
#' @param time.intervals needs to be specified for robust models
#' @param live defaults to FALSE, if live include encounters
#' @param ... additional arguments that can be passed to process.data
#' @return list of processed data set run through process.data
#' @author Jeff Laake
#' @export
create.data=function(model,nocc,ngroups,nstates=1,time.intervals=NULL,live=FALSE,...)
{
  model_def=sim.setup.model(model,nocc)
  if(model_def$robust)
  {
    if(is.null(time.intervals))
      stop("Must specify time.intervals for robust design models")
    if(length(time.intervals)!=nocc-1)
       stop("Number of time intervals must be nocc-1")
  }
  if(nstates==1)
    return(create.nonstate.data(model,nocc,ngroups,robust=model$robust,time.intervals=time.intervals,divisor=model_def$divisor,live=live,...))
  else
    return(create.ms.data(model,nocc,ngroups,nstates,robust=model$robust,time.intervals=time.intervals,...))
}
#' Dummy data creation for models without states
#'
# Function to create a dummy data set with specified number of occasions and number of groups
# group variable is named group. Could be expanded if wanted to introduce age into the model.
#' 
#' @param model supported c-r model name (eg "CJS","multistrata")
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @param robust logical as to whether it is a robust design model
#' @param time.intervals needs to be specified for robust models
#' @param divisor if 2 then double the nocc to create the capture history
#' @param live defaults to FALSE, if live include encounters
#' @param ... additional arguments that can be passed to process.data
#' @return processed data set list run through process.data
#' @author Jeff Laake
#' @export
  create.nonstate.data=function(model,nocc,ngroups,robust,time.intervals,divisor,live=FALSE,...)
  {
    if(divisor==1)
    {
      ch=paste(rep("1",nocc),collapse="")
      simdata=data.frame(ch=rep(ch,ngroups),group=factor(1:ngroups))
      if(ngroups>1)
        dp=sim.process.data(simdata,model=model,groups="group",time.intervals=time.intervals,...)
      else
        dp=sim.process.data(simdata,model=model,time.intervals=time.intervals,...)
    } else
    {
      chlength=2*nocc
      mat=diag(1,nocc,nocc)
      xmat=matrix(0,nocc,2*nocc)
      xmat[,seq(1,2*nocc,2)]=mat
      for(i in 1:nocc)
      {
        if(i==1)
          chmat=xmat+cbind(rep(0,nrow(xmat)),xmat[,1:(2*nocc-1)])
        else
          chmat=rbind(chmat,xmat+cbind(matrix(0,nrow=nrow(xmat),ncol=2*(i-1)+1),xmat[,1:(2*nocc-(2*(i-1)+1))]))
      }
      ch=apply(chmat,1,paste,collapse="")
      if(live) ch=c(ch,paste(rep(c(1,0),nocc),collapse=""))
      simdata=data.frame(ch=rep(ch,ngroups),group=factor(1:ngroups))
    }      
      simdata=data.frame(ch=rep(ch,ngroups),group=factor(1:ngroups))
      if(ngroups>1)
        dp=sim.process.data(simdata,model=model,groups="group",time.intervals=time.intervals,...)
      else
        dp=sim.process.data(simdata,model=model,time.intervals=time.intervals,...)

    return(dp)
  }

#' Dummy data creation for models with states
#'
# Function to create a multistrata data set with specified number of occasions, number of groups and number of states
# group variable is named group. Could be expanded if wanted to introduce age into the model.
#' 
#' @param model supported c-r model name (eg "CJS","multistrata")
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @param nstates number of states in multistate model
#' @param robust logical as to whether it is a robust design model
#' @param time.intervals needs to be specified for robust models
#' @param ... additional arguments that can be passed to process.data
#' @return processed data set list from process.data
#' @author Jeff Laake
#' @export
  create.ms.data=function(model,nocc,ngroups,nstates,robust,time.intervals,...)
{
  strata=LETTERS[1:nstates]
  simdata=data.frame(ch=apply(matrix(rep(suppressWarnings(t(matrix(strata,ncol=nocc))),ngroups),ncol=nocc),1,paste,collapse=""),group=1:ngroups)
  if(ngroups>1)
  {
    simdata$group=factor(simdata$group)
    dp=sim.process.data(simdata,model=model,groups="group",time.intervals=time.intervals,...)
  }
  else
    dp=sim.process.data(simdata,model=model,time.intervals=time.intervals,...)
  return(dp)
}
