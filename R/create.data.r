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
#' @param ... additional arguments that can be passed to process.data
#' @return list of processed data set run through process.data
#' @author Jeff Laake
#' @export
create.data=function(model,nocc,ngroups,nstates=1,time.intervals=NULL,...)
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
    return(create.nonstate.data(model,nocc,ngroups,robust=model$robust,time.intervals=NULL,...))
  else
    return(create.ms.data(model,nocc,ngroups,nstates,robust=model$robust,time.intervals=NULL,...))
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
#' @param ... additional arguments that can be passed to process.data
#' @return processed data set list run through process.data
#' @author Jeff Laake
#' @export
  create.nonstate.data=function(model,nocc,ngroups,robust,time.intervals,...)
  {
    ch=paste(rep("1",nocc),collapse="")
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
