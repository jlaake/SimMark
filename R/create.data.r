#' Dummy data creation for simulation
#'
#' For a specific capture-recapture model, create some dummy data for code in SimMark to
#' develop design data and input file for simulation with MARK.
#' 
#' @param model supported c-r model name (eg "CJS","multistrata")
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @param nstates number of states in multistate model
#' @return list of processed data set run through process.data
#' @author Jeff Laake
#' @export
create.data=function(model,nocc,ngroups,nstates)
{
  if(model=="CJS")
    return(create.cjs.data(nocc,ngroups))
  else
    if(model=="Multistrata") 
      return(create.ms.data(nocc,ngroups,nstates))
}
#' Dummy data creation for CJS model
#'
# Function to create a CJS data set with specified number of occasions and number of groups
# group variable is named group. Could be expanded if wanted to introduce age into the model.
#' 
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @return list of processed data set run through process.data
#' @author Jeff Laake
#' @export
  create.cjs.data=function(nocc,ngroups)
  {
    ch=paste(rep("1",nocc),collapse="")
    simdata=data.frame(ch=rep(ch,ngroups),group=factor(1:ngroups))
    if(ngroups>1)
      dp=sim.process.data(simdata,model="CJS",groups="group")
    else
      dp=sim.process.data(simdata,model="CJS")
    return(dp)
  }

#' Dummy data creation for multistate model
#'
# Function to create a multistrata data set with specified number of occasions, number of groups and number of states
# group variable is named group. Could be expanded if wanted to introduce age into the model.
#' 
#' @param nocc number of occasions
#' @param ngroups number of groups
#' @param nstates number of states in multistate model
#' @return list of processed data set run through process.data
#' @author Jeff Laake
#' @export
  create.ms.data=function(nocc,ngroups,nstates)
{
  strata=LETTERS[1:nstates]
  simdata=data.frame(ch=apply(matrix(rep(suppressWarnings(t(matrix(strata,ncol=nocc))),ngroups),ncol=nocc),1,paste,collapse=""),group=1:ngroups)
  if(ngroups>1)
  {
    simdata$group=factor(simdata$group)
    dp=sim.process.data(simdata,model="Multistrata",groups="group")
  }
  else
    dp=sim.process.data(simdata,model="Multistrata")
  return(dp)
}
