#' Simulates data for a specific model with a truechat to test median chat procedure
#' 
#' Replicates simulated data and fitted model with a known chat value and runs median chat procedure on each replicate and returns values for each replicate
#' 
#' @param dp processed initial data to start simulations
#' @param ddl design data list previously constructed for processed data
#' @param dm.formulas list of formulas for the parameters in the model
#' @param beta.values beta values to be used for generating the data and fitting the model
#' @param release.values numbers released for use in simulation
#' @param testchat value of chat for simulating data
#' @param lower lower value for range of chat values to simulate (must be 1 or greater)
#' @param upper upper value for range of chat values
#' @param nreps number of simulation replicates
#' @param sample if TRUE, fits an initial sample and then adjusts sample size to get better cv
#' @param targetcv desired cv for estimated median chat value; the final result will be approximate
#' @param nsample number of simulations at a specific true chat value for initial (sample=TRUE) or final (sample==FALSE)
#' @param npts number of true chat values to simulate within the chat.range for initial (sample=TRUE) or final (sample==FALSE)
#' @param adjustment if sample=TRUE and targetcv not achieved, further sample range of initial$chat*(1-adjustment) to initial$chat*(1+adjustment)
#' @param plot if TRUE shows plot of fitted median chat logistic regression data
#' @param drop percentage of extreme values to drop on ends of data
#' @return dataframe of results for chat calculations including Fletcherchat (if available for model), 
#' chat which is the median chat value, lowerci and upperci which are the 95%  confidence interval for median chat,
#' cv which is the coefficient of variation for median chat and covered which is 1 if testchat is in the confidence interval
#' and 0 otherwise.
#' @author Jeff Laake
#' @export
#' @examples{ 
#' # create dummy data to initialize simulation; extra parameters are passed to create.data
#' dm.formulas=list(Phi=list(formula=~1),p=list(formula=~1))
#' beta.values=list(Phi=1,p=-.3)
#' testchat=2
#' nocc=8
#' ngroups=3
#' simdata=create.data("CJS",nocc=nocc,ngroups=ngroups)
#' release.values=array(c(rep(500,nocc-1),rep(200,nocc-1),rep(400,nocc-1)),dim=c(nocc-1,1,ngroups))
#' sim.chat(simdata,dm.formulas=dm.formulas,beta.values=beta.values,
#'                release.values=release.values,nreps=2)
#' }
sim.chat=function(dp,ddl=NULL,dm.formulas=NULL,beta.values=NULL,release.values=NULL,testchat=2,lower=1,upper=testchat+1,nreps=100,sample=TRUE,targetcv=0.1,nsample=10,npts=10,adjustment=0.1,plot=FALSE,drop=0.5)
{
  repfailures=NULL
  # create dataframe for results  
  results=data.frame(FletcherChat=0,chat=1,lowerci=0,upperci=0,cv=0,covered=0)
  #loop over each of the nreps simulations
  for(i in 1:nreps)
  {
    cat("\n simulation ",i)
    # run simulation with specified testchat value
    mod=simmark(dp,ddl=ddl,model.parameters=dm.formulas,releases=release.values,numsims=1,simdata="data.inp",chat=testchat,
                beta=beta.values,simfile="simresults.bin",invisible=TRUE,silent=TRUE,delete=TRUE)
    # read in simulated data
    if(ncol(dp$freq)>1)
       simdata=readSimData("data.inp",group.df=data.frame(grp=1:ncol(dp$freq)))
    else
       simdata=readSimData("data.inp")
    # process data 
    dpsim=process.data(simdata[[1]],model=dp$model,groups="grp")
    # fit model with simulated data
    mod=mark(dpsim,ddl=ddl,model.parameters=dm.formulas,invisible=TRUE,silent=TRUE)
    if(is.null(mod$results))
    {
      cat("\nConvergence or other failure fitting model for simulation ",i,"\n")
      repfailures=c(repfailures,i)
    } else
    {
      unlink(paste(mod$output,c(".out",".inp",".vcv",".res"),sep=""))
      # compute median chat
      chat_results=median_chat(mod,lower=lower,upper=upper,sample=sample,targetcv=targetcv,nsample=nsample,npts=npts,adjustment=adjustment,plot=plot,drop=drop)
      # store results 
      results[i,]=list(FletcherChat=mod$results$FletcherChat,chat=chat_results$chat,
                     lowerci=chat_results$lower,upperci=chat_results$upper,cv=chat_results$cv_chat,
                     covered=as.numeric(chat_results$lower<testchat & chat_results$upper>testchat))
    }
  }
  if(is.null(repfailures))
     return(results)
  else
  {
    cat("\n ",length(repfailures), " simulation reps failed\n")
    return(results[-repfailures,])
  }
 
}
