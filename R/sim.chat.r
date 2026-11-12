#' Simulates data for a specific model with a truechat to test median chat procedure
#' 
#' Replicates simulated data and fitted model with a known chat value and runs median chat procedure on each replicate and returns values for each replicate
#' 
#' @param dp processed initial data to start simulations
#' @param dm.formulas list of formulas for the parameters in the model
#' @param beta.values beta values to be used for generating the data and fitting the model
#' @param release.values numbers released for use in simulation
#' @param testchat value of chat for simulating data
#' @param nreps number of simulation replicates
#' @param sample if TRUE, fits an initial sample and then adjusts sample size to get better cv
#' @param targetcv desired cv for estimated median chat value; the final result will be approximate
#' @param nsample number of simulations at a specific true chat value for initial (sample=TRUE) or final (sample==FALSE)
#' @param maxnsample maximum number of simulations at each point to achieve targetcv; if estimate is too large it uses this value
#' @param npts number of true chat values to simulate within the chat.range for initial (sample=TRUE) or final (sample==FALSE)
#' @param npts_mult multiplier for npts if targetcv is not achieved.
#' @param plot if TRUE shows plot of fitted median chat logistic regression data
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
#' sim.chat(simdata,dm.formulas=dm.formulas,beta.values=beta.values,release.values=release.values,nreps=2)
#' }
sim.chat=function(dp,ddl=NULL,dm.formulas=NULL,beta.values=NULL,release.values=NULL,testchat=2,nreps=100,sample=TRUE,targetcv=0.1,nsample=10,maxnsample=50,npts=10,npts_mult=2,plot=FALSE)
{
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
    simdata=readSimData("data.inp",group.df=data.frame(grp=1:ngroups))
    # process data 
    dp=process.data(simdata[[1]],groups="grp")
    # fit model with simulated data
    mod=mark(dp,ddl=ddl,model.parameters=dm.formulas,delete=TRUE,invisible=TRUE,silent=TRUE)
    # compute median chat
    chat_results=median_chat(mod,lower=1,upper=testchat+1,sample=sample,targetcv=targetcv,nsample=nsample,maxnsample=maxnsample,npts=npts,npts_mult=npts_mult,plot=plot)
    # store results 
    results[i,]=list(FletcherChat=mod$results$FletcherChat,chat=chat_results$chat,
                     lowerci=chat_results$lower,upperci=chat_results$upper,cv=chat_results$cv_chat,
                     covered=as.numeric(chat_results$lower<testchat & chat_results$upper>testchat))
  }
  return(results)
}
