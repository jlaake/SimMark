
#' Computes a single median chat and associated statistics
#' 
#' Computes estimated median chat value and related results and can plot fitted logistic regression
#' 
#' 
#' @param model MARK model created by \code{\link{make.simmark.model}}
#' @param chat.range lower and upper values for range of chat values to simulate
#' @param npts number of true chat values to simulate within the chat.range 
#' @param nsample number of simulations at a specific true chat value
#' @param plot if TRUE plots observed and predicted values and conf interval for logistic regression
#' @param show if TRUE, show progress with simulations
#' @return list of results for median chat calculation including median chat estimate,lrmod the fitted logistic 
#' regression model,obs_prop which is the proportion of samples less than observed chat, lrdata which is the
#' data file for the logistic regression, vcv the variance-covariance matrix of the intercept and slope,
#' var_chat the variance of the estimated median chat, and ci the 95% inverse conf interval.
#' @author Jeff Laake
#' @export
#' @examples{ 
#' data(dipper)
#'dp=process.data(dipper,groups="sex")
#'model=mark(dp,model.parameters=list(p=list(formula=~time)),delete=TRUE)
#'chat_results=compute_median_chat(model,chat.range=c(1,2.3),npts=10,nsample=10)
#'releases=get_releases(model$data)
#'test=simmark(process.data(dipper),releases=releases*1000,numsims=1,chat=2,invisible=TRUE,silent=TRUE,beta=list(Phi=0.242,p=2.226),simdata="data.inp")
#'simdata=readSimData("data.inp")
#'model=mark(simdata[[1]])
#'compute_median_chat(model,chat.range=c(1,3),npts=10,nsample=10)
#' }
compute_median_chat=function(model,chat.range=c(1,3),npts=5,nsample=10,plot=FALSE,show=TRUE)
{
  pnames=sapply(strsplit(rownames(model$results$beta),":"),function(x) x[1])
  beta.values=split(model$results$beta$estimate,pnames)
  releases=get_releases(model$data)
  if(any(releases=="STOP"))stop("Cannot construct median chat with this model")
  observed.chat=model$results$deviance/model$results$deviance.df
  lrdata=data.frame(Truth=NULL,less=NULL)
  if(show)cat(paste("\nSimulation progress\n",paste(rep("_",npts),collapse=""),"\n",sep=""))
  for(i in 1:npts)
  {
    chat=chat.range[1]+(i-1)*(chat.range[2]-chat.range[1])/npts
    test=simmark(model$data,releases=releases,beta=beta.values,model.parameters = model$model.parameters,numsims=nsample,chat=chat,invisible=TRUE,silent=TRUE,delete=TRUE)
    simchat=test$simresults[,"Deviance"]/test$simresults[,"df.dev"]
    y=sum(as.numeric(simchat<observed.chat))
    lrdata=rbind(lrdata,data.frame(y=y,n=nsample,Truth=chat))
    if(show)cat("*")
  }
  if(show)cat("\n")
  lrmod=glm(cbind(y,n-y)~Truth,family="binomial",data=lrdata)
  lrcoef=coef(lrmod)
  obs_prop=lrdata$y/rep(nsample,length(lrdata$y))
  vcv=summary(lrmod)$cov.unscaled
  deriv=c(-1/lrcoef[2],-lrcoef[1]/lrcoef[2]^2)
  var_chat=t(deriv)%*%vcv%*%deriv
  inverse_estimate <- invest(
    object = lrmod, data=lrdata,
    y0 = 0.5,lower=0,upper=chat.range[2])
  if(plot)
  {
    plotFit(lrmod, data=lrdata, pch = 19, cex = 1.2, lwd = 2,
            xlab = "True chat", ylab="Prob(True chat<observed chat)",
            interval = "confidence", shade = TRUE,
            col.conf = "lightskyblue")
    lines(inverse_estimate[2:3],c(0.5,0.5))
    points(inverse_estimate[1],c(0.5),pch="x",cex=1.5)
  }
  return(list(chat=inverse_estimate[[1]],lrmod=lrmod,obs_prop=obs_prop,lrdata=lrdata,vcv=vcv,cv_chat=as.vector(sqrt(var_chat)/inverse_estimate[[1]]),ci=inverse_estimate[2:3]))
}
#' Driver routine to compute a median chat and associated statistics
#' 
#' Uses compute_median_chat to develop an initial median chat estimate and then derive a 
#' a more precise estimate with a target cv based on results of initial estimate. It returns
#' associated statistics and can plot results.
#' 
#' @param model MARK model created by \code{\link{make.simmark.model}}
#' @param lower lower value for range of chat values to simulate (must be 1 or greater)
#' @param upper upper value for range of chat values
#' @param sample if TRUE (default) it computes an initial estimate and then a final estimate with a refined range and target cv
#' @param targetcv desired cv for estimated median chat value; the final result will be approximate
#' @param nsample number of simulations at a specific true chat value for initial (sample=TRUE) or final (sample==FALSE)
#' @param npts number of true chat values to simulate within the chat.range for initial (sample=TRUE) or final (sample==FALSE)
#' @param plot if TRUE plots observed and predicted values and conf interval for logistic regression
#' @param show if TRUE shows simulation progress
#' @return list of results for median chat calculation including median chat estimate,lrmod the fitted logistic 
#' regression model,obs_prop which is the proportion of samples less than observed chat, lrdata which is the
#' data file for the logistic regression, vcv the variance-covariance matrix of the intercept and slope,
#' var_chat the variance of the estimated median chat, and ci the 95% inverse conf interval.
#' @author Jeff Laake
#' @export
#' @examples{ 
#' data(dipper)
#'dp=process.data(dipper,groups="sex")
#'model=mark(dp,model.parameters=list(p=list(formula=~time)),delete=TRUE,invisible=TRUE,silent=TRUE)
#'chat_results=median_chat(model,lower=1,upper=2,sample=TRUE,targetcv=0.2)
#' }
median_chat=function(model,lower=1,upper=3,sample=TRUE,targetcv=0.2,npts=8,nsample=10,plot=TRUE,show=TRUE)
{
  if(lower<1)
  {
    warning("\nlower argument value must be 1 or greater; setting to 1\n")
    lower=1
  }
  if(upper < lower)
  {
    warning("\nlower argument value must be greater than upper; setting to lower*1.5")
    upper=lower*1.5
  }
  if(!sample)
     return(compute_median_chat(model,chat.range=c(lower,upper),npts=npts,nsample=nsample,plot=plot,show=show))
  else
  {
    if(show)cat("\nComputing initial estimate\n")
    initial=compute_median_chat(model,chat.range=c(lower,upper),npts=npts,nsample=nsample,plot=FALSE,show=show)
    b=(initial$cv_chat*initial$chat)^2*(npts*nsample)
    nstar=b/(targetcv*initial$chat)^2
    nsample=ceiling(nstar/npts)
    inverse_estimate_lower <- invest(
    object = initial$lrmod, data=initial$lrdata,
    y0 = 0.8,lower=0,upper=upper)
    if(inverse_estimate_lower$estimate<1)inverse_estimate_lower$estimate=1
    inverse_estimate_upper <- invest(
    object = initial$lrmod, data=initial$lrdata,
    y0 = 0.2,lower=0,upper=upper)
    lower=inverse_estimate_lower$estimate
    upper=inverse_estimate_upper$estimate
    if(show)cat("\nComputing final estimate with specified precision\n")
    return(compute_median_chat(model,chat.range=c(lower,upper),npts=npts,nsample=nsample,plot=plot,show=show))
  }
}  



    





