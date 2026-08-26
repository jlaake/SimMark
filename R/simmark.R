#' Interface to MARK for simulating data from capture-recapture models
#' 
#' Calls mark.exe to simulate data based on a specified capture-recapture model and
#' specified parameter formulas and other parameters.
#' 
#' The function simmark is a shell that calls 5 other functions in the following
#' order as needed: 1) \code{\link{sim.process.data}}, 2)
#' make.design.data in RMark, 3) \code{\link{make.simmark.model}}, 4)
#' \code{\link{run.simmark.model}}, and 5) \code{\link{summarize.simmark}}. 
#' Functions 1,3,4 are modifications of the equivalent functions in RMark to 
#' handle the additional requirements for Proc Simulate in MARK to specify
#' 1. number of simulation replicates (numsims), 2. optional seed for random
#' number generation, 3. filename for binary file containing simulation results,
#' 4. simdata filename if simulated data are to be output without estimation, 
#' 5. releases which are numbers of animals per occasion used in simulation, 
#' 6. parameter values specified as beta for link function values or real for real parameter values.
#' 
#' Because the functions in the package are derivatives of RMark functions they follow the same pattern of model 
#' development which requires a data set which is used to generate the design data for the specific 
#' model and the design data are used with the parameter formulas to generate the design matrix. All of
#' the code to create the PIMs and design matrices and options in RMark are also included in this package
#' because it uses the same code moved over to this package. Thus it is necessary to start off with data that
#' fits the structure of the model (eg. correct values of capture history, number of occasions, group structure etc).
#' For CJS type models a capture history with all 1's and specified number of occasions is sufficient and if there
#' are groups, then a factor variable group is included and a capture history is given for each group. For multistate
#' models all of the states (specified with capital letters) must be present in the data. For models with a two character 
#' capture history creating the data is slightly more complicated but all of this is handled in the function \code{\link{create.data}}
#' and some additional functions.  
#' 
#' There are 2 different approaches you can take in using simmark: 
#' 
#' 1) specify the model, data (from create.data), formulas for the design matrix and simulation parameters to generate 
#' replicate data sets (numsims) which are then analyzed with the same model and design matrix in MARK and the parameter 
#' estimates (beta and real) are output to the binary file named with argument simfile and summarized in the element simresults 
#' in the model list returned from the function. 
#' 
#' 2) specify the model, data (from create.data), formulas for the design matrix and simulation parameters to generate 
#' replicate data sets (numsims) which are then output to the file specifed by the argument simdata. These simulation
#' data sets can then be in RMark with the same or different model and formulas/design matrix and any of the results 
#' extracted from the estimation run can be summarized to compute any value of interest (eg confidence interval coverage,etc)
#' analyzed. An example of this approach is given in \code{\link{SimtoRMark}} using the function \code{\link{readSimData}}.
#' 
#' Numerous examples are provided in \code{\link{SimCJS}}, \code{\link{SimMS}},\code{\link{SimRobust}},\code{\link{SimOccupancy}},
#' \code{\link{SimOpenAbundance}}, and \code{\link{SimClosedAbundance}} with more to follow.
#' 
#' Note that some of these arguments to the function have been carried over from RMark mark function
#' out of ease and they may not necessarily be useful.
#' 
#' @param data Either the raw data which is a dataframe with at least one
#' column named ch (a character field containing the capture history) or a
#' processed dataframe
#' @param ddl Design data list which contains a list element for each parameter
#' type; if NULL it is created
#' @param begin.time Time of first capture(release) occasion
#' @param model.name Optional name for the model
#' @param model Type of c-r model (eg CJS, Burnham, Barker)
#' @param title Optional title for the MARK analysis output
#' @param model.parameters List of model parameter specifications
#' @param initial Optional vector of named or unnamed initial values for beta
#' parameters or previously run model object
#' @param design.parameters Specification of any grouping variables for design
#' data for each parameter
#' @param right if TRUE, any intervals created in design.parameters are closed
#' on the right and open on left and vice-versa if FALSE
#' @param groups Vector of names factor variables for creating groups
#' @param age.var Optional index in groups vector of a variable that represents
#' age
#' @param initial.ages Optional vector of initial ages for each age level
#' @param age.unit Increment of age for each increment of time
#' @param time.intervals Intervals of time between the capture occasions
#' @param numsims number of simulations data sets to create
#' @param seed the seed for random number generation; default is 0 for random seed start
#' @param releases matrix of number of releases with rows being nocc-1 and columns are groups or array with dim occasion,strata,groups
#' @param beta vector of parameters for simulation on link scale
#' @param real vector of parameters for simulation on real scale
#' @param param.link if not NULL, it is used as link for specified simulation parameters
#' @param nocc number of occasions for Nest model; either time.intervals or nocc must be specified for this model
#' @param simfile name of simulation results binary file
#' @param simdata if not NULL it should specify a data file where the numsims simulated data sets are stored; in this case simfile is not used because estimations and summary do not occur
#' @param invisible If TRUE, window for running MARK is hidden
#' @param mixtures number of mixtures for heterogeneity model or number of secondary samples for MultScaleOcc model
#' @param filename base filename for files created by MARK.EXE. Files are named
#' filename.*.
#' @param prefix base filename prefix for files created by MARK.EXE; for
#' example if prefix="SpeciesZ" files are named "SpeciesZnnn.*"
#' @param silent if TRUE, errors that are encountered are suppressed
#' @param options character string of options for Proc Estimate statement in
#' MARK .inp file
#' @param delete if TRUE the output files are deleted after the results are
#' extracted
#' @param chat value of chat used for profile intervals
#' @param mlogit0 if TRUE, any real parameter that is fixed to 0 and has an mlogit link will 
#' have its link changed to logit so it can be simplified
#' @param threads number of cpus to use with mark.exe if positive or number of cpus to remain idle if negative
#' @param hessian if TRUE specifies to MARK to use hessian rather than second partial matrix
#' @param accumulate if TRUE accumulate like data values into frequencies
#' @param allgroups Logical variable; if TRUE, all groups are created from
#' factors defined in \code{groups} even if there are no observations in the
#' group
#' @param strata.labels vector of single character values used in capture
#' history(ch) for ORDMS, CRDMS, RDMSOccRepro models; it can contain one more value beyond what is
#' in ch for an unobservable state except for RDMSOccRepro which is used to specify strata ordering (eg 0 not-occupied, 1 occupied no repro, 2 occupied with repro.
#' @param counts named list of numeric vectors (one group) or matrices (>1
#' group) containing counts for mark-resight models
#' @param wrap if TRUE, data lines are wrapped to be length 80; if length of a row is not a 
#'   problem set to FALSE and it will run faster	 
#' @param nodes number of integration nodes for individual random effects (min 15, max 505, default 101)
#' @param events vector of character events for Hidden Markov models
#' @param useddl if TRUE and no rows of ddl are missing (deleted) then it will use ddl in place of full.ddl that is created internally.
#' @param check.model if TRUE, code does an internal consistency check between PIMs and design data when making model.
#'#' @param default.fixed if TRUE, real parameters for which the design data have
#' been deleted are fixed to default values
#' @param icvalues numeric vector of individual covariate values for computation of real values
#' @param profile.int if TRUE will compute profile intervals for each real
#' parameter; or you can specify a vector of real parameter indices
#' @param input.links specifies set of link functions for parameters with non-simplified structure
#' @param default.fixed if TRUE, real parameters for which the design data have
#' been deleted are fixed to default values
#' @param parm.specific if TRUE, forces a link to be specified for each parameter
#' @return model: a MARK object containing structure used to simulate the data (parmvals) and some output. 
#' If simdata argument is specified, no further results are provided but simulated data is in 
#' the specified file (default data.inp). If mark does the estimation then model contains the simulation results
#' for the beta and real values.
#' @author Jeff Laake
#' @import RMark
#' @export
#' @seealso \code{\link{make.simmark.model}}, \code{\link{run.simmark.model}},
#' \code{\link{sim.process.data}}
#' @keywords models
#' 
simmark <-
function(data,ddl=NULL,begin.time=1,model.name=NULL,model="CJS",title="",model.parameters=list(),initial=NULL,
design.parameters=list(), right=TRUE, groups = NULL, age.var = NULL, initial.ages = 0, age.unit = 1, time.intervals = NULL,
numsims=1,seed=0, releases=NULL,beta=NULL,real=NULL,param.link=NULL,nocc=NULL,simfile="simresults.bin",simdata=NULL,
invisible=TRUE,mixtures=1,filename=NULL,prefix="marksim",default.fixed=TRUE,silent=FALSE,options=NULL,
delete=FALSE,profile.int=FALSE,chat=NULL,input.links=NULL,parm.specific=FALSE,mlogit0=TRUE,threads=-1,hessian=FALSE,accumulate=TRUE,
allgroups=FALSE,strata.labels=NULL,counts=NULL,icvalues=NULL,wrap=TRUE,events=NULL,nodes=101,useddl=FALSE,check.model=FALSE)
{
# 
#  test to see if model is supported for simulation; will mstop if not supported
   dummy=sim.setup.model(model,1,1)
#
#  If the data haven't been processed (data$data is NULL) do it now with specified or default arguments
# 
simplify=TRUE
if(is.null(data$data))
{
   if(!is.null(ddl))
   {
      message("Warning: specification of ddl ignored, as data have not been processed\n")
      ddl=NULL
   }
   data.proc=sim.process.data(data,begin.time=begin.time, model=model,mixtures=mixtures, 
                          groups = groups, age.var = age.var, initial.ages = initial.ages, 
                          age.unit = age.unit, time.intervals = time.intervals,nocc=nocc,
				                  allgroups=allgroups, strata.labels=strata.labels,counts=counts,events=events)
}   
else
   data.proc=data

# create models.txt and params.txt like in RMark to limit models and specify control values

#
# If the design data have not been constructed, do so now
#
if(is.null(ddl)) ddl=make.design.data(data.proc,design.parameters,right=right)
#
#  check to make sure all entered as lists
#
tryCatch(length(model.parameters), error = function(e) message("Make sure you have a tilde at the beginning of each formula\n"))
if(length(model.parameters)!=0)
	for(i in 1:length(model.parameters))
	{
		if(!is.list(model.parameters[[i]]))
			stop("\nEach parameter distribution must be specified as a list\n")
		if(is.language(model.parameters[[i]][[1]])&(is.null(names(model.parameters[[i]])) || names(model.parameters[[i]])[1]==""))
			message("Make sure you have an = between formula and tilde for formula\n")
	}
#
# Make the model with specified or default parameters
#
if(is.list(model.parameters))
{
  if(!is.null(simdata))
    model<-try(make.simmark.model(data.proc,title=title,parameters=model.parameters,
         ddl=ddl,initial=initial,numsims=numsims,seed=seed,releases=releases,beta=beta,real=real,param.link=param.link,
         simfile=NULL, simdata=simdata, call=match.call(),default.fixed=default.fixed,
         model.name=model.name,options=options,profile.int=profile.int,chat=chat,
  			 input.links=input.links,parm.specific=parm.specific,mlogit0=mlogit0,hessian=hessian,
			   accumulate=accumulate,icvalues=icvalues,wrap=wrap,nodes=nodes,useddl=useddl,
  			 check.model=check.model))
  else
    model<-try(make.simmark.model(data.proc,title=title,parameters=model.parameters,
         ddl=ddl,initial=initial,numsims=numsims,seed=seed,releases=releases,beta=beta,real=real,param.link=param.link,
         simfile=simfile, simdata=NULL, call=match.call(),default.fixed=default.fixed,
         model.name=model.name,options=options,profile.int=profile.int,chat=chat,
         input.links=input.links,parm.specific=parm.specific,mlogit0=mlogit0,hessian=hessian,
         accumulate=accumulate,icvalues=icvalues,wrap=wrap,nodes=nodes,useddl=useddl,
         check.model=check.model))
  
  if(inherits(model,"try-error"))
	{
	   stop("Misspecification of model or internal error in code")
	}
	else
     model$model.parameters=model.parameters
}
else
    stop("Model parameters must be specified as a list")
#
# Run model
#
if(silent)
  runmodel<-suppressMessages(run.simmark.model(model,invisible=invisible,threads=threads,ignore.stderr=silent))
else
 runmodel<-run.simmark.model(model,invisible=invisible,threads=threads,ignore.stderr=silent)
if(is.null(runmodel))
{
 if(!silent)message("\n\n********Following model failed to run :",model$model.name,"**********\n\n")
 return(invisible())
}
#
# read in outfile and search for R Variable Values:
# read in next 3 lines until ; and execute them and then call function to summarize
# and store summary in model results
#
# ncovs,nlogit and nderived are set with eval below they are assigned a value NULL to
# prevent check note
ncovs=NULL
nlogit=NULL
nderived=NULL
outfile=paste(runmodel$output,".out",sep="")
out=readLines(outfile)  
x=grep("R Variable Values:",out,fixed=TRUE)
if(length(x)!=0)
{
  eval(parse(text=out[x+1]))
  eval(parse(text=out[x+2]))
  eval(parse(text=out[x+3]))
  runmodel$simresults=summarize.simmark(ncovs,nlogit,nderived,simfile)
} else
  runmodel$simresults=NULL
return(runmodel)
}
