#' Create a MARK model for simulation setup
#' 
#' Creates a MARK model object that contains a MARK input file with PIMS and
#' design matrix specific to the data and model structure and formulas
#' specified for the model parameters. It is very similar to make.mark.model in RMark.
#' with additional statements that are added to specify simulation parameters.
#' 
#' See help file for make.mark.model in RMark for further explanation.
#' 
#' For descriptions of the standard calling arguments see \code{make.mark.model}. Additional 
#' arguments for simulation will be described here.
#' 
#' @param data Data list resulting from function \code{\link{sim.process.data}}
#' @param ddl Design data list from function make.design.data in RMark
#' @param parameters List of parameter formula specifications
#' @param title Title for the analysis (optional)
#' @param model.name Model name to override default name (optional)
#' @param initial Vector of named or unnamed initial values for beta parameters or previously run model (optional)
#' @param numsims number of simulations data sets to create
#' @param simfile name of simulation results file
#' @param seed the seed for random number generation
#' @param releases matrix of number of releases with rows being nocc-1 and columns are groups or array with dim occasion,strata,groups
#' @param beta vector of parameters for simulation on link scale
#' @param real vector of parameters for simulation on real scale
#' @param call Pass function call when this function is called from another
#' function (internal use)
#' @param default.fixed if TRUE, real parameters for which the design data have
#' been deleted are fixed to default values
#' @param options character string of options for Proc Estimate statement in
#' MARK .inp file
#' @param profile.int if TRUE, requests MARK to compute profile intervals
#' @param chat pre-specified value for chat used by MARK in calculations of
#' model output
#' @param simplify if FALSE, does not simplify PIM structure
#' @param input.links specifies set of link functions for parameters with non-simplified structure
#' @param parm.specific if TRUE, forces a link to be specified for each parameter
#' @param mlogit0 if TRUE, any real parameter that is fixed to 0 and has an mlogit link will 
#' have its link changed to logit so it can be simplified
#' @param hessian if TRUE specifies to MARK to use hessian rather than second partial matrix
#' @param accumulate if TRUE accumulate like data values into frequencies
#' @param icvalues numeric vector of individual covariate values for computation of real values
#' @param wrap if TRUE, data lines are wrapped to be length 80; if length of a row is not a 
#'   problem set to FALSE and it will run faster
#' @param nodes number of integration nodes for individual random effects (min 15, max 505, default 101)
#' @param useddl If TRUE and there are no missing rows or parameters (deleted) then it will use ddl in place of full.ddl that is created internally.
#' @param check.model if TRUE, code does an internal consistency check between PIMs and design data when making model.
#' @return model: a MARK object except for the elements \code{output} and
#' \code{results}. See mark in RMark for a detailed description of the
#' list contents.
#' @author Jeff Laake
#' @export
#' @seealso \code{\link{sim.process.data}},
#' \code{\link{run.simmark.model}} \code{\link{simmark}}
#' @keywords model
#' 
#' 
#' 
make.simmark.model <-
function(data,ddl,parameters=list(),title="",model.name=NULL,initial=NULL,
         numsims=1,simfile=NULL,seed=NULL,releases=NULL,beta=NULL,real=NULL,call=NULL,
		     default.fixed=TRUE,options=NULL,profile.int=FALSE,chat=NULL,simplify=TRUE,
		     input.links=NULL,parm.specific=FALSE,mlogit0=TRUE,hessian=FALSE,accumulate=TRUE,
         icvalues=NULL,wrap=TRUE,nodes=101,useddl=FALSE,check.model=FALSE)
{

#  *******************  INTERNAL FUNCTIONS    *********************************
#
#  print.pim: prints pim file to outfile for use in constructing MARK input
#
"print.pim" <-
function(pim,outfile)
{
#
# Arguments:
#
# pim     - pim matrix
# outfile - name of output file to write pim
#
# Value: None
#
# Define internal function "xp" that pastes non-zero values together with
# intervening spaces
#
xp=function(x){paste(x[x>0],collapse=" ")}
#
# For each row in the pim, apply xp to create a vector of concatenated values
# and then paste a ";" to end of each value
#
if(is.matrix(pim))
   strings=paste(apply(pim,1,xp),";")
else
   strings=paste(paste(pim,collapse=" "),";")
#
# Output strings
#
write(strings,outfile,append=TRUE)
return(NULL)
}
#
# spell: changes capitalization on links so it will be acceptable to mark interface
#
"spell" <- function(links)
{
  newlinks=links
  newlinks[newlinks=="logit"]="Logit"
  newlinks[newlinks=="mlogit"]="MLogit"
  newlinks[newlinks=="log"]="Log"
  newlinks[newlinks=="loglog"]="LogLog"
  newlinks[newlinks=="cloglog"]="CLogLog"
  newlinks[newlinks=="identity"]="Identity"
  newlinks[newlinks=="sin"]="Sin"
  return(newlinks)
}
#
#  realign.pims: realigns pim values to represent structure of design matrix
#
"realign.pims" <-
function(model){
#
#  Arguments:
#
#  model - a mark model object that has been created by make.mark.model
#
#  Value:
#
#  new.indices - a vector of new indices for the old PIM values.  The old
#                PIM values are 1:length(new.indices) and the new index is
#                the corresponding value.  For example, new.indices=c(1,1,2,2)
#                renumbers the structure 1,2,3,4 such that 1,2 are now 1
#                and 3,4 are now 2.
#
#  Get all the unique rows in the design matrix and paste all the values
#  in each row together.
#
   uniquevals=apply(unique(cbind(model$design.matrix,model$links)),1,paste,collapse="")
#
#  Get all the rows in the design matrix and paste all the values
#  in each row together.
#
   allvals=apply(cbind(model$design.matrix,model$links),1,paste,collapse="")
#
#  Find the corresponding sets of indices by matching allvals into uniquevals
#
   new.indices=match(allvals, uniquevals)
#
#  Next cope with fixed real parameters; first determine the unique fixed values
#
   uniquefixedvalues=unique(model$fixed$value)
#
#  Now create a parameter index for each of the unique fixed real parameters
#  assuming that they are all different from parameters defined by design
#  matrix (ie add onto max of uniqueindices),
#
   uniqueindices=match(model$fixed$value,uniquefixedvalues)+max(new.indices)
#
#  Assign these new indices to their position in the original PIM set
#
   new.indices[model$fixed$index]=uniqueindices
#
#  Some may overlap and others are new, so they need to be renumbered once again
#  eliminating extra ones
#
   new.indices=match(new.indices,sort(unique(new.indices)))
#
#  Simplification cannot occur for parameters with an mlogit link, so these
#  must be given new indices
#     Following lines force non-simplification for mlogit parameters
   mlogit.parameters=substr(model$links,1,6)=="mlogit" | substr(model$links,1,6)=="MLogit"
   new.indices[mlogit.parameters]=(1:sum(as.numeric(mlogit.parameters)))+max(new.indices)
   new.indices=match(new.indices,sort(unique(new.indices)))
   return(new.indices)
}
#
#  renumber.pims: using the vector of new indices that match the old
#                 structure to the new structure, change the values in the
#                 pim argument.  The way this is done depends on whether it is
#                 a square or triangular PIM.
#

"renumber.pims" <-
function(pim,newlist,type){
if(type%in%c("Triang","STriang"))
{
#   pim=t(pim)
	pim[pim!=0]=newlist[pim]
#    pim[lower.tri(pim,TRUE)]=newlist[pim]
#   return(t(pim))
    return(pim)
} else
   return(newlist[pim])
}
"pim.header"<- function(group,param.name,parameters,ncol,stratum,tostratum,strata.labels,mixtures,session=NULL,socc=NULL,bracket=FALSE,event=NULL,primary=NULL)
{
  stratum.designation=""	 
  if(!is.null(stratum)&length(strata.labels)>0)
  {
	  if(bracket)stratum.designation=""	 
	  if(!is.null(tostratum))
	  {
		  if(bracket)
			  param.name=paste(param.name,"[",stratum,",",tostratum,"]",sep="")
		  else
			  stratum.designation=paste(stratum,"to",tostratum)
	  }
	  else
	  {
		  if(bracket)
			  param.name=paste(param.name,"[",stratum,"]",sep="")
		  else
		  {
		    if(param.name%in%c("Delta","pi") & !is.null(event))
		    {
		      if(param.name=="Delta") 
		        param.name=paste("Delta ",event,"|",stratum,sep="")
		      else
		        param.name=paste("pi ",stratum,"|",event,sep="")
		    } else
		      stratum.designation= paste(stratum,":Stratum",stratum,sep="")
		   }
	   }
  }
  else
     stratum.designation=""
  if(is.null(session))
     session.designation=""
  else
     if(is.null(socc))
	    session.designation=paste("Session",session)
	 else
		 if(!socc)
		 {
		   if(!is.null(primary))
		     session.designation=paste("Session",session,"Primary",primary)
		   else
		     session.designation=paste("Primary",session)
		 }
		 else
		     session.designation=paste("Sampling Occasion",session)
 if (parameters$type == "Triang")
         string = paste(paste("group=", group,sep=""), param.name, stratum.designation, session.designation, 
                           paste(" rows=",ncol," cols=",ncol,sep=""), parameters$type, ";")
 else
	 if (parameters$type == "STriang")
				   string = paste(paste("group=", group,sep=""), param.name, stratum.designation, session.designation, 
						   paste(" rows=",(mixtures+parameters$rows)*ncol," cols=",ncol,sep=""), parameters$type, ";")
	else
      if(mixtures==1)
          string=paste(paste("group=",group,sep=""),param.name,stratum.designation, session.designation, 
                           paste(" rows=1"," cols=",ncol,sep=""),parameters$type,";")
      else
          if(!is.null(parameters$mix) && parameters$mix)
            string=paste(paste("group=",group,sep=""),param.name,stratum.designation,session.designation, 
              paste(" rows=",mixtures+parameters$rows," cols=",ncol,sep=""),parameters$type,";")
          else
            string=paste(paste("group=",group,sep=""),param.name,stratum.designation,session.designation, 
                          paste(" rows=1"," cols=",ncol,sep=""),parameters$type,";")
return(string)
}
"simplify.pim.structure" <-
function(model)
{
#
# simplify.pim.structure: renumbers PIMS to represent model structure that
#                         was created with the formula. It takes the input
#                         for MARK created in the model by make.mark.model
#                         with the formulas and simplies the PIM structure
#                         represented by the unique rows in the design matrix.
#                         It recreates the new input for MARK to reflect the
#                         change and adds it and some other fields to model
#                         for the pim translation.
#
# Arguments:
#
#  model - a mark model object that has been created by make.mark.model
#
#
# Value:
#
#  model - same mark model object with added list elements simplify
#          and a rewritten input object for MARK
#
#
#
#  Beginning of simplify.pim.structure function; it recreates input for
#  MARK and uses an outfile like make.mark.model
#
tempfilename=tempfile("markxxx",tmpdir=getwd(),fileext=".tmp")
outfile=file(tempfilename,open="wt")
#
# Use realign.pims to simplify PIM structure
#
new.indices=realign.pims(model)
#
# Copy first portion of the MARK input file because it will be unchanged by
# the simplification
#
input=model$input[1:grep("model=\\{",model$input)]
writeLines(input,outfile)
#
#  If there are fixed real parameters then these need to be included in the
#  design matrix if they are not already done so by the formula.
#
if(!is.null(model$fixed))
{
#
#  Get the unique new indices,values from the original indices for the fixed parameters
#  in model$fixed$index
#
   fixed.parms=unique(data.frame(index=new.indices[model$fixed$index],value=model$fixed$value))
   fixed=NULL
   num.fixed=dim(fixed.parms)[1]
#
#  For each fixed real parameter write out the input strings for MARK to assign
#  the number of fixed values and each fixed index (parm) and its value.
#
   for (i in 1:num.fixed)
      fixed = c(fixed, paste("parm(", fixed.parms$index[i], ")=", fixed.parms$value[i],
                    sep = ""))
   string = paste("fixed=", num.fixed, ";",sep="")
   write(string, file = outfile, append = TRUE)
   write(paste(fixed, ";"), file = outfile, append = TRUE)
}
#
#  Assign some values from the model that are used below
#
parameters=model$parameters
param.names=sub("DoublePrime","''",names(parameters))
param.names=sub("Prime","'",param.names)
number.of.groups=model$number.of.groups
nocc=model$nocc
if(is.null(model$mixtures))
   mixtures=1
else
   mixtures=model$mixtures
#
#  For each type of parameter, output the new PIM structure; This largely
#  follows code in make.mark.model except it uses renumber.pims and print.pim
#  to renumber and print out the pim structure.
#
if(model$data$model=="RDMSOccRepro")
  bracket=rep(TRUE,length(parameters))
else 
  if(model$data$model=="RDMSOccupancy")
	    bracket=c(FALSE,FALSE,TRUE)
  else
	    bracket=rep(FALSE,length(parameters))
for (i in 1:length(parameters)) {
  for (j in 1:length(model$pims[[i]]))
  {
         if(model$model=="MSJollySeber" &i==5)model$pims[[i]][[j]]$stratum=NULL
         ncol = dim(model$pims[[i]][[j]]$pim)[2]
         string=pim.header(pim[[i]][[j]]$group,param.names[i],parameters[[i]],
                   ncol,model$pims[[i]][[j]]$stratum,model$pims[[i]][[j]]$tostratum,model$strata.labels,
				           mixtures,model$pims[[i]][[j]]$session,parameters[[i]]$socc,bracket=bracket[i],event=model$pims[[i]][[j]]$event,
				           primary=model$pims[[i]][[j]]$primary)
         write(string, file = outfile, append = TRUE)
         if(parameters[[i]]$type %in% c("Triang","STriang"))
         {
            newpim=renumber.pims(model$pims[[names(model$parameters)[i]]][[j]]$pim,new.indices,parameters[[i]]$type)
            print.pim(newpim,outfile)
         }
         else
         {
             nmix=1
             if(mixtures>1)
               if(!is.null(parameters[[i]]$mix) && parameters[[i]]$mix)
                   nmix=mixtures+parameters[[i]]$rows
             for(k in 1:nmix)
             {
               newpim=renumber.pims(model$pims[[names(model$parameters)[i]]][[j]]$pim[k,],new.indices,parameters[[i]]$type)
               print.pim(newpim,outfile)
             }
         }
     }
}
#
# Next compute the new simplified design matrix.  rownums is the row numbers (indices)
# from the original design matrix but there is only one for each of the new
# parameters with indices 1:length(new.indices).  The new design matrix
# (complete.design.matrix) is obtained by subsetting the rows from the
# original design matrix matching rownums.  This is done using row.names to be
# able to use subset so it will always yield a dataframe.  Using indices for
# row numbers can result in a vector if there is only a single beta.
rownums=match(1:length(unique(new.indices)),new.indices)
# 22-Aug-05; change to use [rownums,] to accomodate fixed parameters
# 1 feb 06; modified to cope with single element selected
if(length(rownums)==1)
	complete.design.matrix=subset(model$design.matrix,1:dim(model$design.matrix)[1]%in%rownums)
else
	complete.design.matrix=model$design.matrix[rownums,,drop=FALSE]
# for any fixed parameter set row to all 0s
if(!is.null(model$fixed))
	for (i in 1:num.fixed)
	complete.design.matrix[fixed.parms$index[i],]="0"
#
# Find any columns that are all 0; left from mlogit0 fix
#
dm=complete.design.matrix
allzero=apply(dm,2,function(x) all(x=="0"))
complete.design.matrix=dm[,!allzero,drop=FALSE]

#
# Look for setting of initial values in the input file; if found write them 
# exclude columns that are now all 0s.
#
if(length(grep("XXXinitialXXX ",model$input))!=0)
{
	initial=strsplit(model$input[grep("XXXinitialXXX ", model$input)]," ")[[1]]
	initial=initial[-c(1:2,length(initial))]
	if(any(allzero)) initial=initial[!allzero]
	string=paste("initial ",paste(initial,collapse=" ")," ;",collapse=" ")
    write(string, file = outfile, append = TRUE)
}
#
#  If profile intervals requested write out needed statements
#

if(is.null(model$chat))chat=1
if(is.numeric(model$profile.int))
{
   if(any(!model$profile.int%in%new.indices)) 
      stop(paste("Profile interval argument requests values not in beta: 1 to ",
                      ncol(model$design.matrix),"\n"))
      string=paste(paste("Profile Intervals chat=",format(chat,digits=5),sep=""),
                          paste(model$profile.int,collapse=" ")) 
      write(paste(string,";",sep=""),file=outfile,append=TRUE)   
}
else
{
    if(model$profile.int)
    {
      string=paste(paste("Profile Intervals chat=",format(chat,digits=5),sep=""),
                            paste(unique(new.indices),collapse=" "))
      write(paste(string,";",sep=""),file=outfile,append=TRUE)      
    } 
}
# 10 Jan 06; change to accomodate S(.) with known fate where design matrix can
# become a single element with simplification
#if(is.vector(complete.design.matrix))
#{
#   complete.design.matrix=as.matrix(complete.design.matrix)
#   row.names(complete.design.matrix)=row.names(model$design.matrix)[1]
#}
# if icvalues not null, write out values to input file
#
if(!is.null(icvalues))
{
	string = paste("icvalues=", paste(icvalues,collapse=","), ";",sep="")
	write(string, file = outfile, append = TRUE)		
}
#
# Write out the design matrix into the MARK input file
#
if(nrow(complete.design.matrix)==ncol(complete.design.matrix)&&all(complete.design.matrix==diag(nrow(complete.design.matrix))))
{
	string=paste("design matrix constraints=",nrow(complete.design.matrix), " covariates=",nrow(complete.design.matrix)," identity;",sep="")
	write(string, file = outfile, append = TRUE)
}
else
{
	string = paste("design matrix constraints=", dim(complete.design.matrix)[1],
			" covariates=", dim(complete.design.matrix)[2], ";",sep="")
	write(string, file = outfile, append = TRUE)
	write.table(complete.design.matrix, file = outfile, eol = ";\n",
			sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
}
# output releases for each group
if(model$model=="CJS")
{
  for (i in 1:number.of.groups)
  {
    write(paste("releases group=",i,";",sep=""),file=outfile,append=TRUE)
    write(paste(paste(releases[,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
  }
} else
  if(model$model=="Multistrata")
  {
    for (i in 1:number.of.groups)
      for(j in 1:nstrata)
      {
         write(paste("releases group=",i," strata=",j,";",sep=""),file=outfile,append=TRUE)
         write(paste(paste(releases[,j,i],collapse=" "),";",sep=""),file=outfile,append=TRUE)
      }
  }  
# output parmvals and link
identityDM=FALSE
if(all(apply(complete.design.matrix,1,function(x) sum(as.numeric(x)))==1)&
   all(apply(complete.design.matrix,2,function(x) sum(as.numeric(x)))==1)) identityDM=TRUE
if(!is.null(beta))
{  
   param.link=link
   param=beta
} else
{
  if(!is.null(real))
  {
    if(!identityDM) stop("\nCannot specify real values when the DM is not an identity matrix")
    param.link="identity"
    param=real
  } else
      stop("either beta or real needs to be specified for parmvals")
}
write(paste("parmvals link=",param.link,";",sep=""),file=outfile,append=TRUE)
if(!is.list(param)) stop("beta or real must be a list of vectors named with parameters")
parmvals=NULL
# need to make them row/column model order
for(i in 1:length(param))
{
  j=which(names(param)==param.names[i])
  iname=names(param)[j]
  if(param.link=="identity")
    icol=grep(paste(iname," ",sep=""),rownames(complete.design.matrix))
  else
    icol=grep(paste(iname,":",sep=""),colnames(complete.design.matrix))
  if(!length(icol)==length(param[[j]])) stop(paste("For ",iname," ",length(param[[j]])," values specified for ",length(icol)," parameter(s)",sep=""))
  parmvals=c(parmvals,param[[j]])
}
write(paste(paste(parmvals,collapse=" "),";",sep=""),file=outfile,append=TRUE)

#
# If there is a link specification for models that use different links for
# each real parameter, write those out now shifting them for translation of
# indices.
#
# 11 Jan 06; modified code for new format of links input
#  1 feb 06; fixed links code to work with simplification - not done previously
#  6 Apr 06; added code to deal with simplification within mlogit parameters
if (!is.null(model$links))
{
   if(length(model$links)>1)
   {
     newlinks=model$links[match(unique(new.indices), new.indices)][order(unique(new.indices))]
     xi=grep("mlogit(",newlinks,fixed=TRUE)
     logit.numbers=as.numeric(gsub(")","",gsub("mlogit(","",newlinks[xi],fixed=TRUE),fixed=TRUE))
     logit.numbers=match(logit.numbers,sort(unique(logit.numbers)))
     newlinks[xi]=paste("mlogit(",logit.numbers,")",sep="")
     write(paste("links=",length(unique(new.indices)),";",sep=""),file=outfile,append=TRUE)
     write(paste(spell(newlinks),";",sep=""), file = outfile, append = TRUE)
   }
   else
     newlinks=NULL
}
else
   newlinks=NULL
#
# Write out labels for betas
#
if(!is.matrix(complete.design.matrix))
   complete.design.matrix=as.matrix(complete.design.matrix)
string = paste("blabel(", 1:dim(complete.design.matrix)[2], ")=", colnames(complete.design.matrix),";",sep="")
write(string, file = outfile, append = TRUE)
#
# Write out labels for real parameters
#
rnames = rep("", dim(complete.design.matrix)[1])
ipos = 0
string = paste("rlabel(", 1:dim(complete.design.matrix)[1], ")=", row.names(complete.design.matrix), ";",sep="")
write(string, file = outfile, append = TRUE)
#
#  Complete with stop statement; then read the outfile into the input vector to
#  store in the model object.  delete the output file and add the fields to the
#  model object and return it.
#
write("proc stop;", file = outfile, append = TRUE)
close(outfile)
outfile=file(tempfilename,open="rt")
text = readLines(outfile)
close(outfile)
unlink(tempfilename)
model$input=text
if(!is.null(newlinks))
   model$simplify=list(design.matrix=complete.design.matrix,pim.translation=new.indices,links=newlinks)
else
   model$simplify=list(design.matrix=complete.design.matrix,pim.translation=new.indices)
return(model)
}

#
# Create internal function to create a pim
#
create.pim=function(nocc,parameters,npar,mixtures,mscale=1)
{
    ncol=(nocc+parameters$num)/mscale
    mat=NULL
    if(parameters$type%in%c("Triang","STriang"))
    {
		   nmix=1
		   if(mixtures>1)
			    if(!is.null(parameters$mix)&&parameters$mix)
				     nmix=mixtures+parameters$rows
		   for (j in 1:nmix)
		   {
		      ncol=nocc+parameters$num
		      for(k in 1:(nocc+parameters$num))
          {
               if(parameters$pim.type=="all")
               {
                   mat=rbind(mat,c(rep(0,k-1),npar:(npar+ncol-1)))
                   npar=npar+ncol
               }
               else
               {
                  if(parameters$pim.type=="time")
                      mat=rbind(mat,c(rep(0,k-1),(npar+k-1):(npar+k-1+ncol-1)))
                  else
					  if(parameters$pim.type=="age")
						  mat=rbind(mat,c(rep(0,k-1),npar:(npar+ncol-1)))
					  else
						  mat=rbind(mat,c(rep(0,k-1),rep(npar,ncol)))
               }
               ncol=ncol-1
		      }
       }
   }
   else
   {
        nmix=1
        if(mixtures>1)
            if(!is.null(parameters$mix)&&parameters$mix)
                nmix=mixtures+parameters$rows
        for(k in 1:nmix)
        {
            if(parameters$pim.type!="constant")
               mat=rbind(mat,npar:(npar+ncol-1))
            else
              mat=rbind(mat,rep(npar,ncol))
            npar=npar+ncol
        }
   }
return(mat)
}
#
# Creates time-dependent covariates for age of nest if there
# is a field called AgeDay1 in the data.  Then the field NestAge
# can be used in the formula for S.
#
create.agenest.var=function(data,init.agevar,time.intervals)
{
   nocc=length(time.intervals)
   age.mat=matrix(data[,init.agevar],nrow=dim(data)[1],ncol=nocc-1)
   age.mat=t(t(age.mat)+cumsum(c(0,time.intervals[1:(nocc-2)])))
   age.mat=data.frame(age.mat)
   names(age.mat)=paste("NestAge",1:(nocc-1),sep="")
   return(age.mat)
}
#
#  *******************  END OF INTERNAL FUNCTIONS    *********************************
#
#Beginning of make.simmark.model
#
# Test to make sure that all rows of design data are there (no more deletion) and make sure they
# are ordered
  missing=FALSE
  for(i in 1:(length(ddl)-1))
  {
	  if(max(ddl[[i]]$par.index) != nrow(ddl[[i]])) 
	  {
	    missing=TRUE
		  stop(paste("\nMissing rows in design dataframe for parameter",names(ddl)[i],
				  "\n Deleting rows from design data is no longer allowed\n"))
	  }
	 if(any(ddl[[i]]$par.index != sort(ddl[[i]]$par.index))) 
	 {
		 stop(paste("\nRows of design dataframe for parameter",names(ddl)[i],
				 "are out of order.\nThey should be in order of par.index.\n"))
 	 }
  }
# Force use of mlogit0=TRUE
  if(!mlogit0){
    warning("mlogit0=FALSE is no longer allowed. Setting to TRUE")
    mlogit0=TRUE
  }
#
# Outfile is assigned a temporary name
#
  tempfilename=tempfile("markxxx",tmpdir=getwd(),fileext=".tmp")
  outfile=file(tempfilename,open="wt")
#
# Check validity of parameter types, if any given
#
  if(!RMark:::valid.parameters(data$model,parameters))stop()
#
# Next check validity of fields defined in each parameter list.
#
  if(length(parameters)>0)
  for(i in 1:length(parameters))
     for(j in 1:length(names(parameters[[i]])))
        if(!(names(parameters[[i]])[j]%in%c("fixed","formula","link","share","remove.intercept","default","contrasts")))
        {
           message("\nInvalid model specification for parameter ",names(parameters)[i],".\nUnrecognized element ",names(parameters[[i]])[j])
           stop()
        }
#
# Initialize some variables
#
  ch=data$data$ch
  mixtures=data$mixtures
  nocc=data$nocc
  nocc.secondary=data$nocc.secondary
  nstrata=data$nstrata
  number.of.groups=dim(data$freq)[2]
  par.list=setup.parameters(data$model,check=TRUE)
  parameters=setup.parameters(data$model,parameters,nocc,number.of.groups=number.of.groups)
  parameters=parameters[par.list]
  temp.rev=data$reverse
  data$reverse=FALSE
  full.ddl=make.design.data(data,parameters=ddl$pimtypes)
  complete=TRUE
  for(iname in names(parameters)[names(parameters)%in%names(full.ddl)])
    if(nrow(full.ddl[[iname]])!=nrow(ddl[[iname]])) complete=FALSE
  if(complete&!missing&useddl)full.ddl=ddl
  data$reverse=temp.rev
  parameters=parameters[names(parameters)%in%names(full.ddl)]
  for(j in names(parameters))
  {
    if(data$model=="NSpeciesOcc" & j=="f")parameters[[j]]$rows=2^data$mixtures-1 -data$mixtures
     parameters[[j]]$pim.type=ddl$pimtypes[[j]]$pim.type
     if(!is.null(ddl$pimtypes[[j]]$subtract.stratum))
        parameters[[j]]$subtract.stratum=ddl$pimtypes[[j]]$subtract.stratum
  }
  for(i in 1:length(parameters))
  {
#     check to make sure that design data row names are in the correct order
	  if(any((1:nrow(ddl[[names(parameters)[i]]]))!=rownames(ddl[[names(parameters)[i]]])))
	  {
		  message("Row names in design data for parameter ",names(parameters)[i]," are out of order or you deleted design data.\n")
	  }	  
#
#     For parameters that can be possibly shared, see if they are not shared and if not then create
#     default formula if one not specified; also use link from dominant parameter
#
	  if(!is.null(parameters[[i]]$share))
	  {
		  if(!parameters[[i]]$share)
      {
		      shared_par=parameters[[i]]$pair
	        if(is.null(parameters[[shared_par]]$formula))parameters[[shared_par]]$formula=~1
	    }else
		  {
			  shared_par=parameters[[i]]$pair
			  parameters[[shared_par]]$link=parameters[[i]]$link
			  if(!is.null(parameters[[i]]$fixed))
			    stop(paste("Cannot use fixed with share=TRUE; If you are using mark.wrapper, \n",
			    "use formulas within a list as in the following example\n",
			    "GammaDoublePrime.dot=list(GammaDoublePrime=list(formula=~1,fixed=0),GammaPrime=list(formula=~1,fixed=0))"))
		  }
	  }
#
#     Test validity of link functions
# 
     if(!(parameters[[i]]$link %in% c("identity","log","logit","mlogit","loglog","cloglog","sin")))
     {
        message("\nInvalid link value ",parameters[[i]]$link)
        stop()
     }
  }
  param.names=sub("DoublePrime","''",names(parameters))
  param.names=sub("Prime","'",param.names)
  model.list= setup.model(data$model,0)
  etype=model.list$etype
#	
# Output data portion of MARK input file:
#   proc title
#   proc chmatrix
#
  if(etype=="Nest")
  {
     zz=subset(data$data,select=c("FirstFound","LastPresent","LastChecked","Fate"))
     zz=cbind(zz,rowSums(data$freq))
     if(!is.null(data$data$AgeDay1))
        data$data=cbind(data$data,create.agenest.var(data$data,"AgeDay1",data$time.intervals))
  }
  else
  {
     zz=as.data.frame(ch)
	 if(substr(etype,1,7)=="Density")
	 {
		 accumulate=FALSE
		 zz=cbind(zz,data$data$TotalIn,data$data$TotalLocations)
	 }
     zz=cbind(zz,data$freq)
  }
#
# p&c in closed models, gammas in robust models, and p1-p2 in MS Occupancy are handled differently
# to allow shared parameters.  If there is no c$formula, gammaDoublePrime$formula, or p2$formula then the design data
# are appended to the shared parameters (p for c and gammaPrime for gammaDoublePrime)
# In the design data for p a covariate "c" is added to the recapture parameters and for
# a covariate emigrate for the gammaDoublePrime parameters.
# 
    for(i in 1:length(parameters))
   {
#
#     For parameters that can be possibly shared, if they are shared, pool design data as long as dimensions match
#
	   if(!is.null(parameters[[i]]$share)&&parameters[[i]]$share)
       {
	       shared_par=parameters[[i]]$pair
	       dim1=dim(ddl[[names(parameters)[i]]])
		   dim2=dim(ddl[[shared_par]])
           if(dim1[2]==dim2[2])
		   {
			   rn1=as.numeric(row.names(ddl[[names(parameters)[i]]]))
			   rn2=as.numeric(row.names(ddl[[shared_par]]))+nrow(full.ddl[[names(parameters)[i]]])
			   ddl[[names(parameters)[i]]]=rbind(ddl[[names(parameters)[i]]],ddl[[shared_par]])
               ddl[[names(parameters)[i]]][shared_par]=c(rep(0,dim1[1]),rep(1,dim2[1]))
		       row.names(ddl[[names(parameters)[i]]])=c(rn1,rn2)
		   } else
		   {
			   message(paste("Error: for a shared ",paste(names(parameters)[i],shared_par,sep="&"),
				" model, their design data columns must match\n. If you add design data to one it must also be added to the other.\n"))
			   message(paste("Columns of",names(parameters)[i]," : ",names(ddl[i]),"\n"))
			   message(paste("Columns of",shared_par,": ",names(ddl[[shared_par]]),"\n"))
			   stop("Function terminated\n") 
		   }
	   }
   }
#
# For each parameter type determine which values in the formula are covariates that need to be
# added to design data and put in data portion of input file.
#
  xcov=list()
  covariates=NULL
  time.dependent=list()
  session.dependent=list()
  for(i in 1:length(parameters))
  {
     if(!is.null(parameters[[i]]$formula))
     {
#
#    First get the variables in the formula.  Identify those not in the design data and make sure
#    that they are in the data. If there are any, add the covariate to the covariate list and add a column 
#    to the design data.
#
#    Note parx is the name of the ith parameter.  ddl is always constructed in the same order for each model
#    but the order of the parameters in the model specification may be different, therefore the indexing into
#    ddl must be done by name rather than position from the parameters specification.
#
      parx=names(parameters)[i]
      vars=all.vars(parameters[[i]]$formula)
      termslist=colnames(attr(terms(parameters[[i]]$formula),"factors"))
      xcov[[parx]]=vars[!(vars%in%names(ddl[[parx]]))]
      time.dependent[[parx]]=rep(FALSE,length(xcov[[parx]]))
      session.dependent[[parx]]=rep(FALSE,length(xcov[[parx]]))
      if(any(!(vars%in%names(ddl[[parx]]))))
         for(j in 1:length(xcov[[parx]]))
         {
            if(!(xcov[[parx]][j]%in%names(data$data)))
            {
              if(!is.null(full.ddl[[parx]]$session))
              {
                # This model was split off to use primary instead of time; but changed back.Could be deleted
                 if(data$model%in%c("RDMultScalOcc"))
                   cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$session),as.character(ddl[[parx]]$time),sep=""))
                 else
                   cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$session),as.character(ddl[[parx]]$time),sep=""))
                 if(any(!cov.bytime%in%names(data$data)))
                 {
                    session.dependent[[parx]][j]=TRUE                 
                    cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$session),sep=""))
                 }
              }                 
              else
                 cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$time),sep=""))
              if(all(!cov.bytime%in%names(data$data)))
                 stop("\nError: Variable ",xcov[[parx]][j]," used in formula is not defined in data\n")
              else
              {
                 time.dependent[[parx]][j]=TRUE
                 if(any(!cov.bytime%in%names(data$data)))
                 {
                     message(paste("\nThe following covariates are missing:",cov.bytime[!cov.bytime%in%names(data$data)],collapse=""))
                     message("\nIf any are used in the resulting model it will fail\n")
                     cov.bytime=cov.bytime[cov.bytime%in%names(data$data)]
                 }
              }
            }
            savenames=names(ddl[[parx]])
            ddl[[parx]]=cbind(ddl[[parx]],rep(1,dim(ddl[[parx]])[1]))
            names(ddl[[parx]])=c(savenames,xcov[[parx]][j])
            if(!time.dependent[[parx]][j])
               covariates=c(covariates,xcov[[parx]][j])
            else
               covariates=c(covariates,cov.bytime)
         }
     }
  }
# 
#  Unless this is nest data, aggregate data
#
# Fix 9 Nov 2013; create unique covariate names before selecting data
  covariates=unique(covariates)
  if(!is.null(covariates))
	    zzd=data.frame(cbind(zz,data$data[,covariates]))
  else
	    zzd=zz
  if(etype!="Nest" & accumulate)
  {
	  pasted.data=apply(zzd, 1, paste, collapse = "")
	  ng=ncol(data$freq)
	  if(ng>1)
		  freq=t(sapply(split(data$freq,pasted.data ),colSums))
	  else
		  freq=sapply(split(data$freq, pasted.data),sum)
	  zzd=zzd[order(pasted.data),]
	  zzd=zzd[!duplicated(pasted.data[order(pasted.data)]),]
	  if(ng>1)
	  {
		  if(nrow(freq)!=nrow(zzd))
			  stop("problem with accumulating data. Set accumulate=FALSE and contact maintainer")
	  }else
	  if(length(freq)!=nrow(zzd))
		  stop("problem with accumulating data. Set accumulate=FALSE and contact maintainer")
	  zzd[,2:(ng+1)]=freq
	  zz=zzd[,1:(ng+1)]
  }
#
# Output proc simulate statement 
  if(is.null(releases))
    stop("\nMust specify releases")
  else
    if(!is.array(releases))
      stop("\nreleases must be an array")
    else
    {
      len=dim(releases)
      len=len[length(len)]
      if(!len==number.of.groups)
        stop("number of rows must be number of groups")
      len=dim(releases)
      len=len[1]
      if(len!=nocc-1)
        stop("number of releases must be # of occasions -1")
      len=dim(releases)
      if(length(len)>2)
      {
         if(len[2]!=nstrata)
           stop("number of columns must be number of strata")
      }
    }
  hist=sum(releases)
  string=paste("proc title ",title,";")
  if(is.null(nocc.secondary))
     if(is.null(data$events))
       string=paste(string,"\nproc simulate occasions=",nocc," groups=",number.of.groups," etype=",etype, " Nodes=",nodes,sep="")
     else
       string=paste("\nproc simulate occasions=",nocc," groups=",number.of.groups," etype=",etype, 
       " events=",length(data$events),sep="")
  else
     string=paste("\nproc simulate occasions=",sum(nocc.secondary)," groups=",number.of.groups," etype=",etype," Nodes=",nodes,sep="")
  if(model.list$strata)string=paste(string," strata=",data$nstrata,sep="")
  string=paste(string," hist=",hist," numsims=",numsims," seed=",seed," simfile=",simfile,sep="")
  #
  # Next determine if a single link was used or differing links
  #
  link=parameters[[1]]$link
  for(i in 1:length(parameters))
    if(link!=parameters[[i]]$link)link="Parm-Specific"
  if(!is.null(input.links) | parm.specific)link="Parm-Specific"
  string=paste(string," link=",link,options,sep="")
  
  if(mixtures!=1)
     string=paste(string," mixtures =",mixtures)
  write(strwrap(paste(string,";"),100,prefix=" "),file=outfile,append=TRUE)
  
#  time intervals
  time.int=data$time.intervals
  if(length(time.int)>0) 
    string=paste("\n time interval ",paste(time.int,collapse=" "),";\n",sep="")
  else
    string=paste("\n")

# strata and events
  
  if(model.list$strata)
    if(is.null(data$events))
      string=paste(string,"strata=",paste(data$strata.labels[1:data$nstrata],collapse=" "),";\n",sep="")
    else
      string=paste(string,"strata=",paste(c(data$strata.labels[1:data$nstrata],data$events),collapse=" "),";\n",sep="")
  write(strwrap(string,100,prefix=" "),file=outfile,append=TRUE)
  
#
#  Output group labels
#
  if(is.null(names(data$freq)))
     group.labels=paste("Simulation Group",1:number.of.groups)
  else
     group.labels=names(data$freq) 
  for(j in 1:number.of.groups)
  {
      string=paste("glabel(",j,")=",group.labels[j],";",sep="")
      write(string,file=outfile,append=TRUE)
  }
#
# First create model name using each parameter unless a model name was given as an argument;
#
  if(is.null(model.name))
  {
    model.name=""
    for(i in 1:length(parameters))
    {
       model.name=paste(model.name,param.names[i],"(",paste(parameters[[i]]$formula,collapse=""),sep="")
       model.name=paste(model.name,")",sep="")
     }
  }
  string=paste("model={",substr(model.name,1,160),"};")
  write(string,file=outfile,append=TRUE)
#
# Next compute PIMS for each parameter in the model - these are the all different PIMS
# 11 Jan 06 code added for multistrata models
#
  pim=list()
  npar=1
  for(i in 1:length(parameters))
  {
     if(data$model=="MSJollySeber"&names(parameters)[i]=="pi")parameters[[i]]$num=parameters[[i]]$num+nstrata-2
     pim[[i]]=list()
     k=0
     for(j in 1:number.of.groups)
     {
	      if(is.null(parameters[[i]]$events)) 
	        events=1 
	      else 
	        events=data$events
	      for(jjj in events)
	      {
	        if(is.null(parameters[[i]]$bystratum)||!parameters[[i]]$bystratum||(data$model=="MSJollySeber"&names(parameters)[i]=="pi"))
	          xstrata=1
	        else
	          if(!is.null(parameters[[i]]$events)&&parameters[[i]]$events)
	              xstrata=unique(ddl[[i]]$stratum[ddl[[i]]$event==jjj])
	          else
	            xstrata=unique(ddl[[i]]$stratum)
	      for (jj in xstrata)
	      {
          other.strata=1
          if(!is.null(parameters[[i]]$tostrata))
			         other.strata=unique(ddl[[i]]$tostratum[ddl[[i]]$stratum==jj])
          for(to.stratum in other.strata)
          {
               if(model.list$robust && parameters[[i]]$secondary)
			         {
				          multi.session=TRUE
                  num.sessions=nocc
			         } else
			         {
				          num.sessions=1
				          multi.session=FALSE
			         }
               nprimary=1
               for (l in 1:num.sessions)
               {
                 if(data$model=="RDMultScalOcc" & names(parameters)[i]=="p")
                   nprimary=nocc.secondary[l]/data$mixtures
                 for(m in 1:nprimary)
                  {
                    k=k+1
                    pim[[i]][[k]]=list()
                    if(data$model%in%c("RDMSOccRepro","RDMSOccupancy") & names(parameters)[i]=="Phi0")
                    {
                      pim[[i]][[k]]$pim=matrix(ddl[[i]]$model.index,ncol=nstrata,byrow=TRUE)   
                    } else	 
                      if(!multi.session)
                        pim[[i]][[k]]$pim=create.pim(nocc,parameters[[i]],npar,mixtures)
                    else
                    {
                      if(is.na(parameters[[i]]$num))
                      {
                        parameterx=parameters[[i]]
                        parameterx$num=0
                        pim[[i]][[k]]$pim=create.pim(1,parameterx,npar,mixtures)
                      }
                      else
                        if(data$model=="RDMultScalOcc")
                        {
                          if(names(parameters)[i]=="Theta") 
                            pim[[i]][[k]]$pim=create.pim(nocc.secondary[l],parameters[[i]],npar,mixtures,mscale=data$mixtures)
                          else
                            if(names(parameters)[i]=="p")
                            {
                                pim[[i]][[k]]$pim=create.pim(nocc.secondary[l],parameters[[i]],npar,mixtures,mscale=nocc.secondary[l]/data$mixtures)
                                pim[[i]][[k]]$primary=m
                            }
                            else
                               pim[[i]][[k]]$pim=create.pim(nocc.secondary[l],parameters[[i]],npar,mixtures)
                        }
                        else
                            pim[[i]][[k]]$pim=create.pim(nocc.secondary[l],parameters[[i]],npar,mixtures)
                      pim[[i]][[k]]$session=l
                      pim[[i]][[k]]$session.label=levels(ddl[[i]]$session)[l]
                    }
                    pim[[i]][[k]]$group=j
                    if(length(data$strata.labels)>0 && !is.null(parameters[[i]]$bystratum) && parameters[[i]]$bystratum) pim[[i]][[k]]$stratum=jj
                    if(!is.null(parameters[[i]]$tostrata)) pim[[i]][[k]]$tostratum=to.stratum
                    if(!is.null(parameters[[i]]$events))pim[[i]][[k]]$event=jjj
                    npar=max(pim[[i]][[k]]$pim)+1
                  }
               }
            }
	       }
       }
     }
  }
  npar=npar-1
  names(pim)=names(parameters)
  check_model=function(pims,ddl,parameters,groups)
  {
    for(parx in names(parameters))
    {
      pim=pims[[parx]] 
      for(i in 1:length(pim))
      {
        index=sort(unique(as.vector(pim[[i]]$pim)))
        index=index[index!=0]
        vars=names(pim[[i]])[!names(pim[[i]])%in%c("pim")]
        df=data.frame(model.index=index)
        for(j in vars)
        {
          if(j=="group"&!is.null(groups))
            df[[j]]=paste(sapply(groups[pim[[i]][[j]], ],as.character),collapse="")
          else
            df[[j]]=as.character(pim[[i]][j])
        }
        if(i==1)
          dfx=df
        else
          dfx=rbind(dfx,df)
      }
      vars=c("model.index",vars)
      vars=vars[vars%in%names(ddl[[parx]])]
      ddlvalues=apply(subset(ddl[[parx]],select=vars),1,paste,collapse="")
      pimvalues=apply(dfx[,vars],1,paste,collapse="")
      if(any(ddlvalues!=pimvalues))
        stop("model not correct for parameter ",parx)
    }
    return(NULL)
  }
  if(check.model) check_model(pims=pim,full.ddl,parameters,data$group.covariates)
  
#
# If there are fixed parameters output the text here
#
  num.fixed=0
  fixed=NULL
  fixedvalues=NULL
  for(i in 1:length(parameters))
  {
     parx=names(parameters)[i]
#
#    Add any default fixed values
#
     fixlist=NULL
     fixvalues=NULL
     fix.indices=NULL
     if(default.fixed)
     {
        rn=row.names(full.ddl[[parx]][!row.names(full.ddl[[parx]])%in%row.names(ddl[[parx]]),])
        if(length(rn)>0)
        {
           fixvalues=rep(parameters[[i]]$default,length(rn))
           fixlist=as.numeric(rn)
        }
     }
#
#    Add any values specified with fix column in ddl
#
	 if(!is.null(ddl[[parx]]$fix))
	 {
		 fixvalues=c(fixvalues,ddl[[parx]]$fix[!is.na(ddl[[parx]]$fix)])
		 fixlist=c(fixlist,as.numeric(row.names(ddl[[parx]][!is.na(ddl[[parx]]$fix),])))
	 }
     if(!is.null(parameters[[i]]$fixed)|!is.null(fixlist))
     {
#
#      All values of this parameter type are fixed at one value
#
       if(length(parameters[[i]]$fixed)==1) 
       {
          for(j in 1:length(pim[[i]]))
          {
             fixlist=unique(as.vector(pim[[i]][[j]]$pim))
             fixlist=fixlist[fixlist>0]
             if(is.null(fixedvalues))
                 fixedvalues=data.frame(index=fixlist,value=rep(parameters[[i]]$fixed,length(fixlist)))
             else
                 fixedvalues=rbind(fixedvalues,data.frame(index=fixlist,value=rep(parameters[[i]]$fixed,length(fixlist))))
             for(k in 1:length(fixlist))
             {
                num.fixed=num.fixed+1
                fixed=c(fixed,paste("parm(",fixlist[k],")=",parameters[[i]]$fixed,sep=""))
             }
          }
        }else
#
#       The parameters with indices in the first list element are given specified value(s)
#
        if(is.list(parameters[[i]]$fixed)|!is.null(fixlist))
        {
             if("index"%in%names(parameters[[i]]$fixed))
                fix.indices=parameters[[i]]$fixed$index
             else
                if(is.list(parameters[[i]]$fixed)&!any(names(parameters[[i]]$fixed)%in%c("time","age","cohort","group")))
                   stop(paste("\nUnrecognized structure for fixed parameters =",parameters[[i]]$fixed))
                else
                    if(!is.null(parameters[[i]]$fixed[["time"]]))
                    {
                        times=parameters[[i]]$fixed[["time"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$time%in%times,]))
                    }
                    else
                    if(!is.null(parameters[[i]]$fixed[["age"]]))
                    {
                        ages=parameters[[i]]$fixed[["age"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$age%in%ages,]))
                    }
		                else
                    if(!is.null(parameters[[i]]$fixed[["cohort"]]))
                    {
                        cohorts=parameters[[i]]$fixed[["cohort"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$cohort%in%cohorts,]))
                    }
                    else
                    if(!is.null(parameters[[i]]$fixed[["group"]]))
                    {
                        groups=parameters[[i]]$fixed[["group"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$group%in%groups,]))
                    }
             if(!is.null(fix.indices))fixlist=c(fixlist,fix.indices)
             if(length(parameters[[i]]$fixed$value)==1)
                 fixvalues=c(fixvalues,rep(parameters[[i]]$fixed$value,length(fix.indices)))
             else
             {
                 fixvalues=c(fixvalues,parameters[[i]]$fixed$value)
                 if(length(fixlist)!=length(fixvalues))
                    stop(paste("\nLengths of indices and values do not match for fixed parameters for",names(parameters)[i],"\n"))
             }
			 # check for duplicates and use latter values
             if(any(duplicated(fixlist)))
			 {
				 message(paste("\nSome indices for fixed parameters were duplicated for ",parx,"; using latter values\n"))
				 uniqIndices=which(!duplicated(rev(fixlist)))
				 fixlist=rev(fixlist)[uniqIndices]
				 fixvalues=rev(fixvalues)[uniqIndices]
			 }
             # assign all.different indices by adding first pim index-1
             fixlist=fixlist+ pim[[i]][[1]]$pim[1,1]-1
             for(k in 1:length(fixlist))
             {
                num.fixed=num.fixed+1
                fixed=c(fixed,paste("parm(",fixlist[k],")=",fixvalues[k],sep=""))
             }
             if(is.null(fixedvalues))
                fixedvalues=data.frame(index=fixlist,value=fixvalues)
             else
                fixedvalues=rbind(fixedvalues,data.frame(index=fixlist,value=fixvalues))
        }
     }
  }
  if(num.fixed>0)
  {
     string=paste("fixed =",num.fixed,";",sep="")
     write(string,file=outfile,append=TRUE)
     write(paste(fixed,";"),file=outfile,append=TRUE)
  }
#
# Unless model will be simplified, output PIMS for each parameter in the model
#
  if(!simplify)
  {
     for(i in 1:length(parameters))
     {
        for(j in 1:length(pim[[i]]))
        {
           ncol=dim(pim[[i]][[j]]$pim)[2]
           string=pim.header(pim[[i]][[j]]$group,param.names[i],parameters[[i]],
                   ncol,pim[[i]][[j]]$stratum,pim[[i]][[j]]$tostratum,
                   data$strata.labels,mixtures,pim[[i]][[j]]$session,parameters[[i]]$socc)
           write(string,outfile,append=TRUE)
           print.pim( pim[[i]][[j]]$pim,outfile)
        }
     }
  }
#
# Create design matrix for each parameter type
#
  design.matrix=list()
  for(i in 1:length(parameters))
  {
  if(is.null(parameters[[i]]$formula))
  {
     design.matrix[[i]]=list()
  } else
  {    
#  Next, if share, combine full ddl
	fullddl=full.ddl[[names(parameters)[i]]]
	if(!is.null(parameters[[i]]$share)&&parameters[[i]]$share)
	{	  
		  shared_par=parameters[[i]]$pair
		  dim1=dim(fullddl)
		  dim2=dim(full.ddl[[shared_par]])
		  fullddl=rbind(fullddl,full.ddl[[shared_par]])
		  fullddl[shared_par]=c(rep(0,dim1[1]),rep(1,dim2[1]))
		  row.names(fullddl)=1:dim(fullddl)[1]
	  } 
#
#    Calculate number of parameters for this type
#
     parx=names(parameters)[i]
     npar=dim(ddl[[parx]])[1]
#
#       Compute design matrix with model.matrix
#         31 Jan 06; made change to allow for NA or missing design data
#
        if(!is.null(parameters[[parx]]$contrasts))
           dm=model.matrix(parameters[[parx]]$formula,ddl[[parx]],contrasts.arg=parameters[[parx]]$contrasts)
        else
          dm=model.matrix(parameters[[parx]]$formula,ddl[[parx]])
#
#       In cases with nested interactions it is necessary to remove the intercept
#       to avoid over-parameterizing the model; this is user-specified
#
        if(!is.null(parameters[[parx]]$remove.intercept)&&parameters[[parx]]$remove.intercept)
        {
            intercept.column=(1:dim(dm)[2])[colSums(dm)==dim(dm)[1]]
            if(length(intercept.column)==0)
               stop("\nIntercept column not found.  Do not use ~-1 with remove.intercept\n")
            else
            {
               if(length(intercept.column)==1)
                  dm=dm[,-intercept.column]
            }
        }
        maxpar=dim(fullddl)[1]
#		Create a complete design matrix using full ddl
        design.matrix[[i]]=matrix(0,ncol=dim(dm)[2],nrow=maxpar)
#       Using row numbers fill in the dm for rows design data; this handles deleted design data
		if(dim(design.matrix[[i]][as.numeric(row.names(ddl[[parx]])),,drop=FALSE])[1]==dim(dm)[1])
           design.matrix[[i]][as.numeric(row.names(ddl[[parx]])),]=dm
        else
           stop(paste("\nProblem with design data. It appears that there are NA values in one or more variables in design data for ",parx,"\nMake sure any binned factor completely spans range of data\n",sep=""))
        colnames(design.matrix[[i]])=colnames(dm)
#
#       It appears that model.matrix can add unneeded columns to the design matrices
#       It can add interactions that are not relevant.  The results are columns in the design
#       matrix that are all zero.  These are stripped out here.
#
        col.sums=apply(design.matrix[[i]],2,function(x) sum(abs(x)))
        design.matrix[[i]]=design.matrix[[i]][,col.sums!=0,drop=FALSE]
#
#       Next substitute variable names for covariates into design matrix 
#
        if(length(xcov[[parx]])!=0)
          for(j in 1:length(xcov[[parx]]))
          {
             which.cols=NULL
             cnames=colnames(design.matrix[[i]])
#
#            fix for v1.6.2 needed to delete [ or ] or ( or ) or , from the names
#            to use the all.vars command.  These are created in using the cut command
#            on a numeric variable.
#
             cnames=sub("\\(","",cnames)
             cnames=sub("\\)","",cnames)
             cnames=sub("\\[","",cnames)
             cnames=sub("\\]","",cnames)
             cnames=sub(",","",cnames)
             for(k in 1:dim(design.matrix[[i]])[2])
                if(xcov[[parx]][j]%in%all.vars(formula(paste("~",cnames[k],sep=""))))
                     which.cols=c(which.cols,k)
             if(length(which.cols)>0)
             for(k in which.cols)
               if(all(design.matrix[[i]][,k]==1 | design.matrix[[i]][,k]==0))
                  if(time.dependent[[parx]][j])
                     if(!is.null(fullddl$session))
                        if(session.dependent[[parx]][j])
                            design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=
                                paste(xcov[[parx]][j],as.character(fullddl$session[design.matrix[[i]][,k]==1]),sep="")
                        else
                           if(!data$model%in%c("RDMultScalOcc"))
                              design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=
                                   paste(xcov[[parx]][j],as.character(fullddl$session[design.matrix[[i]][,k]==1]),
                                     as.character(fullddl$time[design.matrix[[i]][,k]==1]),sep="")
                           else
                             # This model was split off to use primary instead of time; but changed back. Could be deleted
                             design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=
                                    paste(xcov[[parx]][j],as.character(fullddl$session[design.matrix[[i]][,k]==1]),
                                    as.character(fullddl$time[design.matrix[[i]][,k]==1]),sep="")
                     else   
                        design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=paste(xcov[[parx]][j],as.character(fullddl$time[design.matrix[[i]][,k]==1]),sep="")
                  else
                     design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=xcov[[parx]][j]
               else
                  if(time.dependent[[parx]][j])
                  {
                     if(!is.null(fullddl$session))
                     {
                        if(session.dependent[[parx]][j])
                           design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                              paste("product(",paste(xcov[[parx]][j],as.character(fullddl$session[design.matrix[[i]][,k]!=0]),sep=""),
                                       ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
                        else
                           design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                              paste("product(",paste(xcov[[parx]][j],as.character(fullddl$session[design.matrix[[i]][,k]!=0]),
                                      as.character(fullddl$time[design.matrix[[i]][,k]!=0]),sep=""),
                                       ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")                     
                     }
                     else
                        design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                           paste("product(",paste(xcov[[parx]][j],as.character(fullddl$time[design.matrix[[i]][,k]!=0]),sep=""),
                                    ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
                  }
                  else
                     design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                        paste("product(",xcov[[parx]][j],",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
          }

        row.names(design.matrix[[i]])=NULL
     design.matrix[[i]]=as.data.frame(design.matrix[[i]],stringsAsFactors=FALSE)
     if(parameters[[i]]$formula=="~1")
        names(design.matrix[[i]])[1]="(Intercept)"
     names(design.matrix[[i]])=paste(names(parameters)[i],names(design.matrix[[i]]),sep=":")
  } 
  }
  names(design.matrix)=names(parameters)
#
# Merge to create a single design matrix
#
  complete.design.matrix=NULL
  nrows=0
  lastpim=length( pim[[length(parameters)]])
  lastindex=sum(sapply(full.ddl[1:length(parameters)],nrow))
#  lastindex=max(pim[[length(parameters)]][[lastpim]]$pim)
  for(i in 1:length(parameters))
  {
	 # parameters with NULL formula have been merged with a shared parameter
     if(!is.null(parameters[[i]]$formula))
     {    
        mat=NULL
		pair=parameters[[i]]$pair
		if(!is.null(pair) && pair !="" && !is.null(parameters[[pair]]$share) && parameters[[pair]]$share)
		{
			minrow=pim[[names(parameters)[i]]][[1]]$pim[1,1]
			maxrow=max(pim[[names(parameters)[i]]][[length(pim[[names(parameters)[i]]])]]$pim)
			if(minrow>1)
				mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=minrow-1)
			mat=rbind(mat,as.matrix(design.matrix[[i]]))
			if(i<length(parameters))
				mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-maxrow ))
		} else
        {    
           if(i>1)
              mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=nrows)
           mat=rbind(mat,as.matrix(design.matrix[[i]]))
           nrows=dim(mat)[1]
           if(i<length(parameters))
              mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-nrows ))
        }
        names(mat)=names(design.matrix[[i]])
        complete.design.matrix=cbind(complete.design.matrix,mat)
     }
  }
  row.names(complete.design.matrix)=1:dim(complete.design.matrix)[1]
  complete.design.matrix=as.data.frame(complete.design.matrix,stringsAsFactors=FALSE)
#
#  If there are any initial values, output them to the MARK input file
#  after making sure that the vector length matches the number of parameters  
#
   if(!is.null(initial))
   {
#
#     If a vector of values was given check to make sure it is of the correct length and then output
#
      if(is.vector(initial))
      {
         if(length(initial)==dim(complete.design.matrix)[2])
            initial.values=initial
         else
         {
            if(length(initial)==1)
               initial.values=rep(initial,dim(complete.design.matrix)[2])
            else
            {
                if(length(names(initial))==0)
				{
				  message("\nLength of initial vector doesn't match design matrix: ",ncol(complete.design.matrix)," \n")
				  print(colnames(complete.design.matrix))
                  stop()
			    }else
				{
					beta.index=match(names(complete.design.matrix),names(initial))
					initial.values=rep(0,dim(complete.design.matrix)[2])
					initial.values[!is.na(beta.index)]=initial[beta.index[!is.na(beta.index)]]				
				}
            }
         }
      } 
      else
#
#     If it was a MARK object; check to make sure it has output and then use the betas from the other object
#     2 May 06 jll; use names instead of values in design matrix to match initial values
#
      {
         if(inherits(initial,"mark"))
         {
            initial=RMark:::load.model(initial)
            if(!is.null(initial$output))
            {
               beta.index=match(names(complete.design.matrix),colnames(initial$design.matrix))
               initial.values=rep(0,dim(complete.design.matrix)[2])
               initial.values[!is.na(beta.index)]=initial$results$beta$estimate[beta.index[!is.na(beta.index)]]
            }
         }
      }
      if(simplify)
		  string=paste("XXXinitialXXX ",paste(initial.values,collapse=" "),";")
	  else
		  string=paste("initial ",paste(initial.values,collapse=" "),";")
	  write(string,file=outfile,append=TRUE)
   }
#
#  If model will not be simplified, output design matrix to the MARK input file
#
  if(!simplify)
  {
    string=paste("design matrix constraints=",dim(complete.design.matrix)[1], " covariates=",dim(complete.design.matrix)[2],";",sep="")
    write(string,file=outfile,append=TRUE)
    write.table(complete.design.matrix,file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  }
#
#  If parm-specific links, output them here
# 
  mlogit.list=list(structure=NULL,ncol=1)
  if(is.null(input.links))
  {
     max.logit.number=0
     if(link=="Parm-Specific")
     {
        string=NULL
        for(i in 1:length(parameters))
        {
           parx=names(parameters)[i]
           if(parameters[[i]]$link=="mlogit"|parameters[[i]]$link=="MLogit")
           {
             if(parx=="Psi")
             {
                 logit.numbers = max.logit.number+1:(nrow(full.ddl[[parx]])/(nstrata*(nstrata-1)*number.of.groups))
                 logits.per.group=nstrata*length(logit.numbers)
                 for (k in 1:number.of.groups)
                 {
                    if(k>1)logit.numbers=logit.numbers+logits.per.group
                    for (j in 1:nstrata)	 
                      string=c(string,paste("mlogit(",rep(logit.numbers+(j-1)*length(logit.numbers),(nstrata-1)),")",sep="")) 
			           }
			           max.logit.number=max.logit.number+logits.per.group*number.of.groups
             } 
             if(parx%in% c("pent","alpha"))
             {
                nsets=length(pim[[parx]])
                for (kk in 1:nsets)
                {
                   x.indices=as.vector(t(pim[[parx]][[kk]]$pim))
                   x.indices=x.indices[x.indices!=0]
                   max.logit.number=max.logit.number+1
                   string=c(string,paste("mlogit(",rep(max.logit.number,length(x.indices)),")",sep=""))
                 }
              }
              if(parx %in% c("pi","Omega"))
              { 
	                if(is.null(data$events)) 
	                    number.of.events=1
                  else
	                    number.of.events=length(data$events)
                  for(kkk in 1:number.of.events)
	                for (kk in 1:number.of.groups)
	                {
		   	            logit.numbers=max.logit.number+rep(1:(nrow(full.ddl[[parx]])/(number.of.events*number.of.groups*(nstrata-1))),nstrata-1)
			              max.logit.number=max(logit.numbers)
			              string=c(string,paste("mlogit(",logit.numbers,")",sep=""))
	                }			 				 
               }
				       if(parx=="Delta"){
				              if(is.null(data$events)) 
				                  number.of.events=1
				              else
				                  number.of.events=length(data$events)
				              for (kk in 1:number.of.groups)
				              {
				                 logit.numbers=max.logit.number+rep(1:(nrow(full.ddl[[parx]])/(number.of.events*number.of.groups)),number.of.events)
				                 max.logit.number=max(logit.numbers)
				                 string=c(string,paste("mlogit(",logit.numbers,")",sep=""))
				              }			 				 
				       }
               if(parx=="p" & data$model=="RDMSOccupancy")
				       {
				              subp=subset(full.ddl[[parx]],select=c("session","time","tostratum","group"))
				              uniquevals=apply(unique(subp),1,paste,collapse="")
				              allvals=apply(subp,1,paste,collapse="")
				              new.indices=match(allvals, uniquevals)
				              new.indices=new.indices+max.logit.number
				              for (k in 1:length(new.indices))
			                    string=c(string,paste("mlogit(",new.indices[k],")",sep="")) 
				              max.logit.number=max.logit.number+max(new.indices)
				       }
				       if(!parx%in%c("Psi","pent","alpha","pi","Omega","Delta")&!(parx=="p" & data$model=="RDMSOccupancy"))
				              stop(paste("Mlogit link not allowed with parameter",parx))
            } else
            {
              xstring=rep(spell(parameters[[i]]$link),dim(full.ddl[[parx]])[1])
              string=c(string,xstring)
            }
        }
   	    write(paste("links=",length(string),";",sep=""),file=outfile,append=TRUE)
        links=string
        string=paste(string,";")
        write(string,file=outfile,append=TRUE)
     } else
     {
		 links=link
     }
  }	else
  {
	  string=paste(input.links,collapse=",")
	  write(paste("links=",length(string),";",sep=""),file=outfile,append=TRUE)
	  links=input.links
	  string=paste(string,";")
	  write(string,file=outfile,append=TRUE)
  }  
#
# write out labels for design matrix columns
#
  for(i in 1:dim(complete.design.matrix)[2])
  {
     string=paste("blabel(",i,")=",colnames(complete.design.matrix)[i],";",sep="")
     write(string,file=outfile,append=TRUE)
  }
#
# write out labels for real parameters
#
  labstring=NULL
  rnames=NULL
  ipos=0
  for(i in 1:length(parameters))
  {
      parx=names(parameters)[i]
      plimit=dim(full.ddl[[parx]])[1]
      stratum.strings=rep("",plimit)
      if(!is.null(full.ddl[[parx]]$stratum)) stratum.strings=paste(" s",full.ddl[[parx]]$stratum,sep="")
      if(!is.null(full.ddl[[parx]]$tostratum)) stratum.strings=paste(stratum.strings," to",full.ddl[[parx]]$tostratum,sep="")
      strings=paste(param.names[i],stratum.strings," g",full.ddl[[parx]]$group,sep="")
      if(data$reverse)
	  {
		  if(!is.null(full.ddl[[parx]]$cohort))strings=paste(strings," c",full.ddl[[parx]]$cohort,sep="")
		  if(!is.null(full.ddl[[parx]]$occ.cohort))strings=paste(strings," c",full.ddl[[parx]]$occ.cohort,sep="")
	  } else {
		  if(!is.null(full.ddl[[parx]]$cohort))
			  strings=paste(strings," c",full.ddl[[parx]]$cohort,sep="")
		  else
		  if(!is.null(full.ddl[[parx]]$occ.cohort))strings=paste(strings," c",full.ddl[[parx]]$occ.cohort,sep="")
	  }  
	  if(!is.null(full.ddl[[parx]]$age))strings=paste(strings," a",full.ddl[[parx]]$age,sep="")
	  if("occ"%in%names(full.ddl[[parx]]))strings=paste(strings," o",full.ddl[[parx]]$occ,sep="")
	  if(model.list$robust && parameters[[parx]]$secondary)
         strings=paste(strings," s",full.ddl[[parx]]$session,sep="")
      if(!is.null(full.ddl[[parx]]$time))strings=paste(strings," t",full.ddl[[parx]]$time,sep="")
      if(mixtures >1 && !is.null(parameters[[i]]$mix) &&parameters[[i]]$mix)
         strings=paste(strings," m",full.ddl[[parx]]$mixture,sep="")
      if(!is.null(full.ddl[[parx]]$event))strings=paste(strings," e",full.ddl[[parx]]$event,sep="")
      rnames=c(rnames,strings)
      if(!simplify)
      {
         strings=paste("rlabel(",ipos+1:plimit,")=",strings,";",sep="")
         labstring=c(labstring,strings)
         ipos=ipos+plimit
      }
  }
  if(any(duplicated(rnames))) stop("Contact package maintainer. Following row names are duplicated:",paste(rnames[duplicated(rnames)],collapse=" "))
  if(!simplify) write(labstring,file=outfile,append=TRUE)
  row.names(complete.design.matrix)=rnames
#
# Write out Proc stop statement
#
  write("proc stop;",file=outfile,append=TRUE)
  close(outfile)
  outfile=file(tempfilename,open="rt")
  text=readLines(outfile)
  close(outfile)
  unlink(tempfilename)
  if(mixtures==1)
     mixtures=NULL
  if(is.null(call))call=match.call()
  model = list(data = substitute(data), model = data$model,
        title = title, model.name = model.name, links = links, mixtures=mixtures,
        call = call, parameters=parameters,time.intervals=data$time.intervals, input = text, number.of.groups = number.of.groups,
        group.labels = group.labels, nocc = nocc, begin.time = data$begin.time, covariates=covariates,
        fixed=fixedvalues,design.matrix = complete.design.matrix, pims = pim,
        design.data = full.ddl,strata.labels=data$strata.labels,mlogit.list=mlogit.list)
  if(model.list$robust)model$nocc.secondary=nocc.secondary
#
#  If requested, simplify model which reconstructs PIMS, design matrix and rewrites the
#  MARK input file for the simplified model.
#
  model$profile.int=profile.int
  model$chat=chat
# Assign Mlogits that are set to a fixed value to a Logit link so they can be simplified
  if(mlogit0)
  {
	  fixedvalue=model$fixed$index
	  mlogit.indices=grep("mlogit",model$links)
	  if(length(mlogit.indices)>0 & length(fixedvalue)>0)
		  model$links[fixedvalue[fixedvalue%in%mlogit.indices]]="Logit"
  }

# Simplify the pim structure
  if(simplify) model=simplify.pim.structure(model)
#
# Check to make sure that the only rows in the design matrix that are all zeros are
# ones that correspond to fixed parameters.
#
  if(simplify)
  {                      
      dm=model$simplify$design.matrix
      fixed.rows=unique(model$simplify$pim.translation[model$fixed$index])
      zero.rows=(1:dim(dm)[1])[apply(dm,1,function(x) return(all(x=="0")))]
      if(length(fixed.rows)==0)
      {
         if(length(zero.rows)!=0)
            stop("One or more formulae are invalid because the design matrix has all zero rows for the following non-fixed parameters\n",
                  paste(row.names(dm)[zero.rows],collapse=","))
      }
      else
      {
         if(any(! (zero.rows%in%fixed.rows)))
            stop("One or more formulae are invalid because the design matrix has all zero rows for the following non-fixed parameters\n",
               paste(row.names(dm)[zero.rows][!(zero.rows %in% fixed.rows)],collapse=","))
      }
  }
#
#  Check to make sure that any parameter that used a sin link has an identity design matrix
#
  if(simplify)
  {
     for(i in 1:length(parameters))
     {
        parx=names(parameters)[i]
        if(model$parameters[[parx]]$link=="sin")
        {
           dm=model$simplify$design.matrix
           rows=unique(model$simplify$pim.translation[sort(unique(as.vector(unlist(sapply(model$pims[[parx]],function(x)x$pim[x$pim>0])))))])
           if(length(grep('[[:alpha:]]',as.vector(dm[rows,,drop=FALSE])))>0)
              stop("\nCannot use sin link with covariates")
           dm=suppressWarnings(matrix(as.numeric(dm),nrow=dim(dm)[1],ncol=dim(dm)[2]))
           if(any(rowSums(dm[rows,,drop=FALSE])>1) | any(colSums(dm[rows,,drop=FALSE])>1))
              stop("\nCannot use sin link with non-identity design matrix")
        }
     }
  }
#
#  check.mlogits(model)
#  model$mlogit.structure=NULL
  if(!is.null(model$simplify$links))
  {
     newlinks=model$simplify$links
     model$simplify$links=model$links
     model$links=newlinks
  }
  class(model)=c("mark",data$model)
  return(model)
}
