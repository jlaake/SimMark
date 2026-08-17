#' CJS Simulation Examples
#' @name SimCJS
#' @author Jeff Laake  
#' @examples
#' {
#'# CJS 4 occasions, 3 groups, Phi(.)p(.) model; specify beta values for simulation
#'nocc=4
#'ngroups=3
#'simdata=create.data("CJS",nocc,ngroups)
#'# design matrix formulas Phi(.)p(.)
#'dm.formulas=list(Phi=list(formula=~1),p=list(formula=~1)) 
#'beta.values=list(Phi=1,p=-.3) # parameters in list can be in any order
#'                              # length of values must match number of columns in design matrix
#' # for CJS releases are a matrix with dimensions nocc-1 and ngroups
#'release.values=matrix(c(rep(500,nocc-1),rep(200,nocc-1),rep(400,nocc-1)),ncol=ngroups) 
#'# run simulations
#'mod=simmark(simdata,model.parameters=dm.formulas,releases=release.values,numsims=1,
#'    beta=beta.values,filename="simresults.bin")
#'# show beta.values used to simulate data
#'mod$beta.values
#'#show simulation results
#'mod$simresults
#'
#'# CJS 6 occasions, 1 group, Phi(.)p(time) model; specify real values
#'nocc=6
#'ngroups=1
#'simdata=create.data("CJS",nocc,ngroups)
#'# design matrix formulas Phi(.)p(time); -1+time makes each beta time separate for identity matrix
#'dm.formulas=list(Phi=list(formula=~1),p=list(formula=~-1+time))  
#'real.values=list(Phi=.7,p=c(0.1,0.3,0.4,0.05,0.7)) # parameters in list can be in any order
#'                                                   # length of values must match number of 
#'                                                   # rows(reals) in design matrix
#' # for CJS releases are a matrix with dimensions nocc-1 and ngroups
#'release.values=matrix(c(rep(500,nocc-1)),ncol=ngroups) # release values for 1 group
#'# run simulations
#'mod=simmark(simdata,model.parameters=dm.formulas,releases=release.values,numsims=1,
#'    real=real.values,filename="simresults.bin")
#'# show real values used for simulation
#'mod$real.values
#'#show simresults
#'mod$simresults
#' }
NULL

#' Multistate Simulation Examples
#' @name SimMS
#' @author Jeff Laake  
#' @examples
#' {
#'# multistrata 5 occasions, 1 group, 3 states 
#'#  S(stratum)p(stratum)Psi(stratum:tostratum) model; specify beta values
#'nocc=5
#'ngroups=1
#'nstates=3
#'# create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'# use make.design.data to specify constant PIM for Psi
#'ddl=make.design.data(simdata,parameters=list(Psi=list(pim.type="constant")))
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(c(1000,0,0,0),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                 Psi=list(formula=~-1+stratum:tostratum))
#'beta.values=list(S=rep(.84,nstates),p=rep(0,nstates),Psi=rep(-2,nstates*(nstates-1)))
#'mod=simmark(simdata,model.parameters=dm.formulas,releases=releases,numsims=1,
#'             beta=beta.values,param.link="logit",filename="simresults.bin")
#'#show betas used for simulation
#'mod$beta.values
#'mod$simresults
#'
#'
#'# multistrata 5 occasions, 2 groups, 5 states 
#'# S(-1+stratum)p(-1+stratum)Psi(-1+group:stratum:tostratum) model; specify beta values
#'# allow parm-specific for link
#'nocc=5
#'ngroups=2
#'nstates=5
#'#create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'ddl=make.design.data(simdata,parameters = list(Psi=list(pim.type="constant")))
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(rep(c(1000,0,0,0),ngroups),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                    Psi=list(formula=~-1+group:stratum:tostratum))
#'beta.values=list(S=rep(0.8473,nstates),p=rep(0,nstates),Psi=rep(-2,ngroups*nstates*(nstates-1)))
#'mod=simmark(simdata,ddl,model.parameters=dm.formulas,
#'    releases=releases,numsims=1,beta=beta.values,filename="simresults.bin")
#'mod$beta.values
#'mod$simresults
#' 
#' 
#'# multistrata 5 occasions, 2 groups, 3 states 
#'# S(-1+stratum)p(-1+stratum)Psi(-1+stratum:tostratum) model; specify beta values
#'# use logit link for Psi rather than mlogit
#'nocc=5
#'ngroups=2
#'nstates=5
#'#create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(rep(c(1000,1000,1000,1000),ngroups),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                Psi=list(formula=~-1+stratum:tostratum,link="logit"))
#'beta.values=list(S=rep(0.8473,nstates),p=rep(0,nstates),Psi=rep(-2,nstates*(nstates-1)))
#'mod=simmark(simdata,model.parameters=dm.formulas,
#'    releases=releases,numsims=1,beta=beta.values,filename="simresults.bin")
#'mod$beta.values
#'mod$simresults
#' }
NULL


#' Simulation Example Using RMark
#' @name SimtoRMark
#' @author Jeff Laake  
#' @examples
#' {
#'# CJS 4 occasions, 3 groups, Phi(.)p(.) model; specify beta values for simulation
#'nocc=4
#'ngroups=3
#'simdata=create.data("CJS",nocc,ngroups)
#'# design matrix formulas Phi(.)p(.)
#'dm.formulas=list(Phi=list(formula=~1),p=list(formula=~1)) 
#'beta.values=list(Phi=1,p=-.3) # parameters in list can be in any order
#'                              # length of values must match number of columns in design matrix
#' # for CJS releases are a matrix with dimensions nocc-1 and ngroups
#'release.values=matrix(c(rep(500,nocc-1),rep(200,nocc-1),rep(400,nocc-1)),ncol=ngroups) 
#'# run simulations with 10 reps
#'reals=list(length(10))
#'for(i in 1:10)
#'{
#'  mod=simmark(simdata,model.parameters=dm.formulas,releases=release.values,numsims=1,
#'              beta=beta.values,filename="simresults.bin", options=c("nodetail simdata=data.inp"))
#'  data=convert.inp("data.inp",group.df=data.frame(group=1:ngroups))
#'  mod=mark(data,model="CJS",model.parameters=list(Phi=list(formula=~1),p=list(formula=~1)),options="batch")
#'  reals[[i]]=mod$results$real
#'}
#'reals
#'}
NULL
