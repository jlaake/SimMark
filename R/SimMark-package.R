#' CJS Simulation Examples
#' @name SimCJS
#' @author Jeff Laake  
#' @examples
#' {
#'# CJS 4 occasions, 3 groups, Phi(.)p(.) model
#'nocc=4
#'ngroups=3
#'simdata=create.data("CJS",nocc,ngroups)
#'
#'# design matrix formulas Phi(.)p(.)
#'dm.formulas=list(Phi=list(formula=~1),p=list(formula=~1)) 
#'beta.values=list(Phi=1,p=-.3) # parameters in list can be in any order
#'                              # length of values must match number of columns in design matrix
#' # for CJS releases are a matrix with dimensions nocc-1 and ngroups
#'release.values=matrix(c(rep(500,nocc-1),rep(200,nocc-1),rep(400,nocc-1)),ncol=ngroups) 
#'# run simulations
#'mod=simmark(simdata,model.parameters=dm.formulas,releases=release.values,numsims=1,
#'    beta=beta.values,filename="simresults.bin")
#'
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
#' }
NULL

#' Multistate Simulation Examples
#' @name SimMS
#' @author Jeff Laake  
#' @examples
#' {
#'# multistrata 5 occasions, 1 group, 3 states 
#'#  S(stratum)p(stratum)Psi(stratum:tostratum) model; specify real values
#'nocc=5
#'ngroups=1
#'nstates=3
#'# create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'# create design data so we can specify constant pims for Psi
#'ddl=RMark::make.design.data(simdata,parameters=list(Psi=list(pim.type="constant")))
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(c(1000,0,0,0),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                 Psi=list(formula=~-1+stratum:tostratum))
#'real.values=list(S=rep(.7,nstates),p=rep(.5,nstates),Psi=rep(0.2,nstates*(nstates-1)))
#'mod=simmark(simdata,ddl,model.parameters=dm.formulas,releases=releases,numsims=1,
#'             real=real.values,filename="simresults.bin")
#'
#'
#'# multistrata 5 occasions, 2 groups, 5 states 
#'# S(-1+stratum)p(-1+stratum)Psi(-1+group:stratum:tostratum) model; specify real values
#'nocc=5
#'ngroups=2
#'nstates=5
#'#create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'# create design data so we can specify constant pims for Psi
#'ddl=RMark::make.design.data(simdata,parameters=list(Psi=list(pim.type="constant")))
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(rep(c(1000,0,0,0),ngroups),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                    Psi=list(formula=~-1+group:stratum:tostratum))
#'real.values=list(S=rep(.7,nstates),p=rep(.5,nstates),
#'                Psi=rep(0.2,ngroups*nstates*(nstates-1)))
#'mod=simmark(simdata,ddl,model.parameters=dm.formulas,
#'    releases=releases,numsims=1,real=real.values,filename="simresults.bin")
#' 
#' 
#'# multistrata 5 occasions, 1 group, 3 states 
#'# S(-1+stratum)p(-1+stratum)Psi(-1+stratum:tostratum) model; specify beta values
#'nocc=5
#'ngroups=2
#'nstates=5
#'#create dummy data
#'simdata=create.data("Multistrata",nocc,ngroups,nstates)
#'# create design data so we can specify constant pims for Psi
#'ddl=RMark::make.design.data(simdata,parameters=list(Psi=list(pim.type="constant")))
#'# for MS model, releases are a 3-way array with dimensions nocc-1,nstates,ngroups
#'releases=array(rep(c(1000,1000,1000,1000),ngroups),dim=c(nocc-1,nstates,ngroups))
#'dm.formulas=list(S=list(formula=~-1+stratum),p=list(formula=~-1+stratum),
#'                Psi=list(formula=~-1+stratum:tostratum,link="logit"))
#'beta.values=list(S=rep(0.8473,nstates),p=rep(0,nstates),Psi=rep(-1.4,nstates*(nstates-1)))
#'mod=simmark(simdata,ddl,model.parameters=dm.formulas,
#'    releases=releases,numsims=1,beta=beta.values,filename="simresults.bin")
#' }
NULL


