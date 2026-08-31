#' CJS Simulation Examples
#' @name SimCJS
#' @author Gary White 
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
#'    beta=beta.values,simfile="simresults.bin")
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
#'    real=real.values,simfile="simresults.bin")
#'# show real values used for simulation
#'mod$real.values
#'#show simresults
#'mod$simresults
#'
#'# Fixing a parameter example
#'
#'data(dipper)
#'mark(dipper,model="CJSRandom",
#'     model.parameters=list(Phi=list(formula=~-1+time,link="sin"),
#'                           sigmaphi=list(formula=~1,fixed=0), 
#'                           sigmap=list(formula=~1),p=list(formula=~-1+time,link="sin")))
#'
#'# CJSRandom 10 occasions, 2 groups, Phi(group*time)p(group*time) model
#'
#'# Example of fixing a parameter
#'nocc=10
#'ngroups=2
#'
#'simdata=create.data("CJSRandom",nocc,ngroups)
#'
#'ddl=make.design.data(simdata)
#'
#'ddl$sigmaphi
#'ddl$sigmap#'
#'ddl$sigmaphi$fix=0.0
#'ddl$sigmaphi
#'
#'mod=simmark(simdata,ddl=ddl,numsims=1,   
#'            releases=matrix(c(rep(1000,ngroups)),ncol=ngroups),                
#'            model.parameters=list(sigmaphi=list(formula=~1),
#'            Phi=list(formula=~-1+time:group,link="sin"),   
#'            sigmap=list(formula=~-1+group),p=list(formula=~-1+time:group,link="sin")),
#'            beta=list(Phi=rep(asin(0.9*2-1),(nocc-1)*ngroups),
#'            sigmap=log(c(0.5,1.0)),p=rep(asin(0.5*2-1),(nocc-1)*ngroups)),              
#'            filename="simresults.bin",silent=TRUE,invisible=TRUE)
#'nocc=10
#'ngroups=2
#'
#'simdata=create.data("CJSRandom",nocc,ngroups)
#'
#'mod=simmark(simdata,numsims=1,releases=matrix(c(rep(1000,ngroups)),ncol=ngroups), 
#'            model.parameters=list(sigmaphi=list(formula=~1,fixed=0),
#'            Phi=list(formula=~-1+time:group,link="sin"),  
#'            sigmap=list(formula=~-1+group),p=list(formula=~-1+time:group,link="sin")),
#'            beta=list(Phi=rep(asin(0.9*2-1),(nocc-1)*ngroups),
#'            sigmap=log(c(0.5,1.0)),p=rep(asin(0.5*2-1),(nocc-1)*ngroups)),
#'            filename="simresults.bin",silent=TRUE,invisible=TRUE)
#' }
NULL

#' Multistate Simulation Examples
#' @name SimMS
#' @author Gary White   
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
#'             beta=beta.values,param.link="logit",simfile="simresults.bin")
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
#'    releases=releases,numsims=1,beta=beta.values,simfile="simresults.bin")
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
#'    releases=releases,numsims=1,beta=beta.values,simfile="simresults.bin")
#'mod$beta.values
#'mod$simresults
#' }
NULL


#' Simulation Example Using RMark
#' @name SimtoRMark
#' @author Gary White   
#' @examples
#' {
#'# CJS 4 occasions, 3 groups, Phi(.)p(.) model; specify beta values for simulation
#'nocc=4
#'ngroups=3
#'nreps=10
#'simdata=create.data("CJS",nocc,ngroups)
#'# design matrix formulas Phi(.)p(.)
#'dm.formulas=list(Phi=list(formula=~1),p=list(formula=~1)) 
#'beta.values=list(Phi=1,p=-.3) # parameters in list can be in any order
#'                              # length of values must match number of columns in design matrix
#' # for CJS releases are a matrix with dimensions nocc-1 and ngroups
#'release.values=matrix(c(rep(500,nocc-1),rep(200,nocc-1),rep(400,nocc-1)),ncol=ngroups) 
#'# generate simulation data 
#'mod=simmark(simdata,model.parameters=dm.formulas,releases=release.values,numsims=nreps,
#'              beta=beta.values,simdata="data.inp", options=c("nodetail"),
#'              invisible=TRUE,silent=TRUE)
#'simdata=readSimData("data.inp",nreps=10,group.df=data.frame(group=1:ngroups))
#'reals=list(length(10))
#'for(i in 1:10)
#'{
#'  mod=mark(simdata[[i]],model="CJS",options="batch",
#'   model.parameters=list(Phi=list(formula=~1),p=list(formula=~1)))
#'  reals[[i]]=mod$results$real
#'}
#'reals
#'}
NULL

#' Occupancy Simulation Examples
#' @name SimOccupancy
#' @author Gary White   
#' @examples
#' {
#' 
# RDOccupPG
#'
#'nocc=40
#'ngroups=2
#'time.intervals=c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#'
#'simdata=create.data("RDOccupPG",nocc,ngroups,time.intervals=time.intervals)
#'
#'mod=simmark(simdata,numsims=1,releases=1000,
#'            model.parameters=list(Psi=list(formula=~-1+time:group),
#'            Gamma=list(formula=~-1+time:group),
#'              p=list(formula=~-1+session:time:group)),
#'              beta=list(Psi=rep(logit_from_real(0.65),simdata$nocc*ngroups),
#'              Gamma=rep(logit_from_real(0.6),(simdata$nocc-1)*ngroups),
#'                      p=rep(logit_from_real(0.4),nocc*ngroups)),
#'            simfile="occupancyresults.bin",silent=TRUE,invisible=TRUE)
#'
#'
#'# RDOccupPE
#'
#'nocc=40
#'ngroups=2
#'time.intervals=c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#'
#'simdata=create.data("RDOccupPE",nocc,ngroups,time.intervals=time.intervals)
#'
#'mod=simmark(simdata,numsims=1,releases=1000,
#'            model.parameters=list(Psi=list(formula=~-1+time:group),Epsilon=list(formula=~-1+time:group),
#'                                p=list(formula=~-1+session:time:group)),
#'            beta=list(Psi=rep(logit_from_real(0.65),simdata$nocc*ngroups),
#'            Epsilon=rep(logit_from_real(0.3),(simdata$nocc-1)*ngroups),
#'            p=rep(logit_from_real(0.4),nocc*ngroups)),
#'            simfile="occupancyresults.bin",silent=TRUE,invisible=TRUE)
#'
#'
#'# RDOccupEG
#'
#'nocc=40
#'ngroups=2
#'time.intervals=c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#'
#'simdata=create.data("RDOccupEG",nocc,ngroups,time.intervals=time.intervals)
#'
#'mod=simmark(simdata,numsims=1,releases=1000,
#'                    model.parameters=list(Psi=list(formula=~-1+group),
#'                                  Epsilon=list(formula=~-1+time:group),
#'                                  Gamma=list(formula=~-1+time:group),
#'                                  p=list(formula=~-1+session:time:group)),
#'                     beta=list(Psi=rep(logit_from_real(0.8),ngroups),
#'                               Epsilon=rep(logit_from_real(0.3),(simdata$nocc-1)*ngroups),
#'                               Gamma=rep(logit_from_real(0.6),(simdata$nocc-1)*ngroups),
#'                               p=rep(logit_from_real(0.4),nocc*ngroups)),
#'                     simfile="occupancyresults.bin",silent=TRUE,invisible=TRUE)
#'
#'
#'# Occupancy
#'
#'nocc=10
#'ngroups=2
#'
#'simdata=create.data("Occupancy",nocc,ngroups)
#'
#'mod=simmark(simdata,numsims=1,releases=matrix(c(rep(1000,ngroups)),ncol=ngroups),
#'            model.parameters=list(p=list(formula=~-1+time:group,link="sin"),
#'            Psi=list(formula=~-1+group,link="sin")),
#'            real=list(p=c(rep(0.4,nocc),rep(0.2,nocc)),Psi=c(0.6,0.8)),
#'            simfile="occupancyresults.bin",silent=TRUE,invisible=TRUE)
#' }
NULL

#' Robust Simulation Examples
#' @name SimRobust
#' @author Gary White   
#' @examples
#' {
#'# Robust Design Models
#'
#'# Robust
#'nocc=20
#'ngroups=1
#'time.intervals=c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#'simdata=create.data("Robust",nocc,ngroups,time.intervals=time.intervals)
#'simdata=create.data("RDHuggins",nocc,ngroups,time.intervals=time.intervals)
#'releases=1000
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(S=list(formula=~-1+time),
#'                                  p=list(formula=~-1+session:time,share=TRUE),
#'                                  GammaDoublePrime=list(formula=~-1+time,share=TRUE) ),
#'            beta=list(S=c(rep(logit_from_real(0.9),(simdata$nocc-1)*ngroups)),
#'                      p=c(rep(logit_from_real(0.6),nocc*ngroups)),
#'                      GammaDoublePrime=c(rep(logit_from_real(0.2),(simdata$nocc-1)*ngroups))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(S=list(formula=~-1+time),
#'                                  p=list(formula=~-1+session:time,share=TRUE),
#'                                  GammaDoublePrime=list(formula=~-1+time),
#'                                  GammaPrime=list(formula=~-1+time) ),
#'            beta=list(S=c(rep(logit_from_real(0.9),(simdata$nocc-1)*ngroups)),
#'                      p=c(rep(logit_from_real(0.6),nocc*ngroups)),
#'                      GammaDoublePrime=c(rep(logit_from_real(0.2),simdata$nocc-1)),
#'                      GammaPrime=c(rep(logit_from_real(0.2),simdata$nocc-2))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#' 
#'            
#'# RD Huggins
#'nocc=20
#'ngroups=1
#'time.intervals=c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0)
#'simdata=create.data("RDHuggins",nocc,ngroups,time.intervals=time.intervals)
#'releases=1000
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(S=list(formula=~-1+time),
#'                                  p=list(formula=~-1+session:time,share=TRUE),
#'                                  GammaDoublePrime=list(formula=~-1+time,share=TRUE) ),
#'            beta=list(S=c(rep(logit_from_real(0.9),(simdata$nocc-1)*ngroups)),
#'                      p=c(rep(logit_from_real(0.6),nocc*ngroups)),
#'                      GammaDoublePrime=c(rep(logit_from_real(0.2),(simdata$nocc-1)*ngroups))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(S=list(formula=~-1+time),
#'                                  p=list(formula=~-1+session:time,share=TRUE),
#'                                  GammaDoublePrime=list(formula=~-1+time),
#'                                  GammaPrime=list(formula=~-1+time) ),
#'            beta=list(S=c(rep(logit_from_real(0.9),(simdata$nocc-1)*ngroups)),
#'                      p=c(rep(logit_from_real(0.6),nocc*ngroups)),
#'                      GammaDoublePrime=c(rep(logit_from_real(0.2),simdata$nocc-1)),
#'                      GammaPrime=c(rep(logit_from_real(0.2),simdata$nocc-2))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'
#'# parmvals with real list -- will fail because of p session:time and sharing with c 
#'#                           because duplicate rows in design matrix
#'\dontrun{
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(S=list(formula=~-1+time),
#'                                  p=list(formula=~-1+session:time,share=TRUE),
#'                                  GammaDoublePrime=list(formula=~-1+time),
#'                                  GammaPrime=list(formula=~-1+time) ),
#'                                  real=list(S=c(rep(0.9,simdata$nocc-1)),p=c(rep(0.6,nocc*ngroups)),
#'                                  GammaDoublePrime=c(rep(0.2,simdata$nocc-1)),
#'                                  GammaPrime=c(rep(0.2,simdata$nocc-2))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'}
#'
#'}
NULL

#' Abundance Estimation Simulation Examples with Open Populations
#' @name SimOpenAbundance
#' @author Gary White   
#' @examples
#' {
#' # Pradrec  
#'
#'nocc=10
#'ngroups=2
#'#'
#'simdata=create.data("Pradrec",nocc,ngroups)
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+time:group,link="sin"),
#'                                  f=list(formula=~-1+group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),p=rep(0.6,nocc*ngroups),f=rep(0.1,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  f=list(formula=~-1+group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),p=rep(0.6,ngroups),f=rep(0.1,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  f=list(formula=~-1+time:group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      f=rep(0.1,(nocc-1)*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'            
#'# Pradlambda  
#'
#'nocc=10
#'ngroups=2
#'
#'simdata=create.data("Pradlambda",nocc,ngroups)
#'
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+time:group,link="sin"),
#'                                  Lambda=list(formula=~-1+group)),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,nocc*ngroups),
#'                      Lambda=rep(1.0,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  Lambda=list(formula=~-1+group)),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      Lambda=rep(1.0,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  Lambda=list(formula=~-1+time:group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      Lambda=rep(1.0,(nocc-1)*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'            
#'# Pradsen  
#'nocc=10
#'ngroups=2
#'simdata=create.data("Pradsen",nocc,ngroups)
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+time:group,link="sin"),
#'                                  Gamma=list(formula=~-1+group,link="sin")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,nocc*ngroups),
#'                      Gamma=rep(0.9,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  Gamma=list(formula=~-1+group,link="sin")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      Gamma=rep(0.9,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  Gamma=list(formula=~-1+time:group,link="sin")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      Gamma=rep(0.9,(nocc-1)*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'# Link-Barker
#'
#'nocc=10
#'ngroups=2
#'simdata=create.data("LinkBarker",nocc,ngroups)
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+time:group,link="sin"),
#'                                  f=list(formula=~-1+group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,nocc*ngroups),
#'                      f=rep(0.1,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  f=list(formula=~-1+group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),p=rep(0.6,ngroups),f=rep(0.1,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=1000,numsims=1,
#'            model.parameters=list(Phi=list(formula=~-1+time:group,link="sin"),
#'                                  p=list(formula=~-1+group,link="sin"),
#'                                  f=list(formula=~-1+time:group,link="log")),
#'            real=list(Phi=rep(0.9,(nocc-1)*ngroups),
#'                      p=rep(0.6,ngroups),
#'                      f=rep(0.1,(nocc-1)*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'}
NULL            


#' Abundance Estimation Simulation Examples with Closed Populations
#' @name SimClosedAbundance
#' @author Gary White   
#' @examples
#' {
#' # Closed
#' nocc=5
#' ngroups=2
#' 
#' #create dummy data
#' simdata=create.data("Closed",nocc,ngroups)
#' releases=1000
#' 
#'mod=simmark(simdata,releases=releases,numsims=1,
#'             model.parameters=list(p=list(formula=~-1+time,link="sin",share=TRUE),
#'                                   f0=list(formula=~-1+group)),
#'             real=list(p=rep(0.4,nocc),f0=rep(3,ngroups)),
#'             simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'             
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~-1+time:group,link="sin",share=TRUE),
#'                                 f0=list(formula=~-1+group)),
#'            real=list(p=rep(0.4,nocc*ngroups),f0=rep(3,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=releases,numsims=1,beta=list(p=0,c=0,f0=3),
#'                    simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'                    
#'                    
#'# Huggins
#'
#'nocc=10
#'ngroups=2
#'releases=1000
#'
#'#create dummy data
#'simdata=create.data("Huggins",nocc,ngroups)
#'
#'# p(g*t)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~-1+time:group,link="sin",share=TRUE)),
#'            beta=list(p=rep(asin(0.3*2-1),nocc*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'# p(t)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~-1+time,link="sin",share=TRUE)),
#'            beta=list(p=rep(asin(0.3*2-1),nocc)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'# p(g)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~-1+group,link="sin",share=TRUE)),
#'            beta=list(p=rep(asin(0.3*2-1),ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'            
#'# p(.)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~1,link="sin",share=TRUE)),
#'            beta=list(p=asin(0.3*2-1)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'# p(p,c,g)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(p=list(formula=~-1+group,link="sin"),
#'                                  c=list(formula=~-1+group,link="sin")),
#'            beta=list(p=rep(asin(0.3*2-1),ngroups),c=rep(-1,ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'            
#'# HugHet
#'
#'nocc=10
#'ngroups=2
#'#create dummy data
#'simdata=create.data("HugHet",nocc,ngroups,mixtures=2)
#'releases=1000
#'
#'# pi(g) p(g*mix)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(pi=list(formula=~-1+group),
#'                                  p=list(formula=~-1+mixture:group,link="sin")),
#'            beta=list(pi=rep(logit_from_real(0.4),ngroups),
#'                      p=rep(c(asin(0.3*2-1),asin(0.7*2-1)),ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(pi=list(formula=~-1+group),
#'                                  p=list(formula=~-1+mixture:group,link="sin")),
#'            beta=list(pi=c(logit_from_real(0.4),logit_from_real(0.3)),
#'                      p=c(asin(0.3*2-1),asin(0.7*2-1),asin(0.2*2-1),asin(0.8*2-1))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#'
#'# HugFullHet
#'nocc=10
#'ngroups=1
#'
#'#create dummy data
#'simdata=create.data("HugFullHet",nocc,ngroups,mixtures=2)
#'
#'releases=1000
#'
#'# pi(g) p(g*mix)
#'
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(pi=list(formula=~1),
#'                                  p=list(formula=~mixture+time,share=TRUE)),
#'            beta=list(pi=logit_from_real(0.6),
#'               p=c(logit_from_real(0.3),logit_from_real(0.5)-logit_from_real(.3),rep(0,nocc-1))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'   
#'   
#'# HugFullHet
#'nocc=10
#'ngroups=2
#'simdata=create.data("HugFullHet",nocc,ngroups,mixtures=2)
#'releases=1000
#'
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(pi=list(formula=~-1+group),
#'                                  p=list(formula=~-1+time:group+mixture:group,share=TRUE)),
#'            beta=list(pi=rep(logit_from_real(0.4),ngroups),
#'                      p=rep(c(logit_from_real(0.3),rep(0,nocc-1),
#'                              logit_from_real(0.5)-logit_from_real(0.3),
#'                              logit_from_real(0.6)-logit_from_real(0.3),rep(0,nocc-1),
#'                              logit_from_real(0.5)-logit_from_real(0.6)))),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#' 
#'            
#'# HugginsRE
#'nocc=10
#'ngroups=2
#'
#'#create dummy data
#'simdata=create.data("HugginsRE",nocc,ngroups)
#'
#'releases=1000
#'
#'# sigma(g) p(g*t)
#'mod=simmark(simdata,releases=releases,numsims=1,
#'            model.parameters=list(sigmap=list(formula=~-1+group),
#'                                  p=list(formula=~-1+time:group,link="sin",share=TRUE)),
#'            beta=list(sigmap=log(c(0.5,1)),p=rep(asin(0.3*2-1),nocc*ngroups)),
#'            simfile="simresults.bin",invisible=TRUE,silent=TRUE)
#'            
#' }
NULL




