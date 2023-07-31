rm(list=ls())

fname = 'C:/Users/a1065639/Work/RGN/paper/AUparam.RData'
load(fname)

nCat = length(LVAUparam$param)

muVec = betaVec = sigmaVec = rhoVec = c()
har.mu.mean = har.mu.amp = har.mu.phase.ang = c()
har.beta.mean = har.beta.amp = har.beta.phase.ang = c()
har.sigma.mean = har.sigma.amp = har.sigma.phase.ang = c()
har.rho.mean = har.rho.amp = har.rho.phase.ang = c()

#skip = c(which(muVec<(-50)),which(muVec>(5)))
skip = c(126,128,209,430,431,135,139,358)

catList = 1:nCat

catList = catList[!(catList%in%skip)]

for (cat in catList){

#  rho = LVAUparam$rho[[cat]]
#  paramMat = cbind(LVAUparam$param[[cat]][,1:3],rho)
  paramMat = LVAUparam$param[[cat]][,1:3]

  # mu

  mu = paramMat[,'mu']
  muVec = c(muVec,mu)

#  if (length(mu)!=12){browser()}
#  if (length(muVec)!=(12*cat)){browser()}

  har.mu = fit.harmonic.opts(nperiod = 12,v.stat = mu,k = 1)
  har.mu.mean = c(har.mu.mean,har.mu$mean)
  har.mu.amp = c(har.mu.amp,har.mu$amp)
  har.mu.phase.ang = c(har.mu.phase.ang,har.mu$phase.ang)

  # beta

  beta = paramMat[,'beta']
  betaVec = c(betaVec,beta)

  har.beta = fit.harmonic.opts(nperiod = 12,v.stat = beta,k = 1)
  har.beta.mean = c(har.beta.mean,har.beta$mean)
  har.beta.amp = c(har.beta.amp,har.beta$amp)
  har.beta.phase.ang = c(har.beta.phase.ang,har.beta$phase.ang)

  # sigma

  sigma = paramMat[,'sigma']
  sigmaVec = c(sigmaVec,sigma)

  har.sigma = fit.harmonic.opts(nperiod = 12,v.stat = sigma,k = 1)
  har.sigma.mean = c(har.sigma.mean,har.sigma$mean)
  har.sigma.amp = c(har.sigma.amp,har.sigma$amp)
  har.sigma.phase.ang = c(har.sigma.phase.ang,har.sigma$phase.ang)

  # rho

#  rho = paramMat[,'rho']
  rho = LVAUparam$rho[[cat]]
  rhoVec = c(rhoVec,rho)

  # har.rho = fit.harmonic.opts(nperiod = 12,v.stat = rho,k = 1)
  # har.rho.mean = c(har.rho.mean,har.rho$mean)
  # har.rho.amp = c(har.rho.amp,har.rho$amp)
  # har.rho.phase.ang = c(har.rho.phase.ang,har.rho$phase.ang)

}

probs = c(0.01,0.99)


ann.bounds.mu = quantile(muVec,p=probs)
ann.bounds.beta = quantile(betaVec,p=probs)
ann.bounds.sigma = quantile(sigmaVec,p=probs)
ann.bounds.rho = quantile(rhoVec,p=probs)

har.bounds.mu.mean = quantile(har.mu.mean,p=probs)
har.bounds.mu.amp = quantile(har.mu.amp,p=probs)
har.bounds.mu.phase.ang = quantile(har.mu.phase.ang,p=probs)

har.bounds.beta.mean = quantile(har.beta.mean,p=probs)
har.bounds.beta.amp = quantile(har.beta.amp,p=probs)
har.bounds.beta.phase.ang = quantile(har.beta.phase.ang,p=probs)

har.bounds.sigma.mean = quantile(har.sigma.mean,p=probs)
har.bounds.sigma.amp = quantile(har.sigma.amp,p=probs)
har.bounds.sigma.phase.ang = quantile(har.sigma.phase.ang,p=probs)


