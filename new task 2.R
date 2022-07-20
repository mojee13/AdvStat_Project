library(coda)
library("rjags")
set.seed(12345)
N <-3 #number of subgroups by age 
age <- c(1,2,3) #where 1 = 16-64, 2= 65-74, 3 = 75 and older
model_age <- "model{
#likeihood:
for(i in 1:length(age)){
  nP.I[i] ~ dbin(pA[i], nP[i])         
  nV.A[i] ~ dbin(pA[i], nV[i]) 
  nV.I[i] ~ dbin(prob[i], nV.A[i])     
}
# Priors
 
for(i in 1:length(age)){
pA[i] ~ dbeta(1,1)
prob[i] ~ dbeta(1,1) 
epsilon[i] <- 1 - prob[i]
}
}"
#pfizer
nvI.pf <-c(7,1,0)  
nv.pf <-c(13549,3074,774)
nPI.pf <-c(143,14,5)
nP.pf<-c(13618,3095,785)

#janssen
nvI.jc <-c(60,6,0)
nv.jc<-c(2518,477,106)
nPI.jc <- c(170,20,3)
nP.jc <- c(2490,482,98)
#moderna
nvI.md <- c(7,4,0)
nv.md <- c(2875,5586,0)
npI.md <- c(156,22,7)
np.md <- c(64625,31744,41968)
#total statistics by subroups
nvI <- nvI.jc+nvI.md + nvI.pf
nv <- nv.jc+nv.md + nv.pf
npI <- nPI.jc + npI.md + nPI.pf
np <- nP.jc + np.md + nP.pf
df
<- data.frame(nvI,nv,npI,np, age)
df
jags_data <- with(df, list(nV.I =nvI, nV = nv, nP.I = npI, nP = np , age=age))
jags_age <- jags.model(textConnection(model_age),data=jags_data,inits = list(.RNG.name = "base::Wichmann-Hill"))
df
sim <- coda.samples(model=jags_age,variable.names = c("epsilon","nV.A"),n.iter=100000)
summary(sim)
