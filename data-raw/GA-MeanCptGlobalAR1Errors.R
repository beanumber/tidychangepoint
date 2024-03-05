## Central England 
## Data Analysis for Prague
## Journal of Climate
## Author: Xueheng Shi
## Date: 10/30/2020

rm(list = ls())


library(GA)


##Objective Function for GA search
##loc.ind is the binary input of length N
# Note X[1] cannot be a changepoint,
# so loc.ind[1] is set to be zero
# loc.ind = 0/1. 1 corresponds a CPT location, 0 is not.
meanshift.BIC = function(loc.ind, Xt=xt){
  loc.ind[1]=0
  N = length(Xt) #length of the series
  m = sum(loc.ind) #Number of CPTs
  
  if(m == 0){
    ##Case 1, Zero Changepoint
    mu.hat = mean(Xt)
    phi.hat = sum((Xt-mu.hat)[-N]*(Xt-mu.hat)[-1])/sum((Xt-mu.hat)[-1]^2)
    Xt.hat = c(mu.hat, mu.hat + phi.hat*(Xt[-N]-mu.hat))
    sigma.hatsq = sum( (Xt-Xt.hat)^2 )/N
    BIC.obj = N*log(sigma.hatsq)+N+N*log(2*pi)+ 3*log(N) #1mean,1var,1autocorrelation
  }
  else{
    tau.vec = loc.ind*(1:N) #convert binary to CPT location
    tau = tau.vec[tau.vec>0] #keep CPT locations only
    tau.ext = c(1,tau,(N+1)) #include CPT boundary 1 and N+1
    
    ## Split Xt to regimes/segments to
    ## compute phi.hat and sigma.hat.sq
    seg.len = diff(tau.ext) #length of each segments
    ff = rep(0:m, times=seg.len) ##create factors for segmentation
    Xseg = split(Xt, ff) ##Segmentation list
    mu.seg = unlist(lapply(Xseg,mean), use.names=F)
    mu.hat = rep(mu.seg, seg.len)
    phi.hat = sum((Xt-mu.hat)[-N]*(Xt-mu.hat)[-1])/sum((Xt-mu.hat)[-1]^2)
    Xt.hat = c(mu.hat[1], mu.hat[-1] + phi.hat*(Xt[-N]-mu.hat[-N]))
    sigma.hatsq = sum( (Xt-Xt.hat)^2 )/N
    BIC.obj = N*log(sigma.hatsq) +N+N*log(2*pi)+(2*m + 3)*log(N)
  }
  return(-BIC.obj)
}




#################### Data Analysis ###################

CentralEngland.Yr.Temp = read.csv(file='data-raw/CentralEnglandT1659-2020_yearly.csv', header=T)
#View(CentralEngland.Yr.Temp)

Yr.Temp = ts(CentralEngland.Yr.Temp[ ,2], frequency = 1, start=1659, end=2020) 
# avg.temp = mean(Yr.Temp)
# pdf("CET_AvgTemp.pdf", width = 10, height = 5)
# plot(Yr.Temp, xlab="Year", ylab=TeX("Average Temperature (^oC)"))
# #segments(1659, avg.temp , 2020, avg.temp, col= "red", lwd=3)
# abline(v = 1772, col="red")
# text("1772", x = 1772, y = Yr.Temp[114]-1.4, srt = 0, pos = 1) # near top
# dev.off()


xt = CentralEngland.Yr.Temp[ ,2]
N = length(xt)
##362

BIC.out = GA::ga(type="binary", fitness = meanshift.BIC,
               nBits = N, maxiter = 500, run = 10000,
               popSize = 200, monitor = T)
summary(BIC.out)
##BIC Score: -652.6407
BICsol = (BIC.out@solution)[1, ] ##can be 2 sols since X[1] is free, take 1st sol
BICsol[1] = 0 
CPTNo.BIC = sum(BICsol) ##Number of CPTs

tBIC = as.vector( which(BICsol == 1) ) ##CPT location



