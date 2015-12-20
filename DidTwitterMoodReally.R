##Did Twitter Mood Really Predict the DJIA?



## @knitr C3S3
#set seed for reproducibility
set.seed(7); library(PerformanceAnalytics); library(xlsxjars); library(xlsx)
library(quantmod);library(dynlm); library(lmtest); library(ggplot2); library(xlsx); library(zoo)
library(robustbase); library(Rsafd)
#read in data using quantmod

getSymbols("IVV", from = "2000-07-31", to = "2014-12-31") #calibrate using SP500
getSymbols("DJIA", from = "2000-07-31", to = "2014-12-31") #DJIA data
getSymbols("^IRX", from="2000-07-31") 
rf = (1+(Cl(IRX)/100))^(1/252) - 1; rf=rf[-1]
#convert Treasuries to daily yield

tail(rf); head(rf)

SP.r = log(lag(Ad(IVV))) - log(Ad(IVV)); head(SP.r)
SP.r = SP.r[-1]; head(SP.r)
vol.00 = sd(SP.r)*sqrt(252)
vol.06 = sd(SP.r["2006-07-15/2014-12-31"])*sqrt(252)

#calibrate model; sharpe bound
RRA = 6.4; SRB.00 = vol.00*RRA*max((rf+1)^252); SRB.00
SRB.06 = vol.06*RRA*max((rf+1)^252); SRB.06

#calculate DJIA returns/excess returns and S&P volatility
DJIA.r = log(lag(Ad(DJIA))) - log(Ad(DJIA)); DJIA.r = DJIA.r[-1]; tail(DJIA.r)
head(DJIA.r); summary(DJIA.r)

#sanity check, do our kappas match Pav's
DJIA.k = sum(DJIA.r^2)*length(DJIA.r)/(4*(sum(abs(DJIA.r)))^2); DJIA.k
SP.k = sum(SP.r^2)*length(SP.r)/(4*(sum(abs(SP.r)))^2); SP.k #check passed

#the best market timing strategy ever!!
g = 11/30; #define market predictability

#reproduce Pav
SR.Theory.Pav1 = (g/sqrt(0.59/253)) #actually he doesn't use his own approx
SR.Theory.Pav1

SR.Theory.Pav2 = (g/sqrt(0.59 - g^2))*sqrt(253) #he uses closed form
SR.Theory.Pav2 #what he shows on his website

#our theory

SR.Theory.RfEquals0 = (g/sqrt(DJIA.k - g^2))*sqrt(253)
SR.Theory.RfEquals0

#monte carlo simulations; first create Sharpe Ratios to be filled in
SR.RiskFreeEqZero = matrix(nrow=1000,ncol=11)
SR.RiskFreeEqZero.06 = matrix(nrow=1000,ncol=11)
SR.RiskFreeEqTBill = matrix(nrow=1000,ncol=11)
SR.RiskFreeEqTBill.06 = matrix(nrow=1000,ncol=11)

UpDownPredAbility = 13/15; 
DJIA.r.06 = DJIA.r["2006-07-15/2014-12-31"]

sim.Return = data.frame(DJIA.r); sim.Return.06 = data.frame(DJIA.r.06)
ExcessReturn = DJIA.r - rf; sim.ExcessReturn = data.frame(ExcessReturn)
ExcessReturn.06 = DJIA.r["2006-07-15/2014-12-31"] - rf["2006-07-15/2014-12-31"]
sim.ExcessReturns.06 = data.frame(ExcessReturn.06)

length(DJIA.r) - length(rf["2000-08-01/2014-12-31"]) #3 days of Treasury missing
#guarantee subtraction aligned
which(index(rf["2000-08-01/2014-12-31"]) != index(DJIA.r))[1]
length(ExcessReturn) # we lose three days of data from our excess returns object

set.seed(7); 
for (i in 1:1000){
  guess.RfEqZero.00 = (runif(length(DJIA.r)) < UpDownPredAbility)
  guess.RfEqTBill.00 = (runif(length(ExcessReturn)) < UpDownPredAbility)
  guess.RfEqZero.06 = (runif(length(DJIA.r.06))<UpDownPredAbility)
  guess.RfEqTBill.06 = (runif(length(ExcessReturn.06))<UpDownPredAbility)
  
  #first case risk-free equals 0; arb exists from 2000
  strat.RfEqZero.00 = abs(DJIA.r)*guess.RfEqZero.00
  strat.RfEqZero.00[which(guess.RfEqZero.00!=1)] =
    -abs(DJIA.r[which(guess.RfEqZero.00!=1)])
  sim.Return[,i] =  strat.RfEqZero.00
  
  #second case risk-free equals T Bill; arb exists from 2000
  strat.RfEqTBill.00 = abs(ExcessReturn)*guess.RfEqTBill.00
  strat.RfEqTBill.00[which(guess.RfEqTBill.00!=1)] =
    -abs(ExcessReturn[which(guess.RfEqTBill.00!=1)])
  sim.ExcessReturn[,i] =  strat.RfEqTBill.00
  
  #third case risk-free equals 0; arb exists from the development of Twitter
  strat.RfEqZero.06 = abs(DJIA.r.06)*guess.RfEqZero.06
  strat.RfEqZero.06[which(guess.RfEqZero.06!=1)] =
    -abs(DJIA.r.06[which(guess.RfEqZero.06!=1)])
  sim.Return.06[,i] =  strat.RfEqZero.06
  
  #fourth case risk-free equals TBill; arb after the development of Twitter
  strat.RfEqTBill.06 = abs(ExcessReturn.06)*guess.RfEqTBill.06
  strat.RfEqTBill.06[which(guess.RfEqTBill.06!=1)] =
    -abs(ExcessReturn.06[which(guess.RfEqTBill.06!=1)])
  sim.ExcessReturns.06[,i] =  strat.RfEqTBill.06
}

for(j in 0:10){
  temp1 = apply(sim.Return,2,"-",j/10000)
  temp2 = apply(sim.ExcessReturn,2,"-",j/10000)
  temp3 = apply(sim.Return.06,2,"-",j/10000)
  temp4 = apply(sim.ExcessReturn.06,2,"-",j/10000)
  
  for (i in 1:1000){
    SR.RiskFreeEqZero[i,j+1] = SharpeRatio.annualized(temp1[,i],
                                                         scale = 252, 
                                                         geometric = F)
    SR.RiskFreeEqTBill[i,j+1] = SharpeRatio.annualized(temp2[,i],
                                                       scale = 252, 
                                                       geometric = F)
    SR.RiskFreeEqZero.06[i,j+1] = SharpeRatio.annualized(temp3[,i],
                                                         scale = 252, 
                                                         geometric = F)
    SR.RiskFreeEqTBill.06[i,j+1] = SharpeRatio.annualized(temp4[,i],
                                                          scale = 252, 
                                                          geometric = F)
  }
}

apply(SR.RiskFreeEqZero, 2, mean); min(SR.RiskFreeEqZero)
apply(SR.RiskFreeEqTBill, 2, mean); min(SR.RiskFreeEqTBill)
apply(SR.RiskFreeEqZero.06, 2, mean); min(SR.RiskFreeEqZero.06)
apply(SR.RiskFreeEqTBill.06, 2, mean); min(SR.RiskFreeEqTBill.06)

#saner approach is to not be able to guess the excess return, but regular return and just subtract risk free rate
#afterwards we model this below; this is what appears in our tables

sim.ExcessReturn.p = data.frame(DJIA.r); set.seed(7); #set seed for reproducibility
sim.ExcessReturn.06.p = data.frame(DJIA.r.06)
for (i in 1:1000){
  guess.RfEqTBill.00.p = (runif(length(DJIA.r)) < UpDownPredAbility)
  guess.RfEqTBill.06.p = (runif(length(DJIA.r.06))<UpDownPredAbility)
  
  #first case risk-free equals T Bill; arb exists from 2000
  strat.RfEqTBill.00.p = abs(DJIA.r)*guess.RfEqTBill.00.p
  strat.RfEqTBill.00.p[which(guess.RfEqTBill.00.p!=1)] =
    -abs(DJIA.r[which(guess.RfEqTBill.00.p!=1)])
  sim.ExcessReturn.p[,i] =  strat.RfEqTBill.00.p
  
  #second case risk-free equals TBill; arb after the development of Twitter
  strat.RfEqTBill.06.p = abs(DJIA.r.06)*guess.RfEqTBill.06.p
  strat.RfEqTBill.06.p[which(guess.RfEqTBill.06.p!=1)] =
    -abs(DJIA.r.06[which(guess.RfEqTBill.06.p!=1)])
  sim.ExcessReturn.06.p[,i] =  strat.RfEqTBill.06.p
}

sim.ExcessReturn.p = 
  apply(sim.ExcessReturn.p[c(-1305,-1328,-2562),], 2, "-", coredata(rf["2000-08-01/2014-12-31"]))
sim.ExcessReturn.06.p = apply(sim.ExcessReturn.06.p[-1068,], 2, "-", coredata(rf["2006-07-15/2014-12-31"]))

sim.ExcessReturn.p = as.xts(sim.ExcessReturn.p, order = index(rf["2000-08-01/2014-12-31"]))
sim.ExcessReturn.06.p = as.xts(sim.ExcessReturn.06.p, order = index(rf["2006-07-15/2014-12-31"]))


SR.RiskFreeEqTBill.p = matrix(nrow=1000,ncol=11)
SR.RiskFreeEqTBill.06.p = matrix(nrow=1000,ncol=11)

for(j in 0:10){
  temp1.p = apply(sim.ExcessReturn.p,2,"-",j/10000)
  temp2.p = apply(sim.ExcessReturn.06.p,2,"-",j/10000)  
  for (i in 1:1000){
    SR.RiskFreeEqTBill.p[i,j+1] = SharpeRatio.annualized(temp1.p[,i],
                                                       scale = 252, 
                                                       geometric = F)
    SR.RiskFreeEqTBill.06.p[i,j+1] = SharpeRatio.annualized(temp2.p[,i],
                                                          scale = 252, 
                                                          geometric = F)
  }
}

apply(SR.RiskFreeEqTBill.p, 2, mean); min(SR.RiskFreeEqTBill.p)
apply(SR.RiskFreeEqTBill.06.p, 2, mean); min(SR.RiskFreeEqTBill.06.p)

## @knitr mult
set.seed(10)
library(data.table)

#feed in Bollen's p-values; one for each lag
D1 = cbind(0.085, 0.272, 0.952, 0.648, 0.120, 0.848, 0.388)
D2 = cbind(0.268, 0.013, 0.973, 0.811, 0.369, 0.991, 0.7061)
D3 = cbind(0.446, 0.022, 0.981, 0.349, 0.418, 0.991, 0.723)
D4 = cbind(0.218, 0.030, 0.998,	0.415, 0.989, 0.989, 0.750)
D5 = cbind(0.300, 0.036, 0.989, 0.544, 0.553, 0.996, 0.173)
D6 = cbind(0.446, 0.065, 0.996,	0.691, 0.682, 0.994, 0.081)
D7 = cbind(0.620, 0.157, 0.999,	0.381, 0.713, 0.999, 0.150)
TMP.linear = data.table(rbind(D1,D2,D3,D4,D5,D6,D7))
setnames(TMP.linear, c("OF","Calm", "Alert", "Sure", "Vital","Kind", "Happy"))
raw.pvalues = unlist(stack(TMP.linear))[1:49]; names(raw.pvalues) = NULL

#under no standard multiple hypothesis testing framework are TMP's results significant.
p.adjust(raw.pvalues, method = "bonferroni");
p.adjust(raw.pvalues, method = "holm");
p.adjust(raw.pvalues, method = "hochberg");
p.adjust(raw.pvalues, method = "hommel");

#get mutoss library
source("http://bioconductor.org/biocLite.R")
biocLite("multtest")
library(mutoss)
SidakSD(raw.pvalues,alpha=0.05)

#test for FDR
p.adjust(raw.pvalues, method = "BY")
p.adjust(raw.pvalues, method = "fdr")
library(fdrtool)
fdrtool(raw.pvalues, statistic="pvalue")
BY(raw.pvalues,alpha=0.1)
BH(raw.pvalues,alpha=0.1)

ABH_pi0_est(raw.pvalues)
adaptiveBH(raw.pvalues,alpha=0.1)
adaptiveSTS(raw.pvalues,alpha=0.1)
aorc(raw.pvalues,alpha=0.1)

# non-linear hypotheses testing

pbinom(2, s=15, p=0.5) #no adjustment
1- (1-pbinom(2, s=15, p=0.5))^8 #adjustment for multiple hypotheses
dbinom(13, s=15, p=0.5)

1-(1-pbinom(2,s=15,p=0.4666667))^8 #adjustment for ease "down" predictor
1-(1-pbinom(2,s=15,p=0.52))^8 #adjustment for ease "down" predictor

# calculations to obtain 0.52 daily probability of an up-movement for DJIA
# not actually executed in this script
# DJIA.raw = read.xlsx("DJIA.xlsx",1, header = TRUE)
# head(DJIA.raw)
# the market is not a random walk without; evidence from a simple binomial test
# close = diff(rev(DJIA.raw$Close))[1:4000]
# sum(close<0)/(sum(close<0)+sum(close>0)+sum(close==0))
# V = length(close)*(1/2)*(1/2); std.DJIA.ud = sqrt(V)
# 8359/2+3*std.DJIA.ud

#DJIA values from 15 day test in TMP as obtained from Yahoo! Finance
# A = c(8579.11,8604.99,8824.34,8924.14,8564.53, 8629.68, 8565.09, 
#      8761.42, 8691.33, 8934.18, 8635.42,8376.24, 8591.69, 8419.09, 8149.09)



## @knitr pav

#graph shows no significant deviation from uniform distribution; taken from Pav
library(gap); library(grid)
set.seed(10); jpeg("TMP versus Uniform.jpg")
par(mfrow=c(1,2))
tmp.plot = qqunif(raw.pvalues, logscale = F); 
title("Q-Q Plot of TMP's p-values")
calm.plot = qqunif(c(runif(7,min=0,max=0.1),runif(42)),logscale=F);
title("Significant CALM Effects")
dev.off()

## @knitr C3S2ex1

library(dynlm); library(lmtest); library(mutoss); library(ggplot2)
normalize = function(x,k){ #define local normalization function
        T = length(x); n.ts = x
        for (t in 1:T){
                if (t - k < 1){
                        #first demean, then divide by standard deviation
                        n.ts[t] = (x[t] - mean(x[1:(t+k)]))/sd(x[1:(t+k)])
                }
                else if(t + k > T){
                        n.ts[t] = (x[t] - mean(x[(t-k):T]))/sd(x[(t-k):T])
                }
                else{
                        n.ts[t] = (x[t] - mean(x[(t-k):(t+k)]))/sd(x[(t-k):(t+k)])
                }
                
        }
        return(n.ts)
}

set.seed(7); # set seed for reproducibility

X.gc.D1 = NULL;  X.gc.D2 = NULL; nrm.X.gc.D1 = NULL; nrm.X.gc.D2 = NULL
for (i in 1:1000){
        D = rnorm(173) #make D difference time series
        X = D*rpois(1,lambda=10); X = X + rnorm(173)
        #set two values of X to one to avoid multicollinearity
        
        D = zoo(D); X = zoo(X); nrm.X = zoo(normalize(X,3))
        
        L1 = dynlm(D ~ L(D)); L2 = dynlm(D ~ L(D) + L(X))
        X.gc.D1[i] = waldtest(L1, L2)["Pr(>F)"][2,]
        
        L1.D = dynlm(D ~ L(D) + L(L(D)))
        L2.D = dynlm(D ~ L(D) + L(L(D)) + L(X) + L(L(X)))        
        X.gc.D2[i] = waldtest(L1.D, L2.D)["Pr(>F)"][2,]
        
        L1.D.1 = dynlm(D ~ L(D)); L2.D.1 = dynlm(D ~ L(D) + L(nrm.X))
        nrm.X.gc.D1[i] = waldtest(L1.D.1, L2.D.1)["Pr(>F)"][2,]
        
        L1.D.2 = dynlm(D ~ L(D) + L(L(D)))
        L2.D.2 = dynlm(D ~ L(D) + L(L(D)) + L(nrm.X) + L(L(nrm.X)))
        nrm.X.gc.D2[i] = waldtest(L1.D.2, L2.D.2)["Pr(>F)"][2,]
}

# as expected, p-values of underlying mood time series distributed uniformly w/
# mean approximately 0.5; normalized mood time series first lag is significant
# normalized mood time series second lag is insignificant

mean(nrm.X.gc.D1); mean(nrm.X.gc.D2); mean(X.gc.D2); mean(X.gc.D1);

quantile(nrm.X.gc.D1, c(0.01,.99)); quantile(nrm.X.gc.D2, c(0.01,.99))
quantile(X.gc.D1, c(0.01,.99)); quantile(X.gc.D2, c(0.01,.99))

dat = data.frame(mood = factor(rep(c("Reg series lag 1",
                                     "Reg series lag 1 & 2",
                                     "Normed series lag 1",
                                     "Normed series lag 1 & 2"), 
                                   each=1000)),
                 pvals = c(X.gc.D1, X.gc.D2, nrm.X.gc.D1, nrm.X.gc.D2))
ggplot(dat, aes(x=mood, y = pvals)) + geom_boxplot() + geom_jitter(size=0.001) +
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        xlab("Four granger causality tests") +
        ylab("p-value from Monte Carlo simulation") + 
        ggtitle("Boxplots of p-values from Granger causality tests")

ggplot(dat, aes(x=1:1000, y = nrm.X.gc.D1[order(nrm.X.gc.D1)])) + geom_point()+
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        geom_segment(aes(x=1,xend=1000,y=0.05,yend=0.05), color = 'red') +
        ggtitle("Monte Carlo results: Granger causality test on first lag of locally 
                normalized mood time series") +
        theme(plot.title = element_text(size = rel(0.9))) +
        xlab("p-values ordered from least to greatest") +
        ylab("p-value")

## @knitr C3S2ex2
X.gc.D1 = NULL;  X.gc.D2 = NULL; nrm.X.gc.D1 = NULL; nrm.X.gc.D2 = NULL
set.seed(7); # set seed for reproducibility
for (i in 1:1000){
        D = rnorm(173,0,1) #make D difference time series
        X = rpois(1,10)*diff(D)+rnorm(172); X = c(1,X); nrm.X = zoo(normalize(X,2))
        D = zoo(D); X = zoo(X); 
        
        L1.D.1 = dynlm(D ~ L(D))
        L2.D.1 = dynlm(D ~ L(D) + L(X))
        X.gc.D1[i] = waldtest(L1.D.1, L2.D.1)["Pr(>F)"][2,]
        
        L3.D.1 = dynlm(D ~ L(D, k= 1:2))
        L4.D.1 = dynlm(D ~ L(D, k= 1:2) + L(X, k = 1:2))
        X.gc.D2[i] = waldtest(L3.D.1, L4.D.1)["Pr(>F)"][2,]
        
        L1.D.2 = dynlm(D ~ L(D)); L2.D.2 = dynlm(D ~ L(D) + L(nrm.X))
        nrm.X.gc.D1[i] = waldtest(L1.D.2, L2.D.2)["Pr(>F)"][2,]
        
        L3.D.2 = dynlm(D ~ L(D, k=1:2))
        L4.D.2 = dynlm(D ~ L(D, k=1:2) + L(nrm.X, k = 1:2))
        nrm.X.gc.D2[i] = waldtest(L3.D.2, L4.D.2)["Pr(>F)"][2,]
        
        
}

mean(nrm.X.gc.D1); mean(nrm.X.gc.D2); mean(X.gc.D2); mean(X.gc.D1);

quantile(nrm.X.gc.D1, c(0.01,.99)); quantile(nrm.X.gc.D2, c(0.01,.99))
quantile(X.gc.D1, c(0.01,.99)); quantile(X.gc.D2, c(0.01,.99))

dat = data.frame(mood = factor(rep(c("Reg series lag 1",
                                     "Reg series lag 1 & 2",
                                     "Normed series lag 1",
                                     "Normed series lag 1 & 2"), 
                                   each=1000)),
                 pvals = c(X.gc.D1, X.gc.D2, nrm.X.gc.D1, nrm.X.gc.D2))

ggplot(dat, aes(x=mood, y = pvals)) + geom_boxplot() + geom_jitter(size=0.001) +
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        xlab("Four granger causality tests") +
        ylab("p-value from Monte Carlo simulation") + 
        ggtitle("Boxplots of p-values from Granger causality tests")

ggplot(dat, aes(x=1:1000, y = nrm.X.gc.D2[order(nrm.X.gc.D2)])) + geom_point()+
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        geom_segment(aes(x=1,xend=1000,y=0.05,yend=0.05), color = 'red') +
        ggtitle("Monte Carlo results: Granger causality test on first and second lag of locally 
                normalized mood time series") +
        theme(plot.title = element_text(size = rel(0.85))) +
        xlab("p-values ordered from least to greatest") +
        ylab("p-value")

## @knitr C3S2ex3
#
#

setwd("C:/Users/msl0/Dropbox/Thesis/EssayI"); 
DJIA.raw = rev(read.xlsx("Depression.xlsx",1, header = FALSE)[,1])
d.DJIA = zoo(diff(DJIA.raw)); pacf(d.DJIA) #pacf plot suggests lag 2
L.DJIA = dynlm(d.DJIA ~ L(d.DJIA, 1:2)); summary(L.DJIA); D = NULL
D[1:2] = d.DJIA[1:2]; D[3] = 2
acf(L.DJIA$res) #time dependence removed in mean
qqnorm(L.DJIA$res) #evidence of heavy tails, fit GPD to residuals
DJIA.res.est = fit.gpd(as.numeric(L.DJIA$res)) #GPD captures heavy tails

X.gc.D1 = NULL;  X.gc.D2 = NULL; nrm.X.gc.D1 = NULL; nrm.X.gc.D2 = NULL
nrm.X.gc.D3 = NULL; set.seed(7); # set seed for reproducibility 

for (i in 1:1000){
        DJIA.err = rgpd(DJIA.res.est, length(d.DJIA))  #residuals accounted for
        D = as.numeric(D)
        for (j in 4:173){ #generate a realistic DJIA series using residuals and 
                D[j] = L.DJIA$coef[1] + L.DJIA$coef[2]*D[j-1] + 
                        L.DJIA$coef[3]*D[j-2] + 0.01*D[j-3] + DJIA.err[j]
        }
        X = c(1,diff(DJIA.err)) + rnorm(173);
        nrm.X = zoo(normalize(X,2))
        D = zoo(D); X = zoo(X);
        
        L1.D.1 = dynlm(D ~ L(D))
        L2.D.1 = dynlm(D ~ L(D) + L(X))
        X.gc.D1[i] = waldtest(L1.D.1, L2.D.1)["Pr(>F)"][2,]
        
        L3.D.1 = dynlm(D ~ L(D, k= 1:2))
        L4.D.1 = dynlm(D ~ L(D, k= 1:2) + L(X, k = 1:2))
        X.gc.D2[i] = waldtest(L3.D.1, L4.D.1)["Pr(>F)"][2,]
        
        L1.D.2 = dynlm(D ~ L(D)); L2.D.2 = dynlm(D ~ L(D) + L(nrm.X))
        nrm.X.gc.D1[i] = waldtest(L1.D.2, L2.D.2)["Pr(>F)"][2,]
        
        L3.D.2 = dynlm(D ~ L(D, k=1:2))
        L4.D.2 = dynlm(D ~ L(D, k=1:2) + L(nrm.X, k = 1:2))
        nrm.X.gc.D2[i] = waldtest(L3.D.2, L4.D.2)["Pr(>F)"][2,]
        
        L3.D.3 = dynlm(D ~ L(D, k=1:3))
        L4.D.3 = dynlm(D ~ L(D,k=1:3) + L(nrm.X, k=1:3))
        nrm.X.gc.D3[i] = waldtest(L3.D.3, L4.D.3)["Pr(>F)"][2,]        
}
library(beepr)
beep()

mean(nrm.X.gc.D1); mean(nrm.X.gc.D2); mean(X.gc.D2); mean(X.gc.D1);
mean(nrm.X.gc.D3)
quantile(nrm.X.gc.D1, c(0.01,.99)); quantile(nrm.X.gc.D2, c(0.01,.99))
quantile(X.gc.D1, c(0.01,.99)); quantile(X.gc.D2, c(0.01,.99))

dat = data.frame(mood = factor(rep(c("Reg series lag 1",
                                     "Reg series lag 1 & 2",
                                     "Normed series lag 1",
                                     "Normed series lag 1 & 2"), 
                                   each=1000)),
                 pvals = c(X.gc.D1, X.gc.D2, nrm.X.gc.D1, nrm.X.gc.D2))

ggplot(dat, aes(x=mood, y = pvals)) + geom_boxplot() + geom_jitter(size=0.001) +
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        xlab("Four granger causality tests") +
        ylab("p-value from Monte Carlo simulation") + 
        ggtitle("Boxplots of p-values from Granger causality tests")

ggplot(dat, aes(x=1:1000, y = nrm.X.gc.D2[order(nrm.X.gc.D2)])) + geom_point()+
        scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
        geom_segment(aes(x=1,xend=1000,y=0.05,yend=0.05), color = 'red') +
        ggtitle("Monte Carlo results: Granger causality test on first and second lag of locally 
                normalized mood time series") +
        theme(plot.title = element_text(size = rel(0.85))) +
        xlab("p-values ordered from least to greatest") +
        ylab("p-value")