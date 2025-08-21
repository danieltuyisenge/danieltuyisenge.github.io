
###############################################################################
#  Code BY Daniel Tuyisenge
#  Last Modified : 4/18/2025
#  SUMMARY OF CODE
#
# This script implements and evaluates multiple confidence interval methods 
# for Randomized Response Techniques (RRTs). It includes:
#
# 1. Frey and Perez Method:
#    - Function likeci_log(): likelihood ratio confidence intervals on log scale.
#
# 2. Chang and Kuo Methods:
#    - Function ChangKuo_CI(): three weighted confidence intervals (CI1–CI3).
#
# 3. General RRT Confidence Interval Function:
#    - Function RR.conf.int(): unified interface for Warner, Singh–Joarder,
#      Devore, Horvitz, Soberanis-Cruz, Mangat–Singh–Singh, Frey, Shan (1–3),
#      and Chang–Kuo (1–3).
#
# 4. Simulation Framework:
#    - Function BIN.SIM(): computes coverage probabilities for given parameters.
#    - Scenarios 1–6: examine effects of varying sample size, alpha/P levels,
#      design parameter p, upper bound pi_U, support bounds (a1,a2), and 
#      design parameters (v,w).
#
# 5. Visualization:
#    - Uses ggplot2 to plot coverage probabilities across scenarios and methods.
#
# 6. Shan Methods:
#    - Functions Shan_CI1(), Shan_CI2(), Shan_CI3(), Shan_CI_LR(): alternative
#      CI constructions with simulation-calibrated coverage.
#
# 7. Method Comparison:
#    - Compares coverage across Warner, Singh–Joarder, Devore, Horvitz,
#      Soberanis-Cruz, Mangat–Singh–Singh, Chang–Kuo, and Frey methods.
#    - Produces facetted and overlay plots for comparison.
#
###############################################################################




#FREY AND PEREZ CODE
##########################################
##########################################
#This function computes the adjusted exact
# likelihood ratio confidence intervals.
#The arguments are the sample size n,
# the value alpha that determines the
# coverage probability 1-alpha, and the
# bounds [a,b] for the true proportion.
##########################################
################################################
#New version with calculations on the log scale.
################################################

likeci_log<-function(n,alpha,a,b,v=0.25, W=0.75){
  
  minc<-0
  maxc<-1
  
  while ((maxc-minc)>1.5e-5){
    
    c<-round((minc+maxc)/2,5)
    #print(c)
    
    xvals<-0:n
    mle<-xvals/n
    for (i in 0:n){
      if (mle[i+1]<a){mle[i+1]<-a}
      if (mle[i+1]>b){mle[i+1]<-b}}
    lbw<-double(n+1)
    ubw<-double(n+1)
    
    for (i in 0:n){
      cx<-c
      if (i<=sqrt(n)){cx<-c/5}
      if ((n-i)<=sqrt(n)){cx<-c/5}
      maxlike<-dbinom(i,n,mle[i+1],log=T)
      lo<-a
      hi<-mle[i+1]
      mid<-(lo+hi)/2
      while ((hi-lo)>1.0e-14){
        mid<-(lo+hi)/2
        midlike<-dbinom(i,n,mid,log=T)
        val<-exp(midlike-maxlike)
        if (val>cx){hi<-mid}
        if (val<=cx){lo<-mid}}
      lbw[i+1]<-mid
      lo<-mle[i+1]
      hi<-b
      mid<-(lo+hi)/2
      while ((hi-lo)>1.0e-14){
        mid<-(lo+hi)/2
        midlike<-dbinom(i,n,mid,log=T)
        val<-exp(midlike-maxlike)
        if (val>cx){lo<-mid}
        if (val<=cx){hi<-mid}}
      ubw[i+1]<-mid}
    
    bounds<-c(lbw,ubw)
    cpbounds<-double(length(bounds))
    
    for (i in 1:length(bounds)){
      
      p<-bounds[i]
      ind<-((lbw+1.0e-12)<p)*(p<(ubw-1.0e-12))
      if (sum(ind)==0){cpbounds[i]<-0}
      if (sum(ind)>0){
        xlo<-min(xvals[ind==1])
        xhi<-max(xvals[ind==1])
        cpbounds[i]<-pbinom(xhi,n,p)-pbinom(xlo-1,n,p)}}
    
    cc<-min(cpbounds[bounds>(a+1.0e-12) & bounds<(b-1.0e-12)])
    
    if (cc>(1-alpha)){minc<-c}
    if (cc<=(1-alpha)){maxc<-c}}
  
  c<-minc
  
  xvals<-0:n
  mle<-xvals/n
  for (i in 0:n){
    if (mle[i+1]<a){mle[i+1]<-a}
    if (mle[i+1]>b){mle[i+1]<-b}}
  lbw<-double(n+1)
  ubw<-double(n+1)
  
  for (i in 0:n){
    cx<-c
    if (i<=sqrt(n)){cx<-c/5}
    if ((n-i)<=sqrt(n)){cx<-c/5}
    maxlike<-dbinom(i,n,mle[i+1],log=T)
    lo<-a
    hi<-mle[i+1]
    mid<-(lo+hi)/2
    while ((hi-lo)>1.0e-14){
      mid<-(lo+hi)/2
      midlike<-dbinom(i,n,mid,log=T)
      val<-exp(midlike-maxlike)
      if (val>cx){hi<-mid}
      if (val<=cx){lo<-mid}}
    lbw[i+1]<-mid
    lo<-mle[i+1]
    hi<-b
    mid<-(lo+hi)/2
    while ((hi-lo)>1.0e-14){
      mid<-(lo+hi)/2
      midlike<-dbinom(i,n,mid,log=T)
      val<-exp(midlike-maxlike)
      if (val>cx){lo<-mid}
      if (val<=cx){hi<-mid}}
    ubw[i+1]<-mid}
  
  for (i in 1:n){
    if (ubw[i+1]<ubw[i]){ubw[i+1]<-ubw[i]}
    if (lbw[n+1-i]>lbw[n+2-i]){lbw[n+1-i]<-lbw[n+2-i]}}
  
  bounds<-c(lbw,ubw)
  bounds<-sort(c(bounds,seq(a,b,length=500)))
  cpbounds<-double(length(bounds))
  
  for (i in 1:length(bounds)){
    
    p<-bounds[i]
    ind<-((lbw+1.0e-12)<p)*(p<(ubw-1.0e-12))
    if (sum(ind)==0){cpbounds[i]<-0}
    if (sum(ind)>0){
      xlo<-min(xvals[ind==1])
      xhi<-max(xvals[ind==1])
      cpbounds[i]<-pbinom(xhi,n,p)-pbinom(xlo-1,n,p)}}
  
  cpbounds[cpbounds<(1-alpha)]<-1
  cc<-min(cpbounds[bounds>(a+1.0e-12) & bounds<(b-1.0e-12)])
  
  list(cc=cc,crit=c,lb=lbw,ub=ubw,bounds=bounds,cpbounds=cpbounds)}
########################################################################
#######################################################################
##End of Frey



############################################################################
#################################################################################
#--------------------------------------------------------------------------------
# R Code to Construct the Three Types of Confidence Intervals (CI1, CI2, CI3)
#using Chang and Kuo Method
#################################################################################
#################################################################################
ChangKuo_CI<- function(n,x, alpha,v=.25,w=0.75) {
  # z_alpha/2 for the given confidence level
  
  z_alpha <- qnorm(1 - alpha / 2)
  
  # Weights for CI1, CI2, and CI3
  W1 <- n / (n + z_alpha^2)
  W2 <- n / (n + sqrt(n))
  W3 <- n / (n + z_alpha^2 - 1)
  
  pi_yes=x/n
  
  # K constant for CI3
  K <- (W3 * (1 - 2 * pi_yes)^2) / (4 * z_alpha^4 * w^2)
  
  ##############################################################################
  ##CI1
  ##############################################################################
  # estimator pi
  pi_hat1 <- W1 * (pi_yes - v) / w + (1 - W1) * (1 - 2 * v) / (2 * w)
  #variance 1
  var_hat1<-(W1 * pi_yes * (1 - pi_yes)) / ((n + z_alpha^2) * w^2) + (1 - W1) / (4 * (n + z_alpha^2) * w^2)
  
  
  CI1 <- c(pi_hat1  - z_alpha * sqrt(var_hat1), pi_hat1  + z_alpha * sqrt(var_hat1))
  
  ##############################################################################
  ##CI2
  ##############################################################################
  #estimator pi_hat2
  pi_hat2 <- W2 * (pi_yes - v) / w + (1 - W2) * (1 - 2 * v) / (2 * w)
  #variance of pi_hat2
  var_hat2<-n / (4 * (n + sqrt(n))^2 * w^2)
  
  CI2 <- c(  pi_hat2 - z_alpha * sqrt(var_hat2),   pi_hat2+ z_alpha * sqrt(var_hat2))
  
  ##############################################################################
  ##CI3
  ##############################################################################
  #estimator pi_hat3
  pi_hat3 <- W3 * (pi_yes - v) / w + (1 - W3) * (1 - 2 * v) / (2 * w)
  #variance pi_hat3
  var_hat3<-((z_alpha^2 - 1)/(z_alpha^2))*(( W3 * pi_yes * (1 - pi_yes)) / ((n + z_alpha^2 - 1) * w^2) + (1 - W3) / (4 * (n + z_alpha^2 - 1) * w^2)) + K
  
  CI3 <- c(pi_hat3 - z_alpha * sqrt(var_hat3), pi_hat3 + z_alpha * sqrt(var_hat3))
  
  # Return the intervals
  list(pi_hat1=pi_hat1,var_hat1=var_hat1, CI1 = CI1,pi_hat2=pi_hat2,var_hat2=var_hat2,CI2 = CI2,pi_hat3=pi_hat3, var_hat3=var_hat3, CI3 = CI3)
}
#################################################################################
#################################################################################
#End of CHANG and KUO


##############################################################################
##############################################################################
#START OF TOLERANCE FUNCTION
#######################################################################
#######################################################################
RR.conf.int<-function (z,n,p,alpha,P = 0.99, side = 1, pi_U=0.5,cl=0.95, m=NULL, pi=NULL,v=0.25,w=0.75, a1=0,a2=1,
                       method = c("warner","singhjoarder","devore",
                                  "horvitz", "soberaniscruz","mangatsinghsingh",
                                  "frey","shan1","shan2","shan3",
                                  "changKuo1","changKuo2","changKuo3"
                       )){
  
  
  #library(RRTCS)
  #library(mase)
  
  if (side != 1 && side != 2) {
    stop(paste("Must specify a one-sided or two-sided procedure!",
               "\n"))
  }
  if (side == 2) {
    alpha <- alpha/2
    P <- (P + 1)/2
  }
  
  
  #################################################################################
  #1 on p54
  #warner1
  method <- match.arg(method)
  if (method == "warner") {
    
    x=sum(z)
    pi_yes=x/n
    
    if (pi_yes < (1-p)){ pi_yes=(1-p)}
    if (pi_yes>p)    {pi_yes=p}
    
    pi_hat=(pi_yes-(1-p))/(2*p-1)
    #var_hat=(1/(n-1))*(1/(16*(p-0.5)^2)-(pi_yes-0.5)^2)
    var_hat=(pi_yes*(1-pi_yes))/(n-1)+(p*(1-p))/((n-1)*((2*p-1)^2))
    
    z_score=qnorm(1 - alpha / 2)
    
    lower.pi=pi_hat-z_score*sqrt(var_hat)
    upper.pi=pi_hat+z_score*sqrt(var_hat)
    
    #transformed
    lower.yes=p* lower.pi+(1-p)*(1- lower.pi)
    upper.yes=p* upper.pi+(1-p)*(1- upper.pi)
  }
  
  ###############################################################################
  #9
  #singjoarder9
  
  if (method == "singhjoarder"){
    
    # Calculate mean of z and Pi
    x <- sum(z)
    pi_yes<-x/n
    
    
    if (pi_yes < (1-p)){ pi_yes=(1-p)}
    if (pi_yes>(p+(1-p)*p))    {pi_yes=p+(1-p)*p}
    
    pi_hat <- (pi_yes - (1 - p)) / ((2 * p - 1) + p * (1 - p))
    var_hat<-(pi_yes * (1 - pi_yes))/((n-1)*(2*p-1+(p*(1-p)))^2)
    
    
    # Confidence interval calculation
    
    z_score <- qnorm(1 - alpha / 2)
    
    lower.pi<-pi_hat - z_score * sqrt(var_hat)
    upper.pi<-pi_hat + z_score * sqrt(var_hat)
    
    #transformed
    lower.yes=lower.pi*((2 * p - 1) + p * (1 - p))+(1-p)
    upper.yes=upper.pi*((2 * p - 1) + p * (1 - p))+(1-p)
  }
  #6##############################################################################
  #4 on page 12
  #horvitz
  
  if (method == "devore"){
    x=sum(z)
    
    pi_yes=x/n
    
    if (pi_yes < (1-p)){ pi_yes=(1-p)}
    if (pi_yes>1)    {pi_yes=1}
    
    pi_hat=(pi_yes-(1-p))/p
    var_hat=((pi_hat*(1-pi_hat))/(2*n))+((p*(1-p))/(2*n*(2*p-1)^2))
    #var_hat=(pi_hat*(1-pi_hat))/((n-1)*p^2)
    
    
    # Step 4: Calculate confidence interval
    z_score <- qnorm(1 - alpha / 2)
    lower.pi<-pi_hat - z_score * sqrt(var_hat)
    upper.pi<-pi_hat + z_score * sqrt(var_hat)
    
    
    #transformed
    lower.yes=p* lower.pi+(1-p)
    upper.yes=p* upper.pi+(1-p)
  }
  
  
  #################################################################################
  ###############################################################################
  #2 on page 26
  #Horvitz
  if (method == "horvitz") {
    x=sum(z)
    pi_yes=x/n
    
    if (pi_yes < (1-p)*pi_U){ pi_yes=(1-p)*pi_U}
    if (pi_yes>p+(1-p)*pi_U)    {pi_yes=p+(1-p)*pi_U}
    
    pi_hat=(pi_yes-((1-p)*pi_U))/p
    var_hat=((pi_yes*(1-pi_yes))/(2*n))+((p*(1-p))/(2*n*(2*p-1)^2))
    
    z_score=qnorm(1 - alpha / 2)
    
    lower.pi=pi_hat-z_score*sqrt(var_hat)
    upper.pi=pi_hat+z_score*sqrt(var_hat)
    
    #transformed
    lower.yes=p* lower.pi+((1-p)*pi_U)
    upper.yes=p* upper.pi+((1-p)*pi_U)
    
  }
  
  #3###############################################################################
  #3 on on p52
  #SoberanisCruz
  
  if (method == "soberaniscruz") {
    x=sum(z)
    pi_yes=x/n
    
    if (pi_yes < ((1-p)*pi_U)){ pi_yes=(1-p)*pi_U}
    if (pi_yes>(p+(1-p)*pi_U) ){pi_yes=p+(1-p)*pi_U}
    
    
    
    pi_hat=(pi_yes - (1 - p) * pi_U)/p
    var_hat=(pi_yes*(1-pi_yes))/n+(1/n)*((((1-p)*p)/p*2)+(((1-2*(1-p)-p)/p)*pi_yes))
    
    z_score=qnorm(1 - alpha / 2)
    
    lower.pi=pi_hat-z_score*sqrt(var_hat)
    upper.pi=pi_hat+z_score*sqrt(var_hat)
    
    #transformed
    lower.yes=p*lower.pi+(1-p)*pi_U
    upper.yes=p*upper.pi+(1-p)*pi_U
    
  }
  
  
  
  #################################################################################
  #11 on page 39
  #MangatSinghSinghData
  
  if (method == "mangatsinghsingh"){  
    x=sum(z)
    pi_yes<-x/n
    
    
    
    if (pi_yes < (1 - p) * pi_U){ pi_yes=(1 - p) * pi_U}
    if (pi_yes>1)    {pi_yes=1}
    
    pi_hat <- (pi_yes - (1 - p) * pi_U) / (1-(1-p)*pi_U)
    var_hat <- (pi_yes* (1 - pi_yes)) / ((n-1) * (1-pi_U*(1-p))^2)
    
    
    z_score <- qnorm(1 - alpha / 2)
    lower.pi<-pi_hat - z_score * sqrt(var_hat)
    upper.pi<-pi_hat + z_score * sqrt(var_hat)
    
    
    #transformed
    lower.yes=lower.pi*(1-(1-p)*pi_U)+(1-p)*pi_U
    upper.yes=upper.pi*(1-(1-p)*pi_U)+(1-p)*pi_U
    
    
  }
  
  
  #7##############################################################################
  #Frey
  ################################################################################
  
  if (method == "frey"){
    ##Start of Frey's method  
    out<-likeci_log(n,alpha,a1,a2,v,w)
    x=sum(z)
    pi_hat=x/n
    var_hat=0
    lower.yes<-out$lb[x+1]
    upper.yes<-out$ub[x+1]
    
    lower.pi<-(lower.yes-v)/w
    upper.pi<-(upper.yes-v)/w
  }
  
  ################################################################################
  #7##############################################################################
  # Change Wilson Type Intervals
  ################################################################################  
  ################################################################################
  
  ##############################################################################
  # Chang CW1
  ################################################################################  
  if (method == "changKuo1"){
    x=z
    result <- ChangKuo_CI(n,x,alpha,v,w)
    pi_hat=result$pi_hat1
    var_hat=result$var_hat1
    lower.pi<-result$CI1[1]
    upper.pi<-result$CI1[2]
    
    #transformed
    lower.yes=w* lower.pi+v
    upper.yes=w* upper.pi+v
  }
  
  ##############################################################################
  # Chang CW2
  ################################################################################  
  
  if (method == "changKuo2"){
    x=z
    result <- ChangKuo_CI(n, x,alpha,v,w)
    pi_hat=result$pi_hat2
    var_hat=result$var_hat2
    
    lower.pi<-result$CI2[1]
    upper.pi<-result$CI2[2]
    
    #transformed
    lower.yes=w* lower.pi+v
    upper.yes=w* upper.pi+v
  }
  
  ##############################################################################
  # Chang CW3
  ################################################################################  
  
  if (method == "changKuo3"){
    x=z
    result <- ChangKuo_CI(n, x,alpha,v,w)
    pi_hat=result$pi_hat3
    var_hat=result$var_hat3
    lower.pi<-result$CI3[1]
    upper.pi<-result$CI3[2]
    
    #transformed
    lower.yes=w*lower.pi+v
    upper.yes=w*upper.pi+v
  }
  
  
  ##############################################################################
  # Shan CW1
  ################################################################################  
  if (method == "shan1"){
    x=z
    result <- Shan_CI1(x,n,alpha,v,w)
    pi_hat=result$pi_hat1
    var_hat=result$var_hat1
    lower.pi<-result$CI1[1]
    upper.pi<-result$CI1[2]
    
    #transformed
    lower.yes=w* lower.pi+v
    upper.yes=w* upper.pi+v
  }
  
  ##############################################################################
  # Shan CW2
  ################################################################################  
  if (method == "shan2"){
    x=z
    result <- Shan_CI2(x,n,alpha,v,w)
    pi_hat=result$pi_hat2
    var_hat=result$var_hat2
    
    lower.pi<-result$CI2[1]
    upper.pi<-result$CI2[2]
    
    #transformed
    lower.yes=w* lower.pi+v
    upper.yes=w* upper.pi+v
  }
  
  ##############################################################################
  # Shan CW3
  ################################################################################  
  
  if (method == "shan3"){
    x=z
    result <- Shan_CI3(x,n,alpha,v,w)
    pi_hat=result$pi_hat3
    var_hat=result$var_hat3
    lower.pi<-result$CI3[1]
    upper.pi<-result$CI3[2]
    
    #transformed
    lower.yes=w*lower.pi+v
    upper.yes=w*upper.pi+v
  }
  
  
  ##############################################################################
  # Shan  Likelihood Ratio
  ################################################################################  
  #
  # if (method == "shan_LR"){
  #   x=z
  #   result <- Shan_CI_LR(x,n,alpha,v,w)
  #   pi_hat=x/n
  #   var_hat=0 # Frey and Perez did not calculate variance
  #   lower.pi<-result[1]
  #   upper.pi<-result[2]
  #  
  #   #transformed
  #   lower.yes=w*lower.pi+v
  #   upper.yes=w*upper.pi+v
  # }
  #
  # #} #end of when N or Pi is not supplied  
  #
  #
  
  ###############################################################################
  #
  lower.yes<- max(0, lower.yes)
  upper.yes<- min(upper.yes, 1)
  
  #lower.pi <- max(0, lower.pi)
  #upper.pi <- min(upper.pi, 1)
  
  lower <- qbinom(1 - P, size = n, prob = lower.yes)
  upper <- qbinom(P, size = n, prob = upper.yes)
  
  #lower <- qbinom(1 - P, size = n, prob = lower.pi)
  #upper <- qbinom(P, size = n, prob = upper.pi)
  
  if (side == 2) {
    alpha <- 2 * alpha
    P <- (2 * P) - 1
  }
  temp <- data.frame(cbind(method,alpha,P, round(pi_hat,5), round(var_hat,5),round(lower.yes,5), round(upper.yes,5),round(lower.pi,5), round(upper.pi,5),lower,upper))
  if (side == 2) {
    colnames(temp) <- c("Method","alpha","P" ,"pi_hat", "var_hat","lower.yes", "upper.yes" ,"lower.pi", "upper.pi","2-sided.lower","2-sided.upper")
  }
  else {
    colnames(temp) <- c("Method","alpha","P", "pi_hat", "var_hat","lower.yes", "upper.yes", "lower.pi", "upper.pi","1-sided.lower","1-sided.upper")
  }
  temp
  
  
}#end of the function
################################################################################
#################################################################################
###start of simulations for different scenerio
############################################################################
###########################################################################
library(ggplot2)
library(gridExtra)

# Function to compute coverage probabilities
BIN.SIM <- function(n, m, P, alpha, side, method, p, pi, a1, a2, pi_U=1.0,v=0.25,w=0.75) {
  out.cp <- numeric(length(pi))
  
  for (j in seq_along(pi)) {
    i <- pi[j]
    x <- 0:n
    m <- length(x)
    DB <- dbinom(x, n, i)
    
    TI <- sapply(1:m, function(idx) {
      ci <- as.numeric(RR.conf.int(z=x[idx], n=n, p=p, alpha=2*alpha, P=P, side=side, pi_U=pi_U, method=method,v=v,w=w))
      return(ci[10:11])
    })
    
    out.cp[j] <- sum(DB * (TI[1, ] <= qbinom((1 - P) / 2, n, i) & TI[2, ] >= qbinom((1 + P) / 2, n, i)))
    print(i)
  }
  
  data.frame(pi = pi, Coverage = out.cp)
}



# Generate pi values
set.seed(123)
pi_vals <- sort(runif(500, 0.2, .8))
a1 <- 0
a2 <- 1
pi_U <- 1
method="warner" # change what is in the quotes for other RRTs


# Scenario 1: Different n values
n_values <- c(80) #(40,80,160,320)
scenario1 <- do.call(rbind, lapply(n_values, function(n) {
  df <- BIN.SIM(n=n, m=n, P=0.95, alpha=0.05, side=2, method=method, p=0.75, pi=pi_vals, a1=a1, a2=a2)
  df$n <- as.factor(n)
  df$Scenario <- "Scenario 1"
  df
}))
summary_stats <- scenario1$Coverage[scenario1$n == 40]
c(mean = mean(summary_stats), min = min(summary_stats), median = median(summary_stats), sd = sd(summary_stats))
summary_stats <- scenario1$Coverage[scenario1$n == 80]
c(mean = mean(summary_stats), min = min(summary_stats), median = median(summary_stats), sd = sd(summary_stats))
summary_stats <- scenario1$Coverage[scenario1$n == 160]
c(mean = mean(summary_stats), min = min(summary_stats), median = median(summary_stats), sd = sd(summary_stats))
summary_stats <- scenario1$Coverage[scenario1$n == 320]
c(mean = mean(summary_stats), min = min(summary_stats), median = median(summary_stats), sd = sd(summary_stats))





# Scenario 2: Different P and alpha values
params2 <- expand.grid(P=c(0.95, 0.90), alpha=c(0.05, 0.10))
n_fixed <- 80
scenario2 <- do.call(rbind, apply(params2, 1, function(row) {
  df <- BIN.SIM(n_fixed, m=n_fixed, P=row[1], alpha=row[2], side=2, method=method, p=0.75, pi=pi_vals, a1=a1, a2=a2)
  df$P <- as.factor(row[1])
  df$alpha <- as.factor(row[2])
  df$Scenario <- "Scenario 2"
  df
}))


#Summary tables P and alpha
#------------------------------------------------------------
summary_stats2 <- scenario2$Coverage[scenario2$P == 0.90]
c(mean = mean(summary_stats2), min = min(summary_stats2), median = median(summary_stats2), sd = sd(summary_stats2))
summary_stats2 <- scenario2$Coverage[scenario2$P == 0.95]
c(mean = mean(summary_stats2), min = min(summary_stats2), median = median(summary_stats2), sd = sd(summary_stats2))

summary_stats2 <- scenario2$Coverage[scenario2$alpha ==0.05]
c(mean = mean(summary_stats2), min = min(summary_stats2), median = median(summary_stats2), sd = sd(summary_stats2))
summary_stats2 <- scenario2$Coverage[scenario2$n == 0.10]
c(mean = mean(summary_stats2), min = min(summary_stats2), median = median(summary_stats2), sd = sd(summary_stats2))


# Scenario 3: Different side and p values
params3 <- expand.grid(side=c(2, 2), p=c(0.70,0.75,0.80, 0.85))
n_fixed2 <- 80
scenario3 <- do.call(rbind, apply(params3, 1, function(row) {
  df <- BIN.SIM(n_fixed2, m=n_fixed2, P=0.95, alpha=0.05, side=row[1], method=method, p=row[2], pi=pi_vals, a1=a1, a2=a2)
  df$side <- as.factor(row[1])
  df$p <- as.factor(row[2])
  df$Scenario <- "Scenario 3"
  df
}))



#Summary tables p
#------------------------------------------------------------
summary_stats3 <- scenario3$Coverage[scenario3$p == 0.70]
c(mean = mean(summary_stats3), min = min(summary_stats3), median = median(summary_stats3), sd = sd(summary_stats3))
summary_stats3 <- scenario3$Coverage[scenario3$p == 0.75]
c(mean = mean(summary_stats3), min = min(summary_stats3), median = median(summary_stats3), sd = sd(summary_stats3))
summary_stats3 <- scenario3$Coverage[scenario3$p == 0.80]
c(mean = mean(summary_stats3), min = min(summary_stats3), median = median(summary_stats3), sd = sd(summary_stats3))
summary_stats <- scenario3$Coverage[scenario3$p == 0.85]
c(mean = mean(summary_stats3), min = min(summary_stats3), median = median(summary_stats3), sd = sd(summary_stats3))




# Trellis plot for Scenario 1
plot_scenario1 <- ggplot(scenario1, aes(x=pi, y=Coverage)) +
  geom_line(aes(color=n)) +
  facet_wrap(~n, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 1: Different Sample Sizes for changKuo3 Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()

# Trellis plot for Scenario 2
plot_scenario2 <- ggplot(scenario2, aes(x=pi, y=Coverage)) +
  geom_line(aes(linetype=P, color=alpha)) +
  facet_wrap(~P + alpha, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 2: Different P and Alpha Values for changKuo3 Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()

# Trellis plot for Scenario 3
plot_scenario3 <- ggplot(scenario3, aes(x=pi, y=Coverage)) +
  geom_line(aes(color=p, linetype=side)) +
  facet_wrap(~side + p, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 3: Two sided TI for different p Values for changKuo3 Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()

# Display plots
print(plot_scenario1)
print(plot_scenario2)
print(plot_scenario3)





#---------------unrelated Question only---------------------------------------------
# Scenario 4: Different values of pi_u
params4 <- expand.grid(side=c(2, 2), pi_U=c(0.25,0.50, 0.75,1.00))
n_fixed4 <- 80
scenario4 <- do.call(rbind, apply(params4, 1, function(row) {
  df <- BIN.SIM(n_fixed4, m=n_fixed4, P=0.95, alpha=0.05, side=row[1], method=method, p=0.75, pi=pi_vals, a1=a1, a2=a2,pi_U=row[2])
  df$side <- as.factor(row[1])
  df$pi_U <- as.factor(row[2])
  df$Scenario <- "Scenario 4"
  df
}))


#Summary tables pi_U
#------------------------------------------------------------
summary_stats4 <- scenario4$Coverage[scenario4$pi_U == 0.25]
c(mean = mean(summary_stats4), min = min(summary_stats4), median = median(summary_stats4), sd = sd(summary_stats4))
summary_stats4 <- scenario4$Coverage[scenario4$pi_U == 0.50]
c(mean = mean(summary_stats4), min = min(summary_stats4), median = median(summary_stats4), sd = sd(summary_stats4))
summary_stats4 <- scenario4$Coverage[scenario4$pi_U == 0.75]
c(mean = mean(summary_stats4), min = min(summary_stats4), median = median(summary_stats4), sd = sd(summary_stats4))
summary_stats4 <- scenario4$Coverage[scenario4$pi_U == 1.0]
c(mean = mean(summary_stats4), min = min(summary_stats4), median = median(summary_stats4), sd = sd(summary_stats4))


# Trellis plot for Scenario 4
plot_scenario4 <- ggplot(scenario4, aes(x=pi, y=Coverage)) +
  geom_line(aes(linetype=side, color=pi_U)) +
  facet_wrap(~side + pi_U, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 4: Two sided TI for different pi_U Values for mangatsinghsingh Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()

print(plot_scenario4)




#---------------Frey Method only ---------------------------------------------

# Scenario 5: Different values of a1 and a2
params5 <- expand.grid(a1=c(0.00, 0.25,0.50), a2=c(0.75,1.00))
n_fixed5 <- 80
scenario5 <- do.call(rbind, apply(params5, 1, function(row) {
  df <- BIN.SIM(n_fixed5, m=n_fixed5, P=0.95, alpha=0.05, side=2, method="frey", p=0.75, pi=pi_vals, a1=row[1], a2=row[2])
  df$a1 <- as.factor(row[1])
  df$a2 <- as.factor(row[2])
  df$Scenario <- "Scenario 5"
  df
}))



# Trellis plot for Scenario 5
plot_scenario5 <- ggplot(scenario5, aes(x=pi, y=Coverage)) +
  geom_line(aes(color=a1, linetype=a2)) +
  facet_wrap(~a1 + a2, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 5: Two sided TI for different a1 and a2 Values for changKuo2 Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()


print(plot_scenario5)



#-----------------ChangKuo and Shan----------------------------
# Scenario 6: Different v and W values
params6 <- data.frame(v = c(0.15,0.15, 0.25, 0.25), w= c(0.85, 0.70, 0.50, 0.75))
n_fixed6 <- 80
scenario6 <- do.call(rbind, apply(params6, 1, function(row) {
  df <- BIN.SIM(n_fixed6, m=n_fixed6, P=0.95, alpha=0.05, side=2, method=method, pi=pi_vals, a1=a1, a2=a2,v=row[1],w=row[2])
  df$v <- as.factor(row[1])
  df$w <- as.factor(row[2])
  df$Scenario <- "Scenario 6"
  df
}))



# Trellis plot for Scenario 6
plot_scenario6 <- ggplot(scenario6, aes(x=pi, y=Coverage)) +
  geom_line(aes(color=v, linetype=w)) +
  facet_wrap(~v + w, scales="free") +
  ylim(0.85,1)+
  labs(title="Scenario 6: Two sided TI for different v and w Values for changKuo3 Method", x="True Probability", y="Coverage Probability") +
  theme_minimal()


print(plot_scenario6)
#############################################################
#############################################################
### End of simulation for different scenerio

#############################################################
#############################################################
### End of simulation for different scenerio



##################
##Additional Code
##Very slow since they involve simulations to get the results


# Example Usage
x=63
n <- 80
alpha <- 0.05
w=0.75
v=0.25


result1 <- ChangKuo_CI(n,x, alpha,v,w)
print(result1)



#################################################################################
#################################################################################
##Code for Shan
#################################################################################
#################################################################################


################################################################################
# START CW1
################################################################################

Shan_CI1<- function(x, n, alpha, v, w) {
  set.seed(123356)
  z_alpha <- qnorm(1 - alpha / 2)
  pi_grid <- seq(0.01, 0.99, length.out = 1) #length.ou=100
  n_sim <- 1 #50
  
  calculate_CIW1 <- function(x, n, C_w1, v=0.25, w=0.75) {
    pi_yes <- x / n
    pi_hat1<- (n / (n + C_w1^2)) * ((pi_yes - v) / w) + (1 - (n / (n + C_w1^2))) * ((1 - 2 * v) / (2 * w))
    var_hat1<-(n / (n + C_w1^2)) * (pi_yes * (1 - pi_yes)) / ((n + C_w1^2) * w^2) +
      (1 - (n / (n + C_w1^2))) / (4 * (n + C_w1^2) *w^2)
    
    CI1<-c(pi_hat1 - C_w1 * sqrt(var_hat1), pi_hat1 + C_w1 * sqrt(var_hat1))
    list(pi_hat1=pi_hat1,var_hat1=var_hat1, CI1 = CI1,C_w1=C_w1)
  }
  
  coverage_prob <- function(C_w1) {
    min(sapply(pi_grid, function(pi) {
      mean(sapply(1:n_sim, function(i) {
        x_sim <- rbinom(1, n, pi)
        ci <- calculate_CIW1(x_sim, n, C_w1, v, w)
        ci$CI1[1] <= pi && ci$CI1[2] >= pi
      }))
    }))
  }
  
  C_w1 <- z_alpha
  max_iter <- n_sim  # Prevent infinite loop
  iter <- 0
  while (coverage_prob(C_w1) < 1 - alpha && iter < max_iter) {
    C_w1 <- C_w1 + 0.1
    iter <- iter + 1
  }
  
  calculate_CIW1(x, n, C_w1, v, w)
}

# Example usage
# Example usage
Shan_CI1(x=63, n=80, alpha=0.05,v=0.25,w=0.75) #95%CI
Shan_CI1(x=63, n=80, alpha=0.10,v=0.25,w=0.75) #90%CI
# Example usage
s1<-Shan_CI1(x=63, n=80, alpha=0.05,v=0.25,w=0.75) #95%CI
s2<-Shan_CI1(x=63, n=80, alpha=0.10,v=0.25,w=0.75) #90%CI


################################################################################
# START CW2
################################################################################

Shan_CI2 <- function(x, n, alpha, v, w) {
  set.seed(123356)
  z_alpha <- qnorm(1 - alpha / 2)
  pi_grid <- seq(0.01, 0.99, length.out = 1)
  n_sim <- 1
  
  calculate_CIW2 <- function(x, n, C_w2, v, w) {
    pi_yes <- x / n
    pi_hat2 <- (n / (n + sqrt(n))) * ((pi_yes - v) / w) + (1 - (n / (n + sqrt(n)))) * ((1 - 2 * v) / (2 * w))
    var_hat2 <- n / (4 * (n + sqrt(n))^2 * w^2)
    
    CI2 <- c(pi_hat2 - C_w2 * sqrt(var_hat2), pi_hat2 + C_w2 * sqrt(var_hat2))
    list(pi_hat2 = pi_hat2, var_hat2 = var_hat2, CI2 = CI2,C_w2=C_w2)
  }
  
  coverage_prob <- function(C_w2) {
    min(sapply(pi_grid, function(pi) {
      mean(sapply(1:n_sim, function(i) {
        x_sim <- rbinom(1, n, pi)
        ci <- calculate_CIW2(x_sim, n, C_w2, v, w)
        ci$CI2[1] <= pi && ci$CI2[2] >= pi
      }))
    }))
  }
  
  C_w2 <- z_alpha
  max_iter <- n_sim  # Prevent infinite loop
  iter <- 0
  while (coverage_prob(C_w2) < 1 - alpha && iter < max_iter) {
    C_w2 <- C_w2 + 0.1
    iter <- iter + 1
  }
  
  calculate_CIW2(x, n, C_w2, v, w)
}


# Example usage
Shan_CI2(x=63, n=80, alpha=0.05,v=0.25,w=0.75) #95%CI
Shan_CI2(x=63, n=80, alpha=0.10,v=0.25,w=0.75) #90%CI

################################################################################
# START CW3
################################################################################
Shan_CI3<- function(x, n, alpha,v,w) {
  set.seed(123356)
  
  z_alpha <- qnorm(1 - alpha / 2)
  pi_grid <- seq(0.01, 0.99, length.out = 1000)
  n_sim <- 50
  
  calculate_CIW3 <- function(x, n, C_w3, v,w) {
    pi_yes <- x / n
    W3 <- n / (n + C_w3^2 - 1)
    pi_hat3<- (W3 * ((pi_yes -v) / w)) + ((1 - W3) * ((1 - 2 * v) / (2 * w)))
    In_par <- (W3 * pi_yes * (1 - pi_yes)) / ((n + C_w3^2 - 1) * w^2) +(1 - W3) / (4 * (n + C_w3^2 - 1) * w^2)
    var_hat3<-((C_w3^2 - 1) / C_w3^2) * In_par +(W3 * (1 - 2 * pi_yes)^2) / (4 * w^2 * C_w3^4)
    CI3<-c(pi_hat3 - C_w3 * sqrt(var_hat3), pi_hat3+ C_w3 * sqrt(var_hat3))
    list(pi_hat3=pi_hat3,var_hat3=var_hat3, CI3 = CI3,C_w3=C_w3)
  }
  
  coverage_prob <- function(C_w3) {
    min(sapply(pi_grid, function(pi) {
      mean(sapply(1:n_sim, function(i) {
        x_sim <- rbinom(1, n, pi)
        ci <- calculate_CIW3(x_sim, n, C_w3, v, w)
        ci$CI3[1] <= pi && ci$CI3[2] >= pi
      }))
    }))
  }
  
  C_w3 <- z_alpha
  max_iter <- n_sim  # Prevent infinite loop
  iter <- 0
  while (coverage_prob(C_w3) < 1 - alpha && iter < max_iter) {
    C_w3 <- C_w3 + 0.1
    iter <- iter + 1
  }
  
  calculate_CIW3(x, n, C_w3, v, w)
}
# Example usage
# Example usage
Shan_CI3(x=63, n=80, alpha=0.05,v=0.25,w=0.75) #95%CI
Shan_CI3(x=63, n=80, alpha=0.10,v=0.25,w=0.75) #90%CI



################################################################################
# START CLR
################################################################################

Shan_CI_LR<- function(x, n, alpha, v=0.25,w=0.75,p=0.75,a1=0,a2=1) {
  set.seed(123356)
  z_alpha <- qnorm(1 - alpha / 2)
  pi_grid <- seq(0.01, 0.99, length.out = 1)
  n_sim <- 1
  
  
  calculate_CLR <- function(x, n, CLR, v, w) {
    out <- likeci_log(n, CLR, a1, a2)  # Ensure likeci_log is defined and returns correct values
    if (is.null(out) || is.na(out$lb[x+1]) || is.na(out$ub[x+1])) {
      return(c(NA, NA))  # Return NA if there's an issue
    }
    lower_yes <- out$lb[x+1]
    upper_yes <- out$ub[x+1]
    lower.pi <- (lower_yes - (1 - p)) / p
    upper.pi <- (upper_yes - (1 - p)) / p
    CI_LR<-return(c(lower.pi, upper.pi))
    list(pi_hat=(x/n-v)/w,CLR=CLR,CI_LRR=CI_LR)
  }
  
  
  coverage_prob <- function(CLR) {
    min(sapply(pi_grid, function(pi) {
      mean(sapply(1:n_sim, function(i) {
        x_sim <- rbinom(1, n, pi)
        ci <- calculate_CLR(x_sim, n, CLR, v, w)
        ci[1] <= pi && ci[2] >= pi
      }))
    }))
  }
  
  CLR <- z_alpha
  max_iter <- n_sim # Prevent infinite loop
  iter <- 0
  while (coverage_prob(CLR) < 1 - alpha && iter < max_iter) {
    CLR <- CLR + 0.1
    iter <- iter + 1
  }
  
  calculate_CLR(x, n, CLR, v, w)
}

# Example usage
# note that the
# Example usage
Shan_CI_LR(x=63, n=80, alpha=0.05,v=0.25,w=0.75) #95%CI
Shan_CI_LR(x=63, n=80, alpha=0.10,v=0.25,w=0.75) #90%CI
################################################################################
################################################################################
#End











# Load required libraries
library(ggplot2)
library(dplyr)

# Define methods to compare
methods_to_compare <- c("warner", "singhjoarder", "devore", "horvitz",
                        "soberaniscruz", "mangatsinghsingh", "changKuo1", "frey")

# Parameters
n_fixed <- 80
p_fixed <- 0.75
P_fixed <- 0.95
alpha_fixed <- 0.05
a1 <- 0
a2 <- 1
pi_vals <- sort(runif(500, 0.2, 0.8))  # Adjust as needed
v <- 0.25
w <- 0.75

# Wrapper to collect results from each method
compare_methods <- function(method) {
  df <- BIN.SIM(n=n_fixed, m=n_fixed, P=P_fixed, alpha=alpha_fixed, side=2,
                method=method, p=p_fixed, pi=pi_vals, a1=a1, a2=a2, v=v, w=w)
  df$Method <- method
  return(df)
}

# Run for all methods
comparison_results <- do.call(rbind, lapply(methods_to_compare, compare_methods))

# Plot coverage for all methods
comparison_plot <- ggplot(comparison_results, aes(x=pi, y=Coverage, color=Method)) +
  geom_line() +
  facet_wrap(~Method, scales="free_y", ncol=2) +
  ylim(0.85, 1) +
  labs(
    title = "Comparison of Coverage Probabilities Across RRT Methods",
    subtitle = "n = 80, p = 0.75, P = 0.95, α = 0.05",
    x = "True Proportion (π)",
    y = "Coverage Probability"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(comparison_plot)


#####
# Overlay all methods in one plot
comparison_plot <- ggplot(comparison_results, aes(x = pi, y = Coverage, color = Method)) +
  geom_line(size = 1) +
  ylim(0.85, 1) +
  labs(
    title = "Comparison of Coverage Probabilities Across RRT Methods",
    subtitle = "n = 80, p = 0.75, P = 0.95, α = 0.05",
    x = "True Proportion (π)",
    y = "Coverage Probability",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(comparison_plot)
