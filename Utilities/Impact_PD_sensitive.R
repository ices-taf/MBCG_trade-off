## calculate impact from RBS for the 10% most sensitive part of the community

RBS_sens <- function(Fd,a,b){        
  #  a  = slope of binomial model, 
  #  b  = intercept of binomial model, 
  #  Fd = fishing SAR x depletion rate (gear specific)

  # 3 equations
  step.size=.5
  longevity=seq(.5,200,by=step.size)
  
  r = 5.31/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  idx <- ifelse(is.na(sum(K)),NA,c(which(abs(cumsum(K)/sum(K) - 0.9) == min(abs(cumsum(K)/sum(K) - 0.9))):length(K)))
  K_sen <- K[idx]/sum(K[idx]*step.size)
  B = K_sen*(1 - Fd / r[idx]); B[B<0]=0
  
  RBS_sens=sum(B)*step.size
  RBS_sens
}