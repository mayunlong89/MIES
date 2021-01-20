
#### CI of a vecter of numeric
## @param sd_value: strandard deviation
## @param n: number of numeric
#### return the width of CI
numberCI = function(sd_value,n){
  sd_value*sd_value*qnorm(1-0.05 / 2)/sqrt(n)
}



#### CI of rate
## @param p: rate
## @param n: number of samples
#### return the width of CI
CI = function(p,n, z=1.96){
  sd = z*sqrt(p*(1-p)/n)
  return(sd)
}