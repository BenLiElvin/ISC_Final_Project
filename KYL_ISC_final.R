# variable name: virus strain + outburst year
A77 <- matrix(c(66,87,25,22, 4, 
                13,14,15, 9, 4,
                NA, 4, 4, 9, 1,
                NA, NA, 4, 3, 1, 
                NA, NA, NA, 1, 1,
                NA, NA, NA, NA, 0 ), nrow = 6, ncol = 5, byrow = TRUE)

A80 <- matrix(c(44,62,47,38, 9, 
                10,13, 8,11, 5,
                NA, 9, 2, 7, 3,
                NA, NA, 3, 5, 1, 
                NA, NA, NA, 1, 0,
                NA, NA, NA, NA, 1 ), nrow = 6, ncol = 5, byrow = TRUE)

B75 <- matrix(c( 9,12,18, 9, 4, 
                 1, 6, 6, 4, 3,
                 NA, 2, 3, 4, 0,
                 NA, NA, 1, 3, 2, 
                 NA, NA, NA, 0, 0,
                 NA, NA, NA, NA, 0 ), nrow = 6, ncol = 5, byrow = TRUE)

A78 <- matrix(c(15,12, 4, 
                11,17, 4, 
                NA,21, 4,
                NA,NA, 5 ), nrow = 4, ncol = 3, byrow = TRUE)

A77 <- sweep(A77, 2, colSums(A77,na.rm=T), "/")
A80 <- sweep(A80, 2, colSums(A80,na.rm=T), "/")
B75 <- sweep(B75, 2, colSums(B75,na.rm=T), "/")
A78 <- sweep(A78, 2, colSums(A78,na.rm=T), "/")
distance_statistic = function(D1,D2,Dpred1,Dpred2){
  FnormD1 = sqrt(sum((D1-Dpred1)^2,na.rm=T))
  FnormD2 = sqrt(sum((D2-Dpred2)^2,na.rm=T))
  result = .5 * (FnormD1 + FnormD2)
  return(result)
}
priors = function(){
  paramN = 4
  paramVec = runif(paramN,min = 0,max= 1)
  return(paramVec)
}
#the probability of j out of s people are infected, with qc the probability of not infected from the community, and qh not infected in a household

w = function(j,s,qc,qh){
  if (j == 0){
    result = qc^s
    return(result)
  } else if (j == s){
    CompVec = sapply(0:(j-1),w,s=s,qc=qc,qh=qh)
    result = 1-sum(CompVec)
    return(result)
  } else {
    result = choose(s,j)*w(j,j,qc,qh)*(qc*qh^j)^(s-j)
    return(result)
  }
}
# total_num is total number of people in a household
# output a (n+1)*n matrix in which column indicates the total number of people, row indicates the number of people infected. 
data_generating_model = function(total_num,qc,qh){
  results = matrix(NA,total_num+1,total_num)
  for (i in 1:(total_num+1)){
    for (j in 1:total_num){
      if ((i-1)<=j){
        results[i,j] = w(i-1,j,qc,qh)
      }
    }
  }
  return(results)
}
generate_abc_sample = function(observed_data,
                               priors,
                               data_generating_model,
                               distance_statistic,
                               epsilon) {
  while(TRUE) {
    theta = priors()
    # naming parameters
    qc1 = theta[1];qh1 = theta[2];qc2 = theta[3];qh2 = theta[4]
    D1 = observed_data$D1;D2 = observed_data$D2
    # compute model simulations
    Dpred1 = data_generating_model(ncol(D1),qc1,qh1)
    Dpred2 = data_generating_model(ncol(D2),qc2,qh2)
    if (distance_statistic(D1,D2,Dpred1,Dpred2) < epsilon) {
      # to track the progress
      print("One Simulation Done...")
      return(theta)
    }
  }
}



