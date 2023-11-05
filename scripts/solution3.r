# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(doParallel)
library(tictoc)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2: 
# Problem 2.3. 
# Changing MTweedieTests 

MTweedieTests <-  
  function(N,M,sig){ 
    simTweedieTest <-  # it somehow could not find this function if I did not write it again
      function(N){ 
        t.test( 
          rtweedie(N, mu=10000, phi=100, power=1.9), 
          mu=10000 
        )$p.value 
      } 
    
    # Add info and misc 
    # Cores 
    maxcores <- 8
    Cores <- min(parallel::detectCores(), maxcores)
    # Cluster
    cl <- makeCluster(Cores)
    # Register
    registerDoParallel(cl)
    
    # M tests split 
    m_tests<- 
      foreach(
        m = 1:M,
        .combine = c,
        .packages = c('tweedie')
      ) %dopar% {
      simTweedieTest(M)
      }
    
    # writing what to return 
      ans <- sum(m_tests < sig)/M 
    
    # Stop the clusters
    stopCluster(cl)
    
    return(ans)
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

