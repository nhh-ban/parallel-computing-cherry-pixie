# Assignment 1:  
library(tweedie) 
library(ggplot2)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

# switch out the for loop 
library(doParallel)
library(tictoc)
library(purrr)
library(furrr)

# timer from lecture material 
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

tic.clearlog()


# New loop: using parallel computing  
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Cluster the cores:
cl <- makeCluster(Cores)

# Register the cluster:
registerDoParallel(cl)

# Taking the time:
tic(paste0("Parallel loop, ", Cores, " cores"))

two <-
  foreach(
    i = 1:nrow(df),
    .combine = 'rbind',
    .packages = c('magrittr', 'dplyr')
  ) %dopar%
  tibble(
    N=df$N[i], 
    M=df$M[i]
  )

# Stop the clusters
stopCluster(cl)

toc(log = TRUE)

# Check comparison 
printTicTocLog() %>%
  knitr::kable()

