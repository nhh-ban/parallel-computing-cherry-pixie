# Timer file 

# Load packages 
library(tictoc)

# Source files will be referred to like this 
# source("scripts/solution1.r")
# source("scripts/solution2.r")
# source("scripts/solution3.r")

# Timer 

tic.clearlog()
tic.clear()

# solution 1 - 63.443 sec
tic("time 1")
source("scripts/solution1.r")
toc(log = TRUE)

# solution 2 - 2.128 sec
tic("time 2")
source("scripts/solution2.r")
toc(log = TRUE)

# solution 3 - 17.714 sec
tic("time 3")
source("scripts/solution3.r")
toc(log = TRUE)


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

# Comparison 
printTicTocLog() %>%
  knitr::kable()

# In this the parallel loop from solution 2 is fastest. My best guess would be 
# it is because I have no idea how to answer problem 2.3 so it is not 
# entirely correct, therefore making solution 2 the best one. 

