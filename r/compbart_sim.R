#
# COMPETITIVE BART SIMULATION
#

library(dplyr)
library(yarrr)
library(snowfall)
library(snow)

# Players 

# Player's mean pump rate is a function of..
#   prior.m :         prior mean
#   ind.last.group:   recent outcomes
#   comp.last.group:  competitor's recent outcomes
#   ind.pos.ur:       solitary updating rates when no pop
#   ind.neg.ur:       solitary cupdating rates when pop
#   comp.pos.ur:      competitive updating rates when beat opp
#   comp.neg.ur:      competitive updating rates when lose to opp

cbart.update.fun <- function(prior.m,
                             ind.last.group,
                             comp.last.group = NULL,
                             ind.pos.ur = 1,
                             ind.neg.ur = 1,
                             comp.pos.ur = 3,
                             comp.neg.ur = 1) {

  
  # If there is no competitive feedback
  if(is.null(comp.last.group)) {
    
    if(ind.last.group[length(ind.last.group)] == 0) {
      
      new.m <- prior.m - ind.neg.ur
      
    } else {
      
      new.m <- prior.m + ind.pos.ur
      
    }
    
  }
  
  # If there is competitive feeback
  if(is.null(comp.last.group) == FALSE) {
    
    # I LOST this round
    if(sum(comp.last.group) >= sum(ind.last.group)) {
      
      # I have popped lots of balloons
      
      if(mean(ind.last.group == 0) >= .5) {
        
        new.m <- prior.m - comp.neg.ur
        
      }
      # I have NOT popped lots of balloons
      
      if(mean(ind.last.group == 0) < .5) {
        
        new.m <- prior.m + comp.neg.ur
        
      }
      
    }
    
    # I WON this round...update by comp.pos.ur
    
    if(sum(comp.last.group) < sum(ind.last.group)) {
      
      new.m <- prior.m + comp.pos.ur
      
    }
    
  }
 
  return(new.m)
   
}


cbart.sim <- function(bpop.vec = sample(100, 
                                        size = 100, 
                                        replace = TRUE),
                      ind.pos.ur = c(2, 2),
                      ind.neg.ur = c(5, 5),
                      comp.pos.ur = c(0, 0),
                      comp.neg.ur = c(5, 10),
                      start.m = c(20, 20),
                      start.s = c(2, 2),
                      ff = 10
) {
# 
  # bpop.vec = sample(100, size = 100, replace = TRUE)
  # ind.pos.ur = c(1, 1)
  # ind.neg.ur = c(3, 3)
  # comp.pos.ur = c(2, 2)
  # comp.neg.ur = c(10, 10)
  # start.m = c(20, 20)
  # start.s = c(5, 5)
  # ff = 10
#   
  balloons.n <- length(bpop.vec)
  feedback.times <- seq(0, balloons.n, by = ff)
  
  p1.ind.pos.ur <- ind.pos.ur[1]
  p2.ind.pos.ur <- ind.pos.ur[2]
  
  p1.ind.neg.ur <- ind.neg.ur[1]
  p2.ind.neg.ur <- ind.neg.ur[2]
  
  p1.comp.pos.ur <- comp.pos.ur[1]
  p2.comp.pos.ur <- comp.pos.ur[2]
  
  p1.comp.neg.ur <- comp.neg.ur[1]
  p2.comp.neg.ur <- comp.neg.ur[2]
  
  
  # Set up storage
  
  p1.result <- as.data.frame(matrix(NA, nrow = length(bpop.vec), ncol = 6))
  p2.result <- as.data.frame(matrix(NA, nrow = length(bpop.vec), ncol = 6))
  
  names(p1.result) <- c("balloon", "m.current", "target", "end", "pop", "points")
  names(p2.result) <- c("balloon", "m.current", "target", "end", "pop", "points")
  
  p1.result$balloon <- 1:balloons.n
  p2.result$balloon <- 1:balloons.n
  
  p1.result$group <- rep(1:(length(feedback.times) - 1), each = ff)
  p2.result$group <- rep(1:(length(feedback.times) - 1), each = ff)

  for(i in 1:balloons.n) {

    
    group.current <- which(feedback.times == i) - 1
  
    # Current popping time
    
    pop.i <- bpop.vec[i]
    
    p1.result$pop[i] <- pop.i
    p2.result$pop[i] <- pop.i
    
    # Current means
    if(i == 1) {p1.m.current <- start.m[1]} else {p1.m.current <- p1.result$m.current[i]}
    if(i == 1) {p2.m.current <- start.m[2]} else {p2.m.current <- p2.result$m.current[i]}
    
    # Write
    p1.result$m.current[i] <- p1.m.current
    p2.result$m.current[i] <- p2.m.current
    
    # Get samples
    
    p1.s <- round(rnorm(1, mean = p1.m.current, sd = start.s[1]), 0)
    p2.s <- round(rnorm(1, mean = p2.m.current, sd = start.s[2]), 0)
    
    if(p1.s < 1) {p1.s <- 1}
    if(p2.s < 1) {p2.s <- 1}
    
    
    # Write
    p1.result$target[i] <- p1.s
    p2.result$target[i] <- p2.s
    
    # Determine outcome
    
    if(p1.s < pop.i) {
      
      p1.result$end[i] <- p1.s
      p1.result$pop[i] <- 0
      p1.result$points[i] <- p1.s
      
    }
    
    if(p1.s >= pop.i) {
      
      p1.result$end[i] <- pop.i
      p1.result$pop[i] <- 1
      p1.result$points[i] <- 0
      
    }
    
    if(p2.s < pop.i) {
      
      p2.result$end[i] <- p2.s
      p2.result$pop[i] <- 0
      p2.result$points[i] <- p2.s
      
    }
    
    if(p2.s >= pop.i) {
      
      p2.result$end[i] <- pop.i
      p2.result$pop[i] <- 1
      p2.result$points[i] <- 0
      
      
    }
    
    # Individual updating
    
    if((i %in% feedback.times) == FALSE) {
    
    p1.m.new <- cbart.update.fun(prior.m = p1.m.current,
                                 ind.last.group = p1.result$points[i],
                                 ind.pos.ur = p1.ind.pos.ur,
                                 ind.neg.ur = p1.ind.neg.ur,
                                 comp.pos.ur = p1.comp.pos.ur,
                                 comp.neg.ur = p1.comp.pos.ur
                                 )
    
    p2.m.new <- cbart.update.fun(prior.m = p2.m.current,
                                 ind.last.group = p2.result$points[i],
                                 ind.pos.ur = p2.ind.pos.ur,
                                 ind.neg.ur = p2.ind.neg.ur,
                                 comp.pos.ur = p2.comp.pos.ur,
                                 comp.neg.ur = p2.comp.pos.ur
    )
    }
    
    if((i %in% feedback.times) == TRUE) {
      
      p1.m.new <- cbart.update.fun(prior.m = p1.m.current,
                                   ind.last.group = p1.result$points[p1.result$group == group.current],
                                   comp.last.group = p2.result$points[p2.result$group == group.current],
                                   ind.pos.ur = p1.ind.pos.ur,
                                   ind.neg.ur = p1.ind.neg.ur,
                                   comp.pos.ur = p1.comp.pos.ur,
                                   comp.neg.ur = p1.comp.neg.ur
      )
      
      p2.m.new <- cbart.update.fun(prior.m = p2.m.current,
                                   ind.last.group = p2.result$points[p2.result$group == group.current],
                                   comp.last.group = p1.result$points[p1.result$group == group.current],
                                   ind.pos.ur = p2.ind.pos.ur,
                                   ind.neg.ur = p2.ind.neg.ur,
                                   comp.pos.ur = p2.comp.pos.ur,
                                   comp.neg.ur = p2.comp.neg.ur
      )
      
    }
    
    # Catch extreme values
    
    if(p1.m.new > 99) {p1.m.new <- 99}
    if(p2.m.new > 99) {p2.m.new <- 99}
    
    if(p1.m.new < 1) {p1.m.new <- 2}
    if(p2.m.new < 1) {p2.m.new <- 2}
      
    if(i < balloons.n) {
    p1.result$m.current[i + 1] <- p1.m.new
    p2.result$m.current[i + 1] <- p2.m.new
    
    }
  }
    
    # Done, summarise results
    
    p1.result$points.cum <- cumsum(p1.result$points)
    p2.result$points.cum <- cumsum(p2.result$points)
    
     
    return(list(p1.result, p2.result))
    
}
    
big.sim <- expand.grid(sim = 1:100,
                       p1.comp.pos.ur = c(0, 5),
                       p1.comp.neg.ur = c(0, 5),
                       p1.ind.pos.ur = c(0, 2),
                       p1.ind.neg.ur = c(0, 5),
                       p2.comp.pos.ur = c(0, 5),
                       p2.comp.neg.ur = c(0, 5),
                       p2.ind.pos.ur = c(0, 2),
                       p2.ind.neg.ur = c(0, 5))

sim.fun <- function(x) {
  
  print(x)
  p1.comp.pos.ur <- big.sim$p1.comp.pos.ur[x]
  p1.comp.neg.ur <- big.sim$p1.comp.neg.ur[x]
  p1.ind.pos.ur <- big.sim$p1.ind.pos.ur[x]
  p1.ind.neg.ur <- big.sim$p1.ind.neg.ur[x]
  
  p2.comp.pos.ur <- big.sim$p2.comp.pos.ur[x]
  p2.comp.neg.ur <- big.sim$p2.comp.neg.ur[x]
  p2.ind.pos.ur <- big.sim$p2.ind.pos.ur[x]
  p2.ind.neg.ur <- big.sim$p2.ind.neg.ur[x]
  
  
  result.i <- cbart.sim(bpop.vec = sample(100, 
                                          size = 100, 
                                          replace = TRUE),
                        ind.pos.ur = c(p1.ind.pos.ur, p2.ind.pos.ur),
                        ind.neg.ur = c(p1.ind.neg.ur, p2.ind.neg.ur),
                        comp.pos.ur = c(p1.comp.pos.ur, p2.comp.pos.ur),
                        comp.neg.ur = c(p1.comp.neg.ur, p2.comp.neg.ur),
                        start.m = c(20, 20),
                        start.s = c(2, 2),
                        ff = 10)
  
  output <- c(result.i[[1]]$points.cum[100], result.i[[2]]$points.cum[100])
  
  return(output)
  
}

snowfall::sfInit(parallel = TRUE, cpus = 4)
snowfall::sfExport('big.sim')
snowfall::sfExport('cbart.sim')
snowfall::sfExport('cbart.update.fun')
result.ls <- snowfall::sfClusterApplySR(1:nrow(big.sim), fun = sim.fun, perUpdate = 1)


sim.result <- as.data.frame(matrix(unlist(result.ls), nrow = nrow(big.sim), ncol = 2, byrow = TRUE))
names(sim.result) <- c("my.points", "comp.points")

big.sim <- cbind(big.sim, sim.result)    

big.sim$win <- big.sim$my.points > big.sim$comp.points
big.sim$group.points <- (big.sim$my.points + big.sim$comp.points) / 2

# DONE!

pirateplot(group.points ~ p1.comp.pos.ur + p1.comp.neg.ur, 
           data = subset(big.sim, 
                         p1.ind.pos.ur == 2 & p1.ind.neg.ur == 5 &
                         p2.ind.pos.ur == 2 & p1.ind.neg.ur == 5 &
                         p2.comp.pos.ur == 0 & p2.comp.neg.ur == 0
                         ))

big.sim.agg <- big.sim %>% group_by(p1.comp.pos.ur, 
                     p1.comp.neg.ur, 
                     p1.ind.pos.ur, 
                     p1.ind.neg.ur,
                     p2.comp.pos.ur, 
                     p2.comp.neg.ur, 
                     p2.ind.pos.ur, 
                     p2.ind.neg.ur) %>% summarise(
  win.p = mean(win),
  my.points.mean = mean(my.points),
  comp.points.mean = mean(comp.points),
  group.points.mean = mean(group.points),
  N = n()
)
 