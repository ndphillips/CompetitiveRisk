# Given a player's pumps, max pumps, and balloons, what is their final cumulative point distribution?
bart.pmf.fun <- function(pumps = 5,          # How many pumps? Can be an integer, vector, or dataframe
                        max.pumps = 10, 
                        balloon.n = 10,
                        nsim = 1e4,          # Only for vectors of pumping rates
                        plot = FALSE,
                        ylim = NULL,
                       xlim = NULL,
                        main = NULL,
                        add = FALSE, 
                        col = gray(.5, .5)) {

  
  # pumps = 5
  # max.pumps = 10
  # balloon.n = 10
  # nsim = 1e4
  # plot = FALSE
  # ylim = NULL
  # xlim = NULL
  # main = NULL
  # add = FALSE
  # col = gray(.5, .5)
  # 
  
  y.p.pmf <- NULL
  
  if(class(pumps) != "data.frame") {
    
    if(length(pumps) == 1) {
      
      success.p <- 1 - (pumps / max.pumps)
      success.pmf <- dbinom(0:balloon.n, size = balloon.n, prob = success.p)
      points.pmf <- pumps * 0:balloon.n
      
      pmf <- data.frame("points" = points.pmf,
                        "prob" = success.pmf)
      


    }
    
    if(length(pumps) > 1) {
      
      y.p.pmf <- data.frame(pumps, rep(1 / length(pumps), length(pumps)))
      
    }
    
  }
  
  if(class(pumps) == "data.frame" | (is.null(y.p.pmf) == FALSE)) {
    
    # Get pump rates for all simulations
    y.p <- sample(y.p.pmf[,1], size = balloon.n * nsim, prob = y.p.pmf[,2], replace = TRUE)
    
    # Get pop value for each balloon
    pops <- sample(1:max.pumps, size = nsim * balloon.n, replace = TRUE)
    
    # Get points for each balloon
    points <- y.p
    points[y.p >= pops] <- 0
    
    # Aggregate points within each group of balloon.n balloons
    game <- rep(1:nsim, each = balloon.n)
    df <- data.frame("points" = points, game = game)
    agg <- aggregate(points ~ game, data = df, FUN = sum)
    
    # Calculate final pmf
    
    final.pmf <- data.frame(points = 0:(max.pumps * balloon.n),
                            prob = NA)
    
    # Get probabilities from agg
    final.pmf$prob <- sapply(final.pmf$points, FUN = function(x) {
      
      mean(agg$points == x)
      
    })
    
    pmf <- subset(final.pmf, prob > 0)
    
  }

  ev <- sum(pmf$points * pmf$prob)
  variance <- sum((pmf$points - ev) ^ 2 * pmf$prob)
    
    if(is.null(xlim)) {xlim <- c(min(pmf$points), max(pmf$points))}
    if(is.null(ylim)) {ylim <- c(0, max(pmf$prob))}
    
    if(plot) {
      
      if(add == FALSE) {
        
        if(length(pumps) > 1) {pumps.txt <- paste0(pumps, collapse = ",")} else{
          
          pumps.txt <- pumps
        }
        
        if(is.null(main)) {main <- paste0("xp = ", pumps.txt, ", max = ", max.pumps, ", N = ", balloon.n, "\nEV = ", round(ev, 2), " sd = ", round(sqrt(variance), 2))}
        
        plot(1, 
             xlim = xlim,
             ylim = ylim,
             type = "n",
             xlab = "Earnings",
             ylab = "Probability", 
             main = main
        )
        
      }
      
      segments(pmf$points, 
               rep(0, nrow(pmf)), 
               pmf$points, 
               pmf$prob, lwd = 5, col = col)
      
    }
    
    return(list("pmf" = pmf, "ev" = ev, "var" = variance, "sd" = sqrt(variance)))
    
}


# Compare two pumping distributions
cbart.fun <- function(x.p = 5,         # A vector of equally likely pumping times
                      y.p = 1,         # A vector of equally likely pumping times
                      max.pumps = 10, 
                      balloon.n = 10,
                      nsim = 1e4,
                      plot = FALSE
) {

  
  # x.p = 1
  # y.p = 1:10 
  # max.pumps = max.pumps
  # balloon.n = balloon.n
  # nsim = nsim
  # 
  
  x.result <- bart.pmf.fun(x.p, max.pumps = max.pumps, balloon.n = balloon.n)
  x.pmf <- x.result$pmf
  
  y.result <- bart.pmf.fun(y.p, max.pumps = max.pumps, balloon.n = balloon.n)
  y.pmf <- y.result$pmf
  
  diff.pmf <- expand.grid(x.points = x.pmf$points,
                          y.points = y.pmf$points)
  
  diff.pmf <- merge(diff.pmf, data.frame(x.points = x.pmf$points, x.prob = x.pmf$prob), all = TRUE)
  diff.pmf <- merge(diff.pmf, data.frame(y.points = y.pmf$points, y.prob = y.pmf$prob), all = TRUE)
  
  diff.pmf$diff.points <- with(diff.pmf, x.points - y.points)
  diff.pmf$prob <- with(diff.pmf, x.prob * y.prob)
  diff.pmf <- aggregate(prob ~ diff.points, FUN = sum, data = diff.pmf)
  
  diff.ev <- sum(diff.pmf[,1] * diff.pmf[,2])
  diff.var <- sum((diff.pmf[,1] - diff.ev) ^ 2  * diff.pmf[,2])

  p.x.win <- sum(diff.pmf$prob[diff.pmf$diff.points > 0])
  p.tie <- sum(diff.pmf$prob[diff.pmf$diff.points == 0])
  p.y.win <- sum(diff.pmf$prob[diff.pmf$diff.points < 0])
  
  if(plot) {
    
    plot(diff.pmf[,1], diff.pmf[,2], xlim = c(min(diff.pmf$diff.points), max(diff.pmf$diff.points)),
         ylim = c(0, max(diff.pmf$prob)), type = "b"
         )
    
    abline(v = diff.ev, lty = 2)
    
    
  }
  
  
  return(list("p.x.win" = p.x.win, 
              "p.tie" = p.tie, 
              "p.x.lose" = p.y.win, 
              diff.pmf = diff.pmf,
              diff.ev = diff.ev,
              diff.var = diff.var,
              x.pmf = x.pmf,
              x.ev = x.result$ev,
              x.var = x.result$var,
              x.sd = x.result$sd,
              y.pmf = y.pmf,
              y.ev = y.result$ev,
              y.var = y.result$var,
              y.sd = y.result$sd))
  
}


# cbart best response function:
#  Given a player y with a pumping probability mass function,
#  what is player x's best response?

cbart.br.fun <- function(y.p,
                         max.pumps = 10, 
                         balloon.n = 1, 
                         nsim = 1e4,
                         plot = FALSE) {
  
  # Compare all possible x pumping rates to y.p
  
  comp.pmfs <- lapply(1:max.pumps, FUN = function(x) {
    
    cbart.fun(x.p = x, 
              y.p = y.p, 
              max.pumps = max.pumps, 
              balloon.n = balloon.n, 
              nsim = nsim)
    
  })
  
  # Get win probabilities
  
  x.win.v <- sapply(1:length(comp.pmfs), FUN = function(x) {comp.pmfs[[x]]$p.x.win})
  x.tie.v <- sapply(1:length(comp.pmfs), FUN = function(x) {comp.pmfs[[x]]$p.tie})
  x.lose.v <- sapply(1:length(comp.pmfs), FUN = function(x) {comp.pmfs[[x]]$p.x.lose})
  
  x.win.nt.v <- x.win.v + x.tie.v / 2
  
  # Get best response
  br.nt <- which(round(x.win.nt.v, 4) == max(round(x.win.nt.v, 4)))
  br <- which(round(x.win.v, 4) == max(round(x.win.v, 4)))
  
  result.df <- data.frame("x" = 1:max.pumps,
                          "p.win"  = x.win.v,
                          "p.tie" = x.tie.v,
                          "p.lose" = x.lose.v,
                          "p.win.nt" = x.win.nt.v)
  
  
  
  output <- list("br.nt" = br.nt, br = br, result = result.df)
  
  if(plot) {
    
    br.pwin <- result.df$p.win.nt[br.nt]
    
    y.pump.txt <- paste0(y.p, collapse = ",")
    
    plot(result.df$x, result.df$p.win.nt, type = "b", ylim = c(0, 1.1), xaxt = "n",
         xlab = "Player x's pumps", main = paste("Y pumps:", y.pump.txt), ylab = "p(x wins)", yaxt = "n")
    
    
    mtext(3, text = paste0("max pumps = ", max.pumps, ", balloons = ", balloon.n), line = .5)
    
    axis(1, result.df$x)
    axis(2, seq(0, 1, .1), las = 1)
    
    grid()
    
    abline(h = .5, lty = 2)
    
    text(br.nt, br.pwin, labels = paste0("[", br.nt, ", ", round(br.pwin, 2) * 100, "%]"), pos = 3)
    points(br.nt, br.pwin, pch = 16, col = "skyblue", cex = 1.5)
    
    arrows(br.nt, 0, br.nt, br.pwin - .05, length = .1, lty = 1, col = gray(.5))
    text(br.nt + .25, 0, labels = "X's BR to Y", adj = 0, cex = .7)
    
  }
  
  return(output)
  
}


# plot only the best response to another player
iso.curve.fun <- function(df, 
                          main = "", 
                          dv = "p.x.win.nt") {
  
  
pump.vals <- unique(df$x.p)
balloon.n.vec <- unique(df$balloon.n)
  
  result.df <- matrix(NA, nrow = length(pump.vals), ncol = length(balloon.n.vec))
  
  for(i in 1:length(balloon.n.vec)) {
    
    # Get best response for each y.p value
    
    best.resp.v <-  sapply(pump.vals, FUN = function(x) {
      
    if(dv == "p.x.win") {
      
      best.val <- df %>% 
        mutate(p.x.win.r = round(p.x.win, 3)) %>%
        filter(balloon.n == balloon.n.vec[i] & y.p == x) %>%
        filter(p.x.win.r == max(p.x.win.r)) %>% select(x.p)
      
    }
      
    if(dv == "p.x.win.nt") {
      
      best.val <- df %>% 
        mutate(p.x.win.nt.r = round(p.x.win.nt, 3)) %>%
        filter(balloon.n == balloon.n.vec[i] & y.p == x) %>%
        filter(p.x.win.nt.r == max(p.x.win.nt.r)) %>% select(x.p)
      
    }
      
      best.val <- as.numeric(mean(unlist(best.val)))
      
      
      
    })
    
    result.df[,i] <- best.resp.v 
    
  }
  
  
  plot(1, xlim = c(1, max(pump.vals)), 
       ylim = c(1, max(pump.vals)), 
       type = "n", xlab = "y.pumps", 
       ylab = "x's best response to y.pumps", xaxt = "n", yaxt = "n", main = main)
  grid()
  
  axis(1, pump.vals, las = 1)
  axis(2, pump.vals, las = 1)
  
  col.vec <- piratepal("xmen", trans = .2, length.out = length(balloon.n.vec))
  
  for(i in 1:length(balloon.n.vec)) {
    
    lines(pump.vals, result.df[,i], lty = i, col = col.vec[i], lwd = 3)
    
  }
  
  abline(h = mean(result.df[,i]), lty = 2, lwd =2)
  text(9,  mean(result.df[,i]) - .4, labels = paste0("Mean =", round(mean(result.df[,i]), 2)), adj = 1)
  
  
  legend("topleft", legend = paste("N = ", balloon.n.vec, "balloons"), 
         lty = 1:length(balloon.n.vec), 
         bty = "n", 
         col = col.vec, lwd = 3)
  
}

# Function for creating heatplots
heat.fun <- function(formula, 
                     data, 
                     min.val = NULL, 
                     max.val = NULL,
                     main = NULL,
                     rounding = 0,
                     xlab = NULL,
                     ylab = NULL,
                     cex = 2, 
                     header = FALSE,
                     summaries = FALSE) {
  
  
  
  data.mf <- model.frame(formula, data)
  dv <- data.mf[,1]
  dv.name <- names(data.mf)[1]
  n.iv <- ncol(data.mf) - 1
  
  
  
  # Adjust min and max val
  
  if(is.null(min.val)) {min.val <- min(dv)}
  if(is.null(max.val)) {max.val <- max(dv)}
  
  if(max(dv) > max.val) {max.val <- max(dv)}
  if(min(dv) < min.val) {min.val <- min(dv)}
  
  
  iv.names <- names(data.mf[,2:ncol(data.mf)])
  
  
  if(is.null(xlab)) {xlab <- iv.names[1]}
  if(is.null(ylab)) {ylab <- iv.names[2]}
  
  
  if(n.iv == 2) {n.plots <- 1}
  if(n.iv == 3) {n.plots <- length(unique(data.mf[,4])) ; iv3.name <- iv.names[3]}
  
  iv.vals.ls <- lapply(1:n.iv, function(x) {data.mf[,x + 1]})
  iv.unique.ls <- lapply(1:n.iv, function(x) {sort(unique(data.mf[,x + 1]))})
  
  data.agg.ls <- vector("list", length = n.plots)
  
  # Create aggregate Data
  
  for(i in 1:n.plots) {
    
    if(n.iv == 2) {
      
      data.temp <- data.mf
      
    }
    
    if(n.iv == 3) {
      
      iv3.val <- iv.unique.ls[[3]][i]
      
      data.temp <- data.mf[data.mf[,4] == iv3.val,]
      
    }
    
    data.agg <- aggregate(formula = formula, 
                          data = data.temp, 
                          FUN = mean, 
                          na.rm = TRUE)
    
    
    # Scale
    
    dv.i <- data.agg[,ncol(data.agg)]
    dv.s <- (dv.i - min.val) / (max.val - min.val)
    
    data.agg$dv.s <- dv.s
    
    
    data.agg.ls[[i]] <- data.agg
    
  }
  
  # PLOTTING
  
  heat.col.fun <- circlize::colorRamp2(c(0, 1), colors = c("red", "blue"))
  
  if(header) {
    
    layout(matrix(c(rep(1, n.plots), 2:(n.plots + 1)), nrow = 2, ncol = n.plots, byrow = TRUE), widths = rep(3, n.plots), heights = c(.5, 3))
    
  }
  
  if(header == FALSE) {
    
    par(mfrow = c(1, n.plots))
    
  }
  
  # par(mfrow = c(1, n.plots))
  
  
  if(is.null(main)) {main <- names(data.mf)[1]}
  
  if(header) {
    par(mar = c(0, 1, 0, 1))
    
    plot.new()
    
    text(.5, .5, main, cex = 3)
    
  }
  
  par(mar = c(5, 4, 4, 1) + .1)
  
  for(plot.i in 1:n.plots) {
    
    x1.vals <- data.agg.ls[[plot.i]][,1]
    x2.vals <- data.agg.ls[[plot.i]][,2]
    dv.vals <- data.agg.ls[[plot.i]]$dv.s
    dv.o.vals <- data.agg.ls[[plot.i]][,n.iv + 1]
    
    x1.index.vals <- match(x1.vals, iv.unique.ls[[1]])
    x2.index.vals <- match(x2.vals, iv.unique.ls[[2]])
    
    if(n.plots > 1) {main <- ""}
    
    plot(1, xlim = c(.25, length(iv.unique.ls[[1]]) + .75), ylim = c(.25, length(iv.unique.ls[[2]]) + .75), 
         type = "n", xaxt = "n", yaxt = "n", 
         xlab = xlab, ylab = ylab, main = main)
    
    if(n.iv == 3) {mtext(paste(iv3.name, " = ", iv.unique.ls[[3]][plot.i]), 3)}
    
    axis(1, at = 1:length(iv.unique.ls[[1]]), labels = iv.unique.ls[[1]], las = 1)
    axis(2, at = 1:length(iv.unique.ls[[2]]), labels = iv.unique.ls[[2]], las = 1)
    
    rect(x1.index.vals - .5, x2.index.vals - .5, x1.index.vals + .5, x2.index.vals + .5, pch = 15, col = heat.col.fun(dv.vals))
    
    text(x1.index.vals, x2.index.vals, round(dv.o.vals, rounding), col = "white", font = 2, cex = cex)
    
  }
  
  
}
