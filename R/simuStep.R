#' simuStep function
#' 
#' This function allows you to simulate a random process of walking.
#' Assuming you have equally probability to take your next step towards
#' all the directions, and then another step again. The output will be a
#' figure show all the foot prints.
#' 
#' @n enter a integer number to indicate the steps you want to take.
#' @keywords simuStep
#' @export
#' @examples 
#' simuStep(10000) # assuming 10000 steps
#'
simuStep <- function(n){
  if(n %% 1 != 0 | n <= 0 ) {
    print("Enter an positive interger!")
  }
  sim_dat <- data.frame("time" = seq(1,n,1), "X" = rep(0,n), "Y" = rep(0,n))
  x <- 0
  y <- 0
  for (i in 1:n){
    t <- oneStep(x,y)
    x <- t[1]
    y <- t[2]
    sim_dat[i,2] <- t[1]
    sim_dat[i,3] <- t[2]
    #sim_dat <- rbind(sim_dat,data.frame("time" = n,"X" = t[1], "Y" = t[2]))
  }
  plot(sim_dat$X, sim_dat$Y, pch = 19, xlab = "", ylab = "", cex = 0.5)
  abline(v = 0, h = 0, lty = 3)
  points(0,0, ,pch = 19,col = "red")
  points(sim_dat[n,2],sim_dat[n,3], pch = 17, col = "red")
}

oneStep <- function(x,y){
  angle <- runif(1,0,360)
  x1 <- x + cos(angle*pi/180)
  y1 <- y + sin(angle*pi/180)
  return(c(x1,y1))
}

