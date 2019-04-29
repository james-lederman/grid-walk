m <- 11
n <- 7

set.seed(100)

deviations <- sapply(1:1000, function(x){  
  x0 <- sample(c(rep(0, n), rep(1, m)))
  y0 <- c(0, cumsum(as.integer(!x0)))
  x0 <- c(0, cumsum(x0))
  
  deviations <- abs((x0/m) - (y0/n))
})

x0 <- sample(c(rep(0, n), rep(1, m)))
y0 <- c(0, cumsum(as.integer(!x0)))
x0 <- c(0, cumsum(x0))

a <- -n
b <- m
closest_x = (b*((b*x0) - (a*y0))) / (a^2 + b^2)
closest_y = (a*((-b*x0) + (a*y0))) / (a^2 + b^2)

plot(
  x0, y0,
  type = 'l', lwd = 2,
  xlab = 'm', ylab = 'n',
  xlim = c(0, max(m,n)), ylim = c(0, max(m, n)),
  xaxt = 'n',
  yaxt = 'n'
)
lines(c(0, m), c(0, n), lwd = 2, col = "gray44", lty = 1)

#draw the grid
segments(
  x0 = 0:m,
  y0 = 0,
  x1 = 0:m,
  y1 = n,
  lty = 3
)
segments(
  x0 = 0,
  y0 = 0:n,
  x1 = m,
  y1 = 0:n,
  lty = 3
)

#plot the deviations
segments(
  x0 = x0,
  y0 = y0,
  x1 = closest_x,
  y1 = closest_y,
  col = "red",
  lwd = 2,
  lty = 2
)
axis(1, at = 0:m)
axis(2, at = 0:n)
########################

nways.f <- function(m, n){ #recursive functions to solve the number of grid walks from a point to 0,0
  if(m == 0 & n == 0) return(1) #if m and n are both 0, we have arrived at our destination, so we have found a valid path, thus return 1
  if(m * n > 0) return(nways.f(m-1, n) + nways.f(m, n-1)) #if m nor n are 0, return the sum of ways from the point east and point north
  if(m == 0) return(nways.f(m, n-1)) #if m is 0, return the number of ways from the only accessible point (m, n-1)
  if(n == 0) return(nways.f(m-1, n)) #if n is 0, same as above, return ways from (m-1, n)
}

waysByPoint <- unlist(lapply(0:m, function(x){
  lapply(0:n, function(y){
#    waysTo <- factorial(x+y)/(factorial(x) * factorial(y))
    waysTo <- nways.f(x, y)
#    waysFrom <- factorial((n-y) + (m-x))/(factorial(n-y) * factorial(m-x))
    waysFrom <- nways.f(m-x, n-y)
    totalWays <- waysTo * waysFrom
    dev <- abs((x/m) - (y/n))
    return(totalWays)
  })
}))

devsByPoint <- unlist(lapply(0:m, function(x){
  lapply(0:n, function(y){
    deviation <- abs((x/m) - (y/n))
    return(deviation)
  })
}))

#inverse.rle will create a vector with each value of devsByPoint repeated waysByPoint-times (for the corresponding value in waysByPoint)
allPossibleDevs <- inverse.rle(list(values = devsByPoint, lengths = waysByPoint))
paste("mean: ", mean(allPossibleDevs)) #this should give the correct solution for mean and sd
paste("sd: ", sd(allPossibleDevs))

#what is the conditional probability that D is greater than 0.6 given that it is greater than 0.2?
devsOver0.2 <- allPossibleDevs[allPossibleDevs > 0.2]
length(devsOver0.2[devsOver0.2 > 0.6]) / length(devsOver0.2)


    

