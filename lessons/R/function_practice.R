set.seed(4)

## make some fake data
nreps <- 5
fake.data <- data.frame(location = rep(c("A", "B", "C"), each=nreps),
                        nitrogen = abs(c(rnorm(nreps, 2, 1),
                                         rnorm(nreps, 4, 1),
                                         rnorm(nreps, 1, 1))))



makeFakeData <- function(x, slope, intercept){
  y <- intercept + slope*x  
  return(y)
}

## all the locations have the same slope
fake.data$growth <- makeFakeData(fake.data$nitrogen,
                                    slope=2,
                                    intercept=4)

locs <- unique(fake.data$location)
cols <- c("darkolivegreen", "goldenrod3", "dodgerblue")
names(cols) <- locs

## first try
for(location in locs){
  if(location == "A"){
    plot(x = fake.data$nitrogen[fake.data$location == location], 
         y = fake.data$growth[fake.data$location == location], 
         col=cols[location], pch=16,
         ylab="Growth",
         xlab="Nitrogen")
  }else{
    points(x = fake.data$nitrogen[fake.data$location == location],
           y = fake.data$growth[fake.data$location == location], 
           col=cols[location], pch=16)
  }
}

## a bit better
for(location in locs){
  this.N <- fake.data$nitrogen[fake.data$location == location]
  this.growth <- fake.data$growth[fake.data$location == location]
  if(location == "A"){
    plot(x = this.N, 
         y = this.growth, 
         col=cols[location], pch=16,
         ylab="Growth",
         xlab="Nitrogen")
  }else{
    points(x = this.N,
           y = this.growth, 
           col=cols[location], pch=16)
  }
}



## what if we wanted them to have different slopes
new_growth <- vector(mode="list", 
                     length=length(locs))
slopes <- c(2,3,7)
intercepts <- c(1,2,1)
names(new_growth) <- names(slopes) <- names(intercepts) <- locs


for(location in locs){
  new_growth[[location]] <- 
    makeFakeData(fake.data$nitrogen[fake.data$location == location],
                    slope=slopes[location],
                    intercept=intercepts[location])
}

fake.data$new.growth <- unlist(new_growth)



## close but the ylim are not nice
plotFakeData <- function(fake.data, cols, ycol, locs){
  for(location in locs){
    this.N <- fake.data$nitrogen[fake.data$location == location]
    this.growth <- fake.data[fake.data$location == location, ycol]
    if(location == "A"){
      plot(x = this.N, 
           y = this.growth, 
           col=cols[location], pch=16,
           ylab="Growth",
           xlab="Nitrogen")
    }else{
      points(x = this.N,
             y = this.growth, 
             col=cols[location], pch=16)
    }
  }
}

plotFakeData(fake.data, cols, "new.growth", locs)



## close but the ylim and xlim are not nice
plotFakeData <- function(fake.data, cols, ycol, locs){
  for(location in locs){
    this.N <- fake.data$nitrogen[fake.data$location == location]
    this.growth <- fake.data[fake.data$location == location, ycol]
    if(location == "A"){
      plot(x = this.N, 
           y = this.growth, 
           col=cols[location], pch=16,
           ylab="Growth",
           xlab="Nitrogen",
           ylim = range(fake.data[,ycol]),
           xlim = range(fake.data$nitrogen))
    }else{
      points(x = this.N,
             y = this.growth, 
             col=cols[location], pch=16)
    }
  }
}

plotFakeData(fake.data, cols, "new.growth", locs)

pdf("fig/testfig.pdf")
plotFakeData(fake.data, cols, "new.growth", locs)
dev.off()
