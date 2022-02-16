library(viridis)
library(magick)
library(animation)

drawCircle <- function(x0, y0, rad, startAngle, endAngle, cols="blue", lwd=1){
  
  # calculate the circle
  x <- seq(x0-rad, x0+rad, length.out = 180)
  y <- c(y0 + sqrt(rad^2 - (x-x0)^2), y0 - sqrt(rad^2 - (sort(x, T) -x0)^2))
  x <- c(x, sort(x, T))
  
  # extract based on angle
  if(startAngle == 1 & endAngle == 360){
    x <- c(x, x[1])
    y <- c(y, y[1])
  }
  if(endAngle < startAngle){
    x <- c(x[startAngle:360], x[1:endAngle])
    y <- c(y[startAngle:360], y[1:endAngle])
  } else{
    x <- x[startAngle:endAngle] 
    y <- y[startAngle:endAngle]
  }
  lines(x, y, col=cols, lwd=lwd)
}

colr <- rainbow(360)

saveGIF({
  ani.options(interval = 0.05, nmax = 360)
  for (i in 1:ani.options('nmax')) {
    dev.hold()
    # set up env
    par(pty="s", bg="black",mar=c(0.2,0.2,1.5,0.2))
    plot(0, type="n", xlim=c(0,100),ylim=c(0,100), axes=F, xlab="", ylab="")
    # draw circle segment from small to big
    if(i <= 180) for(j in 1:i) drawCircle(50, 50, 1+j/5, 1, j*2, cols=colr[2*j],  lwd=1.5)
    # reverse the circle segment
    if(i > 180 & i <= 360) for(j in 1:(361-i)) drawCircle(50, 50, 1+j/5, 1, j*2, cols=colr[2*j],  lwd=1.5)
    ani.pause()   
  }
}, movie.name = "rainbow.gif", img.name = "rainbow", convert = "magick", cmd.fun, clean = TRUE)


