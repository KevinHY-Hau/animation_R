library(dplyr)
library(magick)
library(animation) 
library(scales)
library(RColorBrewer)

# number of square wrap around the points
l <- 180

# variations
v <- 0.05

# colours
clrs <- c(hue_pal(h = c(60, 120))(floor(l/2)),
          hue_pal(h = c(120, 240))(ceiling(l/2)))

# matrix of signs
matSign <- expand.grid(x=c(-1,1),y=c(-1,1))

# data of original points and the random centre of gradient
grd <- data.frame(num = 1:25, expand.grid(x=-2:2, y = -2:2), 
                   as.data.frame(matrix(rnorm(100,0,v),nrow=25, ncol=4)))

# different numbers cycle of steps 
grd$stp <- c(rep(20,6), 
              rep(15,3),20, 
              20, 15,10,15, 20,
              20, rep(15,3), rep(20,6))

# make sure destination of next points are a bit further
grd$V3 <- -sign(grd$V1)*0.3+abs(grd$V3)
grd$V4 <- -sign(grd$V2)*0.3+abs(grd$V4)

# original position of x, y and steps
grd$OriV1 <- grd$V1
grd$OriV2 <- grd$V2
grd$Oristp <- grd$stp

saveGIF({
  ani.options(interval = 0.1, nmax = 240)
  for (k in 1:ani.options('nmax')) {
    dev.hold()
    
    # setup environments
    par(pty="s", bg="chocolate4",mar=c(0.2,0.2,0.2,0.2))
    plot(0, type="n", xlim=c(-2,2), ylim=c(-2,2), xlab="", ylab="", axes=F)
    
    # plot the points
    for(i in 1:nrow(grd))
        points(x=seq(grd$x[i], grd$x[i]+grd$V1[i], length.out=l),
               y=seq(grd$y[i], grd$y[i]+grd$V2[i], length.out=l), cex=(l:1)/10, 
               col= clrs ,pch=22)
    
    # calculate next position
    grd$V1 <- grd$V1 + (grd$V3 - grd$V1)/grd$stp
    grd$V2 <- grd$V2 + (grd$V4 - grd$V2)/grd$stp
    grd$stp <- grd$stp - 1
    
    if(any(grd$stp == 0)){
      for(i in 1:25){
        if(grd$stp[i] == 0){
          # make sure the next destination is in another court using the matrix of sign
          m1 <- matSign[matSign$x!=sign(grd$V1[i])|matSign$y!=sign(grd$V2[i]),][sample(1:3,1),]
          # generate the next random destination
          grd[i, c(6,7)] <- m1*(0.3+abs(rnorm(2,0,v)))
          
          if(grd$Oristp[i]+k==ani.options('nmax')){
            # back to the original position if it's the last round
            grd[i, c(6,7)] <- grd[i, c("OriV1","OriV2")]
          }
        }
      }
      # use the same step
      grd$stp[grd$stp==0] <- grd$Oristp[grd$stp==0]
    }
    ani.pause()   
  }
}, movie.name = "NFT_grid_pt.gif", img.name = "NFT_grid_pt", convert = "magick", cmd.fun, clean = TRUE)
