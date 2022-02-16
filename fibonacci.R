library(dplyr)
library(magick)
library(animation) 
library(scales)
library(RColorBrewer)

# for rotation and sizing 
GoldenRatio <- (1+sqrt(5))/2

# the next sequence of square is either 4 directions
dr <- c("up", "right", "down", "left")

# calculate the next square location 4 points x y from the other points 
fib_fn <- function(df){

  # calculate the fibonacci sequence   
  fib <- rep(1,nrow(df)+1)
  for(i in 3:length(fib)) fib[i] <- fib[i-1] + fib[i-2]

  # length of square 
  leng <- fib[length(fib)]
  
  # get the next direction
  direction_xy <- dr[((nrow(df)) %% 4)+1]
  
  # designed in a way to always use the 3rd point from previous
  xy1 <- c(df$x3[nrow(df)], df$y3[nrow(df)])
  
  # calculate the other 3 points
  if(direction_xy == "up"){
    xy2 <- c(xy1[1], xy1[2]+leng)
    xy3 <- c(xy1[1]+leng, xy1[2]+leng)
    xy4 <- c(xy1[1]+leng, xy1[2])
  } else if(direction_xy == "right"){
    xy2 <- c(xy1[1]+leng, xy1[2])
    xy3 <- c(xy1[1]+leng, xy1[2]-leng)
    xy4 <- c(xy1[1], xy1[2]-leng)
  } else if(direction_xy == "down"){
    xy2 <- c(xy1[1], xy1[2]-leng)
    xy3 <- c(xy1[1]-leng, xy1[2]-leng)
    xy4 <- c(xy1[1]-leng, xy1[2])
  } else if(direction_xy == "left"){
    xy2 <- c(xy1[1]-leng, xy1[2])
    xy3 <- c(xy1[1]-leng, xy1[2]+leng)
    xy4 <- c(xy1[1], xy1[2]+leng)
  } 
  # output the next sequence of location on graph
  data.frame(x1 = xy1[1], y1 = xy1[2], 
             x2 = xy2[1], y2 = xy2[2], 
             x3 = xy3[1], y3 = xy3[2],
             x4 = xy4[1], y4 = xy4[2])
}                  

# first 2 fibonacci location 
fibdf <- data.frame(
  x1=c(0,1),y1=c(0,1),  x2=c(0,2),y2=c(1,1),
  x3=c(1,2),y3=c(1,0),  x4=c(1,1),y4=c(0,0)
)
# calculate the rest
for(i in 3:31) fibdf <- rbind(fibdf, fib_fn(fibdf))

# for rotation angle
ang <- 0*pi/180

j <- 1 # the number of sequence
r <- 1 # for rotation

# colours
clrs <- hcl.colors(31, palette = "Tropic")

saveGIF({
  ani.options(interval = 0.1, nmax = 540)
  for (k in 1:ani.options('nmax')) {
    dev.hold()
    
    # graph environment
    par(pty="s", bg="chocolate4",mar=c(0.2,0.2,0.2,0.2))
    plot(0, type="n", xlim=c(-14,18), ylim=c(-18,14), xlab="", ylab="", axes=F)
  
    # 18 small steps to display next square
    if(k %% 18 == 0) j <- j + 1
    
    # draw all the squares up to j
    for(i in 1:j){
      if(any(as.numeric(fibdf[i,])>0.001)){
        
        # square location
        x1 <- c(as.numeric(fibdf[i,(0:3)*2+1]),fibdf[i,1])
        y1 <- c(as.numeric(fibdf[i,(1:4)*2]),fibdf[i,2])
        
        # calculate rotation
        polygon(x=x1*cos(ang)-y1*sin(ang),
                y=x1*sin(ang)+y1*cos(ang),
                border="dark blue", col=clrs[i])
      }
    }
    
    # draw the transition of next sequence square
    if(k %% 18 !=0){
      
      # direction
      direction_xy <- dr[((j) %% 4)+1]
      
      # extract the next location
      x_1 <- as.numeric(fibdf[j+1,(0:3)*2+1])
      y_1 <- as.numeric(fibdf[j+1,(1:4)*2])
      
      # in the square, only 2 points need to change through time
      if(direction_xy == "up"){
        y_1[2] <- y_1[1] + (y_1[2]-y_1[1]) * (k %% 18)/18
        y_1[3] <- y_1[4] + (y_1[3]-y_1[4]) * (k %% 18)/18
      } else if(direction_xy == "right"){
        x_1[2] <- x_1[1] + (x_1[2]-x_1[1]) * (k %% 18)/18
        x_1[3] <- x_1[4] + (x_1[3]-x_1[4]) * (k %% 18)/18
      } else if(direction_xy == "down"){
        y_1[2] <- y_1[1] - (y_1[1]-y_1[2]) * (k %% 18)/18
        y_1[3] <- y_1[4] - (y_1[4]-y_1[3]) * (k %% 18)/18
      } else if(direction_xy == "left"){
        x_1[2] <- x_1[1] - (x_1[1]-x_1[2]) * (k %% 18)/18
        x_1[3] <- x_1[4] - (x_1[4]-x_1[3]) * (k %% 18)/18
      } 
      
      # rotation
      x1 <- x_1*cos(ang)-y_1*sin(ang)
      y1 <- x_1*sin(ang)+y_1*cos(ang)
      
      # draw
      polygon(x=c(x1,x1[1]), y=c(y1,y1[1]), col= alpha(clrs[j],(k%%18/18)))
      lines(x=x1,y=y1,col=alpha("dark blue", (k%%18+1)/20))
      lines(x=x1[c(1,4)],y=y1[c(1,4)],col="dark blue")
    }

    if(j >= 7) {
      ang <- (r*5 %% 360)*pi/180
      fibdf <- fibdf * (2/(1+sqrt(5)))^(1/18)
      if(k > ani.options('nmax') -20) fibdf <- fibdf * (ani.options('nmax')+10-k)/20 
      r <- r + 1
    }
  }
  ani.pause()   
    
}, movie.name = "fibonacci.gif", img.name = "fibonacci", convert = "magick", cmd.fun, clean = TRUE)
    
 
