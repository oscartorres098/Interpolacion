
x=c(14.6, 14.8, 15.2, 15.7, 17.0,  17.5, 17.3, 16.8, 15.4, 15.0, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 13.9, 13.8, 13.5, 13.1, 13.0, 13.1, 13.0, 12.8, 12.3, 11.9, 11.7, 11.5, 11.3, 10.9, 10.8, 10.6, 10.6, 10.1, 9.7, 9.3, 9.6, 9.7, 9.9, 10.0, 9.7, 9.10, 8.60, 8.0, 7.55, 7.3, 7.70, 8.00,  8.40, 8.90,  9.80)                                                                                                       
y=c(14.7, 12.3, 11.0, 10.2, 8.20,  6.70, 6.60, 6.80, 8.30, 8.80, 9.10, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 5.40, 5.80, 6.90, 8.20, 7.60, 5.2, 4.50, 4, 4.2, 5.70, 7.00, 7.80, 8.30, 7.30, 6.70, 5.50, 5.50, 4.60, 4.4, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.25, 9.1, 9.3, 11.7, 12.3,  13.0, 13.7,  15.5)     


plot(x,-y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama ")

Interpolacion <- function(x,y) {
  
  a = rep(y)
  n = length(x)
  
  h <- (c(x,0) - c(0,x))[2:n]
  alph <- (3/c(1,h,1,1)*(c(a,1,1) - c(1,a,1)) - 3/c(1,1,h,1)*(c(1,a,1)-c(1,1,a)))[3:n]
  
  A <- c(1,rep(0,times=n-1))
  for (i in 1:(n-2)) {
    A <- rbind(A,c( rep(0,times=i-1) , c(h[i],2*(h[i]+h[i+1]),h[i+1]) , rep(0,times=n-i-2) ) )
  }
  
  A <- rbind(A,c(rep(0,times=n-1),1))
  b <- c(0,alph,0)
  c <- solve(A, b)
  
  b <- ((c(a,0) - c(0,a))/c(1,h,1) - c(1,h,1)/3*(c(c,0) + 2*c(0,c)))[2:n]
  d <- ((c(c,0) - c(0,c))/(3*c(1,h,1)))[2:n]
  ans = rbind(a[1:n-1],b,c[1:n-1],d)
}

draw <- function(x,y) {
  t = 1:length(x)
  sx = Interpolacion(t,x)
  sy = Interpolacion(t,y)
  
  for (i in 1:(length(t)-1)) {
    dat<- data.frame(t=seq(t[i],t[i+1], by=0.1) )
    fx <- function(x) (sx[1,i] + sx[2,i]*(x-t[i]) + sx[3,i]*(x-t[i])^2 + sx[4,i]*(x-t[i]))
    fy <- function(x) (sy[1,i] + sy[2,i]*(x-t[i]) + sy[3,i]*(x-t[i])^2 + sy[4,i]*(x-t[i]))
    
    dat$y=fy(dat$t)
    dat$x=fx(dat$t)
    points(dat$x,dat$y,type='l', col='red')
  }
}
draw(x,-y)
