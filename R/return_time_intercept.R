## Author: Olle Odijk, 2020
## Function to determine intercept of return time in plots
return_time_intercept<-function(DivPreHuman,d,timestep){
  i<-0
  for(i in 1:(length(d)/timestep)){
    if (d[i]>DivPreHuman){
      #using linear interpolation between timesteps: y=ax+b -> x=(y-b)/a
      y2=d[i]
      y1=d[i-1]
      dy<-y2-y1
      x2<-(i-1)*timestep
      x1<-(i-2)*timestep
      dx <- x2-x1
      a=dy/dx
      b=y2-a*x2
      x=(DivPreHuman-b)/a
      break}
  }
  return(x)
}
