Z=runif(50)
f_cap<-function(sample,t){
  count<-0
  for(i in 1:length(sample)){
    if(sample[i]<=t){
      count<-count+1
    }
  }
  return(count/length(sample))
}


for(i in 1:10){
  a=runif(1)
  b=sqrt(length(Z))*(f_cap(sample=Z,t=a)-a)
  plot(x=a,y=b)
  
}
