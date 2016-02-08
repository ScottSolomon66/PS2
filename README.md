##Problem Set 2
##By Scott Solomon

##Yes I am starting this during the Super Bowl

BenfordStat<-function(x, returnm=T,returnd=F){
  i=1:9
  M<-max((x-log(1+1/i,base=10))) #Defining the M statistic function
  D<-sum((x-log(1+1/i,base=10))^2) #Defining the D statistic function
  D<-sqrt(D)
  
  #Determining which statistics are returned
  if(returnm==T & returnd==T) return(list((M),(D)))
  if(returnM==T & returnD==F) return(list("M"=M))
  if(returnM==F & returnD==T) return(list("D"=D))
  if(returnM==F & returnD==F) return()
}



  
