##Problem Set 2
##By Scott Solomon

##Yes I am starting this during the Super Bowl

benford_stat<-function(x, return_m=T,return_d=F){
  i=1:9
  m<-max((x - log(1 + 1 / i,base=10))) #Defining the M statistic function
  d<-sum((x - log(1 + 1 / i,base=10))^2) #Defining the D statistic function
  d<-sqrt(d)
  
  #Determining which statistics are returned
  if(return_m == T & return_d == T) return(list(("M" = m),("D" = d)))
  if(return_m == T & return_d == F) return(list("M" = m))
  if(return_m == F & return_d == T) return(list("D" = d))
  if(return_m == F & return_d == F) return()
}

#Recreating the function without the option because it isn't needed for the table

benford_stat_no_choice<-function(x){
  i=1:9
  M<-max((x-log(1 + 1 / i,base = 10))) #Defining the M statistic function
  D<-sum((x-log(1 + 1 / i,base = 10))^2) #Defining the D statistic function
  D<-sqrt(D)
  both_stats<-c(M,D)
}


print_benford<-function(x){
  rows<-c("stat","stattype") #creating a vector for row names, need to add more still
  table<-data.frame(benford_stat_no_choice(x),rownames = rows) #turning the stats into a datframe
  return(table)
}



