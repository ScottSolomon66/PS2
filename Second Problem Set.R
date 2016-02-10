##Problem Set 2
##By Scott Solomon

##Yes I am starting this during the Super Bowl

benford_stat<-function(x, return_m=T,return_d=F){
  i=1:9
  m<-max((x - log(1 + 1 / i,base=10))) #Defining the M statistic function
  d<-sum((x - log(1 + 1 / i,base=10))^2) #Defining the D statistic function
  d<-sqrt(d) #wouldn't work in one line for some reason
  
  #Determining which statistics are returned
  if(return_m == T & return_d == T) return(list(("M" = m),("D" = d))) #returning both statistics
  if(return_m == T & return_d == F) return(list("M" = m)) #returning just the M statistic
  if(return_m == F & return_d == T) return(list("D" = d)) #returning just the D statistic
  if(return_m == F & return_d == F) return("") #returning none
}

#Recreating the function without the option because it isn't needed for the table

benford_stat_no_choice<-function(x){
  
  ##this function won't be used alone, but will be called upon in print_benford
  
  i=1:9 #creating a list variable to be used in the function
  M<-max((x-log(1 + 1 / i, base = 10))) #Defining the M statistic function
  D<-sum((x-log(1 + 1 / i, base = 10))^2) #Defining the D statistic function
  D<-sqrt(D) #for some reason I couldn't get this to all work in one line
  both_stats<-c(M,D) #concatanating the two stats into a single object
  return(both_stats) #returning the stats
}


print_benford<-function(x, write_csv = T){
  rows<-c("stat","stattype") #creating a vector for row names, need to add more still
  table<-data.frame(benford_stat_no_choice(x)) #turning the stats into a datframe
  
  names(table)[1]<-paste("Statistic") #making the first column name Statistic
  row.names(table)<-list("Leemis m", "Cho-Gain's d") #naming the rows after each statistic name
  
  ##writing if statements for the significance levels for the leemis m statistic
  ##creating both an asteriks object and a sig_level object that will serve as a legend for the signifcance level
  
  if (table$Statistic[1] > .851){ #simple if statment calling on the first row of the Statistic column, which is m
    asteriks_m<-c("*") #adding the single asteriks
    sig_level_m<-"     .10 level significance" #adding the definition for the level, the extra spaces make the table look more visually appealing
  } 
  
  ##after this, I repeat all these steps for all of the significance levels
  ##because of the order of the functions, it will work until it stops reaching the next significance level
  
  if (table$Statistic[1] > .967){
    asteriks_m<-c("**")
    sig_level_m<-"     .05 level significance"
  }
  if (table$Statistic[1] > 1.212){
    asteriks_m<-c("***")
    sig_level_m<-"     .01 level significance"
  }
  if (table$Statistic[1] < .851){
    asteriks_m<-c("")
    sig_level_m<-"     not significant"
  }
  
  ##recreating the if statements for the Cho-Gain's d statistic
  
  if (table$Statistic[2] > 1.212){
    asteriks_d<-c("*")
    sig_level_d<-"     .10 level significance"
  }
  if (table$Statistic[2] > 1.33){
    asteriks_d<-c("**")
    sig_level_d<-"     .05 level significance"
  }
  if (table$Statistic[2] > 1.569){
    asteriks_d<-c("***")
    sig_level_d<-"     .01 level significance"
  }
  if (table$Statistic[2] < .851){
    asteriks_d<-c("")
    sig_level_d<-"    not significant"
  }
  
  ##combining these values by row so they line up properly in the dataframe
  asteriks<-rbind(asteriks_m,asteriks_d) #creating an object combinging the asteriks for both statistics
  sig_level<-rbind(sig_level_m, sig_level_d) #creating an object combining the explanation for both statistics
  
  ##now to add these values to the dataframe
  table<-cbind(table, asteriks, sig_level) #combinining them by column to add new columns
  
  if (write_csv == T){ #if the user of the function wants to write the table
    write.csv(table, file = "Benford_Stat_Table.csv") #writing the table as a csv file
  }
  
  return(table) #returning the dataframe with all of the relevant information
  
}
  



