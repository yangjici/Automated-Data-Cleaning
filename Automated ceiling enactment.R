#Automated ceiling enactment 

#By: Jici Yang

#Important Note: before enacting new ceiling: make sure the data is 
#1. free of errors (no missing values/wrong values)
#2. ceiling errors have been checked and corrected
#3. the empty spaces after each row has been filled with zeroes

#working directory:

#"M:/Automation Protocols and Scripts - Jici"


#Data structure: to use this function, the data for input should be a data frame with test result for a single subtest only (no site, id etc)

#For this function, we will need the following package

library(stringr)

# a function to create a data set in which the ceiling rules ar enforced

#the parameters in the function are of the following:

#dat: the data frame with test result for a single subtest only, note: data only

#ceiling_regex: the regular expression of which we will use to split each line of the data frame by: aka the artificial ceiling rule

#zero: the text we will replace the regular expression we take out from the row of data

enact_ceiling=function(dat,ceiling_regex,zero){
  
  collapsed=paste(dat,collapse = "")
  
  collapsed=gsub(" ", "",collapsed)
  
  if(grepl(ceiling_regex,collapsed)){
    
    sp=unlist(str_split(collapsed,pattern=zero))
    
    sp[2:length(sp)]=gsub("[1-2]","0",sp[2:length(sp)])
    
    sp[2:length(sp)]=sapply(sp[2:length(sp)],paste,zero,sep="")
    
    res=do.call(paste,c(as.list(sp),sep=""))
    
    res=as.numeric(unlist(str_split(res,pattern="")))
    
    return(res)
  }
  
  else{
    return(as.numeric(dat))}
}


#Example: creating a data set with post-hoc imposed ceiling rule of 5

#I will use the 25-item model for subtest 6 from TAPS-4 pilot study in the demonstration

#Here I import the cleaned, ceiling-checked data with the empty ceils filled with 0

#the data is model that has the order rearranged according to difficulty and item parallelism  

library(readr)

subtest6 <- read_csv("M:/Automation Protocols and Scripts - Jici/subtest6_25_item_model.csv")

View(subtest6)


#Step 1:Delete the extra column and site and id and examples as well as age group (we only want data)

subtest_6_dat_only=subtest6[,-c(1,2,28,29)]


View(subtest_6_dat_only)

#Step 2: Create a empty data frame with the same number of rows and columns as the raw data to store the result

result=matrix(nrow=nrow(subtest_6_dat_only),ncol=ncol(subtest_6_dat_only))

#Step 3: decide on input varialbes: we are spliting the row of data if they have a late ceiling according to the new ceiling rule
#in the case of a ceiling of 5 consecutive error the ceiling_regex is "00000[1-2+]" that is we see in data if there exist a pattern of five
#zeroes and one or more of 1 or 2's after it we will proceed to replace the late ceiling with the same number of zeros in the ceiling rule

for (i in 1:nrow(subtest_6_dat_only)){
  result[i,]=enact_ceiling(subtest_6_dat_only[i,],"00000[1-2+]","00000")
}


#give the column names back

colnames(result)=colnames(subtest_6_dat_only)

View(result)


#validate result to check for late ceilings

#use the same ceiling_regex as in the ceiling-enactment function


ceiling_check=function(dat,ceiling_regex){
  
  newdat=dat
  
  #Paste the vector into a string
  collapsed=paste(newdat,collapse = "")
  
  #Get rid of white space in the string
  collapsed=gsub(" ", "",collapsed)
  
  #for data that has late ceiling after 5 consecutive mistakes
  
  if(grepl(ceiling_regex,collapsed)){
    
    return(NA)}
  
} 


#result is Null, that is the data frame with ceiling rule of 5 does not contain any late ceiling cases, ceiling enactment worked




#Export result into a new csv file (or Excel)

#file imported to the working directory on your computer

#combine new raw data with ceiling of 5 with site and id and examples and age group

subtest_ceiling_5=cbind(subtest6[,c(1,2,28)],result)

write.csv(subtest_ceiling_5,file="subtest6_ceiling_5.csv") 






