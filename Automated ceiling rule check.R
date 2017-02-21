
#Automated ceiling rule check

#By Jici Yang

#Important Note: before checking ceiling: make sure the data is free of error (no missing values/wrong values)

#Detailed information on proper data structure assumption/procedure for the ceiling rule is found in the Word document
#"Data cleaning and ceiling check info"

#Step 1: import cleaned data set from excel sheet (use latest version of R-studio)

#this data set we are using the cleaned subtest 4 of TAPS-4 pilot study
#details on data set import is found in the protocol document

#View the data imported, note that spaces have been converted to NA


library(readxl)

subtest_4_cleaned <- read_excel("M:/Automation Protocols and Scripts - Jici/TAPS-4  Pilot Study Data_11_30_16_case_deleted.xlsx", sheet = "Subtest4 PB")

View(subtest_4_cleaned)

#The last column is redundant
#In addition the rows after row 646 is non-data, so a simple subset is required
# in addition, we delete the example A and B


subtest_4_cleaned=subtest_4_cleaned[1:646,-c(3,4,46)]



#check to see that data imported is properly subsetted: non-data will cause error in running the function

View(subtest_4_cleaned)



#The followings are a list of functions for different ceiling rule scenarios, 

############################################################################################

#demo

dat=as.matrix(subtest_4_cleaned[171,])


newdat=dat[3:tail(which(!is.na(dat)),n=1)]
collapsed=paste(newdat,collapse = "")
collapsed=gsub(" ", "",collapsed)




#CEILING RULE FUNCTION 1: 4 Errors in a row

#Note this function is good for both binary and 0-1-2 scoring





ceiling_4=function(dat){
  
  #take only the data starting from the 3 row (to ignore Site and ID) 
  
  #All the way till the last data value that is not NA
  newdat=dat[3:tail(which(!is.na(dat)),n=1)]
  
  #Paste the vector into a string
  collapsed=paste(newdat,collapse = "")
  
  #Get rid of white space in the string
  collapsed=gsub(" ", "",collapsed)
  
  #Ceiling rule for data that hits a ceilings before full test is complete but does not have four consecutive mistakes
  #check for early ceiling
  
  if((length(newdat)!=(length(dat)-2))&(grepl("0{4}",collapsed)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  #for data that has late ceiling after 4 consecutive mistakes
  #
  
  if(grepl("0{4}[1-2+]",collapsed)){
    
    return(dat[c("Site","ID")])}
  
} 

#################################################################################################
#CEILING RULE FUNCTION 2: 6 Errors in a row

#Note this function is good for both binary and 0-1-2 scoring


ceiling_6=function(dat){
  
  #take only the data starting from the 3 row (to ignore Site and ID) 
  #All the way till the last data value that is not NA
  newdat=dat[3:tail(which(!is.na(dat)),n=1)]
  
  #Paste the vector into a string
  collapsed=paste(newdat,collapse = "")
  
  #Get rid of white space in the string
  collapsed=gsub(" ", "",collapsed)
  
  #Ceiling rule for data that hits a ceilings before full test is complete but does not have six consecutive mistakes
  
  if((length(newdat)!=(length(dat)-2))&(grepl("0{6}",collapsed)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  #for data that has late ceiling after 6 consecutive mistakes
  
  if(grepl("0{6}[1-2+]",collapsed)){
    
    return(dat[c("Site","ID")])}
  
} 

###################################################################################################
#CEILING RULE FUNCTION 1: 5 errors out of 7 items

#Note this function is only good for binary scoring (1 or 0)



ceiling_5=function(dat){
  
  regex="1100000|1010000|0110000|1001000|0101000|0011000|0100100|0010100|1000100|0001100|1000010|0100010|0010010|0001010|0000110|0010001|0001001|0100001|0000101|1000001|0000011"
  
  regex1="1000000|0100000|0010000|0001000|0000100|0000010|0000001|0000000"
  
  
  regex2="1100000[1+]|1010000[1+]|0110000[1+]|1001000[1+]|0101000[1+]|0011000[1+]|0100100[1+]|0010100[1+]|1000100[1+]|0001100[1+]|1000010[1+]|0100010[1+]|0010010[1+]|0001010[1+]|0000110[1+]|0010001[1+]|0001001[1+]|0100001[1+]|0000101[1+]|1000001[1+]|0000011[1+]"
  
  regex3="1000000[1+]|0100000[1+]|0010000[1+]|0001000[1+]|0000100[1+]|0000010[1+]|0000001[1+]|0000000[1+]"
  
  newdat=dat[3:tail(which(!is.na(dat)),n=1)]
  collapsed=paste(newdat,collapse = "")
  collapsed=gsub(" ", "",collapsed)
  
  
  #Ceiling rule for data that hits a ceilings before full test is complete but does not have any of the ending conditions
  
  
  if((length(newdat)!=(length(dat)-2))&(grepl(regex,collapsed)==FALSE&grepl(regex1,collapsed)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  #for data that have any of the ending conditions but still have 1 after wards
  if(grepl(regex2,collapsed)|grepl(regex3,collapsed)){
    return(dat[c("Site","ID")])}
  
} 


######################################################################################################

# a function to check for ceiling rule of 3 out of 4 errors

#only works for binary

ceiling_4_3=function(dat){
  
  regex="0010|0100|1000|000"
  
  regex1="0010[1+]|0100[1+]|1000[1+]|000[1+]"
  
  newdat=dat[3:tail(which(!is.na(dat)),n=1)]
  collapsed=paste(newdat,collapse = "")
  collapsed=gsub(" ", "",collapsed)
  
  #for data that hits a ceilings before full test is complete but does not have any of the ending conditions
  
  if((length(newdat)!=(length(dat)-2))&(grepl(regex,collapsed)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  #for data that have any of the ending conditions but still have 1 after wards
  if(grepl(regex1,collapsed)){
    return(dat[c("Site","ID")])}
  
} 


##############################################################################################################

#Step 2: apply function to data

#In this specific case of TAPS-4 subtest 4, the ceiling is 4 in a row, so we apply ceiling_4 to the data row by row


#"apply" function iterate through one row or column of the data frame at a time and apply the function to each row,
#returning the result each time

#more detail on apply is here

?apply

#apply(X, MARGIN, FUNCTION, ...)

#I choose 1 in the margin for row

res=apply(subtest_4_cleaned,1,ceiling_4)

res

#the output is return as a list of 646 elements, NULL means that row has no issues

#to break down the list


unlist(res)

#These are the Site and ID that has potential issues with ceilings

######################################################################################

#APPENDIX:

#Possible Error 1:

# since in the function we have "return(dat[c("Site","ID")])"

# make sure in the data "Site" and "ID" is named exactly the same case and label as in the function

# Change the "Site" and "ID" in the function to reflect the original data if neccessary



