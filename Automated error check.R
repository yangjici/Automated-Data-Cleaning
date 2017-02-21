#Automated data cleaning 

#By Jici Yang

#This script returns the site and id of data that has error (missing values/wrong values)

#Detailed information on proper data structure assumption/procedure for the ceiling rule is found in the Word document
#"Data cleaning and ceiling check info"

#The example used in this script is from TAPS-4, which meet the data structure assumptions of the automation script

#See possible error/debugging in Appendix

#Step 1: import data set from excel sheet (use latest version of R-studio)
#this data set we are using the subtest 4 of TAPS-4 pilot study
#steps on data set import is found in the protocol document

library(readxl)

subtest4<- read_excel("M:/Automation Protocols and Scripts - Jici/TAPS-4  Pilot Study Data_11_29_16_finalji2.xlsx",sheet = "Subtest4 PB")

#View the data imported, spaces have been converted to NA

View(subtest4)

#The last column is redundant

#In addition the rows after row 646 is non-data, so a simple subset is required


subtest4=subtest4[1:646,-c(3,4,46)]

#check to see that it is gone

View(subtest4)

#Step 2: Check the function to the data

#a function to check all the values in each observation up to the last entry (that is not NA)
#and return the ID and Site of those who contain values that are not 1 or 0

check1=function(dat){
  
  #take only the data starting from the 3 row (to ignore Site and ID) 
  #All the way till the last data value that is not NA
  
  #first delete site and id
  newdat=dat[3:length(dat)]
  
  
  if (any(!is.na(newdat))==FALSE){
    return(dat[c("Site","ID")])
  }
  
  else{
    # check for monkey business till the last occurance of number
    newdat=newdat[1:tail(which(!is.na(newdat)),n=1)] 
    newdat=as.numeric(newdat)
    
    #if any of the data is (not 0 or not 1) or is NA,
    
    #return the site and id
    if (any(newdat!=0&newdat!=1|is.na(newdat))){
      return(dat[c("Site","ID")])
    }
  }
}



#If data is 0, 1 and 2, use the following function 

check2=function(dat){
  
  #take only the data starting from the 3 row (to ignore Site and ID) 
  #All the way till the last data value that is not NA
  
  #first delete site and id
  newdat=dat[3:length(dat)]
  
  
  if (any(!is.na(newdat))==FALSE){
    return(dat[c("Site","ID")])
  }
  
  else{
    # check for monkey business till the last occurance of number
    newdat=newdat[1:tail(which(!is.na(newdat)),n=1)] 
    newdat=as.numeric(newdat)
    
    #if any of the data is (not 0 or not 1 or not 2) or is NA,
    #return the site and id
    if (any(newdat!=0&newdat!=1&newdat!=2|is.na(newdat))){
      return(dat[c("Site","ID")])
    }
  }
}


#Further modification to the function is possible for other data sets, with slight change in the if-statement

#Step 3, depend on the data, use either check2 or check1 or modified function to check for errors
#Save the function in the system by highlighting the entire function and click run


#"apply" function iterate through one row or column of the data frame at a time and apply the function to each row,
#returning the result each time

#more detail on apply is here

?apply

#apply(X, MARGIN, FUNCTION, ...)

#I choose 1 in the margin for row

res=apply(subtest4,1,check2)

res

#the output is return as a list of 646 elements, NULL means that row has no issues

#to break down the list

unlist(res)

#These are the Site and ID that has potential issues.

#Verify whether these flagged responses are error and take appropriate actions to fix them in Excel, delete responses that cannot be used
#then proceed to ceiling check



#APPENDIX:

#Possible Error 1:

# since in the function we have return(dat[c("Site","ID")])

# make sure in the data "Site" and "ID" is named exactly the same case and label as in the function

# Change the "Site" and "ID" in the function to reflect the original data if neccessary

