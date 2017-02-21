#basal and ceiling rule check (bonus: data cleaning with basal data)

#By Jici Yang


#for demo we set the directory to the M drive folder 

setwd("M:/Automation Protocols and Scripts - Jici/Basal Rule Automation")

library(readxl)

df <- read_excel("~/Basal Rule Automation/BasalRuleScriptData 2-1-17.xlsx")

View(df)

#1. initial subset/cleaning of data frame

#clean the data first using the new check function for basal data


check3=function(dat){
  
  #first delete site and id
  newdat=dat[3:length(dat)]
  
  #return site and id if encounters empty rows
  if (any(!is.na(newdat))==FALSE){
    return(dat[c("Site","ID")])
  }
  
  
  else{
    # check for monkey business at the first occurance of number till the last occurance of number
    newdat=newdat[which(!is.na(newdat))[1]:tail(which(!is.na(newdat)),n=1)] 
    newdat=as.numeric(newdat)
    
    #if any of the data is (not 0 or not 1 or not 2) or is NA,
    #return the site and id
    if (any(newdat!=0&newdat!=1&newdat!=2|is.na(newdat))){
      return(dat[c("Site","ID")])
    }
  }
  
}



#Use Site and ID and raw data into the check function

unlist(apply(df[,c(1,2,6:43)],1,check3))



#find and fix the flagged site and ids

#may return same site and id twice (duplicate site and id exist in data)

#note: since there may exist response with two same site and id, check both responses with the site and id.

#the function also picked up the following incorrect basal rule: 2 2 2 2 2 NA NA NA NA 2 2 2 2 2
#where the 2 after NA is the basal

#proceed to basal and ceiling check once no more error can be found





#basal and ceiling check function for 0,1,2 data
##################################################################



# Basal is established with 3 consecutive items scored with 1 or 2
# 
# No 2s 1s or 0s should appear on any item lower than the basal
# 
# Starting Points are as follows
# .	Age 5 and 6- Start at item 5
# .	Age 7 and 8- Start at item 8
# .	Age 9 and 10- Start at item 11
# .	Ages 11 and above- Start at item 14







basal_3_ceiling_4=function(dat){
  
  #prepare work, assembling all the parts needed to check ceiling and basal
  
  #extracting data only
  #omit delete site and id and age
  newdat=dat[4:length(dat)]
  
  #get rid of NA for entire data set
  newdat=newdat[which(!is.na(newdat))[1]:tail(which(!is.na(newdat)),n=1)]
  
  #paste data into a string
  dat_str=paste(newdat,collapse = "")
  
  #Get rid of white space in the string
  dat_str=gsub(" ", "",dat_str)
  
  #split the data frame into prebasal and postbasal depend on age
  
  ###For age 5 and 6
  #split the data into two parts: the prebasal part and basal part
  #according to the age of the participant
  
  if(dat["YearAdj1"]%in%(c(5,6))){
    post=dat[-c(1:7)]
    pre=dat[c(4:7)]
  } else if(dat["YearAdj1"]%in%(c(7,8))){
    post=dat[-c(1:10)]
    pre=dat[c(4:10)]
  } else if(dat["YearAdj1"]%in%(c(9,10))){
    post=dat[-c(1:13)]
    pre=dat[c(4:13)]
  } else{
    post=dat[-c(1:16)]
    pre=dat[c(4:16)]
  }
  
  
  
  #get rid of NA for pre-basal
  #if all NA exist in pre_basal, create a vector of length zero
  
  if (any(!is.na(pre))==FALSE){
    pre_dat = vector(mode="numeric", length=0)
  }else {
    pre_dat=pre[which(!is.na(pre))[1]:length(pre)]
  }
  
  #Paste the prebasal vector into a string
  pre_str=paste(pre_dat,collapse = "")
  
  #Get rid of white space in the string
  pre_str=gsub(" ", "",pre_str)
  
  
  #get rid of NA for post-basal
  #if all NA exist in post_basal, create a vector of length zero
  
  if (any(!is.na(post))==FALSE){
    post_dat = vector(mode="numeric", length=0)
  }else {
    post_dat=post[1:tail(which(!is.na(post)),n=1)]
  }
  
  #Paste the prebasal vector into a string
  post_str=paste(post_dat,collapse = "")
  
  #Get rid of white space in the string
  post_str=gsub(" ", "",post_str)
  
  
  ####
  #cASE scenario 1: pre-basal items needed
  
  #if the first three items of the basal part is not a combination of either 1 or 2 and prebasal is not length 0 (due to default setting) 
  #: therefore pre-basal need to be checked
  if(grepl("^[1-2]{3}",post_str)==FALSE&length(pre)!=0){
    
    #1. check for whether basal is over done
    
    #if exist more than three consecutive 2 or 1 in prebasal, we have a case of late basal
    if(grepl("[1-2]{4,}",pre_str)){
      return(dat[c("Site","ID")])
      
      #if the entire data start with more than 3 consecutive 2 or 1's, overdone
      
    } else if(grepl("^[1-2]{4,}",dat_str)){
      return(dat[c("Site","ID")])
      
      #2. check for whether basal is under done
      #if did not finish the entire pre-basal question but the non-NA part does not start with 3 consecutive 2's
    }else if((length(pre)!=length(pre_dat))&grepl("^[1-2]{3}",dat_str)==FALSE){
      return(dat[c("Site","ID")])
    } 
  }
  
  
  #####case scenario 2: 3 consecutive 2 or 1 is found in the beginning of basal    
  if(grepl("^[1-2]{3}",post_str)){
    
    #1.check to make sure nothing is in the pre_basal
    
    #if anything is not NA exist in pre-basal, return site and id
    
    #check for anything that is not NA in prebasal
    if(any(!is.na(pre))){
      return(dat[c("Site","ID")])
    }
    ####
    #Case scenario 3: the case starter at later point in the basal rule 
  }
  
  if(any(is.na(post_dat))){
    return(dat[c("Site","ID")])
    
    ####  
    #Case scenario 4: spot all the late ceiling
  }else if(grepl("0{4}[1-2+]",dat_str)){
    return(dat[c("Site","ID")])
    
    ####
    #Case scenario 5: spot all early ceilings
  }else if((length(post)!=(length(post_dat)))&(grepl("0{4}",dat_str)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  
}


##############################################################END OF FUNCTION



#Import the cleaned data set

df_clean <- read_excel("~/Basal Rule Automation/BasalRuleScriptData_cleaned.xlsx")

View(df_clean)

#run check function again to double check for errors

unlist(apply(df_clean[,c(1,2,6:43)],1,check3))




# run basal ceiling function on

#delete other info only leave age, site and id and raw data

df_clean_check=df_clean[,-c(3,4)]

unlist(apply(df_clean_check,1,basal_3_ceiling_4))









##################################################################FUNCTION START

############################################
#basal ceiling check function for 0,2 data

# Basal is established with 3 consecutive items scored with "2"
# 
# No 1s or 0s should appear on any item lower than the basal
# 
# Starting Points are as follows
# .	Age 5 and 6- Start at item 5
# .	Age 7 and 8- Start at item 8
# .	Age 9 and 10- Start at item 11
# .	Ages 11 and above- Start at item 14



basal_3_ceiling_4_binary=function(dat){
  
  #prepare work, assembling all the parts needed to check ceiling and basal
  
  #extracting data only
  #omit delete site and id and age
  newdat=dat[4:length(dat)]
  
  #get rid of NA for entire data set
  newdat=newdat[which(!is.na(newdat))[1]:tail(which(!is.na(newdat)),n=1)]
  
  #paste data into a string
  dat_str=paste(newdat,collapse = "")
  
  #Get rid of white space in the string
  dat_str=gsub(" ", "",dat_str)
  
  #split the data frame into prebasal and postbasal depend on age
  
  ###For age 5 and 6
  #split the data into two parts: the prebasal part and basal part
  #according to the age of the participant
  
  if(dat["YearAdj1"]%in%(c(5,6))){
    post=dat[-c(1:7)]
    pre=dat[c(4:7)]
  } else if(dat["YearAdj1"]%in%(c(7,8))){
    post=dat[-c(1:10)]
    pre=dat[c(4:10)]
  } else if(dat["YearAdj1"]%in%(c(9,10))){
    post=dat[-c(1:13)]
    pre=dat[c(4:13)]
  } else{
    post=dat[-c(1:16)]
    pre=dat[c(4:16)]
  }
  
  
  
  #get rid of NA for pre-basal
  #if all NA exist in pre_basal, create a vector of length zero
  
  if (any(!is.na(pre))==FALSE){
    pre_dat = vector(mode="numeric", length=0)
  }else {
    pre_dat=pre[which(!is.na(pre))[1]:length(pre)]
  }
  
  #Paste the prebasal vector into a string
  pre_str=paste(pre_dat,collapse = "")
  
  #Get rid of white space in the string
  pre_str=gsub(" ", "",pre_str)
  
  
  #get rid of NA for post-basal
  #if all NA exist in post_basal, create a vector of length zero
  
  if (any(!is.na(post))==FALSE){
    post_dat = vector(mode="numeric", length=0)
  }else {
    post_dat=post[1:tail(which(!is.na(post)),n=1)]
  }
  
  #Paste the prebasal vector into a string
  post_str=paste(post_dat,collapse = "")
  
  #Get rid of white space in the string
  post_str=gsub(" ", "",post_str)
  
  
  ####
  #cASE scenario 1: pre-basal items needed
  
  #if the first three items of the basal part is not 2 and prebasal is not length 0 (due to default setting) : aka pre-basal needed
  if(substr(post_str,1,3)!="222"&length(pre)!=0){
    
    #1. check for whether basal is over done
    
    #if exist more than three consecutive 2 in basal, we have a case of late basal
    if(grepl("[0-2+]222",pre_str)){
      
      return(dat[c("Site","ID")])
      
      #2. check for whether basal is under done
      #if did not finish the entire pre-basal question but the non-NA part does not start with 3 consecutive 2's
    } else if((length(pre)!=length(pre_dat))&grepl("^222",dat_str)==FALSE){
      
      return(dat[c("Site","ID")])
      #3 if the data start with more than 3 consecutive 2's, overdone
      
    } else if(grepl("^2{4,}",dat_str)){
      return(dat[c("Site","ID")])
      
    }
    
  }
  
  
  #####case scenario 2: 3 consecutive 2 is found in the beginning of basal    
  if(substr(post_str,1,3)=="222"){
    
    #1.check to make sure nothing is in the pre_basal
    
    #if anything is not NA exist in pre-basal, or over/under - ceiling return site and id
    
    #check for NA in prebasal
    if(any(!is.na(pre))){
      return(dat[c("Site","ID")])
    }
    ####
    #Case scenario 3: the case starter at later point in the basal rule 
  }
  
  if(any(is.na(post_dat))){
    return(dat[c("Site","ID")])
    
    ####  
    #Case scenario 4: spot all the late ceiling
  }else if(grepl("0{4}[1-2+]",dat_str)){
    return(dat[c("Site","ID")])
    
    ####
    #Case scenario 5: spot all early ceilings
  }else if((length(post)!=(length(post_dat)))&(grepl("0{4}",dat_str)==FALSE)){
    return(dat[c("Site","ID")])
  }
  
  
}


##############################################################END OF FUNCTION



#import a new set of data with all instances of 1 converted to 2 


df_binary <- read_excel("~/Basal Rule Automation/BasalRuleScriptData_cleaned_1_0.xlsx")

#delete other info only leave age, site and id and raw data

basal_binary=df_binary[,-c(3,4)]


#run the basal and ceiling function on the binary data

unlist(apply(basal_binary,1,basal_3_ceiling_4))





