#What-if-analysis for data with new ceiling rule

#By Jici Yang

#This script compares the artifical data set with new ceiling rule to the original data set

#New data set: subtest 6 of TAPS-4 25 item model with a ceiling rule of 5

#the new data set was created using the automated ceiling enactment script


library(readr)

subtest6_ceiling_5 <- read_csv("M:/Automation Protocols and Scripts - Jici/subtest6_ceiling_5.csv")

View(subtest6_ceiling_5)

#we just need the raw data in this one, so subset everything else

subtest6_ceiling_5_dat=subtest6_ceiling_5[,-c(1,2,3,4)]


#original data set without ceiling rule

#Here I import the cleaned, ceiling-checked, data with the empty ceils filled with 0 (with orders rearranged)



#In addition there is a columnn on the age of the participant to check the effect of ceiling on various age group

subtest6 <- read_csv("M:/Automation Protocols and Scripts - Jici/subtest6.csv")

View(subtest6)


#only want raw data

subtest_6_dat_only=subtest6[,-c(1,2,28,29)]



#and age

#note that after converting to excel the original grouped age got mistaken to date, but you get the point 

Age=subtest6$YearAdj1


#sum of the original score

score=rowSums(subtest_6_dat_only)

#sum of the score with artificial ceiling

ceiling_score=rowSums(subtest6_ceiling_5_dat)

#unpaired t.test overall

#paired t.test is more appropriate since the original and artificial data  come from the same sample 

t.test(score,ceiling_score,paired=TRUE)

#score by age original data

score_by_age=split(score,Age)

#score by age artificial data

score_by_age_ceiling=split(ceiling_score,Age)



#unpaired t.test by age group

mapply(t.test,score_by_age,score_by_age_ceiling,paired=TRUE)

#tabulating score differences

score_diff=score-ceiling_score

#checking the non-zero score differences

non_zero=score_diff[score_diff!=0]

table(non_zero)

hist(non_zero)

#score differences per age group

by(score_diff,Age,table)

#histogram of age difference per age group

by(score_diff,Age,hist)




