---
title: "Needs General Demographics"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Steps: 
1. Get rid of extra data for each 
2. Combine the relevant data
3. Grab demographics gender, degree, type of teacher, race
4. CCR grab all the and transform to numbers
5. Get an overall average value
6. Export the data 

Probably should get general demographics first.  Need to combine them and then 

Get the variables I want for SEL  

SELQuant1 = SEL first survey quantitative question 
SELQualRes = SEL resources question
SELQualBarr = SEL barriers question

Here is for RBBCSC
```{r}
#setwd("~/Desktop/QualData")
#rbbcsc = read.csv("RBBCSCStaffSurvey.csv", header = TRUE); head(rbbcsc)

rbbcsc = rbbcsc[-c(1:11),]; head(rbbcsc, 20)
rbbcsc = rbbcsc[,-c(1:16)]; head(rbbcsc, 20)

rbbcsc = rbbcsc[,-c(38:54)]; head(rbbcsc, 20)
rbbcsc = rbbcsc[,-c(40:44)]; head(rbbcsc, 20)
head(rbbcsc)
rbbcsc = as.data.frame(rbbcsc)
rbbcsc1 = rbbcsc[,1:6]
eth = rbbcsc[c("Q11")]
gender = rbbcsc[c("Q10")]
edu = rbbcsc[c("Q12")]
job = rbbcsc[c("Q15")]
rbbcsc = cbind(rbbcsc1,eth, gender, edu, job)
head(rbbcsc, 20)
sum(is.na(rbbcsc))
rbbcsc[157:159,]
# Quantiative, qual questions, and demographics
head(rbbcsc)
colnames(rbbcsc) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6", "eth", "gender", "edu", "job") 
head(rbbcsc)
write.csv(rbbcsc, "rbbcsc.csv", row.names = FALSE)
rbbcsc = as.data.frame(read.csv("rbbcsc.csv", header = TRUE, na.strings = ""))
dim(rbbcsc)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
rbbcsc = as.data.frame(na.omit(rbbcsc))

dim(rbbcsc)
write.csv(rbbcsc, "rbbcsc.csv", row.names = FALSE)

### Need to clean the top data out
#setwd("~/Desktop/QualData")
#mccsc = read.csv("MCCSCStaffSurvey.csv", header = TRUE)
head(mccsc)

mccsc1 = mccsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6")]
eth = mccsc[c("Q30")]
gender = mccsc[c("Q36")]
edu = mccsc[c("Q38")]
job = mccsc[c("Q44")]
mccsc = cbind(mccsc1,eth, gender, edu, job)
head(mccsc, 20)
# Quantiative, and demographics
head(mccsc)
colnames(mccsc) = c("SELQuant1", "SELQuant2", "SELQuant3", "SELQuant4", "SELQuant5", "SELQuant6", "eth", "gender", "edu", "job") 
mccsc = mccsc[-c(1:11),]
write.csv(mccsc, "mccsc.csv", row.names = FALSE)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
mccsc = as.data.frame(read.csv("mccsc.csv", header = TRUE, na.strings = ""))
mccsc = as.data.frame(na.omit(mccsc))
dim(mccsc)
sum(is.na(mccsc))
head(mccsc)
write.csv(mccsc, "mccsc.csv", row.names = FALSE)
```

Now MCCSC SEL
So grab the ones you are interested in get the length then divide that both the total to get the percentage for the category that you are interested in.
```{r}
both = as.data.frame(rbind(mccsc, rbbcsc))
dim(both)
bothQuan = na.omit(both)
bothQuan = as.data.frame(both)
dim(both)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
both = na.omit(both)
both = as.data.frame(both)

bothN = nrow(both)

## Gender only male rest if female
bothMale = as.data.frame(both[both$gender == "Male",])
bothMaleN = nrow(bothMale)
bothMalePerc = bothMaleN / bothN; bothMalePerc 

bothFemalePerc = 1-(bothMalePerc+bothOtherGIPerc)

bothOtherGI = as.data.frame(both[both$gender == "Other gender identity (please specify)",])
bothOtherGIN = nrow(bothOtherGI)
bothOtherGIPerc = bothOtherGIN / bothN; bothOtherGIPerc

sum(bothFemalePerc, bothMalePerc, bothOtherGIPerc)

```
Now we are getting ethnicity white, black, hispanic, and other
Make things add up to one.
```{r}
both = as.data.frame(rbind(mccsc, rbbcsc))
dim(both)
both = na.omit(both)
both = as.data.frame(both)
dim(both)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
both = na.omit(both)
write.csv(both, "both.csv", row.names = FALSE)
both =read.csv("both.csv", header = TRUE)
both
both = na.omit(both)
both = as.data.frame(both)
bothN = nrow(both)

## Now white people
bothWhite = as.data.frame(both[both$eth == "White",])
bothWhiteN = nrow(bothWhite)
bothWhitePerc = bothWhiteN / bothN; bothWhitePerc 

# Now Hispanic people
bothHis = as.data.frame(both[both$eth == "Hispanic, Latino, or Spanish origin",])
bothHisN = nrow(bothHis)
bothHisPerc = bothHisN / bothN; bothHisPerc 

## Now black people
bothBlack = as.data.frame(both[both$eth == "Black or African American",])
bothBlackN = nrow(bothBlack)
bothBlackPerc = bothBlackN / bothN; bothBlackPerc 

# Now Asian people
bothAsian = as.data.frame(both[both$eth == "Asian",])
bothAsianN = nrow(bothAsian)
bothAsianPerc = bothAsianN / bothN; bothAsianPerc

# Now American Indian or Alaska Native
bothAI = as.data.frame(both[both$eth == "American Indian or Alaska Native",])
bothAIN = nrow(bothAI)
bothAIPerc = bothAIN / bothN; bothAIPerc

# Middle Eastern or North African
bothME = as.data.frame(both[both$eth == "Middle Eastern or North African",])
bothMEN = nrow(bothME)
bothMEPerc = bothMEN / bothN; bothMEPerc

# Native Hawaiian or Other Pacific Islander
bothNH = as.data.frame(both[both$eth == "Native Hawaiian or Other Pacific Islander",])
bothNHN = nrow(bothNH)
bothNHPerc = bothNHN / bothN; bothNHPerc


# Now Some other race, ethnicity, or origin (please specify)
bothOther = as.data.frame(both[both$eth == "Some other race, ethnicity, or origin (please specify)",])
bothOtherN = nrow(bothOther)
bothOtherPerc = bothOtherN / bothN; bothOtherPerc

sum(bothWhitePerc, bothHisPerc, bothBlackPerc, bothAsianPerc, bothAIPerc, bothMEPerc, bothNHPerc, bothOtherPerc)


```
Now we need to get job type
```{r}
both = as.data.frame(rbind(mccsc, rbbcsc))
dim(both)
bothQuan = na.omit(both)
bothQuan = as.data.frame(both)
dim(both)
both = apply(both,2, function(x){ifelse(x == "NA", NA, x)})
both = na.omit(both)
both = as.data.frame(both)
bothN = nrow(both)

# Now primary teachers
```

