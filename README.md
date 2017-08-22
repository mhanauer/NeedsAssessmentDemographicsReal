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
mccsc = as.data.frame(read.csv("mccsc.csv", header = TRUE, na.strings = ""))

## RBBCSC had a response of 85% so take what you have times 15% and add that to get the full RBBCSC sample.  I am counting all the NA's expect for the ones that I filled out early as tests.  I believe we had a 40% response rate for MCCSC
rbbcscFullSample = round(nrow(rbbcsc) + .15*nrow(rbbcsc),0)
mccscFullSample = 474+ 474*.6
responseRate = 474/ (rbbcscFullSample +mccscFullSample); responseRate



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
dim(rbbcsc)
dim(both)
sum(is.na(both))
both = na.omit(both)
dim(both)
# Here we are getting the sample size, which gets rid anyone who did not respond to any of the quantitative questions, gender, ethincity, job, or education level

bothN = nrow(both)

## Gender only male rest if female
bothMale = as.data.frame(both[both$gender == "Male",])
bothMaleN = nrow(bothMale)
bothMalePerc = bothMaleN / bothN; bothMalePerc 

bothOtherGI = as.data.frame(both[both$gender == "Other gender identity (please specify)",])
bothOtherGIN = nrow(bothOtherGI)
bothOtherGIPerc = bothOtherGIN / bothN; bothOtherGIPerc

bothFemalePerc = 1-(bothMalePerc+bothOtherGIPerc)

sum(bothFemalePerc, bothMalePerc, bothOtherGIPerc)
```
Now we are getting ethnicity white, black, hispanic, and other
Make things add up to one.
```{r}
## Now white people
write.csv(both, "both.csv", row.names = FALSE)
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


bothTotal = sum(bothWhitePerc, bothHisPerc, bothBlackPerc, bothAsianPerc, bothAIPerc, bothMEPerc, bothNHPerc, bothOtherPerc)

bothMulti = 1-bothTotal

```
Now we need to get job type
```{r}
# Now primary teachers
bothPST = as.data.frame(both[both$job == "Primary school teacher",])
bothPSTN = nrow(bothPST)
bothPSTPerc = bothPSTN / bothN; bothPSTPerc 

# Now Secondary school teacher
bothSST = as.data.frame(both[both$job == "Secondary school teacher",])
bothSSTN = nrow(bothSST)
bothSSTPerc = bothSSTN / bothN; bothSSTPerc 

# Now School Social Worker
bothSSW = as.data.frame(both[both$job == "School Social Worker",])
bothSSWN = nrow(bothSSW)
bothSSWPerc = bothSSWN / bothN; bothSSWPerc 

# Principal
bothP = as.data.frame(both[both$job == "Principal",])
bothPN = nrow(bothP)
bothPPerc = bothPN / bothN; bothPPerc

# School Psychologist
bothSP = as.data.frame(both[both$job == "School Psychologist",])
bothSPN = nrow(bothSP)
bothSPPerc = bothSPN / bothN; bothSPPerc

# School Counselor
bothSC = as.data.frame(both[both$job == "School Counselor",])
bothSCN = nrow(bothSC)
bothSCPerc = bothSCN / bothN; bothSCPerc

# Special Education teacher
bothSET = as.data.frame(both[both$job == "Special Education teacher",])
bothSETN = nrow(bothSET)
bothSETPerc = bothSETN / bothN; bothSETPerc

# Administrator 
bothA = as.data.frame(both[both$job == "Administrator",])
bothAN = nrow(bothA)
bothAPerc = bothAN / bothN; bothAPerc

# Support Staff 
bothSS = as.data.frame(both[both$job == "Support Staff",])
bothSSN = nrow(bothSS)
bothSSPerc = bothSSN / bothN; bothSSPerc

# Pre-K teacher 
bothPK = as.data.frame(both[both$job == "Pre-K teacher",])
bothPKN = nrow(bothPK)
bothPKPerc = bothPKN / bothN; bothPKPerc

# Other (please specify)
bothO = as.data.frame(both[both$job == "Other (please specify)",])
bothON = nrow(bothO)
bothOPerc = bothON / bothN; bothOPerc

total = sum(bothPSTPerc, bothSSTPerc, bothSSWPerc, bothPPerc, bothSPPerc, bothSCPerc, bothSETPerc, bothAPerc, bothSSPerc, bothPKPerc,bothOPerc)

bothMulti = 1-total


1-.26-.39-.08-.08



```

