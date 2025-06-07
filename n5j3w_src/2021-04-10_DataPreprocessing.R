## CALCULATION OF ADDITIONAL VARIABLES FOR INCLUDING ENGAGEMENT
## ************************************************************
## Pascal (April 10, 2021)

## The goal of this file is to calculate additional variables
## that allow for a more meaningful analysis of the effect
## of the feedback treatment in the experiment. The main focus is
## on combining the feedback treatment and how engaged participants
## were in this treatment.

## The existing variables include:
## FormatFeedback
table(Rank$FormatFeedback)
## Set
table(Rank$Set)
## Engage
table(Rank$Engage)
## Binary variable indicating whether participants were in the top or
## bottom half of the distribution of the time they spend on the middle 
## feedback. Participants that did not get feedback or were in their first
## set to false like the 'non-engaged'. This is suboptimal. We cannot
## use straight up time spend on feedback to represent engagement as time
## depends heavily on the type of feedback they received
aggregate(Tbl$Page_ScenFeedback1_DurSec, by = list(Tbl$FormatFeedback), summary)

## The challenge is to combine these variables in the most meaningful way
## to include them in the model that we can really see what is going on. 

## 1) Calculate a more granular engagement variables. Splitting them up into
##    thirds is better as it creates an average category that can more
##    meaningfully be compared to the participants that did not get any feedback

Tbl$FeedbackEngage <- NULL
Rank$Engage <- NULL

quantN <- c(0, 1/3, 2/3, 1)
labels <- c("Less", "Avg", "More")
quantN_answers <- quantile(Tbl$Page_ScenFeedback1_DurSec[Tbl$FormatFeedback == "Answers"], quantN)
quantN_artic <- quantile(Tbl$Page_ScenFeedback1_DurSec[Tbl$FormatFeedback == "Artic"], quantN)
quantN_expl <- quantile(Tbl$Page_ScenFeedback1_DurSec[Tbl$FormatFeedback == "Expl"], quantN)

Temp_Answer <- Tbl[Tbl$FormatFeedback == "Answers", c("Id", "Page_ScenFeedback1_DurSec")]
hist(Temp_Answer$Page_ScenFeedback1_DurSec, breaks = 100, xlim = c(0, 200), col = "gold")
Temp_Answer$FeedbackEngage <- cut(Temp_Answer$Page_ScenFeedback1_DurSec, quantN_answers, include.lowest = T)
Temp_Answer$FeedbackEngage <- ordered(Temp_Answer$FeedbackEngage, labels = labels)
table(Temp_Answer$FeedbackEngage)

Temp_Artic <- Tbl[Tbl$FormatFeedback == "Artic", c("Id", "Page_ScenFeedback1_DurSec")]
hist(Temp_Artic$Page_ScenFeedback1_DurSec, breaks = 250, xlim = c(0, 500), col = "gold")
Temp_Artic$FeedbackEngage <- cut(Temp_Artic$Page_ScenFeedback1_DurSec, quantN_artic, include.lowest = T)
Temp_Artic$FeedbackEngage <- ordered(Temp_Artic$FeedbackEngage, labels = labels)
table(Temp_Artic$FeedbackEngage)

Temp_Expl <- Tbl[Tbl$FormatFeedback == "Expl", c("Id", "Page_ScenFeedback1_DurSec")]
hist(Temp_Expl$Page_ScenFeedback1_DurSec, breaks = 100, xlim = c(0, 200), col = "gold")
Temp_Expl$FeedbackEngage <- cut(Temp_Expl$Page_ScenFeedback1_DurSec, quantN_expl, include.lowest = T)
Temp_Expl$FeedbackEngage <- ordered(Temp_Expl$FeedbackEngage, labels = labels)
table(Temp_Expl$FeedbackEngage)

Temp <- rbind(Temp_Answer, Temp_Artic, Temp_Expl)
rm(Temp_Answer, Temp_Artic, Temp_Expl)
str(Temp)

Tbl <- merge(Tbl, Temp[, c("Id", "FeedbackEngage")], all.x = T)
table(Tbl$FormatFeedback, Tbl$FeedbackEngage, useNA = "always")
rm(Temp)

Rank <- merge(Rank, Tbl[, c("Id", "FeedbackEngage")], all.x = T)
Rank$FeedbackEngage[Rank$Set == "Set1"] <- NA
Rank$FeedbackEngage[Rank$FormatFeedback == "None"] <- NA
table(Rank$FeedbackEngage, Rank$FormatFeedback, useNA = "always")


## 2) Numerical for FeedbackEngage centered around Avg
Rank$FeedbackEngageNum <- as.numeric(Rank$FeedbackEngage) - 2
Rank$FeedbackEngageNum[is.na(Rank$FeedbackEngageNum)] <- 0
table(Rank$FeedbackEngage, Rank$FeedbackEngageNum, useNA = "always")


## 3) Interaction of FeedbackEngage with FormatFeedback
Rank$FBArticEngageNum <- 0
Rank$FBArticEngageNum[Rank$FormatFeedback == "Artic"] <- Rank$FeedbackEngageNum[Rank$FormatFeedback == "Artic"]
table(Rank$FBArticEngageNum, Rank$FormatFeedback)

Rank$FBAnswersEngageNum <- 0
Rank$FBAnswersEngageNum[Rank$FormatFeedback == "Answers"] <- Rank$FeedbackEngageNum[Rank$FormatFeedback == "Answers"]
table(Rank$FBAnswersEngageNum, Rank$FormatFeedback)

Rank$FBExplEngageNum <- 0
Rank$FBExplEngageNum[Rank$FormatFeedback == "Expl"] <- Rank$FeedbackEngageNum[Rank$FormatFeedback == "Expl"]
table(Rank$FBExplEngageNum, Rank$FormatFeedback)

