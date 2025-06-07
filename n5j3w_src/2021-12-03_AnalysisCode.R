## ********************************************
## ** LEARNING INTERVENTIONS - ANALYSIS CODE **
## ********************************************
##
## Fisher, Haegeli and Mair: 
## Exploring the avalanche bulletin as an avenue 
## for continuing education by including learning
## interventions
## Dec. 3, 2021


if (!require("glmmTMB")) {install.packages("glmmTMB"); require("glmmTMB")}
if (!require("effects")) {install.packages("car"); require("effects")}
if (!require("car")) {install.packages("car"); require("car")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}
if (!require("partykit")) {install.packages("partykit"); require("partykit")}
if (!require("vcd")) {install.packages("vcd"); require("vcd")}


## Setting contrasts
options(contrasts=c('contr.treatment','contr.treatment')) ## Dummy coding --> main effect represent base level

## ---- 1) GETTING DATA ----

## Load data
load("2021-12-03_FinalDataFileClean.RData")

str(Rank)
length(unique(Rank$Id))

## Distribution of target variable
plot(Rank$ResultCat)

## Basic statistics of ResultCat
nrow(Rank)
table(Rank$ResultCat)
round(100*prop.table(table(Rank$ResultCat)), 1)

## ---- 2) ADDITIONAL PROCESSING ----

## Reclassifying of avalanche training
Rank$BackgrAvTrainingCat <- ordered(Rank$BackgrAvTraining, labels = c("None", "None", "Intro", "Intro", "Advanced", "Prof"))
table(Rank$BackgrAvTraining, Rank$BackgrAvTrainingCat)
Tbl$BackgrAvTrainingCat <- ordered(Tbl$BackgrAvTraining, labels = c("None", "None", "Intro", "Intro", "Advanced", "Prof"))
table(Tbl$BackgrAvTraining, Tbl$BackgrAvTrainingCat)

## Calculate engagement variable
source("2021-04-10_DataPreprocessing.R")

table(Rank$FeedbackEngage, useNA = "always")

table(Rank$FBAnswersEngageNum, Rank$FormatFeedback, useNA = "always")
table(Rank$FBArticEngageNum, Rank$FormatFeedback, useNA = "always")
table(Rank$FBExplEngageNum, Rank$FormatFeedback, useNA = "always")

## Filter to only participant that received feedback
Tbl2 <- Tbl[Tbl$FormatFeedback != "None",]
Tbl2$FormatFeedback <- factor(Tbl2$FormatFeedback, ordered = F)
Rank2 <- Rank[Rank$Id %in% as.character(Tbl2$Id),]

## Transfer engagement to both Set1 and Set2
table(Tbl2$FeedbackEngage, Tbl2$FormatFeedback, useNA = "always")

RelevIds <- as.character(Tbl2$Id[Tbl2$FeedbackEngage == "Less"])
Rank2$FeedbackEngage[Rank2$Id %in% RelevIds] <- "Less"
RelevIds <- as.character(Tbl2$Id[Tbl2$FeedbackEngage == "Avg"])
Rank2$FeedbackEngage[Rank2$Id %in% RelevIds] <- "Avg"
RelevIds <- as.character(Tbl2$Id[Tbl2$FeedbackEngage == "More"])
Rank2$FeedbackEngage[Rank2$Id %in% RelevIds] <- "More"

table(Rank2$FeedbackEngage, Rank2$FormatFeedback, useNA = "always")
table(Rank2$FeedbackEngage, Rank2$Set, useNA = "always")

Rank2$FeedbackEngageNum <- as.numeric(Rank2$FeedbackEngage) - 2

## Create different FormatFeedback variable without 'None' what allows us to specifically 
## compare before and after for each feedback treatment
Tbl2$FormatFeedback2 <- Tbl2$FormatFeedback
Rank2 <- merge(Rank2, Tbl2[,c("Id", "FormatFeedback2")], all.x =  T)

table(Rank2$FormatFeedback, Rank2$FormatFeedback2)
Rank2$FormatFeedback2 <- as.character(Rank2$FormatFeedback2)
Rank2$FormatFeedback2[Rank2$FormatFeedback == "None"] <- paste0(Rank2$FormatFeedback2[Rank2$FormatFeedback == "None"], "1")
Rank2$FormatFeedback2[Rank2$FormatFeedback != "None"] <- paste0(Rank2$FormatFeedback2[Rank2$FormatFeedback != "None"], "2")
table(Rank2$FormatFeedback2)
table(Rank2$FormatFeedback, Rank2$FormatFeedback2)
Rank2$FormatFeedback2 <- factor(Rank2$FormatFeedback2)

## Calulate average time it took in first two tasks
RankTask1 <- Rank2[Rank2$ScenNum == 1, c("Id", "DurSec")]
names(RankTask1)[2] <- "DurSecTask1"
RankTask2 <- Rank2[Rank2$ScenNum == 2, c("Id", "DurSec")]
names(RankTask2)[2] <- "DurSecTask2"
RankSet1 <- merge(RankTask1, RankTask2, by = "Id")
RankSet1$DurSecAvg1 <- (RankSet1$DurSecTask1 + RankSet1$DurSecTask2)/2
hist(RankSet1$DurSecAvg1, breaks = 100, col = "gold", main = "Distribution of average time in first set")

Tbl2 <- merge(Tbl2, RankSet1[, c("Id", "DurSecAvg1")])
head(Tbl2)

## Convert average time into a ordinal variable
Tbl2$DurSecAvg1Cat <- cut(Tbl2$DurSecAvg1, breaks = quantile(Tbl2$DurSecAvg1, c(0, 1/3, 2/3, 1)), include.lowest = T)
aggregate(Tbl2$DurSecAvg1, list(Tbl2$DurSecAvg1Cat), summary)
levels(Tbl2$DurSecAvg1Cat) <- c("Less", "Avg", "More")

## Delete NA for including exercise
Tbl2 <- Tbl2[!is.na(Tbl2$ExerciseIncl),]
(temp <- table(Tbl2$ExerciseIncl, useNA = "always"))
prop.table(temp)


## ---- 2.5) Demographics ----

## Simplify some of the variables
Tbl2$BackgrActivity_1 <- factor(Tbl2$BackgrActivity_1, labels = c("SS", "IC", "OB", "BC", "BC", "SM"))


table(Tbl2$DemogrGender)
round(100*prop.table(table(Tbl2$DemogrGender)),1)

table(Tbl2$DemogrAge)
round(100*prop.table(table(Tbl2$DemogrAge)),1)

table(Tbl2$DemogrEduc)
round(100*prop.table(table(Tbl2$DemogrEduc)),1)
1069+733

table(Tbl2$BackgrAvTraining)
round(100*prop.table(table(Tbl2$BackgrAvTraining)),1)
1078+435+369
47.3+19.1+16.2

table(Tbl2$BackgrActivity_1)
round(100*prop.table(table(Tbl2$BackgrActivity_1)),1)

table(Tbl2$BackgrYrsOfExp)
round(100*prop.table(table(Tbl2$BackgrYrsOfExp)),1)

table(Tbl2$BullUseType)
round(100*prop.table(table(Tbl2$BullUseType)),1)
693+1031

table(Tbl2$DemogrCountry)
round(100*prop.table(table(Tbl2$DemogrCountry)),1)

## Plotting Figure 3
png(filename = "Fig03_Demographics.png", width = 19, height = 10, units = "cm", res = 500, pointsize = 7)
par(mfrow=c(2,3))

## Age
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$DemogrAge))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$DemogrAge, main = "a) Age Categories", las = 1, ylab = "Number participants", ylim = c(0, 1.15*max(counts)), col = "#a6cee3")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Education
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$DemogrEduc))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$DemogrEduc, main = "b) Highest Level of Education Completed", xaxt = "n", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#b2df8a")
axis(1, at = xaxis, labels = c("< HS", "HS", "PostSec", "Trades", "UGrad", "Grad"), tick = F)
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Primariy BC activity
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$BackgrActivity_1))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$BackgrActivity_1, main = "c) Primary Backcountry Activity", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#fb9a99")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Avalanche Training
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$BackgrAvTraining))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$BackgrAvTraining, main = "d) Avalanche Awareness Training", las = 1, ylab = "Number participants", ylim = c(0, 1.15*max(counts)), col = "#fdbf6f")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Years of Experience
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$BackgrYrsOfExp))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$BackgrYrsOfExp, main = "e) Years of Backcountry Experience", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#cab2d6")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

## Bulletin User Type
par(mar = c(5.1, 5.1, 4.1, 2.1))
(counts <- table(Tbl2$BullUseType))
perc <- round(100*prop.table(counts),1)
xaxis <- plot(Tbl2$BullUseType, main = "f) Bulletin User Type", las = 1, ylab = "", ylim = c(0, 1.15*max(counts)), col = "#ffff99")
box()
text(x = xaxis, y = counts, labels = sprintf("%3.1f%%", perc), pos = 3)

dev.off()

## Reset
par(mar = c(5.1, 4.1, 4.1, 2.1))


## ---- 3) MODELS: Who is engaged ----


## Model
Engage <- ctree(FeedbackEngage ~ NumFalseBeforeFB + FormatFeedback + BackgrAvTrainingCat + 
                  BackgrActivity_1 + BullUseType + BullCountry + BackgrDaysPerYr + SeenPhoneMsg + DurSecAvg1Cat, 
                data = Tbl2)
plot(Engage)
## -> How people engaged with the feedback is an interaction between how they performed in the first Set and the type of feedback
##    1) The articulating task was more engaging for participants correctly answered the taks in the first set.
##       (FYI, participants did not know whether their answers were correct.)
##    2) Participants who made some errors in the first set, were more engaged in Answers and Explanation feedback
##    3) The tendancy towards more engagement was strong for the Answers format (participants had to figure out why they got it wrong)
##    4) Completing the survey on the phone had a slight effect. Predicably, participants on a phone where slight less engaged,
##    4) No other predictor that describes the characteristics of the participant was relevant.

EngageColScale <- RColorBrewer::brewer.pal(3, "RdBu")

## Option 1: side-by-side
png(filename = "Fig04_EngageTree_hires_template_side.png", width = 24, height = 14, units = "cm", res = 500, pointsize = 7)
plot(Engage, terminal_panel = node_barplot(Engage, fill = EngageColScale, ylines = 1.5, id = T))
dev.off()


## Explicitly test difference for artic
wilcox.test(as.numeric(Tbl2$FeedbackEngage[Tbl2$FormatFeedback == 'Artic' & Tbl2$NumFalseBeforeFB == 0]),
            as.numeric(Tbl2$FeedbackEngage[Tbl2$FormatFeedback == 'Artic' & Tbl2$NumFalseBeforeFB != 0]))

## Predict node for checking differences in distributions with chi-squared test
Tbl2$NodeRate <- predict(Engage, newdata = Tbl2, type = "node")
table(Tbl2$NodeRate)

(temp <- chisq.test(Tbl2$NodeRate, Tbl2$FeedbackEngage))
temp$observed
round(100*prop.table(temp$observed, 1), 1)
mosaic(temp$observed, shade = T)



### ---- 4) MODELS: Effect of feedback on correctness  ----

## ---- Model with numerical engagement ----

table(Rank2$ResultCat)

CorResult1 <- glmmTMB(ResultCat ~ FormatInfo + FeedbackEngageNum*FormatFeedback2 + BackgrAvTrainingCat + 
                        RouteType + MapTest + BackgrActivity_1 + UsedPhone +
                        (1|Id) + (1|ScenID), 
                      family = binomial, data = Rank2)

## Overview
Anova(CorResult1)
summary(CorResult1)
## -> Highly significant interaction!

## Overview effects plots
fit.eff <- allEffects(CorResult1)
plot(fit.eff)
plot(fit.eff, "FeedbackEngageNum:FormatFeedback2")

## Looking at differences for different engagement levels in first set
at <- list(FeedbackEngageNum = 0,
           FormatFeedback2 = c("Artic1", "Answers1", "Expl1"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ FormatFeedback2))
## -> On average, there is now significant difference in the first set

## Looking at differences for different engagement levels in second set
at <- list(FeedbackEngageNum = 0,
           FormatFeedback2 = c("Artic2", "Answers2", "Expl2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ FormatFeedback2))
## -> On average, there is now significant difference in the second set



## Looking at differences for different engagement levels for Articulate
at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Artic1", "Artic2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_artic <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))
## -> No impact of articulation regardless of engagement

# Looking at differences for different engagement levels for Answers
at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Answers1", "Answers2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_answers <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))
## -> Positive effect for participants with more than average engagement in feedback
##    Negative effect for participants with less than average engagement in feedback

# Looking at differences for different engagement levels for Explanation
at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Expl1", "Expl2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_expl <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))
## -> Positive effect for participants with both average or more engagement in feedback

## Looking at differences among the most engaged
at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Artic2", "Answers2", "Expl2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_mosteng <- emmeans(grid, specs = pairwise ~ FormatFeedback2, by = "FeedbackEngageNum"))
## -> I dont think this is meaningful as it depends again on who engaged with the different feedback.
##    As we know from pervious analysis, on average (EngageNum = 0), there is no significant differences!

## Looking at differences in other predictors
## ---- Problem graphic ----
(emm <- emmeans(CorResult1, specs = ~FormatInfo, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Artic2", "Answers2", "Expl2"))
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_mosteng <- emmeans(grid, specs = pairwise ~ FormatFeedback2, by = "FeedbackEngageNum"))


## ----Avalanche Training---- 
(emm <- emmeans(CorResult1, specs = ~BackgrAvTrainingCat, type = "response"))
contrast(emm, method = "consec")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = consec ~ BackgrAvTrainingCat))


## ----Activity----
(emm <- emmeans(CorResult1, specs = ~BackgrActivity_1, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ BackgrActivity_1))



##---Route type----
(emm <- emmeans(CorResult1, specs = ~RouteType, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ RouteType))

## ---Map test----
(emm <- emmeans(CorResult1, specs = ~MapTest, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ MapTest))

## ----Used phone----
(emm <- emmeans(CorResult1, specs = ~UsedPhone, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(CorResult1, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ UsedPhone))


### ---- 5) MODELS: Effect of feedback on duration  ----

## Model with numerical engagement variable
quantile(Rank2$DurSec)



## Model without interaction
## KF - In the file you sent me this had the orginal format feedback variable, I changed it to format feedback2
## It looks like that's what was used in the figure, so I'm confused. 
DurSec2 <- glmmTMB(DurSec ~ FormatInfo + FormatFeedback2 + FeedbackEngageNum + BackgrAvTrainingCat + 
                     RouteType + MapTest + DemogrAgeLin +
                     (1|Id) + (1|ScenID), 
                   family = Gamma(link = "log"), data = Rank2)

Anova(DurSec2)
summary(DurSec2)

## Overview effects plots
fit.eff <- allEffects(DurSec2)
plot(fit.eff)
plot(fit.eff, "FormatFeedback2")
plot(fit.eff, "FeedbackEngageNum")


(emm <- emmeans(DurSec2, specs = ~FormatFeedback2, type = "response"))
contrast(emm, method = "pairwise")


## Checking out differences based on Format feedback2 at average engagement 
## Am I correct in that it shouldn't matter what level of engagement we make the cut at because there is no interaction? 
## Seems reasonable to put it at average though

at <- list(FeedbackEngageNum = 0, FormatFeedback2 = c('Artic1', 'Artic2'))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur2_engage <- emmeans(grid, specs = pairwise ~ FormatFeedback2))

at <- list(FeedbackEngageNum = 0, FormatFeedback2 = c('Answers1', 'Answers2'))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur2_engage <- emmeans(grid, specs = pairwise ~ FormatFeedback2))

at <- list(FeedbackEngageNum = 0, FormatFeedback2 = c('Expl1', 'Expl2'))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur2_engage <- emmeans(grid, specs = pairwise ~ FormatFeedback2))


## Looking at differences for different engagement levels in first set
at <- list(FeedbackEngageNum = 0,
           FormatFeedback2 = c("Artic1", "Answers1", "Expl1"))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ FormatFeedback2))
## -> On average, there is now significant difference in the first set

## Looking at differences for different engagement levels in second set
at <- list(FeedbackEngageNum = 0,
           FormatFeedback2 = c("Artic2", "Answers2", "Expl2"))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ FormatFeedback2))
## -> On average, there is now significant difference in the first set


## Looking at differences in other predictors

## ----Avalanche Training---- 
(emm <- emmeans(DurSec2, specs = ~BackgrAvTrainingCat, type = "response"))
contrast(emm, method = "consec")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = consec ~ BackgrAvTrainingCat))


##---Route type----
(emm <- emmeans(DurSec2, specs = ~RouteType, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ RouteType))


## ---Map test----
(emm <- emmeans(DurSec2, specs = ~MapTest, type = "response"))
contrast(emm, method = "pairwise")

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = pairwise ~ MapTest))


## --- Graphic format---
(emm <- emmeans(DurSec2, specs = ~FormatInfo, type = "response"))
contrast(emm, method = "trt.vs.ctrl" )

at <- list(FeedbackEngageNum = 0)
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_corr_set1 <- emmeans(grid, specs = trt.vs.ctrl ~ FormatInfo))



## ---- 6) CTREES: Usefulness of feedback ----
round(100*prop.table(table(Tbl2$Useful_FeedbackMiddle)),1)


FeedTree <- ctree(Useful_FeedbackMiddle ~ FormatInfo + FeedbackEngage + FormatFeedback + NumFalseBeforeFB + BackgrAvTrainingCat + 
                    BackgrActivity_1 + BackgrDaysPerYr + BackgrYrsOfExp + BullUseType + DurSecAvg1Cat, data = Tbl2)
plot(FeedTree)

table(Tbl2$Useful_FeedbackMiddle)

FeedTreeColScale <- RColorBrewer::brewer.pal(4, "Purples")

## Option 1: side-by-side
png(filename = "Fig07_FeedTree_hires_template_side.png", width = 14, height = 11, units = "cm", res = 500, pointsize = 7)
plot(FeedTree, terminal_panel = node_barplot(FeedTree, fill = FeedTreeColScale, ylines = 1.5, id = T))
dev.off()


## Predict node for checking differences in distributions with chi-squared test
Tbl2$NodeRate <- predict(FeedTree, newdata = Tbl2, type = "node")
table(Tbl2$NodeRate)

(temp <- chisq.test(Tbl2$NodeRate, Tbl2$Useful_FeedbackMiddle))
temp$observed
round(100*prop.table(temp$observed, 1), 1)
mosaic(temp$observed, shade = T)

## ---- 7) CTREES: Usefulness of excercises----
round(100*prop.table(table(Tbl2$Useful_Exercise)),1)
29.6+56.7

ExTree <- ctree(Useful_Exercise ~ FeedbackEngage + FormatInfo + FormatFeedback + RankTaskPerform + BackgrAvTrainingCat + 
                  BackgrActivity_1 + BackgrDaysPerYr + BackgrYrsOfExp + BullUseType + DurSecAvg1Cat, data = Tbl2)

plot(ExTree)


ExTreeColScale <- RColorBrewer::brewer.pal(4, "Purples")

## Option 1: side-by-side
png(filename = "Fig08_ExTree_hires_template_side.png", width = 14, height = 11, units = "cm", res = 500, pointsize = 7)
plot(ExTree, terminal_panel = node_barplot(ExTree, fill = ExTreeColScale, ylines = 1.5, id = T))
dev.off()



## Predict node for checking differences in distributions with chi-squared test
Tbl2$NodeRate <- predict(ExTree, newdata = Tbl2, type = "node")
table(Tbl2$NodeRate)

(temp <- chisq.test(Tbl2$NodeRate, Tbl2$Useful_FeedbackMiddle))
temp$observed
round(100*prop.table(temp$observed, 1), 1)
mosaic(temp$observed, shade = T)

head(Tbl2$RankTaskPerform)

## ---- 8) CTREES: Include excercises ----

## Including average time it took in the first set in feedback engagement tree
round(100*prop.table(table(Tbl2$ExerciseIncl)),1)

Incl1 <- ctree(ExerciseIncl ~ NumFalseBeforeFB + FormatFeedback + BackgrAvTrainingCat + 
                 BackgrActivity_1 + BullUseType + BullCountry + BackgrDaysPerYr + SeenPhoneMsg + DurSecAvg1Cat, 
               data = Tbl2)
plot(Incl1)

Tbl2$Node <- predict(Incl1, type = "node")
table(Tbl2$Node)

(temp <- table(Tbl2$Node, Tbl2$ExerciseIncl))
round(100*prop.table(temp, 1), 1)



## ---- 10) FIGURES ----

## Additional distance between sets
dist <- 0.25
xaxis <- c(c(1:3)+1*dist, c(4:6)+2*dist, c(7:9)+3*dist)

## Colors
Col_Artic_1 <- "#33a02c"
Col_Artic_2 <- "#b2df8a"
Col_Answers_1 <- "#ff7f00"
Col_Answers_2 <- "#fdbf6f"
Col_Expl_1 <- "#6a3d9a"
Col_Expl_2 <- "#cab2d6"

## General settings
pts.cex <- 1
lbl.cex <- 0.75
ci.lwd <- 1.5
ci.length <- 0.02
shift <- 0.1

## ---- Effects plot for Correctness Model ----

## Opening png file
png(filename = "Fig05_CorrectEngage_hires.png", width = 9, height = 8, units = "cm", res = 500, pointsize = 7)

## Empty plot
plot(NULL, xlim=c(min(xaxis)-0.5, max(xaxis)+0.5), ylim = c(30, 95), xaxt = "n", las = 1, 
     xlab = "Learning intervention", ylab = "Probability of ranking correctly (%)")

## Axes and labels
axis(1, at = xaxis[c(2, 5, 8)], labels = c("Reflection", "Answers", "Explanations"))
axis(1, at = xaxis[1:9], labels = F, tck = -0.02)
axis(3, at = xaxis[1:9], labels = rep(c("Lo", "Me", "Hi"), 3), tck = -0.02)
mtext("Relative engagement in learning intervention", side = 3, at = xaxis[5], line = 2.5)

## Plot positions
xaxis_artic <- xaxis[1:3]
xaxis_answers <- xaxis[4:6]
xaxis_expl <- xaxis[7:9]

## Extracting values from emmeans objects
emm_artic_val <- summary(emm_corr_artic$emmeans)
emm_answers_val <- summary(emm_corr_answers$emmeans)
emm_expl_val <- summary(emm_corr_expl$emmeans)

## Polygons behind grid
polygon(c(xaxis_artic-shift, rev(xaxis_artic-shift)), 100*c(emm_artic_val$lower.CL[c(1, 3, 5)], rev(emm_artic_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Artic_2, "50"), border = NA)
polygon(c(xaxis_artic+shift, rev(xaxis_artic+shift)), 100*c(emm_artic_val$lower.CL[c(2, 4, 6)], rev(emm_artic_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Artic_2, "50"), border = NA)

polygon(c(xaxis_answers-shift, rev(xaxis_answers-shift)), 100*c(emm_answers_val$lower.CL[c(1, 3, 5)], rev(emm_answers_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Answers_2, "50"), border = NA)
polygon(c(xaxis_answers+shift, rev(xaxis_answers+shift)), 100*c(emm_answers_val$lower.CL[c(2, 4, 6)], rev(emm_answers_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Answers_2, "50"), border = NA)

polygon(c(xaxis_expl-shift, rev(xaxis_expl-shift)), 100*c(emm_expl_val$lower.CL[c(1, 3, 5)], rev(emm_expl_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Expl_2, "50"), border = NA)
polygon(c(xaxis_expl+shift, rev(xaxis_expl+shift)), 100*c(emm_expl_val$lower.CL[c(2, 4, 6)], rev(emm_expl_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Expl_2, "50"), border = NA)

grid(nx = NA, ny = NULL, lty = 3, lwd = 0.5, col = "grey")
abline(v = xaxis, lty = 3, lwd = 0.5, col = "grey")
box()

## Articulate plotting
arrows(xaxis_artic-shift, 100*emm_artic_val$lower.CL[c(1, 3, 5)], xaxis_artic-shift, 100*emm_artic_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Artic_2, length = ci.length)
arrows(xaxis_artic+shift, 100*emm_artic_val$lower.CL[c(2, 4, 6)], xaxis_artic+shift, 100*emm_artic_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Artic_2, length = ci.length)
lines(xaxis_artic-shift, 100*emm_artic_val$prob[c(1, 3, 5)], col = Col_Artic_1)
lines(xaxis_artic+shift, 100*emm_artic_val$prob[c(2, 4, 6)], col = Col_Artic_1)
points(xaxis_artic-shift, 100*emm_artic_val$prob[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Artic_1)
points(xaxis_artic+shift, 100*emm_artic_val$prob[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Artic_1)

## Articulate labelling
text(xaxis_artic-shift, 100*emm_artic_val$prob[c(1, 3, 5)], labels = format(round(100*emm_artic_val$prob[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = 1, offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_artic+shift, 100*emm_artic_val$prob[c(2, 4, 6)], labels = format(round(100*emm_artic_val$prob[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 

## Answers significant differences
pval <- summary(emm_corr_artic$contrasts)$p.value
if (pval[1] < 0.05) points(xaxis_artic[1], 100*(emm_artic_val$prob[1] + emm_artic_val$prob[2])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[2] < 0.05) points(xaxis_artic[2], 100*(emm_artic_val$prob[3] + emm_artic_val$prob[4])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[3] < 0.05) points(xaxis_artic[3], 100*(emm_artic_val$prob[5] + emm_artic_val$prob[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Answers plotting
arrows(xaxis_answers-shift, 100*emm_answers_val$lower.CL[c(1, 3, 5)], xaxis_answers-shift, 100*emm_answers_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Answers_2, length = ci.length)
arrows(xaxis_answers+shift, 100*emm_answers_val$lower.CL[c(2, 4, 6)], xaxis_answers+shift, 100*emm_answers_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Answers_2, length = ci.length)
lines(xaxis_answers-shift, 100*emm_answers_val$prob[c(1, 3, 5)], col = Col_Answers_1)
lines(xaxis_answers+shift, 100*emm_answers_val$prob[c(2, 4, 6)], col = Col_Answers_1)
points(xaxis_answers-shift, 100*emm_answers_val$prob[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Answers_1)
points(xaxis_answers+shift, 100*emm_answers_val$prob[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Answers_1)

## Answers labelling
text(xaxis_answers-shift, 100*emm_answers_val$prob[c(1, 3, 5)], labels = format(round(100*emm_answers_val$prob[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = c(3, 1, 3), offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_answers+shift, 100*emm_answers_val$prob[c(2, 4, 6)], labels = format(round(100*emm_answers_val$prob[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = c(1, 3, 3), offset = 1, cex = lbl.cex, font = 2) 

## Answers significant differences
pval <- summary(emm_corr_answers$contrasts)$p.value
if (pval[1] < 0.05) points(xaxis_answers[1], 100*(emm_answers_val$prob[1] + emm_answers_val$prob[2])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[2] < 0.05) points(xaxis_answers[2], 100*(emm_answers_val$prob[3] + emm_answers_val$prob[4])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[3] < 0.05) points(xaxis_answers[3], 100*(emm_answers_val$prob[5] + emm_answers_val$prob[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Expl plotting
arrows(xaxis_expl-shift, 100*emm_expl_val$lower.CL[c(1, 3, 5)], xaxis_expl-shift, 100*emm_expl_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Expl_2, length = ci.length)
arrows(xaxis_expl+shift, 100*emm_expl_val$lower.CL[c(2, 4, 6)], xaxis_expl+shift, 100*emm_expl_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Expl_2, length = ci.length)
lines(xaxis_expl-shift, 100*emm_expl_val$prob[c(1, 3, 5)], col = Col_Expl_1)
lines(xaxis_expl+shift, 100*emm_expl_val$prob[c(2, 4, 6)], col = Col_Expl_1)
points(xaxis_expl-shift, 100*emm_expl_val$prob[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Expl_1)
points(xaxis_expl+shift, 100*emm_expl_val$prob[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Expl_1)

## Expl labelling
text(xaxis_expl-shift, 100*emm_expl_val$prob[c(1, 3, 5)], labels = format(round(100*emm_expl_val$prob[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = c(3, 1, 1), offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_expl+shift, 100*emm_expl_val$prob[c(2, 4, 6)], labels = format(round(100*emm_expl_val$prob[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = c(1, 3, 3), offset = 1, cex = lbl.cex, font = 2) 

## Expl significant differences
pval <- summary(emm_corr_expl$contrasts)$p.value
if (pval[1] < 0.05) points(xaxis_expl[1], 100*(emm_expl_val$prob[1] + emm_expl_val$prob[2])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[2] < 0.05) points(xaxis_expl[2], 100*(emm_expl_val$prob[3] + emm_expl_val$prob[4])/2, pch = 8, cex = pts.cex, lwd = 1)
if (pval[3] < 0.05) points(xaxis_expl[3], 100*(emm_expl_val$prob[5] + emm_expl_val$prob[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Legend
legend("bottomleft", c("Pre-interv.", "Post-interv.", "p < 0.05"), pch = c(25, 24, 8), pt.bg = "grey", pt.cex = 1, bty = "n", horiz = T,
       x.intersp = 1.5, inset = 0.02)

dev.off()


## ---- Effects plot for Completion Time Model ----

## Calculating marginal measin in the same way as for the correctness model
## Only used for printing
## The contrasts just show that participants where fast in the second set.
at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Artic1", "Artic2"))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur_artic <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))

at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Answers1", "Answers2"))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur_answers <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))

at <- list(FeedbackEngageNum = c(-1:1),
           FormatFeedback2 = c("Expl1", "Expl2"))
grid <- ref_grid(DurSec2, type = "response", at = at)
(emm_dur_expl <- emmeans(grid, specs = consec ~ FormatFeedback2, by = "FeedbackEngageNum"))

## Opening png file
png(filename = "Fig06_DurSecEngage_hires.png", width = 9, height = 8, units = "cm", res = 500, pointsize = 7)

## Empty plot
plot(NULL, xlim=c(min(xaxis)-0.5, max(xaxis)+0.5), ylim = c(50, 150), xaxt = "n", las = 1, 
     xlab = "Learning intervention", ylab = "Completion time (sec)")

## Axes and labels
axis(1, at = xaxis[c(2, 5, 8)], labels = c("Reflection", "Answers", "Explanations"))
axis(1, at = xaxis[1:9], labels = F, tck = -0.02)
axis(3, at = xaxis[1:9], labels = rep(c("Lo", "Me", "Hi"), 3), tck = -0.02)
mtext("Relative engagement in learning intervention", side = 3, at = xaxis[5], line = 2.5)

## Plot positions
xaxis_artic <- xaxis[1:3]
xaxis_answers <- xaxis[4:6]
xaxis_expl <- xaxis[7:9]

## Extracting values from emmeans objects
emm_artic_val <- summary(emm_dur_artic$emmeans)
emm_answers_val <- summary(emm_dur_answers$emmeans)
emm_expl_val <- summary(emm_dur_expl$emmeans)

## Polygons behind grid
polygon(c(xaxis_artic-shift, rev(xaxis_artic-shift)), c(emm_artic_val$lower.CL[c(1, 3, 5)], rev(emm_artic_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Artic_2, "50"), border = NA)
polygon(c(xaxis_artic+shift, rev(xaxis_artic+shift)), c(emm_artic_val$lower.CL[c(2, 4, 6)], rev(emm_artic_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Artic_2, "50"), border = NA)

polygon(c(xaxis_answers-shift, rev(xaxis_answers-shift)), c(emm_answers_val$lower.CL[c(1, 3, 5)], rev(emm_answers_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Answers_2, "50"), border = NA)
polygon(c(xaxis_answers+shift, rev(xaxis_answers+shift)), c(emm_answers_val$lower.CL[c(2, 4, 6)], rev(emm_answers_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Answers_2, "50"), border = NA)

polygon(c(xaxis_expl-shift, rev(xaxis_expl-shift)), c(emm_expl_val$lower.CL[c(1, 3, 5)], rev(emm_expl_val$upper.CL[c(1, 3, 5)])), col = paste0(Col_Expl_2, "50"), border = NA)
polygon(c(xaxis_expl+shift, rev(xaxis_expl+shift)), c(emm_expl_val$lower.CL[c(2, 4, 6)], rev(emm_expl_val$upper.CL[c(2, 4, 6)])), col = paste0(Col_Expl_2, "50"), border = NA)

grid(nx = NA, ny = NULL, lty = 3, lwd = 0.5, col = "grey")
abline(v = xaxis, lty = 3, lwd = 0.5, col = "grey")
box()

## Articulate plotting
arrows(xaxis_artic-shift, emm_artic_val$lower.CL[c(1, 3, 5)], xaxis_artic-shift, emm_artic_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Artic_2, length = ci.length)
arrows(xaxis_artic+shift, emm_artic_val$lower.CL[c(2, 4, 6)], xaxis_artic+shift, emm_artic_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Artic_2, length = ci.length)
lines(xaxis_artic-shift, emm_artic_val$response[c(1, 3, 5)], col = Col_Artic_1)
lines(xaxis_artic+shift, emm_artic_val$response[c(2, 4, 6)], col = Col_Artic_1)
points(xaxis_artic-shift, emm_artic_val$response[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Artic_1)
points(xaxis_artic+shift, emm_artic_val$response[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Artic_1)

## Articulate labelling
text(xaxis_artic-shift, emm_artic_val$response[c(1, 3, 5)], labels = format(round(emm_artic_val$response[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_artic+shift, emm_artic_val$response[c(2, 4, 6)], labels = format(round(emm_artic_val$response[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = 1, offset = 1, cex = lbl.cex, font = 2) 

# ## Answers significant differences
# pval <- summary(emm_corr_artic$contrasts)$p.value
# if (pval[1] < 0.05) points(xaxis_artic[1], (emm_artic_val$response[1] + emm_artic_val$response[2])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[2] < 0.05) points(xaxis_artic[2], (emm_artic_val$response[3] + emm_artic_val$response[4])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[3] < 0.05) points(xaxis_artic[3], (emm_artic_val$response[5] + emm_artic_val$response[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Answers plotting
arrows(xaxis_answers-shift, emm_answers_val$lower.CL[c(1, 3, 5)], xaxis_answers-shift, emm_answers_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Answers_2, length = ci.length)
arrows(xaxis_answers+shift, emm_answers_val$lower.CL[c(2, 4, 6)], xaxis_answers+shift, emm_answers_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Answers_2, length = ci.length)
lines(xaxis_answers-shift, emm_answers_val$response[c(1, 3, 5)], col = Col_Answers_1)
lines(xaxis_answers+shift, emm_answers_val$response[c(2, 4, 6)], col = Col_Answers_1)
points(xaxis_answers-shift, emm_answers_val$response[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Answers_1)
points(xaxis_answers+shift, emm_answers_val$response[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Answers_1)

## Answers labelling
text(xaxis_answers-shift, emm_answers_val$response[c(1, 3, 5)], labels = format(round(emm_answers_val$response[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_answers+shift, emm_answers_val$response[c(2, 4, 6)], labels = format(round(emm_answers_val$response[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = 1, offset = 1, cex = lbl.cex, font = 2) 

# ## Answers significant differences
# pval <- summary(emm_corr_answers$contrasts)$p.value
# if (pval[1] < 0.05) points(xaxis_answers[1], (emm_answers_val$response[1] + emm_answers_val$response[2])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[2] < 0.05) points(xaxis_answers[2], (emm_answers_val$response[3] + emm_answers_val$response[4])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[3] < 0.05) points(xaxis_answers[3], (emm_answers_val$response[5] + emm_answers_val$response[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Expl plotting
arrows(xaxis_expl-shift, emm_expl_val$lower.CL[c(1, 3, 5)], xaxis_expl-shift, emm_expl_val$upper.CL[c(1, 3, 5)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Expl_2, length = ci.length)
arrows(xaxis_expl+shift, emm_expl_val$lower.CL[c(2, 4, 6)], xaxis_expl+shift, emm_expl_val$upper.CL[c(2, 4, 6)], angle = 90, code = 3, 
       lwd = ci.lwd, col = Col_Expl_2, length = ci.length)
lines(xaxis_expl-shift, emm_expl_val$response[c(1, 3, 5)], col = Col_Expl_1)
lines(xaxis_expl+shift, emm_expl_val$response[c(2, 4, 6)], col = Col_Expl_1)
points(xaxis_expl-shift, emm_expl_val$response[c(1, 3, 5)], pch = 25, cex = pts.cex, bg = Col_Expl_1)
points(xaxis_expl+shift, emm_expl_val$response[c(2, 4, 6)], pch = 24, cex = pts.cex, bg = Col_Expl_1)

## Expl labelling
text(xaxis_expl-shift, emm_expl_val$response[c(1, 3, 5)], labels = format(round(emm_expl_val$response[c(1, 3, 5)], 1), nsmall = 1), 
     adj = 0.5, pos = 3, offset = 1, cex = lbl.cex, font = 2) 
text(xaxis_expl+shift, emm_expl_val$response[c(2, 4, 6)], labels = format(round(emm_expl_val$response[c(2, 4, 6)], 1), nsmall = 1), 
     adj = 0.5, pos = 1, offset = 1, cex = lbl.cex, font = 2) 

# ## Expl significant differences
# pval <- summary(emm_corr_expl$contrasts)$p.value
# if (pval[1] < 0.05) points(xaxis_expl[1], (emm_expl_val$response[1] + emm_expl_val$response[2])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[2] < 0.05) points(xaxis_expl[2], (emm_expl_val$response[3] + emm_expl_val$response[4])/2, pch = 8, cex = pts.cex, lwd = 1)
# if (pval[3] < 0.05) points(xaxis_expl[3], (emm_expl_val$response[5] + emm_expl_val$response[6])/2, pch = 8, cex = pts.cex, lwd = 1)

## Legend
legend("bottomleft", c("Pre-interv.", "Post-interv."), pch = c(25, 24), pt.bg = "grey", pt.cex = 1, bty = "n", horiz = T,
       x.intersp = 1.5, inset = 0.02)

dev.off()



