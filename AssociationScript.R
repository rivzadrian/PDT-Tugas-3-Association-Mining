#Ekplorasi dan pra - proses
bank <- read.csv("Bank Marketing.csv")
View(bank)

library(dplyr)
bank <- bank %>% 
  rename(
    age = ï..age
  )

bank <- data.frame(bank)
str(bank)
summary(bank)

##Merubah Age menjadi 3 kategori young, mid, Senior####
bank$age <- cut(bank$age, breaks=c(0, 18, 41, Inf), labels=c("young", "mid", "senior"))
summary(bank$age)


bank$balance <- cut(bank$balance, breaks=c(-10000, 448, 1428, Inf), labels=c("lowIncome", "averagerIncome", "highIncome"))
bank$day <- cut(bank$day, breaks=c(0, 7, 15, 22, Inf), labels=c("firstWeek", "secondWeek", "thirdWeek", "fourthWeek"))
bank$duration <- cut(bank$duration, breaks=c(0, 180, 319, Inf), labels=c("short", "medium", "long"))

bank$campaign <- as.numeric(bank$campaign)

bank$campaign <- cut(bank$campaign, breaks=c(0, 1, 3, Inf), labels=c("little", "average", "many"))

bank$pdays <- ifelse(bank$pdays == -1,"Not contacted","Contacted")
bank$pdays <- as.factor(bank$pdays)

bank$previous <- ifelse(bank$previous ==0 ,"Not contacted","Contacted")
bank$previous <- as.factor(bank$previous)

summary(bank$previous)



##bank$balance <- discretize(bank$balance, methods = "interval")

###Generating rules####

###Apriori####
library(arules)
rules_bank <- apriori(bank)
rules_bank
inspect(rules_bank)

start.time<- Sys.time()
rules_bank_2 <- apriori(bank, control=list(verbose=F),
                           parameter=list(minlen=4, supp=0.015, conf=0.5, target = "rules", arem = "diff"),
                           appearance=list(rhs=c("y=no","y=yes"),
                                           default="lhs"))
end.time<-Sys.time()

apriori.exec.time <- end.time - start.time
apriori.exec.time

library("arulesViz")
rules_bank_2
inspect(rules_bank_2[1:10])
ruledf = data.frame(
  lhs = labels(lhs(rules_bank_2)),
  rhs = labels(rhs(rules_bank_2)), 
  rules_bank_2@quality)
head(ruledf)
summary(ruledf)

###Apply gini ke rules####
quality(rules_bank_2) <- cbind(quality(rules_bank_2), 
                               gini = interestMeasure(rules_bank_2, measure = "gini", 
                                                      transactions = bank))

rules_bank_2.sorted <- sort(rules_bank_2, by="support")
rules_bank_2.sorted

 

inspect(rules_bank_2.sorted[1:10])
plot(rules_bank_2.sorted[1:10])
plot(rules_bank_2.sorted[1:10], method="grouped",
     control = list(col=10))

## Visualisasi
plot(rules_bank_2.sorted, method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))

###Apriori supp=0.005 conf=0.5####
library(arules)
rules_bank <- apriori(bank)
rules_bank
inspect(rules_bank)

start.time<- Sys.time()
rules_bank_2 <- apriori(bank, control=list(verbose=F),
                        parameter=list(minlen=4, supp=0.005, conf=0.5, target = "rules", arem = "diff"),
                        appearance=list(rhs=c("y=no","y=yes"),
                                        default="lhs"))
end.time<-Sys.time()

apriori.exec.time <- end.time - start.time
apriori.exec.time

library("arulesViz")
rules_bank_2
inspect(rules_bank_2[1:10])
ruledf = data.frame(
  lhs = labels(lhs(rules_bank_2)),
  rhs = labels(rhs(rules_bank_2)), 
  rules_bank_2@quality)
head(ruledf)
summary(ruledf)

rules_bank_2.sorted <- sort(rules_bank_2, by="support")
rules_bank_2.sorted



inspect(rules_bank_2.sorted[1:15])
plot(rules_bank_2.sorted[1:15])
plot(rules_bank_2.sorted[1:15], method="grouped",
     control = list(col=10))

## Visualisasi
plot(rules_bank_2.sorted, method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))

####FP Growth####
library(rJava)
library(rCBA)

rules_fp <- rCBA::fpgrowth(bank, support=0.015,
                           confidence=0.5, maxLength=4,
                           consequent="y", parallel=FALSE)
rules_fp
inspect(rules_fp[8000:8009])
rules_fp_2.sorted <- sort(rules_fp, by="support")
inspect(rules_fp_2.sorted[8000:8009])

new_data <- subset(bank, y == "yes")
rules_fp_2 <- rCBA::fpgrowth(new_data, support=0.015,
                           confidence=0.5, maxLength=4,
                           consequent="y", parallel=FALSE)
rules_fp_2
rules_fp_2.sorted <- sort(rules_fp_2, by="support")
inspect(rules_fp_2.sorted[1:10])

####FP Growth####
library(rJava)
library(rCBA)

rules_fp <- rCBA::fpgrowth(bank, support=0.005,
                           confidence=0.5, maxLength=4,
                           consequent="y", parallel=FALSE)
rules_fp
inspect(rules_fp[13554:13563])
rules_fp_2.sorted <- sort(rules_fp, by="support")
inspect(rules_fp_2.sorted[13554:13563])

new_data <- subset(bank, y == "yes")
rules_fp_2 <- rCBA::fpgrowth(new_data, support=0.005,
                             confidence=0.5, maxLength=4,
                             consequent="y", parallel=FALSE)
rules_fp_2
rules_fp_2.sorted <- sort(rules_fp_2, by="support")
inspect(rules_fp_2.sorted[1:10])
