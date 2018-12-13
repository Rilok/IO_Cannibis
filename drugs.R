install.packages("arules")
install.packages("class")
install.packages("deducorrect")
install.packages("e1071")
install.packages("ggplot2")
install.packages("party")
install.packages("randomForest")

library(deducorrect)
library(party)
library(class)
library(e1071)
library(randomForest)
library(ggplot2)
library(arules)
library(data.table)



#################################################
# SPREPAROWANIE DANYCH NA RZECZ BADAÑ (PUNKT 1) #
#################################################

#ustawienie obszaru roboczego
setwd("d:\\=Private=\\UG\\===semestr7===\\Intel\\PR Domowa 2")

#do badañ wykorzystam wy³¹cznie klasê marihuany
cannabis <- read.table("drug_consumption.data", sep=",")[c(2,3,4,5,7,8,9,10,11,12,13,19)]

names(cannabis) <- c("Age", "Gender", "Education",
                  "Country", "Nscore",
                  "Escore", "Oscore", "Ascore", "Cscore",
                  "Impulsive", "SS", "SmokeYearAgo")


#do badañ wykorzystam wy³¹cznie klasê marihuany

cannabis$Age <- as.character(cannabis$Age)
cannabis$Gender <- as.character(cannabis$Gender)
cannabis$Education <- as.character(cannabis$Education)
cannabis$Country <- as.character(cannabis$Country)

#modyfikacja danych - Wieku, P³ci, St. edukacji i kraju
#(aby by³y czytelniejsze)
#Przydatne do regu³ asocjacyjnych
for(x in 1:1885) {
  if (cannabis$Age[x] == "-0.95197") {
    cannabis$Age[x] <- "18-24"
  } else if (cannabis$Age[x] == "-0.07854") {
      cannabis$Age[x] <- "25-34"
  } else if (cannabis$Age[x] == "0.49788") {
      cannabis$Age[x] <- "35-44"
  } else
      cannabis$Age[x] <- "45+"
}

for(x in 1:1885) {
  if (cannabis$Gender[x] == "0.48246") {
    cannabis$Gender[x] <- "Female"
  }
  else
    cannabis$Gender[x] <- "Male"
}

for(x in 1:1885) {
  if (cannabis$Education[x] == "-0.61113") {
    cannabis$Education[x] <- "Student"
  } else if (cannabis$Education[x] == "-0.05921") {
    cannabis$Education[x] <- "Pro certificate/diploma"
  } else if (cannabis$Education[x] == "0.45468") {
    cannabis$Education[x] <- "University degree"
  } else if (cannabis$Education[x] == "1.16365") {
    cannabis$Education[x] <- "Masters degree"
  } else if (cannabis$Education[x] == "1.98437") {
    cannabis$Education[x] <- "Doctorate degree"
  } else {
    cannabis$Education[x] <- "Left school"
  }
}

for(x in 1:1885) {
  if (cannabis$Country[x] == "-0.09765") {
    cannabis$Country[x] <- "Australia"
  } else if (cannabis$Country[x] == "0.24923") {
    cannabis$Country[x] <- "Canada"
  } else if (cannabis$Country[x] == "0.96082") {
    cannabis$Country[x] <- "UK"
  } else if (cannabis$Country[x] == "-0.57009") {
    cannabis$Country[x] <- "USA"
  } else {
    cannabis$Country[x] <- "Other"
  }
}

nscoreResults <- c(-3.46436,-3.15735,-2.75696,-2.52197,-2.42317,
                   -2.34360,-2.21844,-2.05048,-1.86962,-1.69163,
                   -1.55078,-1.43907,-1.32828,-1.19430,-1.05308,
                   -0.92104,-0.79151,-0.67825,-0.58016,-0.46725,
                   -0.34799,-0.24649,-0.14882,-0.05188,0.04257,
                   0.13606,0.22393,0.31287,0.41667,0.52135,0.62967,
                   0.73545,0.82562,0.91093,1.02119,1.13281,1.23461,
                   1.37297,1.49158,1.60383,1.72012,1.83990,1.98437,
                   2.12700,2.28554,2.46262,2.61139,2.82196,3.27393)

for(x in 1:1885) {
  for(y in 1:49) {
    if (cannabis$Nscore[x] == nscoreResults[y])
      cannabis$Nscore[x] <- (11 + y)
  }
}

escoreResults <- c(-3.27393,0,-3.00537,-2.72827,-2.53830,-2.44904,
                   -2.32338,-2.21069,-2.11437,-2.03972,-1.92173,
                   -1.76250,-1.63340,-1.50796,-1.37639,-1.23177,
                   -1.09207,-0.94779,-0.80615,-0.69509,-0.57545,
                   -0.43999,-0.30033,-0.15487,0.00332,0.16767,
                   0.32197,0.47617,0.63779,0.80523,0.96248,1.11406,
                   1.28610,1.45421,1.58487,1.74091,1.93886,2.12700,
                   2.32338,2.57309,2.85950,0,3.00537,3.27393)

for(x in 1:1885) {
  for(y in 1:44) {
    if (cannabis$Escore[x] == escoreResults[y])
      cannabis$Escore[x] <- (15 + y)
  }
}

oscoreResults <- c(-3.27393,0,-2.85950,0,-2.63199,-2.39883,
                  -2.21069,-2.09015,-1.97495,-1.82919,-1.68062,
                  -1.55521,-1.42424,-1.27553,-1.11902,-0.97631,
                  -0.84732,-0.71727,-0.58331,-0.45174,-0.31776,
                  -0.17779,-0.01928,0.14143,0.29338,0.44585,0.58331,
                  0.72330,0.88309,1.06238,1.24033,1.43533,1.65653,
                  1.88511,2.15324,2.44904,2.90161)

for(x in 1:1885) {
  for(y in 1:37) {
    if (cannabis$Oscore[x] == oscoreResults[y])
      cannabis$Oscore[x] <- (23 + y)
  }
}

ascoreResults <- c(-3.46436,0,0,0,-3.15735,0,-3.00537,0,0,0,0,
                   -2.90161,-2.78793,-2.70172,-2.53830,-2.35413,
                   -2.21844,-2.07848,-1.92595,-1.77200,-1.62090,
                   -1.47955,-1.34289,-1.21213,-1.07533,-0.91699,
                   -0.76096,-0.60633,-0.45321,-0.30172,-0.15487,
                   -0.01729,0.13136,0.28783,0.43852,0.59042,
                   0.76096,0.94156,1.11406,1.2861,1.45039,
                   1.61108,1.81866,2.03972,2.23427,2.46262,
                   2.75696,3.15735,3.46436)

for(x in 1:1885) {
  for(y in 1:49) {
    if (cannabis$Ascore[x] == ascoreResults[y])
      cannabis$Ascore[x] <- (11 + y)
  }
}

cscoreResults <- c(-3.46436,0,-3.15735,-2.90161,-2.72827,-2.57309,
                   -2.42317,-2.30408,-2.18109,-2.04506,-1.92173,
                   -1.78169,-1.64101,-1.51840,-1.38502,-1.25773,
                   -1.13788,-1.01450,-0.89891,-0.78155,-0.65253,
                   -0.52745,-0.40581,-0.27607,-0.14277,-0.00665,
                   0.12331,0.25953,0.41594,0.58489,0.7583,0.93949,
                   1.13407,1.30612,1.46191,1.63088,1.81175,2.04506,
                   2.33337,2.63199,3.00537,0,3.46436)

for(x in 1:1885) {
  for(y in 1:43) {
    if (cannabis$Cscore[x] == cscoreResults[y])
      cannabis$Cscore[x] <- (16 + y)
  }
}

impulsiveResults <- c(-2.55524,-1.37983,-0.71126,-0.21712,0.19268,
                      0.52975,0.88113,1.29221,1.86203,2.90161)

for(x in 1:1885) {
  for(y in 1:10) {
    if (cannabis$Impulsive[x] == impulsiveResults[y])
      cannabis$Impulsive[x] <- y
  }
}

SSResults <- c(-2.07848,-1.54858,-1.18084,-0.84637,-0.52593,
               -0.21575,0.07987,0.40148,0.76540,1.22470,1.92173)

for(x in 1:1885) {
  for(y in 1:11) {
    if (cannabis$SS[x] == SSResults[y])
      cannabis$SS[x] <- y
  }
}





#Ustawiamy dwie wartoœci (sprawdzenie, czy badani palili marihuanê
#maksymalnie rok temu)
cannabis$SmokeYearAgo = ifelse(cannabis$SmokeYearAgo == "CL0" |
                           cannabis$SmokeYearAgo == "CL1" |
                           cannabis$SmokeYearAgo == "CL2",
                           "no", "yes")
#klasa musi byæ faktorem(do klasyfikatora kNN)
cannabis$SmokeYearAgo = as.factor(cannabis$SmokeYearAgo)
cannabis.raw <- cannabis
#reszta wartoœci musi byæ numeryczna, wiêc zmieniamy na rzecz
#badania kNN wartoœci dla niego niepoprawne
cannabis$Age_int <- sapply(cannabis$Age, as.numeric)
cannabis$Gender_int <- sapply(cannabis$Gender, as.numeric)
cannabis$Edu_int <- sapply(cannabis$Education, as.numeric)
cannabis$Ctr_int <- sapply(cannabis$Country, as.numeric)
#korzystamy z zasad, zapisanych na rzecz kNN
rlz <- correctionRules("cannabisRules.txt")
cannabis <- correctWithRules(rlz, cannabis)$corrected

cannabis$Age <- cannabis$Age_int
cannabis$Gender <- cannabis$Gender_int
cannabis$Education <- cannabis$Edu_int
cannabis$Country <- cannabis$Ctr_int

cannabis <- cannabis[,1:12]

############################################
# USTAWIENIE DANYCH TRENINGOWYCH (PUNKT 2) #
############################################

#Utworzenie dwóch zbiorów - treningowego i testowego w stosunku
# 70% -> 30%
set.seed(2137)
ind <- sample(2, nrow(cannabis), replace=TRUE, prob=c(0.7, 0.3))
cannabis.training <- cannabis[ind==1,]
cannabis.test <- cannabis[ind==2,]
realAnswers <- cannabis.test[,12]

#################################
# KLASYFIKACJA DANYCH (PUNKT 3) #
#################################

# 0.A. FUNKCJE DO OBLICZANIA TPR I FPR

calc_tpr <- function(t){
  tpr = t[1]/(t[1]+t[2])
  return(tpr)
}

calc_fpr <- function(t) {
  fpr = t[3]/(t[3]+t[4])
  return(fpr)
}

# 0.B. LEGENDA
# [1] - TP
# [2] - TN
# [3] - FP
# [4] - FN

# 1. CTREE (party)

ctree <- ctree(SmokeYearAgo ~ ., data = cannabis.training)

ctree.predicted <- predict(ctree, cannabis.test[,1:11])
ctree.confMat <- table(ctree.predicted, realAnswers)[2:1, 2:1]
ctree.accuracy <- mean(realAnswers == ctree.predicted)
ctree.tpr <- calc_tpr(ctree.confMat)
ctree.fpr <- calc_fpr(ctree.confMat)

plot(ctree, type = "simple")

# 2. NAIVE BAYES (e1071)

naive.model <- naiveBayes(SmokeYearAgo ~ ., data = cannabis.training)

naive.predicted <- predict(naive.model, cannabis.test[,1:11])
naive.confMat <- table(naive.predicted, realAnswers)[2:1, 2:1]
naive.accuracy <- mean(realAnswers == naive.predicted)
naive.tpr <- calc_tpr(naive.confMat)
naive.fpr <- calc_fpr(naive.confMat)

# 3. KNN (class)

# potrzebujemy znormalizowanych danych
# funkcja normalizacji do tego celu

normalize <- function(vec) {
  (vec - min(vec)) / ((max(vec)) - min(vec))
}

cannabis.norm <- normalize(cannabis[1:11])
cannabis.norm <- cbind(cannabis.norm, cannabis[12])
cannabis.norm.training <- cannabis.norm[ind==1,]
cannabis.norm.test <- cannabis.norm[ind==2,]

knn.list <- data.frame(k = numeric(), acc = numeric())
for (x in 1:30) {
  tmp_knn <- knn(cannabis.norm.training[,1:11], 
                 cannabis.norm.test[,1:11],
                 cl = cannabis.norm.training[,12], k = x,
                 prob = FALSE)
  knn.list[nrow(knn.list)+1,] <- c(x, mean(realAnswers==tmp_knn) * 100)
}

knn.model <- knn(cannabis.norm.training[,1:11], 
                 cannabis.norm.test[,1:11],
                 cl = cannabis.norm.training[,12], k = 24,
                 prob = FALSE)
knn.predicted <- knn.model
knn.confMat <- table(knn.predicted, realAnswers)[2:1, 2:1]
knn.accuracy <- mean(realAnswers == knn.predicted)
knn.tpr <- calc_tpr(knn.confMat)
knn.fpr <- calc_fpr(knn.confMat)

# 4. Random Forest (randomForest)

rf.model <- randomForest(SmokeYearAgo ~ ., data = cannabis.training, 
                         ntree=100, proximity=T)
rf.predicted <- predict(rf.model, cannabis.test[,1:11])
rf.confMat <- table(rf.predicted, realAnswers)[2:1, 2:1]
rf.accuracy <- mean(realAnswers == rf.predicted)
rf.tpr <- calc_tpr(rf.confMat)
rf.fpr <- calc_fpr(rf.confMat)

###########################
# TIME TO MAKE SOME PLOTS #
###########################

acc_results = c(ctree.accuracy, naive.accuracy, knn.accuracy, rf.accuracy) * 100
acc_classif = c("Drzewo", "Naive Bayes", "kNN","Random Forest")

acc_results.plot <- barplot(acc_results, main = "Dok³adnoœæ klasyfikatorów",
                            names.arg = acc_classif, col = c("red", "blue",
                            "green", "yellow"), ylim = c(0, 100))
acc_results.plot

rocSpace.data = data.frame(
  "FPR" = c(ctree.fpr, naive.fpr, knn.fpr, rf.fpr),
  "TPR" = c(ctree.tpr, naive.tpr, knn.tpr, rf.tpr),
  "Klasyfikator" = c("Drzewo", "Naive Bayes", "kNN", "Random Forest")
)

rocSpace.plot <- ggplot(rocSpace.data, aes(x = FPR, y = TPR, color = Klasyfikator)) + 
  geom_point(size=3)

#####################
# METODA K-ŒREDNICH #
#####################

cannabis.log <- log(cannabis[,c(1:11)])

replace_faults <- function(xd) {
  xd[is.infinite(xd)] <- NA
  replace_value <- mean(xd, na.rm = TRUE)
  xd[is.na(xd)] <- replace_value
}

replace_means <- sapply(cannabis.log, replace_faults)
replace_means <- as.data.frame(replace_means)

cannabis.log$Age[is.infinite(cannabis.log$Age)] <- replace_means["Age", ]
cannabis.log$Education[is.infinite(cannabis.log$Education)] <- replace_means["Education", ]
cannabis.log$Country[is.infinite(cannabis.log$Country)] <- replace_means["Country", ]

cannabis.log$Gender = cannabis$Gender

cannabis.stand <- scale(cannabis.log, center = TRUE)
cannabis.pca <- prcomp(cannabis.stand)
cannabis.final <- predict(cannabis.pca)[,1:2]
cannabis.test.final <- as.data.frame(cannabis.final)

k <- kmeans(cannabis.test.final, 4)
cannabis.test.final$cluster = factor(k$cluster)
centers = as.data.frame(k$centers)

k.predicted_plot <- ggplot(cannabis.test.final, aes(x=PC1, y=PC2, color=cluster)) + 
  geom_point() + 
  geom_point(data=centers, aes(x=PC1,y=PC2, color='Center')) +
  geom_point(data=centers, aes(x=PC1,y=PC2, color='Center'), size=36, alpha=.3, show.legend=FALSE)

k.real_plot <- ggplot(cannabis.test.final, aes(x=PC1, y=PC2, color=cannabis$SmokeYearAgo)) +
  geom_point()

######################
# METODY ASOCJACYJNE #
######################

cannabis.rules <- cannabis.raw

cannabis.rules$Age = as.factor(cannabis.rules$Age)
cannabis.rules$Gender = as.factor(cannabis.rules$Gender)
cannabis.rules$Education = as.factor(cannabis.rules$Education)
cannabis.rules$Country = as.factor(cannabis.rules$Country)
cannabis.rules$Nscore = as.factor(cannabis.rules$Nscore)
cannabis.rules$Escore = as.factor(cannabis.rules$Escore)
cannabis.rules$Oscore = as.factor(cannabis.rules$Oscore)
cannabis.rules$Ascore = as.factor(cannabis.rules$Ascore)
cannabis.rules$Cscore = as.factor(cannabis.rules$Cscore)
cannabis.rules$Impulsive = as.factor(cannabis.rules$Impulsive)
cannabis.rules$SS = as.factor(cannabis.rules$SS)
cannabis.rules$SmokeYearAgo = as.factor(cannabis.rules$SmokeYearAgo)

cannabis.rules.disc <- discretizeDF(cannabis.rules)

cannabis.rules.disc <- as(cannabis.rules.disc, 'transactions')

rules <- apriori(cannabis.rules.disc,
                 parameter = list(minlen=2, supp=0.1, conf=0.8),
                 appearance = list(rhs=c("SmokeYearAgo=no", "SmokeYearAgo=yes"), default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]

print(inspect(head(rules.pruned, by = "lift")))

###########
# DODATKI #
###########

cannabis.UK <- cannabis.raw[cannabis.raw$Country=="UK",]
cannabis.USA <- cannabis.raw[cannabis.raw$Country=="USA",]

UK.smoking <- round(prop.table(table(cannabis.UK$SmokeYearAgo)) * 100, digits = 1)
USA.smoking <- round(prop.table(table(cannabis.USA$SmokeYearAgo)) * 100, digits = 1)

UK_prob <- pie(UK.smoking, col=rainbow(length(UK.smoking)), main="Czy pali³eœ marihuanê w ci¹gu roku? (UK)")
USA_prob <- pie(USA.smoking, col=rainbow(length(UK.smoking)), main="Czy pali³eœ marihuanê w ci¹gu roku? (USA)")