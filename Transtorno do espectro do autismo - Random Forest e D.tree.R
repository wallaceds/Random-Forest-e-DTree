base = read.csv ("Autism-Adult-Data.arff", header =FALSE, comment.char = "@")

summary(base)

names(base)[names(base)=="V11"]<-"Age"
names(base)[names(base)=="V12"]<-"Gender"
names(base)[names(base)=="V13"]<-"Ethnicity"
names(base)[names(base)=="V14"]<-"Jundice"
names(base)[names(base)=="V15"]<-"Austim"
names(base)[names(base)=="V16"]<-"Country"
names(base)[names(base)=="V17"]<-"Used_app"
names(base)[names(base)=="V18"]<-"Result"
names(base)[names(base)=="V19"]<-"Age_desc"
names(base)[names(base)=="V20"]<-"Relation"
names(base)[names(base)=="V21"]<-"ASD"

base[base=="?"]<-NA

base$Ethnicity = NULL
base$Relation = NULL
base$Country = NULL
base$Age_desc = NULL

base$Gender = factor(base$Gender, levels = c('f', 'm'), labels = c(0,1))
base$Jundice = factor(base$Jundice, levels = c('no', 'yes'), labels = c(0,1))
base$Used_app = factor(base$Used_app, levels = c('no', 'yes'), labels = c(0,1))
base$Austim = factor(base$Austim, levels = c('no', 'yes'), labels = c(0,1))
base$ASD = factor(base$ASD, levels = c('NO', 'YES'), labels = c(0,1))

sapply(base, class)

base$Age <- as.numeric(as.character(base$Age))
base$Gender <- as.numeric(as.character(base$Gender))
base$Jundice <- as.numeric(as.character(base$Jundice))
base$Austim <- as.numeric(as.character(base$Austim))
base$Used_app <- as.numeric(as.character(base$Used_app))

mean(base$Age, na.rm = TRUE)
base[is.na(base$Age),]

base$Age = ifelse(is.na(base$Age), mean(base$Age, na.rm = TRUE), base$Age)

# Essa coluna foi deletada, pois tem correlação perfeita com a coluna de classificação, 
# resultando em 100% de acurácia.
base$Result = NULL

#------ d.trees

library(caTools)
set.seed(1)
divisao = sample.split(base$ASD, SplitRatio = 0.7)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(rpart)
classificador = rpart(formula = ASD ~ ., data = base_treinamento)
print(classificador)
library(rpart.plot)
rpart.plot(classificador)

previsoes = predict(classificador, newdata = base_teste[-16], type = 'class')
matriz_confusao = table(base_teste[, 16], previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)

#------- r.forest

library(caTools)
set.seed(1)
divisao = sample.split(base$ASD, SplitRatio = 0.70)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(randomForest)
set.seed(1)
classificador = randomForest(x = base_treinamento[-16], y = base_treinamento$ASD, ntree = 500)
previsoes = predict(classificador, newdata = base_teste[-16])
matriz_confusao = table(base_teste[, 16], previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)

# A acurácia com o D.trees foi de 86% e com o random forest (ntree=500) foi de 96%.