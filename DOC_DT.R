library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#Exercicio: Fraude no seguro, sim ou não?

train <- data.frame(ClaimID  = c(1,2,3),
                    RearEnd  = c(TRUE, FALSE, TRUE),
                    Fraud    = c(TRUE, FALSE, TRUE))

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class")
mytree

#Essa arvore tem apenas 1 nó: É o raiz, com 3 obs, definido como Verdadeiro, Com perda de 1 valor, logo a prob de acerto 0.66 e 0.33 de erro
#criterios de controle da arvore:
#minsplit: valor minimo de observações para abrir um novo nó (default > 3)
#minbucket: Se chegar no ultimo nó, quantas observações minimas ainda vão estar disponiveis para que eu abra mais um nó.

mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", 
                minsplit = 2, minbucket = 1)
prp(mytree)

#Outra forma de plotar a Árvore, utilizando cores adequadas:
  fancyRpartPlot(mytree)
  #a interpretação: o número grande em porcentagem dentro do squircle é a quantia dos dados selecionados.
  
#Nova interpretação:
  
train2 <- data.frame(ClaimID  = c(1,2,3),
                      RearEnd  = c(TRUE, FALSE, TRUE),
                      Fraud    = c(TRUE, FALSE, FALSE))

mytree <- rpart(Fraud ~ RearEnd, data = train2, method = "class", 
                minsplit = 2, minbucket = 1)
mytree
#Agora de novo só o nó raiz, mas pq? Existe mais um parâmetro importante: cp - quanto a arvore ta melhorando?

mytree <- rpart(Fraud ~ RearEnd, data = train2, method = "class", 
                minsplit = 2, minbucket = 1, cp = -1)

fancyRpartPlot(mytree)

#ADIÇÂO DE PESOS PARA AS MEDIDAS
mytree <- rpart(Fraud ~ RearEnd, data = train2, method = "class", 
                minsplit = 2, minbucket = 1,
                weights = c(.4, .2, .2))

#EXEMPLO: TITANIC
titanic <- read.csv("titanic.csv",
  stringsAsFactors=FALSE)

table(data$survived)
summary(data$sex)
titanic$sex <- as.factor(titanic$sex)
summary(titanic$sex)

set.seed(123)

train.index <- sample((nrow(titanic)),0.7*nrow(titanic))

train <-  titanic[train.index,]
test <-  titanic[-train.index,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp +
               Parch + Fare + Embarked,
             data=train,
             method="class")
summary(fit)

prediction <-  predict(fit, test, type = "class")
table (prediction,test$survived)
prop.table(table(prediction,test$survived))

titanic <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/titanic.csv"),
                    stringsAsFactors=FALSE)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp +
               Parch + Fare + Embarked,
             data=train, method="class",
             minsplit = 2, cp = 0 
)

prediction <- predict(fit, test, type = "class")
table(prediction,test$Survived)
prop.table(table(prediction,test$Survived))
