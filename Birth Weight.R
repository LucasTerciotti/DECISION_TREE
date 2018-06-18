#birth weight
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

bw <- read.csv(("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/birthwt.csv"),
                    stringsAsFactors=FALSE,
                    na.strings = c(""))

cols <- c('low', 'race', 'smoke', 'ht', 'ui')
bw[cols] <- lapply(bw[cols], as.factor)
str(bw)

set.seed(123)

train.index <- sample((nrow(bw)),0.7*nrow(bw))

train <- bw[train.index,]
test  <- bw[-train.index,]

#treinando apenas com age para ter noção da evolução:
fit <- rpart(low ~ age,
             data=train,
             method="class")

summary(fit)

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

#agora incrementando a árvore:
fit <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train,
             minsplit = 2, cp = -1,
             method="class")

summary(fit)

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

fancyRpartPlot(fit)

#dessa forma alcançamos:
#         0         1
#0    0.456     0.175
#1    0.193     0.175
#ou seja, acertamos 63.1% das previsões.
#a importancia de ui e ht é baixa, podemos tentar alterar a estrutura da árvore para melhorar.

fit <- rpart(low ~ age + lwt + race + smoke + ptl + ftv,
             data=train,
             minsplit = 2,
             cp = -1,
             method="class")

summary(fit)

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

fancyRpartPlot(fit)

#não foi uma tentativa efetiva. Vamos alterar os valores de minsplit para descobrir como ele altera a previsão:
#usaremos como base a mais efetiva até agora:

fit <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train,
             minsplit = 10, cp = -1,
             method="class")

summary(fit)

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

fancyRpartPlot(fit)
#Ao fazer o plot, verificamos que ao aumentar minsplit, o numero min de observações em cada nó aumenta e
#a árvore diminui.

fit <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train,
             minsplit = 2, cp = -1,
             method="class")

summary(fit)
#o lwt é o mais importante nessa análise. Como seria uma árvore somente de lwt?

fit <- rpart(low ~ lwt,
             data=train,
             minsplit = 2, cp = -1,
             method="class")

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

fancyRpartPlot(fit)
#O valor previsto de recém nascidos sem ser de baixo peso aumenta... mas a acurácia geral do modelo abaixa.
#prediction          0          1
#         0 0.52631579 0.28070175
#         1 0.12280702 0.07017544

#O melhor resultado geral alcançado foi por este modelo:
fit <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train,
             minsplit = 2, cp = -1,
             method="class")

summary(fit)

prediction <- predict(fit, test, type = "class")
table(prediction,test$low)
prop.table(table(prediction,test$low))

fancyRpartPlot(fit)
