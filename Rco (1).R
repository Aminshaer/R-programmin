library("dplyr")
library("pdp")
library("randomForest")
library("readxl")
memory.limit(62000)

memory.size()
D17Man <- read_excel("E:/majidi/D17Man.xlsx", 
                     col_types = c("skip", "text", "skip", 
                                   "numeric","skip", "text",
                                   "text", "text", "text",
                                   "text", "text", "text", 
                                   "text", "text", "text",
                                   "text", "numeric", "numeric",
                                   "text", "skip", "text",
                                   "text", "text", "text",
                                   "text", "text", "skip",
                                   "skip", "skip", "skip",
                                   "skip", "skip", "skip",
                                   "skip", "skip"))
#View(D17Man)
#head(D17Man)
#str(D17Man)
#summary(D17Man)

mkset = sample(nrow(D17Man) , 0.7*nrow(D17Man), replace = FALSE)
data_train = D17Man[ mkset , ]
data_test = D17Man[ -mkset , ]
data_train$TRPTRANS <- as.character(data_train$TRPTRANS)
data_train$TRPTRANS <- as.factor(data_train$TRPTRANS)
class(data_train$TRPTRANS)
#library("beepr")
####################TREE 5000 TRY 5##########
start_time <- Sys.time()
model1 <- randomForest(TRPTRANS ~ .,
                       data = data_train,
                       ntree = 5000,
                       mtry = 5,
                       importance = TRUE)
end_time <- Sys.time()
end_time - start_time
#beep()

#####OOB
#model1$err.rate[,1]
summary(model1$err.rate)

#####Vriable IMportance
importance(model1)
varImpPlot(model1)

#####
pred <- predict(model1,data_train)
table(data_train$TRPTRANS,pred)

pred <- predict(model1,D09Man)
table(D09Man$TRPTRANS,pred)
#####
p1 <- partial(model1,pred.var = c("NBIKETRP"))
plotPartial(D17P,pdp.col = "red" , pdp.lwd = 40)
p2 <- partial(model1,pred.var = c("NWALKTRP"))
plotPartial(p1)
p3 <- partial(model1,pred.var = c("R_AGE"),type = "classifiction",
              which.class = "R_AGE")
plotPartial(p3)
partialPlot(model1,x.var = "R_AGE"
            ,pred.data = data_train , which.class = 7)


