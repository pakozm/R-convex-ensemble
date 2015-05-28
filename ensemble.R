train <- read.csv("DATA/train.csv")
train <- train[sample(nrow(train)),]
target <- train$target
f1 <- read.csv(“model1.csv”)
f2 <- read.csv(“model2.csv”)
f <- rbind(f1,f2)
logloss <- function(outputs,target) {
  p <- max(min(outputs, 1 - 10e-15), 10e-15))
  -sum(target * log(p))/nrows(p)
}
softmax <- function(m) {
  exp(m)/sum(exp(m))
}
logloss_ensemble <- function(wp) {
  w <- softmax(wp)
  p <- T(w) %*% f
  logloss(p, target)
}
x0 <- matrix(1,cols(f), 0.0)
wp <- fminsearch(logloss_ensemble, x0)
w <- softmax(wp)
test_f1 <- read.csv(“model1_test.csv”)
test_f2 <- read.csv(“model2_test.csv”)
test_f <- rbind(test_f1,test_f2)
test_p <- T(w) %*% f
write.csv(test_p, file = "result.csv", row.names=FALSE, na="")

