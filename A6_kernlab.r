#http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/svmbasic/svmbasic_notes.pdf

ex63_tr <- read.csv("ex6data3_tr.csv", head = F, sep =";")
pos3 <- ex63_tr[ex63_tr[,3] == "1" ,c(1,2)]
neg3 <- ex63_tr[ex63_tr[,3] == "0" ,c(1,2)]
plot(pos3[,1], pos3[,2], pch = 19, col = "blue")
points(neg3[,1], neg3[,2], pch = 18, col = "red")


xtr <- cbind(ex63_tr[,1], ex63_tr[,2])
ytr <- matrix(ex63_tr[,3])

C <- 1
sigma <- 0.1
g <- 1/(2*sigma^2)
g
svp <- ksvm(xtr, ytr, type="C-svc", kernel="rbfdot",
            kpar=list(sigma=g), C=1)

plot(svp,data=xtr)

# Predict labels on test
ypred = predict(svp,xtr)
table(ytr,ypred)

# Compute accuracy
sum(ypred==ytr)/length(ytr)

# Compute at the prediction scores
ypredscore = predict(svp,xtr,type="decision")

# Check that the predicted labels are the signs of the scores
table(ypredscore > 0,ypred)

# Package to compute ROC curve, precision-recall etc...
install.packages("ROCR")
library(ROCR)
pred <- prediction(ypredscore,ytr)

# Plot ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

# Plot precision/recall curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
plot(perf)
# Plot accuracy as function of threshold
perf <- performance(pred, measure = "acc")
plot(perf)

##########################################
#predict model on different data

ex63_val <- read.csv("ex6data3_val.csv", head = F, sep =";")
xval <- cbind(ex63_val[,1], ex63_val[,2])
yval <- matrix(ex63_val[,3])


# Predict labels on test
ypredv = predict(svp,xval)
table(yval,ypredv)

# Compute accuracy
sum(ypredv==yval)/length(yval)

# Compute at the prediction scores
ypredscorev = predict(svp,xval,type="decision")

# Check that the predicted labels are the signs of the scores
table(ypredscorev > 0,ypredv)

# Package to compute ROC curve, precision-recall etc...
install.packages("ROCR")
library(ROCR)
predv <- prediction(ypredscorev,yval)

# Plot ROC curve
perfv <- performance(predv, measure = "tpr", x.measure = "fpr")
plot(perfv)

# Plot precision/recall curve
perfv <- performance(predv, measure = "prec", x.measure = "rec")
plot(perfv)
# Plot accuracy as function of threshold
perfv <- performance(predv, measure = "acc")
plot(perfv)

