
# LIBSVM   (http://www.csie.ntu.edu.tw/~cjlin/libsvm/)
install.packages("e1071")
library("e1071")
install.packages("kernlab")
library(kernlab)

#%% =============== Part 1: Loading and Visualizing Data ================
#  %  We start the exercise by first loading and visualizing the dataset. 
#%  The following code will load the dataset into your environment and plot
#%  the data.
#%


ex61 <- read.csv("ex6data1.csv", head = F, sep =";")
plot(ex61[,1], ex61[,2], pch = 19)
     

#%% ==================== Part 2: Training Linear SVM ====================
#  %  The following code will train a linear SVM on the dataset and plot the
#%  decision boundary learned.
#%

C <- 1
model <-svm(V3 ~., data = ex61, kernel = "linear", 
            cost = C, type = "C-classification")
model
summary(model)

plot(model, ex61)


modela <- ksvm(V3~.,data=ex61,type="C-svc", 
                kernel="vanilladot", C=1)
modela
plot(modela, data=ex61[,c(1,2)])

# Attributes that you can access
attributes(model5)
# For example, the support vectors
alpha(model5)
alphaindex(model5)
b(model5)
error(model5)


#%% =============== Part 3: Implementing Gaussian Kernel ===============
#  %  You will now implement the Gaussian kernel to use
#%  with the SVM. You should complete the code in gaussianKernel.m
#%

x1 <- matrix(c(1, 2, 1), ncol = 1, nrow = 3)
x2 = matrix(c(0, 4, -1), ncol = 1, nrow = 3)
sigma <- 2

sim = 1/exp((t(x1 - x2) %*% (x1 -x2))/(2 * sigma^2))
sim

C <- 1
model1 <-svm(V3 ~., data = ex61, kernel = "radial", 
            cost = C, type = "C-classification")
model1
summary(model1)

plot(model1, ex61)


model1a <- ksvm(V3~.,data=ex61,type="C-svc", 
                kernel="rbfdot", C=1)
model1a
plot(model1a, data=ex61[,c(1,2)])

#%% =============== Part 4: Visualizing Dataset 2 ================
#  %  The following code will load the next dataset into your environment and 
#%  plot the data. 
#%

par(mfrow=c(1,2))
ex62 <- read.csv("ex6data2.csv", head = F, sep =";")
pos <- ex62[ex62[,3] == "1" ,c(1,2)]
neg <- ex62[ex62[,3] == "0" ,c(1,2)]
plot(pos[,1], pos[,2], pch = 19, col = "blue")
points(neg[,1], neg[,2], pch = 18, col = "red")

#%% ========== Part 5: Training SVM with RBF Kernel (Dataset 2) ==========
#  %  After you have implemented the kernel, we can now use it to train the 
#%  SVM classifier.
#% 

C <- 1
sigma <- 0.1
g <- 1/(2*sigma^2)
g
model2 <-svm(V3 ~., data = ex62, kernel = "radial", 
             cost = C, type = "C-classification", gamm = g)
plot(model2, ex62)


model3 <- ksvm(V3~.,data=ex62,type="C-svc", kernel="rbfdot",
               kpar=list(sigma=g),C=1)
model3
plot(model3, data=ex62[,c(1,2)])


#%% =============== Part 6: Visualizing Dataset 3 ================
#  %  The following code will load the next dataset into your environment and 
#%  plot the data. 
#%

ex63_tr <- read.csv("ex6data3_tr.csv", head = F, sep =";")
pos3 <- ex63_tr[ex63_tr[,3] == "1" ,c(1,2)]
neg3 <- ex63_tr[ex63_tr[,3] == "0" ,c(1,2)]
plot(pos3[,1], pos3[,2], pch = 19, col = "blue")
points(neg3[,1], neg3[,2], pch = 18, col = "red")


#%% ========== Part 7: Training SVM with RBF Kernel (Dataset 3) ==========
#
#  %  This is a different dataset that you can use to experiment with. Try
#%  different values of C and sigma here.
#% 

C <- 1
sigma <- 0.1
g <- 1/(2*sigma^2)
g
model4 <-svm(V3 ~., data = ex63_tr, kernel = "radial", 
             cost = C, type = "C-classification", gamma = g)
plot(model4, data=ex63_tr, svSymbol = 1, dataSymbol = 2, 
     symbolPalette = rainbow(4),
     color.palette = terrain.colors)

predict4 <- predict(model4,ex63_val)


model5 <- ksvm(V3~.,data=ex63_tr,type="C-svc", kernel="rbfdot",
               kpar=list(sigma=g), C=1, prob.model = T)
model5

plot(model5, data=ex63_tr[,c(1,2)])


ex63_val <- read.csv("ex6data3_val.csv", head = F, sep =";")
pos3val <- ex63_val[ex63_val[,3] == "1" ,c(1,2)]
neg3val <- ex63_val[ex63_val[,3] == "0" ,c(1,2)]
plot(pos3val[,1], pos3val[,2], pch = 19, col = "blue")
points(neg3val[,1], neg3val[,2], pch = 18, col = "red")

predict5_val <-  predict(model5,ex63_val) # type="probabilities"
table(ex63_val[c(1:200),3], predict5_val)


plot(pos3val[,1], pos3val[,2], pch = 14, col = "green")
points(neg3val[,1], neg3val[,2], pch = 14, col = "yellow")
points(ex63_val[,1], ex63_val[,2], pch = 18, col=as.factor(predict5_val))


# Compute accuracy
sum(predict5_val==ex63_val[c(1:200),3])/length(ex63_val[c(1:200),3])

# Compute at the prediction scores
ypredscore = predict(model5, ex63_val,type="decision")


# Check that the predicted labels are the signs of the scores
table(ypredscore > 0, predict5_val)

# Package to compute ROC curve, precision-recall etc...
install.packages("ROCR")
library(ROCR)
pred <- prediction(ypredscore,ex63_tr[c(1:200),3])

# Plot ROC curve
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

# Plot precision/recall curve
perf <- performance(pred, measure = "prec", x.measure = "rec")
plot(perf)
# Plot accuracy as function of threshold
perf <- performance(pred, measure = "acc")
plot(perf)

############################################################
# Find best parameters of C and gamma 

tobj <- tune.svm(V3 ~., data = ex63_tr, 
                 gamma = c(1,10,20,30,40,50,60), 
                 cost = c(1,10,20,30,40,50,60))
summary(tobj)


increments = c(0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 40, 50, 60, 70)
incC <- increments
incg <- increments

best <- rep(0, 10^2)
trialNum <- 0
choices <- matrix(0, nrow <- 8^2, ncol <- 3)

for(i in 1:8){
  for(j in 1:8){
    trialNum <- trialNum + 1
    modelT <- ksvm(V3~.,data=ex63_tr,type="C-svc", kernel="rbfdot",
                   kpar=list(sigma=incg[j]), C=incC[i])
    predictT <- predict(modelT,ex63_val)
    # Compute accuracy
    accuracy <- sum(predictT==ex63_val[c(1:200),3])/length(ex63_val[c(1:200),3])
    choices[trialNum,1] <- incC[i]
    choices[trialNum,2] <- incg[j]
    choices[trialNum,3] <- accuracy
  }
}
best <- apply(choices,2, max)
best

model6 <- ksvm(V3~.,data=ex63_tr,type="C-svc", kernel="rbfdot",
               kpar=list(sigma=best[2]), C=best[1], prob.model = T)
model6
plot(model6, data=ex63_tr[,c(1,2)])

