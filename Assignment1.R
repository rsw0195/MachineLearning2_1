rm(list=ls())

### Functions
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}

needed <- c('e1071', 'ISLR', 'boot', 'MASS', "splines")      
installIfAbsentAndLoad(needed)

############################
#####   Section 1
############################

set.seed(5082)
n <- dim(OJ)[1]
train_inds <- sample(1:n, 800)
test_inds <- (1:n)[-train_inds]
train_dat <- OJ[train_inds,]
test_dat <- OJ[test_inds,]

## SVM model, kernel = linear
svmfit <- svm(Purchase ~ ., data=train_dat, kernel="linear", cost=.01)
summary(svmfit)
# There are 446 support vectors, with 223 support vectors in one class and 223 support vectors
# in another class. This represents a equal split for all of the support vectors. There is a significantly
# fewer number of support vectors in this kernel compared to others.

## Train/Test Error with SVM model, kernel = linear
train_pred <- predict(svmfit, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) # 16.875%
test_pred <- predict(svmfit, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) # 15.185%

## Tune Model, kernel = linear
tune.out <- tune(svm, Purchase ~ ., data=OJ, kernel="linear", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestmod <- tune.out$best.model

## Test/Train Error Tune Model, kernel = linear
train_pred <- predict(bestmod, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) # 16.5%

test_pred <- predict(bestmod, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) # 14.815%

## SVM Model, kernel = radial
svmfit <- svm(Purchase ~ ., data=train_dat, kernel="radial", cost=0.01)
summary(svmfit)
# There are 613 support vectors, with 308 in one class and 305 in another class.
# There is moderately an equal split in the support vectors.

## Train/Test Error for SVM, kernel=radial
train_pred <- predict(svmfit, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) # 38.125%

test_pred <- predict(svmfit, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) # 41.481%

## Tune Model, kernel = radial
tune.out <- tune(svm, Purchase ~ ., data=OJ, kernel="radial", ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestmod <- tune.out$best.model

## Test/Train Error Tune Model, kernel = radial
train_pred <- predict(bestmod, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) # 16.0%

test_pred <- predict(bestmod, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) #13.33%

## SVM Model, kernel = polynomial
svmfit <- svm(Purchase ~ ., data=train_dat, kernel="polynomial", degree = 2, cost=0.01)
summary(svmfit)
# There are 614 support vectors, with 309 support vectors in one class and 305 support vectors in another class.
# This shows that there are moderately an equal number of support vectors in each class.

## Train/Test Error for SVM, kernel = polynomial
train_pred <- predict(svmfit, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) # 38.125%

test_pred <- predict(svmfit, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) # 41.481%

## Tune Model, kernel = polynomial
tune.out <- tune(svm, Purchase ~ ., data=OJ, kernel="polynomial", degree = 2, ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestmod <- tune.out$best.model

## Test/Train Error Tune Model, kernel = polynomial
train_pred <- predict(bestmod, train_dat)
train_error <- mean(train_pred != train_dat$Purchase) #14.875%

test_pred <- predict(bestmod, test_dat)
test_error <- mean(test_pred != test_dat$Purchase) #14.444%

### Final Comments
# The linear model initially gives the best model with significantly lower training and test error rates.
# However, once tuning occurs, the degree 2 polynomial model gives the best training and test error rates.
# Arguably either of these models would work, because the error rates are not significantly different;
# however, there is a significant difference in the rates before tuning occurs.


############################
#####   Section 2
############################
attach(Wage)
# Polynomial Regression of varying degrees with 10-fold cross validation
set.seed(5082)
cv.error <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(wage~poly(age, i), data=Wage)
  cv.error[i] <- cv.glm(Wage, glm.fit, K=10)$delta[1]
}

print(paste("Optimal Degree: ", which.min(cv.error), "gave CV error of:", min(cv.error)))

# Plot CV errors against degree of polynomial
plot(1:10, cv.error, xlab = "Degree of Polynomial", ylab = "CV Error", type = 'b', pch = 16, cex = 2, col = 'black')
points(which.min(cv.error), min(cv.error), pch = 16, cex = 2, col = 'red')

# compute polynomial fit with optimal degree
agelim <- range(age)
age.grid <- seq(from = agelim[1], to = agelim[2], length.out = 100)
optimal_fit <- lm(wage~ poly(age, which.min(cv.error)), data=Wage)
optimal_pred <- predict(optimal_fit, newdata = list(age = age.grid))
plot(age, wage, xlim=agelim, col = "black")
title("Polynomial Fit with Degree 9 chosen by CV")
lines(age.grid, optimal_pred, col = "red", lwd = 2)

# fit step function to predict wage using age (cut() function)
set.seed(5082)
cv.err <- rep(0, 12)
for(i in 2:13){
  Wage$age.cut <- cut(age, i) 
  fit <- glm(wage ~ age.cut, data= Wage)
  cv.err[i-1] <- cv.glm(Wage, fit, K=10)$delta[1]
}

plot(1:12, cv.err, xlab = 'Number of Steps', ylab = 'CV Error', type = 'b', cex = 2, pch = 16)
points(which.min(cv.err), min(cv.err), col = 'red', pch = 16, cex = 2)

print(paste("Optimal Number of Steps: ", which.min(cv.err), "with CV error of:", min(cv.err)))

# create model using optimal cuts and plot model's fitted values as function of Wage$age data
optimal_cut <- lm(wage~cut(age, which.min(cv.err)), data = Wage)
optimal_preds <- predict(optimal_cut, list(age = age.grid))
plot(age, wage, xlim = agelim, col = 'black')
title("Step Function Using Number of Cuts (7) chosen with CV")
lines(age.grid, optimal_preds, col = "red", lwd = 2)

detach(Wage)
############################
#####   Section 3
############################

attach(Boston)

disrange <- range(dis)
dissamples <- seq(from = disrange[1], to = disrange[2], length.out = 100) #use for plots

# plot polynomial fit from 1 to 10, report RSS for training error in table
set.seed(5082)

rss <- rep(0, 10)
color <- c("black", "red", "green", "blue", "yellow", "grey", "brown", "dimgrey", "hotpink", "purple")
plot(dis, nox)
title("Polynomial Fit with Varying Degrees")
legend("topright", legend = c("Degree 1", "Degree 2", "Degree 3", "Degree 4", "Degree 5", "Degree 6", "Degree 7", "Degree 8", "Degree 9", "Degree 10"), col = color, lty = 1, cex = 0.8)
for(i in 1:10){
  fit <- lm(nox~poly(dis, i), data = Boston)
  pred <- predict(fit, list(dis = dissamples))
  rss[i] <- rss[i] + (fit$fit[1] - nox[i])^2
  lines(dissamples, pred, col = color[i])
}

# RSS table for training data by degree of freedom
rsstable <- data.frame(rss)

# calculate CV errors for degreed polynomials
cv.error <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(nox~poly(dis, i), data=Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit, K=10)$delta[1]
}
print(paste("Optimal Degree: ", which.min(cv.error), "gave CV error of:", min(cv.error)))

# Plot original data set with fit from optimal degree
optimal_fit <- lm(nox~poly(dis, 3), data =Boston)
optimal_pred <- predict(optimal_fit, list(dis = dissamples))
plot(dis, nox, xlab = 'dis', yalb = 'nox')
title("Polynomial Fit of Degree 3 Chosen From CV Error")
lines(dissamples, optimal_pred, col = "red", lwd = 2)

# use bs() to fit spline to predict nox using dis - 4 degrees of freedom. report using summary()
bsfit <- lm(nox~ bs(dis, df = 4), data = Boston)
pred <- predict(bsfit, list(dis = dissamples))
summary(bsfit)
attr(bs(dis, df = 4), 'knots')
  # One knot was used at the half-way point in the data (50%) at a value of 3.20745
plot(dis, nox)
lines(dissamples, pred, col= 'red', lwd = 2)

# Regression spline with varying degrees of freedom (3-10)
rss <- rep(0, 8)
color <- c("black", "red", "green", "blue", "yellow", "grey", "brown", "dimgrey")
plot(dis, nox)
title("Regression Spline with Varying Degrees of Freedom")
legend("topright", legend = c("DF 3", "DF 4", "DF 5", "DF 6", "DF 7", "DF 8", "DF 9", "DF 10"), lty = 1, col= color)
for(i in 3:10){
  fit <- lm(nox~bs(dis, df = i), data = Boston)
  pred <- predict(fit, list(dis = dissamples))
  rss[i-2] <- rss[i-2] + (fit$fit[1] - nox[i])^2
  lines(dissamples, pred, col = color[i-2])
}

# RSS table for training data by degree of freedom
rsstable <- data.frame(rss)


# perform 10-fold CV to select best DOF (3-10) for regression spline on data.
# plot results including best DOF in chart title
set.seed(5082)
cv.error <- rep(0,8)
for(i in 3:10){
  glm.fit <- glm(nox~bs(dis, df = i), data=Boston)
  cv.error[i-2] <- cv.glm(Boston, glm.fit, K=10)$delta[1]
}
print(paste("Optimal Degree of Freedom: ", which.min(cv.error)+2, "gave CV error of:", cv.error[10-2]))

best_fit <- lm(nox~bs(dis, df = which.min(cv.error)), data =Boston)
best_pred <- predict(best_fit, list(dis = dissamples)) 
plot(dis, nox)
lines(dissamples, best_pred, col = 'red', lwd = 2)
title("Regression Spline with 10 DF")

# perform 10-fold CV to select best lambda for smoothing spline on data. plot, including best lambda
set.seed(5082)
fit <- smooth.spline(dis, nox, cv = TRUE)
fit$lambda
plot(dis, nox)
lines(fit, col = 'red', lwd = 2)
title("Smoothing Spline with Best Lambda (9.03e-05)")
