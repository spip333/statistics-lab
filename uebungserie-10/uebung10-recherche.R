###########################
# Recherche Uebung 10
###########################

?lm

head(mtcars)

y <- mtcars$wt
x <- mtcars$hp
plot(x, y)


# model wt as function der hp
wt.hp.model <- lm (wt~hp, data=mtcars)
wt.hp.model$coefficients

coeffs <- coefficients(wt.hp.model)
coeffs
est.wt <- coeffs[1]+ 200 * coeffs[2] 
est.wt
# gewicht bei 200hp:
coeffs[1]+ 200 * coeffs[2] 

newdata <- data.frame(hp=200)

predict(wt.hp.model,newdata)

hps <- c(110)

newdata <- data.frame(hp=hps)
predict(wt.hp.model,newdata)

head(mtcars)


model$fitted.values
