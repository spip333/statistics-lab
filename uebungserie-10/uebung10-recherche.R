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

plot(wt.hp.model)


coeffs <- coefficients(wt.hp.model)
coeffs
est.wt <- coeffs[1]+ 200 * coeffs[2] 
est.wt

max(mtcars$hp)

?mtcars  
  
plot(x, y)
lines(x2, coeffs[1] + x2*coeffs[2])
  
?plot


# gewicht bei 200hp:
coeffs[1]+ 200 * coeffs[2] 

paste("a", "b", "c")

newdata <- data.frame(hp=200)

predict(wt.hp.model,newdata)

hps <- c(110)

newdata <- data.frame(hp=hps)
predict(wt.hp.model,newdata)

head(mtcars)


model$fitted.values

summary(wt.hp.model)$r.squared

# Bestimmtheitsmass
eruption.lm <- lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)

