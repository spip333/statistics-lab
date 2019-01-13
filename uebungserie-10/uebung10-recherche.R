###########################
# Recherche Uebung 10
###########################

eruption.lm <- lm(eruptions~waiting , data = faithful)

?lm

head(mtcars)

# model wt as function der hp
model <- lm (wt~hp, data=mtcars)
model$coefficients


waiting <- faithful$waiting

