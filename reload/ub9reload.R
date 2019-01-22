library(TeachingDemos)

setwd("C:/ieu/workspace/R/descriptive-statistik/uebungen/reload")

###############################################################
# Aufgabe: Linksseitiger Test bei $\mu$, $\sigma$ bekannt
# Aufgabe: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung 
# des Herstellers, dass die Glühbirnen eine Mindestlebensdauer von 10'000 Stunden besitzen,
# bei einem Signifikanzniveau von 1% verwerfen? Die Standardabweichung beträgt 120 Stunden.

# H0 :  mu  >= 10'000 Stunden
# Ha :  mu < 10'000 Stunden
lightbulbs <- scan(file = "lightbulbs.txt")

alpha <- 0.01
sigma <- 120
mu <- 10000

n <- length(lightbulbs)
xbar <- mean(lightbulbs)

# 1 : z berechnen
z <- (xbar - mu)/ (sigma /sqrt(n))
z.alpha <- qnorm(alpha)
z > z.alpha # false => h0 verwerfen

# 2. p value
p.val <- pnorm(z)
p.val > alpha # false => h0 verwerfen

# 3. z.test
res <- z.test(x = xbar, mu = mu, stdev = sigma, alternative = "less", n = n)
res$statistic
res$p.value > alpha

###############################################################
# Aufgabe: Rechtsseitiger Test bei $\mu$, $\sigma$ bekannt
# Aufgabe: Die Datei "cookies.txt" enthält eine neue Stichprobe des Keksherstellers. 
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung 
# des Herstellers, dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem 
# Signifikanzniveau von 10% verwerfen? Die Standardabweichung beträgt 0:25 g.
# h0 : mu <= 2
# ha : mu > 2
cookies<- scan(file = "cookies.txt")

mu <- 2
sigma <- 0.25
alpha <- 0.10

n <- length(cookies)
xbar <- mean(cookies)

# 1. z - Value und krit. wert prüfen:
z <- (xbar - mu) / (sigma / sqrt(n))
z.krit <- qnorm(1-alpha)
z < z.krit # true, deshalb h0 behalten

# 2. p- Value und alpha vergleichen
p.val <- pnorm(z, lower = F)
p.val > alpha # true, deshalb h0 behalten

# 3. z-test 
z.test(x = xbar, mu =mu, stdev =sigma, alternative = "greater", conf.level = alpha, n = n)

###############################################################
# Aufgabe: Zweiseitiger Test bei µ, σ bekannt
# Aufgabe: Die Datei „penguins.txt“ enthält eine neue Zufallsstichprobe einer Pinguinkolonie. 
# Laden Sie die Datei mit dem Befehl scan.
# Das durchschnittliche Gewicht von antarktischen Königspinguinen diesern Kolonie betrug im letzten 
# Jahr 15.4 kg. Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das 
# Durchschnittsgewicht der Pinguine nicht verändert hat, bei einem Signiﬁkanzniveau von 5% verwerfen?
# Die Standardabweichung beträgt 2.5 kg.
# h0 : mu = 15.4
# ha : mu != 15.4
rm(list = ls())
penguins <- scan(file = "penguins.txt")

mu <- 15.4
xbar <- mean(penguins)
n <- length(penguins)
sigma <- 2.5
alpha <- 0.05

# 1. z.value mit krit. wert vergleichen
z.val <- (xbar - mu) / (sigma / sqrt(n))

krit.unten <- qnorm(alpha/2)
krit.oben <- qnorm(1-(alpha/2))
z.val < krit.oben && z.val > krit.unten # True, h0 kann behalten werden

# 2. p.value mit conf.level vergleichen
p.val <- pnorm(z.val) * 2

p.val > alpha

# 3. z.test
z.test (x = xbar, n = n, alternative = "two.sided", mu = mu, stdev = sigma)

###############################################################
# Aufgabe: Linksseitiger Test bei μ, σ unbekannt
# Aufgabe: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers. 
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die 
# Behauptung des Herstellers, dass die Glühbirnen eine Mindestlebensdauer von 10000 Stunden
# besitzen, bei einem Signifikanzniveau von 1% verwerfen?
# h0 : mu >= 10000
# ha : mu < 10000

rm (list = ls())

lightbulbs <- scan(file = "lightbulbs.txt")

alpha <- 0.01
n <- length(lightbulbs)
mu <- 10000

xbar <- mean(lightbulbs)

# 1. t.value mit krit. value vergleichen
s <- sd(lightbulbs)
t.val <- (xbar - mu) / (s / sqrt(n))
krit.val <- qt(alpha, df = n -1)
t.val > krit.val # false, h0 kann verworfen werden

# 2. p-value mit Signifikanz niveau
p.val <- pt(t.val, df = n-1)
p.val > alpha # false, h0 kann verworfen werden

# 3. t.test
t.test(x = lightbulbs, alternative = "less", mu = mu, conf.level = alpha)

###############################################################
# Aufgabe: Rechtsseitiger Test bei μ, σ unbekannt
# Aufgabe: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers. 
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe 
# die Behauptung des Herstellers, dass die Kekse einen maximalen Anteil von 2 g enthalten, 
# bei einem Signifikanzniveau von 10% verwerfen
# H0 : mu <= 2
# Ha : mu > 2

rm (list = ls())

cookies <- scan(file = "cookies.txt")

n <- length (cookies)
alpha <- 0.10
mu <- 2

# 1 t.value berechnen und vergleichen mit krit. wert
xbar <- mean(cookies)
s <- sd(cookies)
df <- n - 1

t.val <- (xbar - mu)/ (s/sqrt(n))

krit.val <- qt(1-alpha, df)
t.val < krit.val # true, h0 kann behalten werden

# 2. p-valuetrue
p.val <- pt(t.val, df, lower=F)
p.val > alpha # false, h0 kann behalten werden

# 3. t.test
t.test(x =cookies, alternative = "greater", mu = mu, conf.level = alpha)

###############################################################
# Aufgabe: Zweiseitiger Test bei µ, σ unbekannt
# Aufgabe: Die Datei „penguins.txt“ enthält eine neue Zufallsstichprobe einer Pinguinkolonie.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das Durchschnittsgewicht 
# der Pinguine nicht verändert hat, bei einem Signiﬁkanzniveau von 5% verwerfen
# h0 : mu = 15.4
# ha : mu !=15.4

rm (list = ls())
penguins <- scan(file = "penguins.txt")

alpha <- 0.05
n <- length(penguins)
mu <- 15.4
xbar <- mean(penguins)
s <- sd(penguins)
df <- n-1

# 1. t.val
t.val <- (xbar - mu) / (s / sqrt(n))
krit.unten <- qt(alpha / 2, df)
krit.oben <- qt(1 - (alpha / 2), df)
t.val < krit.oben && t.val > krit.unten # true, h0 behalten

# 2. p.val mit signifikanzniveau vergleichen
p.val <- pt(t.val, df = df) * 2

p.val > alpha  # true, h0 behalten

# 3. t.test
t.test(penguins, alternative = "two.sided", mu = mu, conf.level = alpha)

###############################################################
# Aufgabe: Zweiseitiger Test des Populationsanteils p
# Aufgabe: Der Anteil der Rechtshänder unter den Studierenden von survey wird auf 90% geschätzt.
# Lässt sich diese Behauptung bei einem Signiﬁkanzniveau von 1% verwerfen
# h0 : mu = 0.9
# ha : mu != 0.9

# ref: 3.4
# 
rm (list = ls())

library (MASS)
head(survey)

alpha <- 0.01
n <- nrow(survey)
n.right <- nrow(survey[survey$W.Hnd == "Right",])
p0 <- 0.9
pbar <- n.right / n

# 1. z.value mit krit. werte vergleichen
z.val <- (pbar - p0) / sqrt(p0*(1-p0)/n)
krit.unten <- qnorm(alpha/2) 
krit.oben <- qnorm(1-alpha/2) 

z.val < krit.oben && z.val > krit.unten # true, h0 kann behalten werden

# 2. p-value mit signifikanz
p.val <- pnorm(z.val, lower=F)*2
p.val > alpha # true, h0 kann behalten werden

# 