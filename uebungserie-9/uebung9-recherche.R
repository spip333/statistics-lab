############################################################################
### Deskriptive Statistik - Uebungserie 9
###
### Nicolas Stern
### 16.12.2018
###
#############################################################################


#---------------------------------------------------------------------------------

# Aufgabe: Linksseitiger Test bei $\mu$, $\sigma$ bekannt

# Aufgabe: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers. 
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers, 
# dass die Glühbirnen eine Mindestlebensdauer von 10000 Stunden besitzen, bei einem Signifikanzniveau 
# von 1% verwerfen? Die Standardabweichung beträgt 120 Stunden.
?scan
lightbulbs <- scan("./uebungserie-9/lightbulbs.txt")
lightbulbs <- scan(file = "./uebungserie-9/lightbulbs.txt")

stddev <- 120
mu.probe <- mean(lightbulbs)
mu.h0 <- 10000 
signifikanzniveau <- 0.01
n <- 30
mean.std.error <- stddev / sqrt(n)
z.value <- (mu.probe / mu.h0)/ mean.std.error
p.value <- pnorm(z.value)
qnorm(signifikanzniveau)


#h0: lebensdauer is mindestens 10000
#ha: lebensdauer <  10000


xbar <- 9900 # stichproben mittlewert
mu0 <- 10000 # Wert im NullHypothese
sigma <- 120 # Standardabweichung der Population
n <- 30 # Stichprobengroesse
sem <- sigma/sqrt(n) # standardfehler des mittlewerts : ca. 21.9
z <- (xbar-mu0) / sem # testgroess z-wert: ca. -4.56
# d.h. : resultat ist 4.56 zu tief.

# 5 % alle auf einer Seite: -> graph
qnorm(0.05)

# pwert:
pval <- pnorm(z)
pval


alpha <- 0.01 # Stichprobenmittelwert
z.alpha <- qnorm(alpha) # kritischer Wert
z.alpha

#---------------------------------------------------------------------------------
# Daten einscannen, Eingabe der Parameter aus der Eingabe : Standardabweichung, Populationsmittlewert in Nullhypothese, und Stichprobengrösse:
lightbulbs <- scan("./uebungserie-9/lightbulbs.txt")
stddev <- 120 # Standardabweichung der Population
mu <- 10000  # Populationsmittelwert in H0
n <- 30 # Stichprobengrösse
alpha <- 0.01 # Signifikanzniveau

# Stichprobenmittlewert : 
xbar <- mean(lightbulbs)

# Z-Wert : 
z.value <- (xbar - mu)/ (stddev /sqrt(n))

# Kritischer Wert:
z.value.alpha <- qnorm(alpha)

z.test(x=9912, mu=10000, stdev=120, n=30, conf.level = 0.99, alternative = "less") 


z.test(x=9912, mu=10000, stdev=120, n=30, conf.level = 0.99, alternative = "two.sided") 


#---------------------------------------------------------------------------------
# 
# Aufgabe: Rechtsseitiger Test bei $\mu$, $\sigma$ bekannt
# - Aufgabe: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe 
# die Behauptung des Herstellers, dass die Kekse einen maximalen Anteil von 2 g enthalten,
# bei einem Signifikanzniveau von 10% verwerfen? Die Standardabweichung beträgt 0.25 g.
# H0 : mu <= 2g
# Ha : mu > 2g

cookies <- scan("./uebungserie-9/cookies.txt")
head(cookies)
mean(cookies)
xbar <-  mean(cookies) # stichproben mittlewert
# xbar <- 2.1
mu0 <- 2.0 # Wert im NullHypothese
sigma <- 0.25 # Standardabweichung der Population
n <- 35 # Stichprobengroesse
alpha <- 0.1 # Signifikanzniveau
sem <- sigma/sqrt(n) # standardfehler des mittlewerts : ca. 0.04
sem
z <- (xbar-mu0) / sem # testgroess z-wert:  0.41
z
z.critical <- qnorm(1 - 0.1) # ca 1.3
z > z.critical # TRUE => H0 wird verworfen

# berechnung ueber p-value:
pval <- pnorm(z, lower.tail=FALSE)
pval

pval < alpha

z.test(x=xbar, mu=mu0, stdev=0.25, n=35, conf.level = 0.9, alternative = "greater") 


#============================================
# Aufgabe: Zweiseitiger Test bei mu, sigma  bekannt
# Aufgabe: Die Datei "penguins.txt" enthält eine neue Zufallsstichprobe einer Pinguinkolonie. 
# Laden Sie die Datei mit dem Befehl scan. Das durchschnittliche Gewicht von antarktischen 
# Königspinguinen diesern Kolonie betrug im letzten Jahr 15.4 kg. Lässt sich aufgrund dieser 
# Stichprobe die Behauptung, dass sich das Durchschnittsgewicht der Pinguine nicht verändert hat,
# bei einem Signifikanzniveau von 5% verwerfen? Die Standardabweichung beträgt 2.5 kg
penguins <- scan("./uebungserie-9/penguins.txt")
head(penguins)
# h0: mu = 15.4 ha: mu <> 15.4
# xbar <- mean(penguins)
xbar <-14.6
mu0 <- 15.4
sigma <- 2.5
n <- 35
z <-  (xbar-mu0)/(sigma/sqrt(n))
alpha <- 0.05
z.alpha <- qnorm(1-alpha/2)
c(-z.alpha, z.alpha)
x.unten <- mu0 - z.alpha * sigma / sqrt(35)
x.oben <- mu0 + z.alpha * sigma / sqrt(35)



#============================================
# Aufgabe: Linksseitiger Test des Populationsanteils p
# Aufgabe: Die Datei „grocerystore.csv“ enthält eine Zufallsstichprobe
# von Kunden einer Metzgerei. Neben dem Geschlecht der Kunden
# wurde auch deren Verweilzeit im Laden notiert. Importieren Sie die
# Datei mit dem Befehl read.csv.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass die
# Metzgerei mehrheitlich von Frauen besucht wird, bei einem
# Signifikanzniveau von 5% verwerfen?
grocery <- read.csv(file="./uebungserie-9/grocerystore.csv", header = T, sep=";" )
head(grocery)
dim(grocery)
str(grocery)
grocery
frau.filter <- grocery$gender=="F"
mann.filter <- grocery$gender=="M"
frauen <- grocery[frau.filter,]
maenner <- grocery[mann.filter,]

n.frauen <- nrow(grocery[grocery$gender == "F", ])
n.men <- nrow(grocery[grocery$gender == "M", ])


pbar <- nrow(frauen) / nrow(grocery)
p0 <- 0.5
alpha <- 0.05
n <- nrow(grocery)
z <- (pbar-p0) / sqrt(p0*(1-p0)/n)

z.alpha <- qnorm(1-alpha/2)
c(-z.alpha, z.alpha)
pval <- 2*pnorm(z, lower.tail=FALSE)
pval

test <- prop.test(n.frauen, n, alternative = "less", correct = FALSE)

test$p.value < alpha

# ----------------------------------
xbar <- 104
mu <- 100
n <- 35
s <- 12
alpha <- .05
t <- (xbar - mu) / (s / sqrt(n))
t
krit.value <- qt(1-alpha/2, df = 35)
krit.value

pnorm(1.972,lower.tail = F)
pt(1.972, df=34, lower.tail = F)



# dehydration example:
# h0: mu = 0.95
# ha: mu != 0.95
mu <- 0.95
n <- 25
xbar <- 1
s <- 0.18
t <- (xbar - mu) / (s / sqrt(n))
t
p.val <- pt(t, df=24, lower.tail = F) * 2
p.val
alpha <- 0.05
p.val > alpha # when true: we have no evidence against h0

# compare t-value against critical value
crit.val <- qt(p=1-alpha, df=24)
crit.val
t > crit.val

# ==========================================================
# Aufgabe: Linksseitiger Test bei $\mu$, $\sigma$ unbekannt
# Aufgabe: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die 
# Behauptung des Herstellers, dass die Glühbirnen eine Mindestlebensdauer von 10000 Stunden 
# besitzen, bei einem Signifikanzniveau von 1% verwerfen?
# H0 : mu >= 10000
# Ha : mu < 10000

lightbulbs <- scan(file = "./uebungserie-9/lightbulbs.txt")
head(lightbulbs)
str(lightbulbs)
length(lightbulbs)

alpha <- 0.01
mu <- 10000
n <- length(lightbulbs)
xbar <- mean(lightbulbs)
s <- sd(lightbulbs)
t <- (xbar - mu) / (s / sqrt(n))
t

# compare t-value against critical value
t.alpha <- qt(p=alpha, df=n-1)
t.alpha
crit.val <- -t.alpha
t > crit.val # when true: no evidence against h0, when t < crit.val: h0 rejected

# alternative: 
p.val <- pt(t, df=n-1)
p.val
p.val > alpha # when true: we have no evidence against h0

# ==========================================================
# Aufgabe: Rechtsseitiger Test bei mu, sigma unbekannt
# Aufgabe: Die Datei „cookies.txt“ enthält eine neue Stichprobe des
# Keksherstellers. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem
# Signifikanzniveau von 10% verwerfen

cookies <- scan(file = "./uebungserie-9/cookies.txt")
head(cookies)
str(cookies)
length(cookies)

alpha <- 0.1
mu <- 2
n <- length(cookies)
xbar <- mean(cookies)
s <- sd(cookies)
t <- (xbar - mu) / (s / sqrt(n))
t

# compare t-value against critical value
t.alpha <- qt(p=1-alpha, df=n-1)
t.alpha
t > crit.val # when true: h0 rejected. when t < crit.val: no evidence against h0

# alternative: 
p.val <- pt(t, df=n-1, lower.tail = F)
p.val
p.val > alpha # when true: we have no evidence against h0


# ==========================================================
# Aufgabe: Zweiseitiger Test bei mu, sigma unbekannt
# Aufgabe: Die Datei „penguins.txt“ enthält eine neue Zufallsstichprobe
# einer Pinguinkolonie. Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das
# Durchschnittsgewicht der Pinguine nicht verändert hat, bei einem
# Signifikanzniveau von 5% verwerfen?
# H0 : mu = 15.4
# Ha : mu != 15.4
penguins <- scan(file = "./uebungserie-9/penguins.txt")
head(penguins)
str(penguins)
length(penguins)

alpha <- 0.05
mu <- 15.4
n <- length(penguins)
xbar <- mean(penguins)
s <- sd(penguins)
t <- (xbar - mu) / (s / sqrt(n))
t

# compare t-value against critical value
t.alpha.low <- qt(p=alpha/2, df=n-1)
t.alpha.high <- qt(p=1-alpha/2, df=n-1)

# when t between tlow and thigh : no evidence against h0
t > t.alpha.low  && t < t.alpha.high

# alternative: 
p.val <- pt(t, df=n-1) * 2
p.val
p.val > alpha # when true: we have no evidence against h0




