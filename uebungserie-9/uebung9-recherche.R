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

z.test(x=xbar, mu=mu0, stdev=0.25, n=35, conf.level = 0.1, alternative = "greater") 
