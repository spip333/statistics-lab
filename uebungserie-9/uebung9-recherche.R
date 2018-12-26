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


#h0: lebensdauer > 10000


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
# zweiseitige Hypothese
n <- 400
p <- 0.23
p0 <- 0.2

?z.test
