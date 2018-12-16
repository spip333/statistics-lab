############################################################################
### Deskriptive Statistik - Uebungserie 8
###
### Nicolas Stern
### 11.12.2018
###
#############################################################################


#---------------------------------------------------------------------------------
# Aufgabe: Intervallschätzung von mu, sigma2 bekannt
# Aufgabe: Für die Standardabweichung der Pulsrate der Studierenden
# gelte sigma = 11:69. Bestimmen Sie den Fehlerbereich und die
# Intervallschätzung der durchschnittlichen Pulsrate bei einem
# Konfidenzniveau von 99%.

# Recherche -> cf exemple demo-confidence-intervall
#
library(MASS)

survey$Pulse
pulse <- na.omit(survey$Pulse)
n <- length(pulse)
sigma <- 11.69
sem <- sigma/sqrt(n)
sem
# Fehlerbereich
ME <- qnorm(0.995)*sem
x <- mean(pulse)
x
x+c(-ME, ME)

#---------------------------------------------------------------------------------
# Aufgabe: Intervallschätzung von mu, sigma2 unbekannt
# Aufgabe: Bestimmen Sie für die durchschnittliche Pulsrate den Fehlerbereich 
# und die Intervallschätzung der durchschnittlichen Pulsrate bei einem Konfidenzniveau von 90%.

# Recherche : cf example demo-confidence-intervall

pulse <- na.omit(survey$Pulse)
n <- length(pulse)
s <- sd(pulse)
SE <- s/sqrt(n)
E <- qt(.95, df=n-1)*SE
xbar <- mean(pulse)
xbar + c(-E,E)

#---------------------------------------------------------------------------------
# Aufgabe: Stichprobengrösse bei mu
# Aufgabe: Bestimmen Sie benötigte Stichprobengrösse für die durchschnittliche Pilsrate 
# bei einem Fehlerbereich von 1 Puls und einem Konfidenzniveau von 99%.

# Recherche : cf example demo-confidence-intervall

pulse <- na.omit(survey$Pulse)
zstar <- qnorm(.995)
s <- sd(pulse)
E <- 1
zstar^2*s^2/E^2

#---------------------------------------------------------------------------------
# Aufgabe: Intervallschätzung von Populationsanteils p
# Aufgabe: Bestimmen Sie den Fehlerbereich und die Intervallschätzung für den Anteil 
# der Nichtraucher aus survey bei einem Konfidenzniveau von 90%.
smoke <- na.omit(survey$Smoke)
n <- length(smoke)
k <- sum(smoke == "Never")
pbar <- k/n
pbar
SE <- sqrt(pbar*(1-pbar)/n)
SE
E <- qnorm(.95)*SE
E
pbar + c(-E,E)

#---------------------------------------------------------------------------------
# Aufgabe: Stichprobengrösse für p
# Aufgabe: Bestimmen Sie die Stichprobengrösse einer Umfrage zur Bestimmung des Anteils
# der Nichtraucher. Der Fehlerbereich soll 2% betragen. Sie vermuten aus früheren 
# Umfragen eine Anteil in der Grösse von p = 0:8. Das Konfidenzniveau ist 99%.
zstar <- qnorm(0.995)
p <- 0.8
E <- 0.02
zstar^2 * p * (1-p) / E^2


