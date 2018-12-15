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


