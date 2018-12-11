############################################################################
### Deskriptive Statistik - Uebungserie 7: Stetige Verteilungen
###
### Nicolas Stern
### 17.11.2018
###
#############################################################################
# Uebungserie 7 - Trace

# Stetige Gleichverteilung
# Aufgabe: Sie haben um 9 Uhr ein wichtiges Meeting, aber Sie verschlafen und wachen 
# erst um 8:30 Uhr auf. Um 8:40 rennen Sie aus der Tür, auf dem Weg ins Büro. 
# Sie brauchen 6 Minuten zur Bushaltestelle. Sie warten auf den Bus, der morgens 
# alle fünf Minuten kommt, die Wartezeit in Minuten ist gleichverteilt zwischen 0 und 5. 
# Der Bus braucht zwischen 10 bis 15 Minuten (gleichverteilt) bis ins Büro. 
# Modellieren Sie Ihre Reisezeit mit einer einzigen Variablen X. 
xv <- 0:40
plot(xv, dunif(xv, 6, 11), type="l")

punif(10, 0, 5)


dunif(10, 0,30)
punif(10, 0, 30)
punif(25, 0, 30, lower.tail = F)
punif(20, 0, 30, lower.tail = T) -  punif(10, 0, 30, lower.tail = T) 
  
?punif

#- Welche Verteilung hat die Zufallsvariable X, welche die gesamte Pendelzeit 
# von Haustür bis ins Büro beschreibt?

#############3
#Suppose in a quiz there are 30 participants. A question is given to all 30 
# participants and the time allowed to answer it is 25 seconds. Find the probability 
# of participants responds within 6 seconds?
xv <- 0:30
plot(xv,dunif(xv, 0,25) , type="l")
punif(6, 0, 25) *30 
punif(4, 0, 10) 

# Suppose a flight is about to land and the announcement says that the expected time 
# to land is 30 minutes. Find the probability of getting flight land between 25 to 30 minutes?
xv <- 0:30
plot(xv,dunif(xv, 25,30) , type="l")

x <- 1;
a <- 0;
b <- 4;

plot(punif(x, a, b))

qunif(0.5, a, b)

?punif

xv <- seq(0,5,length=100)
xv <- 0:5
plot(xv, dunif(xv,1,3), type = "l", ylab = "Uni(x)", xlab = "x")
dunif(4, 3, 8)
punif(5, 3, 8)

dunif(0:5, 1, 3)
punif(3, 1, 3)

tot.time <- 0:100
wait.bus <- 1:5
0:15

plot (0:15, dunif(0:15, 1, 3), type="l")


?dunif

# Aufgabe: Exponentialverteilung
# In einer vierwöchigen Datenerhebung missen Sie die Länge der Telefongespräche, 
# die Sie auf Ihrem Handy führen. Sie finden heraus, dass die Dauer der Gespräche (in Minuten) 
# einer Exponentialverteilung folgt, und Ihre Gespräche im Erwartungswert 3 Minuten lang sind
# Aufgabe: 
# 1. Welche Verteilung hat die Zufallsvariable X, welche die Dauer der Telefongespräche in Minuten beschreibt?
# 2. Das Telefon klingelt. Wie gross ist die Wahrscheinlichkeit, dass dieses Gespräch höchstens eine Minute dauert?
# 3. Wie gross ist die Wahrscheinlichkeit, dass das Gespräch länger als eine Minute dauert?
# 4. Mit welcher Wahrscheinlichkeit dauert das Gespräch zwischen einer und drei Minuten?
# 5. Berechnen und interpretieren Sie das 25%-Quantil dieser Verteilung.


xv <- seq(1/3, 10, length=30)
plot(xv, dexp(xv, rate=1/3), type = "l", ylab = "Exp(x)", xlab = "x")


x<-pexp(3, rate=1/3)
quantile(x)


dexp()

?quantile

xvalues <- seq(0, 10, length=11)
prob <- dexp(0:10, rate=1/3)
quantile (prob, probs=0.25 )


plot( dexp(0:10, rate=1/3), type="l")

qexp(0.25, rate=1/3, lower.tail = T)

dexp(0.86, rate=1/3)

.86 * 60


# Aufgabe: Normalverteilung
# Aufgabe: In einer Fabrik werden Tüten mit Kartoffelchips befüllt. Das durchschnittliche 
# Gewicht der Tüten soll nach den Angaben des Werkes 200 g betragen. Da die Tüten 
# maschinell befüllt werden, wird dieser Wert nur mit einer Standardabweichung 
# von 4 g eingehalten. Mit welcher Wahrscheinlichkeit werden Tüten abgefüllt, deren 
# Gewicht 
# - um weniger als 2 g vom Mittelwert abweicht?
# - über 205 g liegt?
# - Welches Gewicht wird von 95% der Tüten überschritten?
plot(dnorm(190:210, mean=200, sd=4), type="l")

pnorm(204, mean=200, sd=4, lower.tail=T) - pnorm(196, mean=200, sd=4, lower.tail=T)


pnorm( 198.3, mean = 200, lower.tail = T)

?qnorm

###########################################################
# Normalverteilung Beispiel	
# Funktionsdauer	von	Taschenrechnern
# Die	Funktionsdauer	x	ist	normalverteilt	mit	Erwartungswert	µ = 120	h	
# und	 theoretischer	Varianz s2 = 100.	
# Wie	wahrscheinlich	ist	es,	dass	die	Funktionsdauer höchstens	135	h dauert
pnorm(q = 135, mean = 120, sd = 10)

# loesungsweg via standardiesierung
z <- (135-120) / 10
pnorm (q = 1.5, mean = 0, sd = 1)
# n.b. : mean und sd sind vorbelegt. Früher haette man aus tabelle diese Werte gelesen
# Um die"Wahrscheinlichkeit, dass die Dauer hoeher ist als x" :  
# 1 - n oder lower tail verwenden

# Rechnen	mit	der	Normalverteilung, dass	Wahrscheinlichkeit zwischen	105	und	135	Stunden	liegt:
pnorm (q= 135, mean = 120, sd= 10) - pnorm(q = 105, mean=120, sd=10)

# wleche lebensdauer wird von 95 % der TR nicht ueberschritten?
qnorm (p = 0.95, mean = 120, sd = 10)

# symmetreischer 95 %  - Bereich:
# untere Grenze  
qnorm (p = 0.025, mean = 120, sd = 10)
# obere Grenze  
qnorm (p = 0.975, mean = 120, sd = 10)

# Aufgabe: Chi-Quadrat-Verteilung
# Aufgabe: Mit welcher Wahrscheinlichkeit liegt der Wert einer χ2-Verteilung mit df = 11 über 15?


###########################################################
# Aufgabe: Chi-Quadrat-Verteilung
# Mit welcher Wahrscheinlichkeit liegt der Wert einer Chi-quadrat-Verteilung  mit df = 11 über 15?
1-pchisq(15, df=11)

qchisq(.8, df=10, lower.tail = T)
qchisq(.8, df=10, lower.tail = F)
qchisq(.2, df=10, lower.tail = T)
qchisq(.2, df=10, lower.tail = F)


x1 <- seq(0.5, 0.95, .05 )
y1 <- qchisq(x1, df=11)
v1 <- matrix(c(x1, y1), nrow = 10, byrow = FALSE)

x2 <- seq(0.8, 0.85, .01 )
y2 <- qchisq(x2, df=11)
v2 <- matrix(c(x2, y2), nrow = 6, byrow = FALSE)

v2

length(x)

qchisq(.000000000000005, df=3, lower.tail = F)

plot(qchisq())

?qchisq

# exemples chi-2
# Bestimmen Sie das 95%-Perzentil der Chi-2-Verteilung mit Freiheitsgrad 7.
qchisq(.95, df=7)

vec <- seq(.05, .9, .01)

plot(qchisq(vec, df=11, lower.tail = F), type = "l")
plot(qchisq(vec, df=11), type = "l")

vec <- seq(.8, .9, .010)

plot(qchisq(vec, df=11))

     

plot(dchisq(1:20, df=1), type="l")

plot(dchisq(1:50, df=10), type="l")
plot(pchisq(1:20, df=10), type="l")
plot(dchisq(1:20, df=3))


# bsps
plot(dchisq(1:30, df=11), type="l")
?dchisq

# Studentsche t-Verteilung
dt(0, df=5 )
pt(2.3, df=5)
qt(.05, df=46)

vec <- seq(-10, 10, .1)
plot(dt(vec, df=5), type="l")


# Aufgabe: Mit welcher Wahrscheinlichkeit liegt der Wert der Studentschen t-Verteilung 
# unter -0.5, respektive unter 1? Der Freiheitsgrad sei 7.
pt(-0.5, df=7)
pt(1, df=7)

# Bestimmen Sie das 2:5%- und das 97:5%-Perzentil der Studentschen t-Verteilung mit Freiheitsgrad 5.
qt(c(0.025, 0.975), df=5)


# Schiefe
# Bestimmen Sie die Schiefe der Wartezeiten von Faithful.
faithful
summary(faithful)
wait <- faithful$waiting
y <- sort(wait)

library(e1071)
skewness(waiting)
## [1] -0.4135498
x <- 1:length(wait)

plot(x, y, type="l")
frequency(wait)
?frequency

# KUrtosis
library(e1071)
duration <- faithful$eruptions
kurtosis(duration)
