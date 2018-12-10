#############################################################################
### Deskriptive Statistik - Übungserie 4
###
### Nicolas Stern
### 10.11.2018
###
#############################################################################
## Aufgabe: arithmetischer Mittelwert
# Problem: Bestimmen Sie die durchschnittliche Wartezeit zwischen den Eruptionen aus faithful.
faithful
head(faithful)
mean(faithful$waiting)


## Aufgabe: Median
# Problem: Bestimmen Sie den Median der Wartezeiten zwischen den Eruptionen aus faithful.
wait.time <- faithful$waiting
s <- sort(wait.time)
median(wait.time)
median(s)
?median

x <- c(1,3,4,2,5)
median(x)

## Aufgabe: Quartile
# Problem: Bestimmen Sie die Quartile der Wartezeiten aus faithful
quantile(wait.time)

## Aufgabe: Quantile
# Problem: Bestimmen Sie das 0.17-Quantil, das 43%-Quantil, das 67%-Quantil und das 0.85-Quantil der Wartezeiten aus faithful.
quantile(wait.time, probs = 0.17)
quantile(wait.time, probs = 0.43)
quantile(wait.time, probs = 0.67)
quantile(wait.time, probs = 0.85)

## Aufgabe: Spannweite
# Problem: Bestimmen Sie die Spannweite der Wartezeiten aus faithful
range(wait.time)

## Aufgabe: Interquartilsabstand
# Problem: Bestimmen Sie den Interquartilsabstand der Wartezeiten aus faithful.
quantile(wait.time)[4] - quantile(wait.time)[2]
IQR(wait.time)

## Aufgabe: Boxplot
# Problem: Bestimmen Sie den Boxplot der Wartezeiten aus faithful
boxplot(wait.time, main="Wartezeite der Eruptionen des Old Faithful",
        ylab = "Wartezeite in Minuten")

wait.time

?boxplot


## Aufgabe: Varianz
# Problem: Bestimmen Sie die beiden Varianzen der Wartezeiten aus faithful.
var(wait.time)
var(wait.time)* (length(wait.time)-1)/ length(wait.time)
print(paste("test",var(wait.time)))
var(wait.time)* (length(wait.time)-1)/ length(wait.time)
print(paste("Stichprobenvarianz: ", round(var(wait.time),2)))

round(var(wait.time),2)

## Aufgabe: Korrelationskoefﬁzient
# Problem: Öffnen Sie den Datensatz swiss. 
# Bestimmen Sie die Korrelation zwischen der Fruchtbarkeitsrate und dem Anteil derjenigen, 
# deren Ausbildung über den Primarschulabschluss hinausgeht
swiss
head(swiss)
summary(swiss)
?summary
?swiss
fruchtbarkeit <- swiss$Fertility
ausb.ueber.prim <- swiss$Education
plot(ausb.ueber.prim, fruchtbarkeit)
cor(ausb.ueber.prim, fruchtbarkeit)

swiss[swiss$Education > 20,]
swiss$Education
colnames(swiss)

duration <- faithful$eruptions
waiting <- faithful$waiting
cor(duration, waiting) # nur interpredierbar nach betrachten des plots
plot(waiting, duration)
cor(waiting, duration)


fertility <- swiss$Fertility
edu.above.prim <- swiss$Education
plot(edu.above.prim, fertility, main = "Fruchtbarkeit gegenüber Ausbildung in 47 Gemeinden in CH (1888)",
     xlab="Anteil der Bevölkerung mit Ausbildung höher als Primarschule",
     ylab = "Fruchtbarkeit")

cor(edu.above.prim, fertility)

summary(swiss)
?swiss
