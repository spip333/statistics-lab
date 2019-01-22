#################################################################
## Uebung 4 - reloaded
##
#################################################################

## Aufgabe: arithmetischer Mittelwert
# - Bestimmen Sie die durchschnittliche Wartezeit zwischen den Eruptionen aus faithful.

faithful
mean(faithful$waiting)

## Aufgabe: Median
# - Bestimmen Sie den Median der Wartezeiten zwischen den Eruptionen aus faithful.
median(faithful$waiting)


## Aufgabe: Quartile
# Bestimmen Sie die Quartile der Wartezeiten aus faithful
quantile(faithful$waiting)

## Aufgabe: Quantile
# - Bestimmen Sie das 0.17-Quantil, das 43%-Quantil, das 67%-Quantil und das 0.85-Quantil 
# der Wartezeiten aus faithful.
quantile(faithful$waiting, c(0.17, 0.43, 0.67, 0.85))

quantile(faithful$waiting, probs = 0.17)


## Aufgabe: Spannweite
# - Bestimmen Sie die Spannweite der Wartezeiten aus faithful
range(faithful$waiting)[2] - range(faithful$waiting)[1]
max(faithful$waiting) - min(faithful$waiting)

## Aufgabe: Interquartilsabstand
# - Bestimmen Sie den Interquartilsabstand der Wartezeiten aus faithful.
IQR(faithful$waiting)

## Aufgabe: Boxplot
# - Bestimmen Sie den Boxplot der Wartezeiten aus faithful
boxplot(faithful$waiting, 
        main="Wartezeite der Eruptionen des Old Faithful",
        ylab = "Wartezeite in Minuten")


## Aufgabe: Varianz
# Problem: Bestimmen Sie die beiden Varianzen der Wartezeiten aus faithful.
sd(faithful$waiting)^2
var(faithful$waiting)

# Stichprobenvarianz:
var(faithful$waiting)

# Populationsvarianz:
var(faithful$waiting) * (length(faithful$waiting)-1) / length(faithful$waiting)

## Aufgabe: Standardabweichung
# - Bestimmen Sie die beiden Standardabweichungen der Wartezeiten aus faithful.

# Stichprobenvarianz
sd(faithful$waiting)

# Population
sqrt(var(faithful$waiting)*(length(faithful$waiting)-1)/length(faithful$waiting))


## Aufgabe: Korrelationskoefﬁzient
# - Öffnen Sie den Datensatz swiss. Bestimmen Sie die Korrelation zwischen der 
# Fruchtbarkeitsrate und dem Anteil derjenigen, deren Ausbildung über den 
# Primarschulabschluss hinausgeht
fertility <- swiss$Fertility
education <- swiss$Education

cor(fertility, education)
