## Regressionsdiagnostik mit R
## Teil 5: normalverteilte Fehler und konstante Fehlervarianzen
## Dr. Uwe Remer



# Zuvor bitte die beiden R Skripte zu den 
# Videos 3 und 4  ausführen. 




#### 0. Regressionsmodell aus Teil 4 #### 

fit.2 <- lm(demzufr_2 ~ 
              lire + bildungsjahre + alter, 
            data = df.2)
summary(fit.2)



#### 1. Residuen und Vorhersagewerte speichern #### 

names(df.2)

df.2$fitted_fit.2 <- fitted(fit.2)

# studentisierte Residuen
df.2$stud_resid_fit.2 <- rstudent(fit.2)



#### 2. Residuen normalverteilt? ####

#  Deskriptive Statistik und grafische Inspektion
Desc(df.2$stud_resid_fit.2)

# QQ-Plot (car Package) mit Simulierten Konfidenzbändern
qqPlot(fit.2)



# Statistischer Test auf Normalverteilung
shapiro.test(df.2$stud_resid_fit.2)





#### 3. Residuen mit konstanter Varianz? #### 


# Residuen-Streudiagramm

library(ggplot2)
ggplot(df.2, aes(x=fitted_fit.2, y=stud_resid_fit.2)) +
  geom_point() 


# Spread-level Plot: 
# Wir nutzen den Betrag der Residuen: abs(stud_resid_fit.2)
# Die Anpassungslinie zeigt den Trend
ggplot(df.2, aes(x=fitted_fit.2, y=abs(stud_resid_fit.2))) +
  geom_point() +
  geom_smooth() 



# Statistische Tests zur Ermittlung von Heteroskedastizität
# Nullhypothese: konstante Varianz der Fehler

# ncv Test aus dem car Package 
ncvTest(fit.2)

# Breusch-Pagan Test aus dem Paket lmtest
library(lmtest)
bptest(fit.2)



# Wie viel Unterschied in den Varianzen
quantile(df.2$fitted_fit.2)
df.2$fitted_fit.2_grouped <- cut(df.2$fitted_fit.2, breaks=quantile(df.2$fitted_fit.2))

df.2$fitted_fit.2_grouped

boxplot(df.2$stud_resid_fit.2 ~ df.2$fitted_fit.2_grouped)

aggregate(df.2$stud_resid_fit.2, by=list(df.2$fitted_fit.2_grouped), FUN=var)



# Spread Level Plot mit Empfehlung für Tranfsformatione
# (kleiner Unterschied zu oben: y-Achse wird logarithmiert dargestellt)
spreadLevelPlot(fit.2)




#### 4. Ergänzung: Robuste Standardfehler berechnen ####

# hccm aus dem car Package berechnet die "Heteroscedasticity-Corrected Covariance Matrices" 
hc3 <- hccm(fit.2)

# Damit lässt sich dann die Teststatistik der Koeffizienten 
# mit der Funktion coeftest aus dem lmtest Paket neu berechnen
library(lmtest)
coeftest(fit.2, vcov. = hc3)
coeftest(fit.2)
coeftest(fit.2, vcov. = hc3)[,"Std. Error"]
coeftest(fit.2)

# Wie groß ist der Unterschied?
coeftest(fit.2)[,"Std. Error"] - coeftest(fit.2, vcov. = hc3)[,"Std. Error"]


