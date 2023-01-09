## Regressionsdiagnostik mit R
## Teil 4: Ausreißer
## Dr. Uwe Remer



# Zuvor bitte Skript aus Video 3 'Skript_v3.R' ausführen
# Es lädt den Datensatz, bereitet die Variablen vor
# und schätzt das erste Regressionsmodell und
# speichert die das gefittete Modell im Objekt fit.0


#### 0. Regressionsmodell aus Teil 3 #### 

fit.0 <- lm(demzufr ~ 
              lire + bildungsjahre + alter, 
            data = df)

summary(fit.0)

#### 1. Residuen spichern #### 

residuals(fit.0)

#df$resid_fit.0 <- residuals(fit.0) #Ergibt einen Fehler


# Fälle mit ungültigen Werten aus Datensatz  ausschließen
variablen <- c("demzufr", "lire", "bildungsjahre", "alter")
df <- na.omit(df[,variablen])

df$resid_fit.0 <- residuals(fit.0)

# Erster Blick auf das Histogramm
hist(df$resid_fit.0)

# Ein paar deskriptive Zahlen
mean(df$resid_fit.0)
min(df$resid_fit.0)
max(df$resid_fit.0)

# negative Residuen: wir überschätzen den Wert
# positive Residuen: wir unterschätzen den Wert
# vor allem für Unzufriedene ist die Vorhersage ungenauer.


# Standardsieren der Residuen durch z-Transformation
scale(residuals(fit.0))

df$z_resid_fit.0 <- scale(residuals(fit.0))     
hist(df$z_resid_fit.0)

# Wie viele Fälle mit Residuen größer oder kleiner als 2.5 oder 3 sd
sum(abs(df$z_resid_fit.0) > 2.5)
sum(abs(df$z_resid_fit.0) > 3)


#### 2. Ursache beheben #### 

# Verteilung der Variable transformieren 

# Beispiel:
# Normalverteilte Zufallszahlen
nv_zz <- rnorm(5000, mean = 5, sd = 1)
hist(nv_zz)

# Verteilung nach rechts ziehen
hist(log(nv_zz))

# Verteilung nach links ziehen
hist(nv_zz)
hist(nv_zz^2)



# Abhängige Variable transformieren und Regression neu schätzen
hist(df$demzufr)
df$demzufr_2 <- df$demzufr^2
hist(df$demzufr_2)


fit.1 <- lm(demzufr_2 ~ 
              lire + bildungsjahre + alter, 
            data = df)

summary(fit.0)
summary(fit.1)

library(sjPlot)
plot_models(fit.0, fit.1)

QuantPsyc::lm.beta(fit.0)
QuantPsyc::lm.beta(fit.1)


# Residuen prüfen

df$resid_fit.1 <- residuals(fit.1)
df$z_resid_fit.1 <- scale(residuals(fit.1))

hist(df$z_resid_fit.1)

sum(abs(df$z_resid_fit.1) > 2.5)
sum(abs(df$z_resid_fit.1) > 3)


#### 3. Einflussreiche Fälle #### 

library(car)

# Hebelwert / Leverage: hat-Werte

# Grenzwert (threshold) für hat Werte: 2*((1+k)/n) 

k = 3
n = length(df$demzufr)
threshold_hat <- 2*((1+k)/n) 
threshold_hat

df$hat <- hatvalues(fit.1)

# Wie viele Fälle mit hohem Leverage?
sum(df$hat > threshold_hat)

# Grafische Inspektion
plot(df$z_resid_fit.1 ~ df$hat)
abline(v=threshold_hat, col="red")

# Das alleine reicht noch nicht


# Cook's D: 
thresh_cook <- 4/(n - k - 1)
thresh_cook

df$cooks.d <- cooks.distance(fit.1)
sum(df$cooks.d > thresh_cook)
# zu streng?

# Grafische Inspektion
influenceIndexPlot(fit.1)


# Fälle ausschließen?
df[rownames(df) %in% c(246, 2459),]

df.2 <- df[!(rownames(df) %in% c(246, 2459)),]
fit.2 <- lm(demzufr_2 ~ 
              lire + bildungsjahre + alter, 
            data = df.2)
summary(fit.2)


# Vergleich des fit.2 mit fit.1
fit.2$coefficients 
fit.1$coefficients

fit.2$coefficients - fit.1$coefficients


library(performance)
compare_performance(fit.1, fit.2,
                    metrics = c("R2_adj"))



# Residuen prüfen
df.2$resid_fit.2 <- residuals(fit.2)
df.2$z_resid_fit.2 <- as.numeric(scale(residuals(fit.2)))
hist(df.2$z_resid_fit.2)

sum(abs(df.2$z_resid_fit.2) > 2.5)
sum(abs(df.2$z_resid_fit.2) > 3)


influenceIndexPlot(fit.2)



# Aufräumen
rm(k, n, nv_zz, variablen, thresh_cook, threshold_hat)
