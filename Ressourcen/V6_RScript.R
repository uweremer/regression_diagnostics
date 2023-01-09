## Regressionsdiagnostik mit R
## Teil 6: unabhängige Fehler und Linearitätsannahme
## Dr. Uwe Remer



# Zuvor bitte die R Skripte zu den 
# Videos 3, 4 und 5 ausführen. 

source("V3_RScript.R")
source("V4_RScript.R")
source("V5_RScript.R")


#### 0. Regressionsmodell aus Teil 4 (und 5) #### 

fit.2 <- lm(demzufr_2 ~ 
              lire + bildungsjahre + alter, 
            data = df.2)
summary(fit.2)



#### 1. Keine Korrelation zwischen Fehlerterm und unabh. Variablen #### 

# Überprüft Spezifikationsprobleme, gibt Hinweise auf:
# - nicht-Linearität
# - fehlende x-Variablen
# - fehlende Interaktionseffekte 


# Residuenstreudiagramm mit Anpassungsline
# auf der x-Achse die unabhängigen Variablen abtragen
# auf der y-Achse die studentisierten Residuen

ggplot(df.2, aes(x=lire, y=stud_resid_fit.2)) +
  geom_point() +
  geom_smooth()

ggplot(df.2, aes(x=alter, y=stud_resid_fit.2)) +
  geom_point() +
  geom_smooth()

ggplot(df.2, aes(x=bildungsjahre, y=stud_resid_fit.2)) +
  geom_point() +
  geom_smooth()



# Component + Residual Plot (aus dem car Package)
# oder auch "Partielles Residuen Diagramm".
crPlots(fit.2)

# oder für eine einzelne unabh. Variable
crPlot(fit.2, "lire")
crPlot(fit.2, "bildungsjahre")
crPlot(fit.2, "alter")


# Nicht-Linerarer Effekt für Links-Rechts und für Alter.
# Besser quadratischer Effekt (hoch 2)

fit.3 <- lm(demzufr_2 ~ 
              lire + I(lire^2) + bildungsjahre + alter + I(alter*alter), 
            data = df.2)
summary(fit.3)

# I(lire^2) und I(lire*lire) wäre das selbe

# Problem bei Interaktionstermen: Multikollinearität

# VIF = Varianz-Inflations-Faktor, sollte < 5 sein, besser < 3
vif(fit.3)


# Lösung: z-transformation
df.2$lire_z <- as.numeric(scale(df.2$lire))
df.2$alter_z <- as.numeric(scale(df.2$alter))

fit.4 <- lm(demzufr_2 ~ 
              lire_z + I(lire_z^2) + bildungsjahre + alter_z + I(alter_z^2) , 
            data = df.2)
summary(fit.4)
vif(fit.4)

crPlots(fit.4)




library(effects)
plot(Effect(c("lire_z"), fit.4))

plot(Effect(c("lire_z"), fit.4, residuals=TRUE),
     partial.residuals=list (lty="dashed"))

plot(Effect(c("lire_z","alter_z"), fit.4, residuals=TRUE),
      partial.residuals=list (lty="dashed"))


plot(Effect(c("lire_z","bildungsjahre"), fit.4, residuals=TRUE),
     partial.residuals=list (lty="dashed"))

plot(Effect(c("bildungsjahre", "alter_z"), fit.4, residuals=TRUE),
     partial.residuals=list (lty="dashed"))


# Interaktionseffekt:

fit.5 <- lm(demzufr_2 ~ 
              lire_z + I(lire_z^2) + bildungsjahre + alter_z + 
              I(alter_z*alter_z) + alter_z:lire_z, 
            data = df.2)
summary(fit.5)
vif(fit.5)




plot(Effect(c("lire_z","alter_z"), fit.5, residuals=TRUE),
     partial.residuals=list (lty="dashed"))

plot(Effect(c("lire_z","bildungsjahre"), fit.5, residuals=TRUE),
     partial.residuals=list (lty="dashed"))

plot(Effect(c("bildungsjahre", "alter_z"), fit.5, residuals=TRUE),
     partial.residuals=list (lty="dashed"))




#### 2. Autokorrelierte Fehler ####

# Test eigentlich nur bei Zeitreihendaten relevant
durbinWatsonTest(fit.2)




#### 3. Darstellung der Ergebnisse ####

library(sjPlot)
tab_model(fit.5)

plot_model(fit.5, type="pred")

plot_model(fit.5, type="int")+
  scale_y_continuous(sec.axis = sec_axis(trans=~sqrt(.), 
                                         name="Demzufr. (orig.Skala)"))



