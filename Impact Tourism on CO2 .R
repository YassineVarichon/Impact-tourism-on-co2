# Script mémoire économetrie Yassine Varichon



# On définit un espace de travail
  setwd("C:/Users/maria/OneDrive/Bureau/Mémoire")

  
# On importe nos bases de données
  
  CO2 <- read.csv2("C:/Users/maria/Downloads/CO2.csv")
  Touristes <- read.csv("C:/Users/maria/Downloads/Touristes.csv", sep=";")
  tourisme.entrées <- read.csv("C:/Users/maria/Downloads/tourisme entrées.csv", sep=";")
  PIB <- read.csv("C:/Users/maria/Downloads/PIB.csv", sep=";")
  Industrie <- read.csv2("C:/Users/maria/OneDrive/Industrie.csv")
  surface.forestiere <- read.csv2("C:/Users/maria/Downloads/surface forestiere.csv")
  superficie <- read.csv("C:/Users/maria/Downloads/superficie.csv", sep=";")
  Population <- read.csv("C:/Users/maria/OneDrive/Bureau/Population.csv", sep=";")
  
  
  
# On fusionne nos 8 bases de données 2 par 2 pour avoir notre base de données finale avec toutes nos variables.
# La fusion est faite sur la base de la colonne "Code Pays"
  
  
  database<-merge(surface.forestiere,Touristes)
  database<-merge(database,tourisme.entrées)
  database<-merge(database,CO2)
  database<-merge(database,PIB)
  database<-merge(database,Industrie)
  database<-merge(database,superficie)
  database<-merge(database,Population)
  
  
  
# On crée de nouvelles variables :
# Le pourcentage des dépenses du tourismes par rapport au PIB
  
database$pourcentage_tourisme <- (database$Depense_tourisme/ (database$PIB_en_M*1000000))*100

# Le pourcentage des surfaces forestieres dans le pays

database$pourcentage_foret <- (database$Surface_forestiere_km2/ database$Superficie)*100

# Les émissions de CO2 par habitant

database$emissions_par_hab <- (database$CO2.en.t/database$Population)

# Le PIB par habitant en dollars

database$PIB_par_habitant <- (database$PIB_en_M/database$Population)*1000000

# On résume les principales statistiques de nos variables grâce à la fonction summary

summary(database)

# On réalise quelques nuages de points et quelques graphiques pour pouvoir mieux analyser notre bases de données:

plot(database$pourcentage_tourisme,database$emissions_par_hab,main="Evolution des emissions de CO2 par rapport au pourcentage des dépenses touristiques", xlab="pourcentage des dépenses du tourisme par rapport au PIB", ylab= "emission de CO2 par habitant en tonnes", col="red")
plot(database$pourcentage_foret,database$emissions_par_hab,main="Evolution des emissions de CO2 par rapport au pourcentage des forets par rapport à la superficie du pays", xlab="pourcentage des forets dans le pays", ylab= "emission de CO2 par habitant en tonnes", col="red")




# On commence tout d'abord par une regression linéaire simple et on l'analyse

Regmodel1<- lm((emissions_par_hab)~pourcentage_tourisme,data=database)
summary(Regmodel1) 


# Puis on ajoute progressivement d'autres variables explicatives pour trouver le modèle le plus adéquat

Regmodel2 <- lm((emissions_par_hab)~pourcentage_tourisme + industrie_en_pourcentage, data= database)
summary(Regmodel2)

Regmodel3 <- lm((emissions_par_hab)~pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant, data= database)
summary(Regmodel3)

Regmodel4 <- lm((emissions_par_hab)~pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant + pourcentage_foret, data= database)
summary(Regmodel4)
 # On décide de garder le Regmodel3 pour la suite de notre étude car le pourcentage_foret n'est pas significatif.


# On fait des tests de corrélation pour nos nuages de points et on dessine la droite de regression linéaire:
# Dépenses Touristiques : 

y=database$emissions_par_hab
x=database$pourcentage_tourisme
plot(x,y,ylab = "emission de CO2 par habitant en tonnes",
     xlab = "pourcentage des dépenses du tourismes par rapport au PIB", 
     main = "Evolution des emissions de CO2 par rapport au pourcentage des dépenses touristiques",
     col="red")
cor(x,y)
cor.test(x,y)
abline(lm(y~x))
Regmodel1=lm(y~x)
Regmodel1
summary(Regmodel1) 

# SURFACES FORESTIERES :

y=database$emissions_par_hab
x=database$pourcentage_foret
plot(x,y,
     ylab = "emission de CO2 par habitant en tonnes",
     xlab = "pourcentage des forets dans le pays", 
     main = "Evolution des emissions de CO2 par rapport pourcentage des forets par rapport à la superficie du pays",
     col="red")
cor(x,y)
cor.test(x,y)
abline(lm(y~x))
Regmodel2=lm(y~x)
Regmodel2
summary(Regmodel2) 

#Corrélation : Le coefficient de corrélation est compris entre -1 et 1. 
#Plus le coefficient est proche de 1, plus la relation linéaire positive entre les variables est forte.
#Plus le coefficient est proche de -1, plus la relation linéaire négative entre les variables est forte.
#Plus le coefficient est proche de 0, ce qui est notre cas, plus la relation linéaire entre les variables est faible.

cor.test(database$emissions_par_hab, database$pourcentage_tourisme)
cor.test(database$emissions_par_hab, database$pourcentage_foret)


# On corrige les écart-type en utilisant la méthode de white

install.packages("lmtest")
install.packages("sandwich")

library(lmtest)
library(sandwich)

coeftest(Regmodel3,vcoc=vcovHC(Regmodel3, type= "HCH0"  ))

# On aurait dû voir, mais ce n'est pas le cas, que les écart-types sont un peu gros et donc la significativité un peu moindre, 
# toutefois, les coefficients restent très significatifs.


# Télécharger les packages 
library(lmtest)
library(MASS)
library (car)

#8°Résultat des estimations du modèle économétrique 
#regression lineaire simple
#->Significatif si pvalue<0,1: existance d'asociation entre les deux variables

##Significativité: 10%(*) 5%(**) 1%(***)

#-->p-value = 0.004536,R2 = 0.05178; pourcentage_tourisme: significatif a 5% (0.004536);intercept (3.33e-07 ***)
lm1<- lm((emissions_par_hab)~pourcentage_tourisme,data=database)
summary(lm1)

#-->p-value=8.23e-08,R2 = 0.1728; industrie_en_pourcentage : significatif a 1,5 et 10% (8.23e-08);intercept (0.207)
lm2<- lm((emissions_par_hab)~industrie_en_pourcentage,data=database)
summary(lm2)

#-->p-value = 2.2e-16,R2 = 0.4125;  PIB_par_habitant: significatif a 1,5 et 10% (2.2e-16);intercept (2.62e-06 ***)
lm3<- lm((emissions_par_hab)~PIB_par_habitant,data=database)
summary(lm3)

#-->p-value=0.0343, R2= 0.02913 ;pourcentage_foret : significatif a 5% (0.0343);intercept (3.22e-12 ***)
lm4<- lm((emissions_par_hab)~pourcentage_foret,data=database)
summary(lm4)


#Regression multiple : 
lmMulti <- lm((emissions_par_hab)~pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant + pourcentage_foret, data= database)
summary(lmMulti)

####################################
#-Il ne reste plus que trois variables 
#-Nommer le modele en lmMulti

lmMulti <- lm((emissions_par_hab) ~ pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant, data = database)
summary(lmMulti)

#test de linéarité
#Rainbow

raintest(lmMulti)
#Rain = 1.1455, df1 = 77, df2 = 73, p-value = 0.2798

#-->Bonne spécification car P-value>0,05

#test de normalité
#Shapiro-Wilk
#-les residus suivent une loi Normale si P-value>5%
shapiro.test(residuals(lmMulti))

# méthodes de régression robuste 
# y : variable dépendante
y <- database$emissions_par_hab

# Utilisation de boxcox pour trouver la meilleure transformation
bc_result <- boxcox(y ~ 1)

bc_result

# Utiliser la valeur optimale de lambda pour la transformation
lambda_optimal <- bc_result$x[which.max(bc_result$y)]
emissions_par_hab_new <- if (lambda_optimal == 0) log(y) else ((y^lambda_optimal) - 1) / lambda_optimal

# distribution après transformation
hist(emissions_par_hab_new, prob = TRUE, main = "Histogramme après transformation de Box-Cox",col = "lightblue")
curve(dnorm(x, mean = mean(emissions_par_hab_new), sd = sd(emissions_par_hab_new)), add = TRUE, col = "blue", lwd = 2)

# Ajouter une constante pour éviter les valeurs négatives
emissions_par_hab_new <- emissions_par_hab_new + abs(min(emissions_par_hab_new)) + 1

# Appliquer une transformation logarithmique
log_emissions_par_hab <- log(emissions_par_hab_new)

# Ajuster le modèle avec la variable dépendante transformée
log_lmMulti <- lm(log_emissions_par_hab ~ pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant, data = database)

# Vérifier la normalité des résidus après transformation

shapiro.test(residuals(log_lmMulti))

par(mfrow=c(2,2))
plot(log_lmMulti, which = 3)

#test d'hétéroscedasticité
#Breush-Pagan
#-Il y a Heteroscedasticite si p-value<5%

bptest(log_lmMulti)
# p-value = 0.5933

# Transformation logarithmique de la variable dépendante
database$log_emissions_par_hab <- log(database$emissions_par_hab)

# Ajustement du modèle avec la variable dépendante transformée

log_lmlmMulti <- lm(log_emissions_par_hab~ pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant, data = database)

bptest(log_lmlmMulti)
# p-value = 0.8442

# Obtenir les résidus du modèle
residuals <- resid(log_lmMulti)

# Obtenir les valeurs prédites du modèle
predicted_values <- predict(log_lmMulti)

# Graphique de dispersion des résidus
plot(residuals, 
     ylab = "Résidus", 
     main = "Graphique de Dispersion des Résidus",
     col ="blue")
abline(h = 0, col = "black", lty = 2)

# Graphique quantile-quantile des résidus

qqnorm(residuals)
qqline(residuals)

######################################################################
#test d'autocorrelation (test de Durbin-Watson)

dwtest(log_lmMulti)

######################################################################

#Multicolinéarité
#Klein et Vif
#-Nommer MAn la regressions ne prenant en compte que les variables explicatives
#-Pour voir le lien entre ces variables explicatives
#-La fonction vif pour vérifier la presence de multicolinearite entre ces variables

MG<- log_lmMulti
summary(log_lmlmMulti)

MA<-lm(log_emissions_par_hab ~ pourcentage_tourisme + industrie_en_pourcentage + PIB_par_habitant, data = database)
summary(MA)
vif(MA)
