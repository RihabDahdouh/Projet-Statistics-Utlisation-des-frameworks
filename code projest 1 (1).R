
#Etape1 : Définition de la problématique et des données
#Etape2 : Collecte des données 
library(readxl)
form <- read_excel("C:/Users/Kaoutar/Downloads/form (2).xlsx")
View(form)
#Etape 3: Prétraitement 

#conversion et codification 

#items 
form$PU1<-factor(form$PU1, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PU2<-factor(form$PU2, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PU3<-factor(form$PU3, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PU4<-factor(form$PU4, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PU5<-factor(form$PU5, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))

form$PF1<-factor(form$PF1, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PF2<-factor(form$PF2, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PF3<-factor(form$PF3, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$PF4<-factor(form$PF4, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))

form$AU1<-factor(form$AU1, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$AU2<-factor(form$AU2, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$AU3<-factor(form$AU3, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$AU4<-factor(form$AU4, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))

form$IU1<-factor(form$IU1, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$IU2<-factor(form$IU2, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$IU3<-factor(form$IU3, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$IU4<-factor(form$IU4, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$IU5<-factor(form$IU5, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))

form$VE1<-factor(form$VE1, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$VE2<-factor(form$VE2, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$VE3<-factor(form$VE3, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))
form$VE4<-factor(form$VE4, order = TRUE, levels =c("Pas du tout d'accord","Pas d'accord","Neutre","D'accord","Tout à fait d'accord"))


levels(form$PU1)<-c("1","2","3","4","5")
levels(form$PU2)<-c("1","2","3","4","5")
levels(form$PU3)<-c("1","2","3","4","5")
levels(form$PU4)<-c("1","2","3","4","5")
levels(form$PU5)<-c("1","2","3","4","5")

levels(form$PF1)<-c("1","2","3","4","5")
levels(form$PF2)<-c("1","2","3","4","5")
levels(form$PF3)<-c("1","2","3","4","5")
levels(form$PF4)<-c("1","2","3","4","5")

levels(form$AU1)<-c("1","2","3","4","5")
levels(form$AU2)<-c("1","2","3","4","5")
levels(form$AU3)<-c("1","2","3","4","5")
levels(form$AU4)<-c("1","2","3","4","5")


levels(form$IU1)<-c("1","2","3","4","5")
levels(form$IU2)<-c("1","2","3","4","5")
levels(form$IU3)<-c("1","2","3","4","5")
levels(form$IU4)<-c("1","2","3","4","5")
levels(form$IU5)<-c("1","2","3","4","5")


levels(form$VE1)<-c("1","2","3","4","5")
levels(form$VE2)<-c("1","2","3","4","5")
levels(form$VE3)<-c("1","2","3","4","5")
levels(form$VE4)<-c("1","2","3","4","5")

typeof(form$Sexe)
if(is.character(form$Sexe)){
  for(i in 1:length(form$Sexe)){
    if(form$Sexe[i] == "Homme"){
      form$Sexe[i]="H"
    }
    else if(form$Sexe[i] == "Femme"){
      form$Sexe[i]="F"
    }
  }
  form$Sexe<-as.factor(form$Sexe)
}

#codification 

if(is.character(form$`Année Universitaire`)){
  for(i in 1:length(form$`Année Universitaire`)){
    if(form$`Année Universitaire`[i] == "1ère année cycle GI"){
      form$`Année Universitaire`[i]="CI1"
    }
    else{
      form$`Année Universitaire`[i]="CI2"
    }
  }
  form$`Année Universitaire`<-as.factor(form$`Année Universitaire`)
}

#nettoyage des donnees 
#traitement des valeurs aberrantes 
out1=boxplot.stats(form$Age)$out #on stocke les valeurs aberrantes dans out1
boxplot(form$Age)

for (out in out1) {           #on parcours les valeurs aberrantes trouvees 
  for (i in 1:length(form$Age)) {
    if(form$Age[i]==out && !is.na(form$Age[i])){
      form$Age[i]=NA         #on affecte NA à chaque valeur aberrante trouvée 
    }
  }
}

#traitement des valeurs manquantes 

#3.2.2 : Traitement des valeurs manquantes

pro=sum(is.na(form$Age))/length(form$Age)
pro

#Incase we had NA values here's how we will treat the columns
# 3 Cases : Add ,Estimate ,Delete

if(pro<0.05 && (length(form$Age)-sum(is.na(form$Age))>=30)){
  
  print("Delete")
  
  #if delete
  for (i in 1:length(form$Age)) {
    if(is.na(form$Age[i])){
      form=(form[-c(i),])
    }
  }
} else{
  print("Estimate")
  
  #if estimate
  for (i in 1:length(form$Age)) {
    if(is.na(form$Age[i])){
      form$Age[i]=mean(form$Age,na.rm=TRUE)
    }
  }
}

if(is.double(form$Age)){
  form$Age<-as.integer(form$Age)
}

#3.3 : Test De Normalité

shapiro.test(form$Age)

#p-value = 3.94e-08 <5% 

#H0 : il n ya pas de differnce signifucative entre la loi normale et la distribution de l age
#H1 : il ya  de differnce signifucative entre la loi normale et la distribution de l age
#p-value <5%  alors on accepte H1 ,il y a une difference significative entre ma distribution et la loi normale 

#verification de la quasi-normalite 

library(moments)
skewness(form$Age)
kurtosis(form$Age)

#CC: la quasi-normalité(skewness et kurtosis appartiennet a l'intervalle [-3:3])
#donc on va appliquer les deux tests parametriques et non parametriques

#Etape 4 : Traitement

#4.1 : Statistique descriptive univariée
#4.1.1 : Statistique descriptive univariée numérique
summary(form)
#4.1.2 : representation graphique 

library(ggplot2)
library(ggdensity)

#Data visualisation
pie(table(form$Sexe), main = "Proportions du genre")
pie(table(form$`Année Universitaire`), main = "Proportions du Niveau")

#Age  is quanti , continuous so we used Histogram
plot(form$Age)
hist(form$Age)

#annné universitaire

pie(table(form$`Année Universitaire`))
plot(form$`Année Universitaire`)

#sexe s qualitative , nominal so we used the pie Chart
plot(form$Sexe)
pie(table(form$Sexe),labels = c("Femme","Homme"))


#items, on va choisir les items les plus importants 

for (i in 7:28) {
  print(table(form[[i]]))
}  

#analyse de fiabilite de questionnaire 


library(ltm)
library(dplyr)

#library(Rcmdr)

#visualisation des items

index = c(7:28)
items = data.frame(form[,index])#on a separer les items des autres informations
View(items) 

#bartlett.test() est une fonction en R qui permet de réaliser le test de Bartlett pour l'égalité des variances 
#dans un ou plusieurs groupes de données.

items[ , ] = lapply(items, as.numeric) # afin d'utiliser le bartlet test

#tester tous les items 
#p-value < 5 % alord on accepte H1 il ya une coresspondance entre items
bartlett.test(items) # p-value = 0.0007997 < 5 % alord on accepte H1 il ya une corresspondance entre items

cronbach.alpha(items) #alpha: 0.827 > 0.6 very good


#PU
cronbach.alpha(items[1:5]) #alpha: 0.529  

bartlett.test(items[1:5]) #pvalue = 0.03478 < 5 % alord on accepte H1 il ya une corresspondance entre items
#PF
cronbach.alpha(items[6:9]) #alpha: 0.691
bartlett.test(items[6:9]) #pvalue = 0.2667  > 5 % alord on accepte H0 il  n ya une grande correspondance entre items de cette partie
#AU
cronbach.alpha(items[10:13]) #alpha = 0,476
bartlett.test(items[10:13]) #p-value = 0.07469  > 5 % alord on accepte H0 il  n ya pas  une correspondance entre items items de cette partie
#IE
cronbach.alpha(items[14:18]) #alpha: 0.164
bartlett.test(items[14:18])  #pvalue = 0.04891 < 5 % alord on accepte H1 il ya une corresspondance entre items de cette partie
#VE
cronbach.alpha(items[19:22]) #alpha = 0,48
bartlett.test(items[19:22]) #pvalue  =  0.004319 < 5 % alord on accepte H1 il ya une corresspondance entre items de cette partie


#The unit of measure (The quiz) is reliable

#Epuration des items 

#PU
cronbach.alpha(items[1:5]) #alpha: 0.805
epurationTest = data.frame(items[,1],items[,2],items[,4],items[,5]) # I removed the third column
cronbach.alpha(epurationTest)

bartlett.test(items[1:5]) # p-value = 0.03478 < 5 % alord on accepte H1 il ya une corresspondance entre items
#PF
cronbach.alpha(items[6:9]) #alpha: 0.691 
bartlett.test(items[6:9]) #p-value = 0.2667 > 5 % alord on accepte H0 il  n ya une grande correspondance entre items de cette partie
#AU
cronbach.alpha(items[10:13]) #alpha: 0.476 
epurationTest = data.frame(items[,10],items[,11],items[,13]) # I removed 11
cronbach.alpha(epurationTest)


#alpha devient 0.52 apres cet operation 

bartlett.test(items[10:13]) #p-value = 0.07469 > 5 % alord on accepte H0 il  n ya pas de correspondance entre items items de cette partie
#IE
cronbach.alpha(items[14:18]) #alpha: 0.164
epurationTest = data.frame(items[,15],items[,16],items[,18]) # I removed th 14th and the 17th qst
cronbach.alpha(epurationTest)

#apres epuration l'alpha devient = 0.61

bartlett.test(items[14:18])  #pvalue = 0.04891 < 5 % alord on accepte H1 il ya une corresspondance entre items de cette partie
#VE
cronbach.alpha(items[19:22]) #alpha = 0,48
epurationTest = data.frame(items[,19],items[,20],items[,21],items[,22]) # I removed th 14th and the 17th qst
cronbach.alpha(epurationTest)
bartlett.test(items[19:22]) #pvalue =  0.004319 < 5 % alord on accepte H1 il ya une corresspondance entre items de cette partie


#4.2 : Statistiques descriptives bivariées : tester des hypothèses

#Test de l'égalité entre l'homme et la femme (Test équilibre du genre) $$$$Representativity test

chisq.test(table(form$Sexe)) #p-value = 0.9055 > 5% alors on accepte H0 , il nya pas de diference signifiative entre le nombre des hommes et femmes

chisq.test(table(form$Sexe,form$PU1))  #p-value = 0.2702 > 5% alors on accepte H0 d'ou il ya pas de diference signifiative entre le sexe et PU1


#important à verifier !!!!!!!!!!!!!!!!

#L'age ne suit pas la loi normale mais il suit la quasinormalité alors on effectue les 2 tests , parametrique et non parametrique et puis on choisit le convenable
#relation entre

##test non parametrique ( comparaison)
wilcox.test(form$Age~form$Sexe, exact = FALSE)  #p-value = 0.1699 > 5% 
#p_value > 5% : H0: no significant difference between age and sexe

#test parametrique( t.test) #p-value = 0.1392  > 5% 
t.test(form$Age~form$Sexe)
#p_value > 5% : H0: no significant difference between age and sexe

#on a applique les deux test car on a la quasi-normalite et le sexe est quanli à 2 modalités et Age variable quanti 
#alors on prend le test parametrique 



##Items (plusieurs modalite) and Age (quanti)

#test parametrique 
#We used anova test because the Age variable is quantitive and items are qualitative with more than 3 modalities
library(car)

for (i in index) {
  print(colnames(form[,i]))
  output = aov(form$Age~form[[i]])
  if(summary(output)[[1]][["Pr(>F)"]][1] >0.05){
    print("H0 : There is no difference between the awnsers of our sample and the age")
  }
  else{
    print("H1 : There is a difference between the awnsers of our sample and the age")
  }
}

#Conclusion <=> The age doesn't affect the answers

#test non parametrique (krustal)
library(car)
library(PMCMRplus)
for (i in index) {
  print(colnames(form[,i]))
  output = kruskal.test(form$Age~form[[i]])
  if(output$p.value>0.05){
    print("H0 : There is no difference between the awnsers of our sample and the age")
  }
  else{
    print("H1 : There is a difference between the awnsers of our sample and the age")
  }
}

#le test parametrique suit le test non parametrique alors on choisit le parametrique

#Conclusion <=> The age doesn't affect the anwers

#Items and Gender

for (i in index) {
  print(colnames(form[,i]))
  output = chisq.test(form[,i],form$Sexe)
  if(output$p.value>0.05){
    print("H0 : There is no difference between the awnsers of our sample and the gender")
  }
  else{
    print("H1 : There is a difference between the awnsers of our sample and the gender")
  }
  
}

#Conclusion <=> The gender doesn't affect the answers except PF 4 / UI 4


#Items and Annee univ
for (i in index) {
  print(colnames(form[,i]))
  output = chisq.test(form[,i],form$`Année Universitaire`)
  if(output$p.value>0.05){
    print("H0 : There is no difference between the awnsers of our sample and the year of study")
  }
  else{
    print("H1 : There is a difference between the awnsers of our sample and the year of study")
  }
  
}
#The gender doesn't affect the answers except PU5 / PF2 / PF3 / IU2

#HYPOTHSE 

#H1 : year of study affects the difficulty using frameworks? —  year of study  vs  PF 1-3

chisq.test(table(form$`Année Universitaire`,form$PF1))  #p-value = 0.5278 > 5% il n' ya pas de difference signinficative entre l'annee de study et la facilité de l'utilisation 


chisq.test(table(form$`Année Universitaire`,form$PF3))  #p-value = 0.04082 < 5%   il ya une difference signinficative  entre La transition du langage de programmation vers son Framework et l'année d'etude
chisq.test(table(form$`Année Universitaire`,form$PF4))  #p-value = 0.06306  > 5%  #les 2 années sont sensés par la difficulté de l'utilisation des frameworks  


#H2 : pedagogical program influence the usefulness of framework — VE1 vs PU1

chisq.test(table(form$VE1,form$PU1)) #p-value = 0.2403 > 5% , H0 accepté ; alors il n ya pas une difference significative entre l'apprentissage et l'utilité des frameworks 
#accepté

#H3 Popularity of frameworks is a major factor in adopting frameworks (VE3 vs IU3)

chisq.test(table(form$VE3,form$IU3))  #p-value = 0.07114 > 5% donc on accepte H0 , il ya pas  une difference significative entre la popularité de framework et son adoption 
#ACCEPTE 


#h4: frameworks are not useful for an engineer ( PU3 vs AU3 ) #######we have a problem item epure 

chisq.test(table(form$PU3,form$AU3)) #p-value = 3.117e-10 < 5% on accepte H1 il ya une difference significative entre l'utilité et l'attitude d'utilisation 

#H5:the ease of frameworks influence the utility (PU1 vs PF1)

chisq.test(table(form$PU1,form$PF1)) #p-value = 0.0463 < 5 % on accepte H1 il ya une difference significative entre la facilité et l'utilité

#if the use affect the utility => the utility is the main factor 



#h6:The simplicity of frameworks make the learning easier ( AU4 vs PF1 )

chisq.test(table(form$AU4,form$PF1)) #  p-value = 0.0002243 < 5% alors on accpete H1 il ya une difference entre la simplicité et l'apprentissage

#regression (on a une seule variable quali) 
















