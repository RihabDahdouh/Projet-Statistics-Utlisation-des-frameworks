#---------------------Step 1:definition de la problematique---------------------#
#refer to the presentation attached

#---------------------Step 2 : Data collection---------------------#
#---------------------2-1:importing the data---------------------#
##importing the data from the excel file generated from the answers to the google form
library(readxl)
data <- read_excel("~/Desktop/mini-projet/data 21.23.39.xlsx")
View(data)
#---------------------Etape3:preprocessing---------------------#
#---------------------3-1:data conversion---------------------#
#Variables codification for ease of reading the code

names(data)[5]="ATT1"
names(data)[6]="ATT2"
names(data)[7]="ATT3"
names(data)[8]="ATT4"
names(data)[9]="ATT5"
names(data)[10]="NS1"
names(data)[11]="NS2"
names(data)[12]="NS3"
names(data)[13]="NS4"
names(data)[14]="PBC1"
names(data)[15]="PBC2"
names(data)[16]="PBC3"
names(data)[17]="PBC4"
names(data)[18]="PBC5"
names(data)[19]="PBC6"
names(data)[20]="INT1"
names(data)[21]="INT2"
names(data)[22]="INT3"
names(data)[23]="INT4"
names(data)[24]="INT5"
names(data)[25]="INT6"

#converting the column genre to factors

if(is.character(data$genre)){
  data$genre = as.factor(data$genre)
}

##converting the column age to numeric,taking the substring made of the two first characters in case the variable isn't numeric 

if(!is.numeric(data$age)){
  data$age[substr(data$age, 1, 2)=="19"] =19
  data$age[substr(data$age, 1, 2)=="20"] =20
  data$age[substr(data$age, 1, 2)=="21"] =21
  data$age[substr(data$age, 1, 2)=="22"] =22
  data$age[substr(data$age, 1, 2)=="23"] =23
  data$age[substr(data$age, 1, 2)=="25"] =25
  data$age=as.numeric(data$age)
}

#converting the column filière to factors

if(is.character(data$filière)){
  data$filière = as.factor(data$filière)
                                                 
}

#converting the column Année to factors

if(is.character(data$Année)){
  data$Année = as.factor(data$Année)

}

library(dplyr)
#select from the package dplyr enables us to select the columns containing the answers to the items
likert_responses <- select(data, 5:25)

#recode gives numerical values to the answers according to the likert scale value
#mutate_all applies the code to all cells of the selected data

likert_responses_numeric <- likert_responses %>% mutate_all(~recode(.,
                                   "pas du tout d'accord"= 1,
                                   "pas d'accord"= 2,
                                   "neutre" = 3,
                                   "d'accord" = 4,
                                   "Tout à fait d'accord" = 5,
                                  ))
data[, 5:25]<-likert_responses_numeric
items <- data[, 5:25]
items

##---------------------3.2:Data cleaning---------------------
##---------------------3.2.1: processing outlier values---------------------

boxplot(data$age)
out=boxplot.stats(data$age)$out
for(i in 1:length(data$age)){
  #since out in an array , %in% vill compare each value of data$age with each value of out
  #puisque "out" est une collection %in% va nous permettre de comparer chaque valeur de data age avec les valeurs de"out"  
  if(data$age[i] %in% out){
      data$age[i]=NA
  }
}

#---------------------3.2.2:processing non available data---------------------

#in order to process missing data in data$Année, we replace na with the most frequente one 

an=table(data$Année)
plusfrequente <- names(sort(an, decreasing = TRUE)[1])
for (i  in 1:length(data$Année)) {
  if(is.na(data$Année[i])){
    data$Année[i]=plusfrequente
    
  }
}
#in order to process missing data in data$age we calculate te proportion of the non available data first,

pNA=(length(out)/length(data$age))*100
pNA  

#we get pNA=6.45>5% 
#we replace the missing data with the round up value of the mean (without counting the na values)

for (i in 1:length(data$age)) {
  if(is.na(data$age[i])){
    if(pNA>5){
        data$age[i]=as.integer(mean(data$age,na.rm = TRUE))
    }else{     
        data<-subset(data,data$age!=NA)
      }
    }
}

#3.3 ---------------------Sample representativity---------------------#
chisq.test(table(data$genre)/length(data$genre),p=c(492/904,412/904)) #0.8524----->there is no significant difference between our sample
#                                                       and the population in terms of gender proportionnalities 
table(data$genre)/length(data$genre)

#3.3---------------------Normality test---------------------#

#les hypothèses: H0:there is no significant difference between the distribution of my sample and the normal distribution
#                H1: there is a significant difference between the distribution of my sample and the normal distribution
library(moments)
shapiro.test(data$age)
# (p-value = 1.004e-07) ----> H1 is not rejected :there is a significant difference between the distribution of my sample and the normal distribution
skewness(data$age) #-------> 0.1024569 : £[-3,3]
kurtosis(data$age) #------->2.654027  :: £[-3,3]
# nous avons la quasi normalité de la variable age(skewness et kurtosis £[-3,3])



#---------------------test de Fiabilité---------------------#
library(psych)
#------ALL ITEMS------#
bartlett.test(items)#--------- 0.001393 :H1: there is a significant difference between the variances of the items
                                     #=>there is a correspondence between items
alpha(items,check.keys=TRUE)
#------ATTITUDE-----#

att=data.frame(data$ATT1,data$ATT2,data$ATT3,data$ATT4,data$ATT5)

table(att)
bartlett.test(att) #---------0.09739 :H0: there is no significant difference between the variances of the items
#                                     =>there is no  correspondence between items




#-------Subjective norms---#
sn=data.frame(data$NS1,data$NS2,data$NS3,data$NS4)
bartlett.test(sn) #---------0.001387 :H1: there is a significant difference between the variances of the items
#                                     =>there is no correspondence between items
alpha(sn) ##l'alpha de cronbach>0 :( 0.59) =>There is a strong correspondence between items

#------Perceived controled behavior ---#
pcb=data.frame(data$PBC1,data$PBC2,data$PBC3,data$PBC4,data$PBC5,data$PBC6)
bartlett.test(pcb) #--------- 0.288 :H0: there is no significant difference between the variances of the items
#                                     =>there is no correspondence between items




#---------INTENTION-------#
int=data.frame(data$INT1,data$INT2,data$INT3,data$INT4,data$INT5,data$INT6)
bartlett.test(int) #--------- 0.8828 :H0: there is no difference between the variances of the items
#                                     =>there is a correspondence between items
alpha(int)
##############################l'alpha de cronbach>0 :( 0.91 ) =>There is a strong correspondence between items


########determination du score de chaque construit chez chaque répondant en calculant la moyenne

#****ATTITUDE****#

att=data.frame(data$ATT1,data$ATT2,data$ATT3,data$ATT4,data$ATT5)
attitudemean=rowMeans(att)
attitudemean
hist(attitudemean)
shapiro.test(attitudemean)#p-value = 0.001338<5%,pas de normalité
skewness(attitudemean)#-0.8968167 £[-3,3]
kurtosis(attitudemean)#3.555553 !£[-3,3]
#pas de quasi normalité

#****SUBJECTIVE NORMS***#

Nsmean=rowMeans(sn)
Nsmean
hist(Nsmean)
shapiro.test(Nsmean)# p-value = 0.2332 >5% -->suit la loi normale

#******PerceivedControled Behavior******#

pcbmean=rowMeans(pcb)
pcbmean
hist(pcbmean)
shapiro.test(pcbmean)# p-value = 0.732 >5% ---> suit la loi normale

#*********INTENTION********#

intmean=rowMeans(int)
intmean
hist(intmean)
shapiro.test(intmean)#p-value = 0.005249,pas de normalité
skewness(intmean)
kurtosis(intmean)
#skewness et kurtosis £[-3,3] on a la quasi normalité

#---------Etape 4:Traitement----------#

#4.1: analyse discriptif
#4.1.1:statistique discriptif univariee numérique

summary(att)
summary(sn)
summary(pcb)
summary(int)

#4.1.2:statistique discriptif univariee graphique

library(ggplot2)
plot(data$genre)
hist(data$age)
pie(table(data$Année))
pie(table(data$genre))

#4.2.1---------------------sytatistique descriptive bivariée---------------------
#4.2.1.1 graphical bivariate descriptive statistics 
#attitude and intention
plot(attitudemean,intmean)
#social norms and intention
plot(Nsmean,intmean)
#controlled planned behaviour and intention
plot(pcbmean,intmean)

plot(attitudemean,Nsmean)
plot(Nsmean,pcbmean)
plot(pcbmean,attitudemean)

#les hypothèses de recherche:

#h1: il y a association significative entre l'attitude et l'intention entreprenariale (--------------------not rejected----------)
#h2: il y a association significative entre les normes sociales et l'intention entreprenariale(----------------rejected--------------)
#h3: il y a association significative entre la perception  et l'intention entreprenariale(--------------------not rejected----------)
#h4:il y a association significative entre le sexe et l'intention entreprenariale(---------------rejected---------------)
#h5: between social norms and attitude (-------------rejected-----------------)
#h6: social norms and pcb(---------------rejected---------------)
#h7: pcb and attitude (---------------not rejected ---------------)


#correlation tests

#the variable attitudemean does not follow the normal distribution ->we use non parametric tests
cor.test(intmean,attitudemean,method = "spearman",exact = FALSE )
#Conclusion <=> p-value=5.865e-13 <5% => H1 : there is a significant association between the entrepreneurial 
#                                   intention and the attitude towards entrepreneurship

#the variable Nsmean does  follow the normal distribution ->we use parametric tests
cor.test(intmean,Nsmean)
#Conclusion <=> p-value=0.7813 >5% => H0 : there is no significant association between the entrepreneurial 
#                                   intention and social norms


#the variable pcbmean does  follow the normal distribution ->we use parametric tests
cor.test(intmean,pcbmean)
#Conclusion <=> p-value=5.004e-05 <5% => H1 : there is a significant association between the entrepreneurial 
#                                   intention and percieved controlled behaviour



cor.test(attitudemean,Nsmean) 
#Conclusion <=> p-value=0.9935 >5% => H0 : there is no significant association between the social norms 
#                                    and the attitude towards entrepreneurship

cor.test(Nsmean,pcbmean)
#Conclusion <=> p-value=0.7877 >5% => H0 : there is no significant association between the social norms 
#                                    and percieved controlled behaviour

cor.test(pcbmean,attitudemean)
#Conclusion <=> p-value=0.003685 <5% => H1 : there is a significant association between percieved controlled behaviour
#                                  and the attitude towards entrepreneurship




t.test(intmean~data$genre,var.equal = TRUE)#p-value = 0.9024
wilcox.test(intmean~data$genre)#p-value = 0.9774
#Conclusion <=>#p-value = 0.902 > 5% => H0 :#there is no significant association beetween 
#                                       gender and the entrepreneurial intention

 

#filiere quali with more than 3 modalities
library(car)
ana.test(data$filière,intmean)
  #Conclusion <=> p-value = 0.7504>5% => H0 : The fillière and intention are indepandant
      #==>the field doesn't affect the intention

#----------------------------------statistical model---------------------------------#
#since intention does not follow the normal distribution, we can't have a regression model
