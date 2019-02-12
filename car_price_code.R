getwd()
car <- read.csv("~/Desktop/car.txt", sep="")
is.fact <- sapply(car, is.factor)
carq <- car[,is.fact] #on récupère les variables qualitatives
set.seed(99)
attach(car)


### PARTIE I Stats descriptives ###
boxplot((car))
par(mfrow = c(2,3))
pct <- round(table(fuelType)/193*100)
lbls=c("gaz","Diesel")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(fuelType),labels = lbls, col=rainbow(length(lbls)),
    main="Type d'essence")

pct <- round(table(aspiration)/193*100)
lbls=c("std","Turbo")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(aspiration),labels = lbls, col=rainbow(length(lbls)),
    main="Aspiration")

pct <- round(table(numOfDoors)/193*100)
lbls=c("quatre","Deux")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(numOfDoors),labels = lbls, col=rainbow(length(lbls)),
    main="Nombre de Portes")
pct <- round(table(bodyStyle)/193*100)
lbls=c("convertible","Hardtop","hatchback","sedab","wagon")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(bodyStyle),labels = lbls, col=rainbow(length(lbls)),
    main="BodyStyle")
summary(price)
pct <- round(table(numOfDoors)/193*100)
lbls=c("4wd","fwd","rwd")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(driveWheels),labels = lbls, col=rainbow(length(lbls)),
    main="La roue motrice")

barplot(table(make),col=blues9)
table(make)
pct <- round(table(engineLocation)/193*100)
lbls=c("front","rear")
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(engineLocation),labels = lbls, col=rainbow(length(lbls)),
    main="L'emplacement du moteur")

boxplot(price,horizontal = TRUE, main="Prix de la voiture"))

library(gclus)
dta.r <- abs(cor(carq)) # donne les correlations
dta.col <- dmat.color(dta.r) # pour les couleurs
#reorganise les variables pour que les plus fortes correlations soient près de la diagonale :
dta.o <- order.single(dta.r) 
cpairs(carq, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )
par(mfrow = c(2,2))
boxplot(wheelBase~fuelType,main="WheelBase selon le type de fuel")
boxplot(width~bodyStyle,main="Largeur selon le style de voiture")
boxplot(peakRpm~aspiration,main="Pick de puissance selon l'aspiration")
boxplot(curbWeight~numOfDoors, main="Poids de la corrosserie selon le nombre de porte")

### PARTIE II : etude sur jeu de donnee quantitatif
#On centre et on reduit les donnees quantitatives pour avoir les memes variances et ordres de grandeurs :
cols3 = c("wheelBase","length","width","height","curbWeight","engineSize","bore","stroke","compressionRatio","horsepower","peakRpm","cityMpg","highwayMpg","price" )
for(i in cols3){
  carq[,i]=scale(carq[,i])
}


#Creation echantillon test et apprentissage 75/25 :
x = carq[ ,1:13] ; y = as.numeric(carq[,14])
D=cbind(x,y) ; n = nrow(carq)
p=0.75; n=dim(D)[1]; n1=floor(n*p); n2= n-n1
ind=sample(1:n,replace=FALSE);
ind1=ind[1:n1]; ind2=ind[(n1+1):n]
length(ind1)
Dtrain=D[ind1,]
Dtest=D[ind2,]
Dtrain

#II.1 regression lineaire

modele1=lm(log(abs(y))~.,data=Dtrain)
step(modele1) #critere AIC de selection de variable methode forward
summary(modele1)
modele2=lm(log(abs(y))~wheelBase + length + width + curbWeight + engineSize + 
             bore + stroke
           ,data=carq) #Avec AIC le plus petit
summary(modele2)
plot(modele2)


#Calcul de l'erreur de prediction sur l'echantillon test
sqrt(sum((Dtrain$y - predict(modele2,newdata = Dtrain))^2))*1/length(predict(modele2,newdata = Dtrain)) #erreur de prediction SOMME(y.i-y.hat)^2

#II.2 Ridge
library(glmnet)
#on ajuste une régression ridge pour chaque valeur de la séquence :
x=as.matrix(Dtrain)[,-1];y=as.matrix(Dtrain)[,1]
fit.ridge =glmnet(x,y)
plot(fit.ridge)
##nbre effectif de paramètres
df.lbd = c()
X=scale(x); p = ncol(X)
lbd = 10^seq(-2,-6,length = 20)
for(i in 1:length(lbd))
  df.lbd[i] = sum(diag(X%*%solve(t(X)%*%X+lbd[i]*diag(1,p))%*%t(X)))
Df = matrix(rep(df.lbd,p),ncol=p)
Df

##profil des coefficients
cf.r=coef(fit.ridge)[-1,]; cf.r[,1];cf.r[,15]
q = ncol(cf.r) #nbre dimplementations ridge abouties
q
matplot(Df[1:q,],t(cf.r),type="b",xlab = "Degrees of freedom",
        ylab="Coefficients")
#y en fonction de 4 variable
#cete methode va nous selectionner les 4 meilleurs
modeler=lm(log(abs(y))~length+width+height,data=Dtrain)
summary(modeler)
plot(modeler)

#Calcul de l'erreur de prediction sur l'echantillon test
sqrt(sum((Dtrain$y - predict(modeler,newdata = Dtrain))^2))*1/length(predict(modeler,newdata = Dtrain)) #erreur de prediction SOMME(y.i-y.hat)^2

pred=predict(modeler)
norm=sum((Dtest$price-pred))^2
norm

#II.2 Lasso

D=as.matrix(Dtrain)
p=0.75; n=dim(Dtrain)[1]; ntrain=floor(n*p); ntest= n-ntrain
ind=sample(1:n,n); ind_train=ind[1:ntrain]; ind_test=ind[(ntrain+1):n]
#Dtrain=D[ind_train,] ; Dtest=D[ind_test,]
ytrain = y[ind_train] ; ytest = y[ind_test]
xtrain= x[ind_train,] ; xtest = x[ind_test,]
xtrain

length(ytrain)
dim(xtrain)
?glmnet
lbd = c(10^seq(-2,-4,length = 20),0)

lasso.fit=glmnet(xtrain,ytrain,alpha=1,lambda =lbd,intercept=FALSE)

#validation croisee :
cv.out = cv.glmnet(xtest,ytest,alpha=1,lambda = lbd, intercept = FALSE)
plot(cv.out)
coef(lasso.fit)
bestlam=cv.out$lambda.min
fit.best = glmnet(xtest,ytest,alpha=1,lambda=bestlam)
min((ytest-predict(fit.best, newx=xtest))^2)/ntest

#II.3 Arbre de decision
library(rpart)
#en multi
treem1=rpart(y~.,data=Dtrain)
treem1
plot(treem1)
text(treem1) #au debut les variabbles qui decrivent le plus l heterogeneité
plotcp(treem1)

#comment les comparer pour la regression
#on peut utiliser le summary
summary(treem1)
#a comparer avec une reg classique de lm pour voir les aic et les p valeur
mod0=lm(price~1, data=carq)

summary(mod0)
names(carq)
step(mod0,price~wheelBase+length+width+height+curbWeight+engineSize+bore+stroke+horsepower+peakRpm+cityMpg+highwayMpg,data=carq, direction= "forward")
#evite de copier toutes les variables
printcp(treem1) 
#estimation de l erreur par validation croisée
#relative erreur par rapport 
#x erreur 0,4849/1,005 = 0.4543 (45.43%)
#dapres le plot : par validation croisée : (avec 0,01 = cp)
#au moment ou ça stagne je prends...
#regarder l ecart type std
0.034777+0.3582 #on a donc 0.39  et cela donne un cp à 0.025

#larbre optimal
#lui il va elaguer l arbre quon lui donne
#pas necessaire de prendre un arbre trop grand

treeopt=rpart(scale(price)~.,data=carq,cp=0.054)
plot(treeopt, main="Arbre opptimal")
text(treeopt)

#Calcul de l'erreur de prediction sur l'echantillon test
sqrt(sum((Dtrain$y - predict(treeopt,newdata = Dtrain))^2))*1/length(predict(treeopt,newdata = Dtrain)) #erreur de prediction SOMME(y.i-y.hat)^2

#II.4 random forest 

library(randomForest)
rf.p=randomForest(scale(y)~.,data = Dtrain, xtest = Dtest[,-14],
                  ytest = Dtest[,14],ntree=500,do.trace=50,importance=TRUE,keep.forest=TRUE)
rf.p
hist(rf.p$oob.times)
rf.p$importance
varImpPlot(rf.oz)
print(rf.p)
margin(rf.p)
summary(rf.p)
pred.rf1 = rf.p$test$predicted
pred.rf1
hist(rf.p$oob.times)

#Calcul de l'erreur de prediction sur l'echantillon test
sqrt(sum((Dtrain$y - predict(rf.p,newdata = Dtrain))^2))*1/length(predict(rf.p,newdata = Dtrain)) #erreur de prediction SOMME(y.i-y.hat)^2

#II.5 reseau d eneurone

nnet.reg=nnet(scale(y)~.,data=Dtrain,size=5,decay=1,
              linout=TRUE,maxit=500)
summary(nnet.reg)
library(e1071)
plot(tune.nnet(scale(y)~length+width+height+engineSize+bore+stroke+compressionRatio+cityMpg,data=Dtrain,size=c(2,3,4),
               decay=c(1,2,3),maxit=200,linout=TRUE))

nnet.reg=nnet(scale(y)~.,data=Dtrain,size=3,decay=2,linout=TRUE,maxit=200)
fit.nnetr=predict(nnet.reg,data=Dtrain)
res.nnetr=fit.nnetr-Dtrain[,"y"]


summary(nnet.reg) 
names(nnet.reg)
nbnet.reg$value

#optimisation du reseau :
nnet.cv = tune.nnet(y~.,data=Dtrain,size=c(2,3,4),decay=c(1,2,3),maxit=200,linout=TRUE)
nnet.cv; nnet.cv$performances; 
#la validation croise pour calculer estimation d'erreuir de pred
#sur l train on choisi le meilleur lambda et sur test on utilise predict
#plus c'est blanc plus lerreur est importante et elle est plus petite quand ça devient foncé
nnet.reg=nnet(y~.,data=Dtrain,size=4,decay=1,linout=TRUE,maxit=200)
plot(nnet.cv)


sqrt(sum((Dtrain$y - predict(nnet.reg,newdata = Dtrain))^2))*1/length(predict(nnet.reg,newdata = Dtrain)) #erreur de prediction SOMME(y.i-y.hat)^2

### III Etude sur jeu de donnes quali/quanti

#Convertion variables qualitatives en factor
head(car)
cols = c("make","fuelType","aspiration","numOfDoors","bodyStyle","driveWheels","engineLocation","engineType","numOfCylinders","fuelSystem")
for(i in cols){
  car[,i]=as.factor(car[,i])
}
str(car)

#On centre et on reduit les donnees quantitatives pour avoir les memes variances et ordres de grandeurs :
cols2 = c("wheelBase","length","width","height","curbWeight","engineSize","bore","stroke","compressionRatio","horsepower","peakRpm","cityMpg","highwayMpg","price" )
for(i in cols2){
  car[,i]=scale(car[,i])
}

### Creation echantillon d'apprentissage contenant 75% des donnees et de l'echantillon test:
p = 0.75; n =  dim(car)[1]
ntrain = floor(n*p)
ntrain
ntest = n - ntrain
ind = sample(1:n,n)
ind_train=ind[1:ntrain]; ind_test=ind[(ntrain+1):n]
car_train = car[ind_train,]
car_test=car[ind_test,]





#III 1. Modele CART
library(rpart)
?rpart
cart.price<-rpart(car_train[,24]~.,car_train[,1:23],method="anova") #methode anova car la reponse est de type numeric
cart.price
sqrt(sum((car_test$price - predict(cart.price,newdata=car_test[,1:23]))^2))*1/length(predict(cart.price,newdata=car_test[,1:23])) #erreur de prediction
#Visualisation de l'arbre
plot(cart.price)
text(cart.price)
#idem avec package rparplot
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(cart.price,type=3,digits = 3, fallen.leaves = TRUE)
#Choix du meilleur Cp
plotcp(print(cart.price))
Cp = seq(0,1,length = 100)
for (i in Cp) {
  print(sqrt(sum((car_test$price - predict(rpart(car_train[,24]~.,car_train[,1:23],method="anova",cp=i),newdata=car_test[,1:23]))^2))*1/length(predict(rpart(car_train[,24]~.,car_train[,1:23],method="anova",cp=i),newdata=car_test[,1:23]))) #erreur de prediction
}
#L'erreur de prediction la plus petite semble etre pour Cp = 0, attention au surajustement
printcp(cart.price) #Cependant le meilleur cp au sens de X-val Relative Error est pour cp = 0.08
cart.price<-rpart(car_train[,24]~.,car_train[,1:23],method="anova",cp=0.08)
#Representation de l'arbre:
plot(cart.price)
text(cart.price)

rpart.plot(cart.price,type=3,digits = 1, fallen.leaves = TRUE)

#erreur de prediction:
sqrt(sum((car_test$price - predict(cart.price,newdata=car_test[,1:23]))^2))*1/length(predict(cart.price,newdata=car_test[,1:23])) 

#III.2 Modele random forest
library(randomForest)
dim(car_train)
?randomForest
rf.price = randomForest(price~.,data = car_train, xtest = car_test[,-24],
                         ytest = car_test[,24],ntree=500,do.trace=50,importance=TRUE,type=regression,keep.forest=TRUE)
print(rf.price)
attributes(rf.price)
plot(rf.price) #Evolution de l'erroeur en fonction du nombre d'arbres
#On choisi le meilleur mtry, c'est a dire le nombre de variables selectionnees aleatoirement 
best_mtry=tuneRF(car_train[,-24],car_train[,24], stepFactor = 0.5, plot=TRUE,ntreeTry = 100,trace = TRUE,improve = 0.05)
best_mtry

#RF avec mtry = 7 (best mtry)
rf.price = randomForest(price~.,data = car_train, xtest = car_test[,-24],
                        ytest = car_test[,24],ntree=500,do.trace=50,importance=TRUE,type=regression,keep.forest=TRUE,mtry=7)

#Calcul de l'erreur de prediction
sqrt(sum((car_test$price - predict(rf.price,newdata = car_test))^2))*1/length(predict(rf.price,newdata = car_test)) #erreur de prediction SOMME(y.i-y.hat)^2

#Contributions des variables. 
#On peut mesurer l’importance des variables en calculant la contribution de chaque variable à la décroissance moyenne du MSE :
sort(round(importance(rf.price),2)[,1])
varImpPlot(rf.price,main="Average Importance plots") #Les variables les plus importantes semblent etre : "numOfCylinders" , "wheelBase", "fuelSystem", "width" , "length" , "highwayMpg" , "cityMpg" , "horsepower" , "curbWeight" ,"engineSize" ,"make"

#III.3 Reseau de neurone
library(nnet)
nnet.price=nnet(price~.,data=car_train,size=5,decay=1,linout=TRUE,maxit=500)
summary(nnet.price)
library(e1071)
nnet.price.cv = tune.nnet(price~.,data=car_train,size=c(1,2,3,4,5),decay=c(1,2,3,4,5),
                                                maxit=200,linout=TRUE)
nnet.price.cv; nnet.price.cv$performances
plot(nnet.price.cv) #optimisation des paramètres (pénalisation et nombre de neurones) par validation croisée.
#Best parameters : size = 3, decay =1
nnet.price.best = nnet(price~.,data=car_train,size=3,decay=1,linout=TRUE,maxit=500)

#Calcul de l'erreur de prediction (modele decay =1, size =3) sur l'echantillon test
sqrt(sum((car_test$price - predict(nnet.price.best,newdata = car_test))^2))*1/length(predict(nnet.price.best,newdata = car_test)) #erreur de prediction SOMME(y.i-y.hat)^2

#plot du reseau de neurone choisi :
install.packages("clusterGeneration")
install.packages("devtools")
library(clusterGeneration)
library(nnet)
library(devtools)
#fonction depuis Github
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
pdf('./nn-example.pdf', width = 7, height = 7)
library(scales)
install.packages("reshape")
library(reshape)
plot.nnet(nnet.price.best) #representation du reseau choisi
dev.off()







