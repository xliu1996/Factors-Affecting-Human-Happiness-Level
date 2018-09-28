projdata=read.table("/Users/xiaokailiu/Desktop/UCSB/2018 Spring PSTAT 126  /Project/projdata.txt",header=T)
head(projdata)
summary(projdata)

#Scatterplot
pairs(projdata)

#First order model 
attach(projdata)
model1 = lm(happy~gender+workhrs+relationship)
summary(model1)

#2-way interactions (Extra SS test):
model2 = lm(happy~.^2,data = projdata)
summary(model2)
anova(model1,model2)

#Interaction plot with separate regression line
#workhrs vs happy
plot(workhrs,happy,col="blue",pch=19,xlab="Work Hours",ylab="Happy")
points(workhrs[gender==1],happy[gender==1],col="red",pch=19)
abline(lm(happy[gender==0]~workhrs[gender==0]),col="blue")
abline(lm(happy[gender==1]~workhrs[gender==1]),col="red")
legend("topleft", inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c("Female","Male"))

#relationship vs happy
plot(relationship,happy,col="blue",pch=19,xlab="Relationship",ylab="Happy")
points(relationship[gender==1],happy[gender==1],col="red",pch=19)
abline(lm(happy[gender==0]~relationship[gender==0]),col="blue")
abline(lm(happy[gender==1]~relationship[gender==1]),col="red")
legend("topleft", inset=.05,cex=.75,pch=19,lty=c(1,1),col=c("red","blue"),legend=c("Female","Male"))


#Final Model
FinalModel =  lm(happy~relationship + gender + workhrs + relationship*gender, data = projdata)
summary(FinalModel)

#residual plot for FinalModel
plot(residuals(FinalModel), main = 'Residuals Plot')

#fitted residual plot for FinalModel
plot(fitted(FinalModel),residuals(FinalModel),main = 'Fitted Residuals Plot ')
abline(h=0)

#QQ plot
qqnorm(residuals(FinalModel))
qqline(residuals(FinalModel))

#histogram
hist(residuals(FinalModel), breaks = 30)

#Time order residual plot
plot(1:100, residuals(FinalModel),main='Time Order Residual Plot')
abline(h=0)

#Model selection
null = lm(happy~1,data = projdata)
full = lm(happy~.^2,data = projdata)
step(full,direction  ='backward')
step(null,scope = list(lower = null, upper = model2), direction = 'forward')
step(null,scope = list(lower = null, upper = model2), direction = 'both')

