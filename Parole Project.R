setwd("~/Soph 2nd Sem/STAT 474/CART stuff")
load('Paper2data.rdata')
working1<-na.omit(Paper2Data)
##stays the same, apparently there are no NAs in this data, because running the na.omit() function on working yields an object with the same number of obs. as Paper2Data
summary(working1)
summary(as.Date(working1$DOB,origin="01-01-1970"))
zeroadultpriors<-subset(working1,working1$AdultPriors==0)
zerojuvpriors<-subset(working1,working1$JuvenilePriors==0)
zeroiViolcount<-subset(working1,working1$iViolCount==0)
summary(zeroadultpriors)
summary(zerojuvpriors)
summary(zeroiViolcount)

jim<-duplicated(working1)
length(jim)
working<-unique(working1)

summary(as.Date(working$DOB,origin="01-01-1970"))
summary(working)


##almost 5 to 1 male
##################
#question 2 making histograms
hist(working$Fail) #how to make this work/what to do with categorical data?
#used as.numeric($Fail), interestingly same thing comes up if you run it you get more values than there were observations
#how to specify what the factors turn into when converted to numeric
#BUTTT I can now make histograms. So I don't get wtf i would do with factors, figure this out
table(working$Fail,dnn='fails indicated by 1s')
hist(as.Date(working$DOB,origin="1970-01-01"),breaks=50)
hist(working$AdultPriors,breaks=50)
hist(working$JuvenilePriors,breaks=50)
morethan1juvprior<-subset(working,working$JuvenilePriors>=1)
hist(morethan1juvprior$JuvenilePriors,breaks=50)
hist(working$iViolCount,breaks=50)
morethan1iViol<-subset(working,working$iViolCount>=1)
hist(morethan1iViol$iViolCount,breaks=50)
hist(working$Male) #how to make this work?
table(working$Male,dnn='males indicated by 1s')


lessthan18<-subset(working,as.Date(working$DOB,origin="1970-01-01")>"1985-01-01")
summary(lessthan18)
lessthan17<-subset(working,as.Date(working$DOB,origin="1970-01-01")>"1986-01-01")
hist(as.Date(shitforandrew$DOB,origin="1970-01-01"),breaks=50)
##################
#question 3 looking at associations between all these
FailandMale=table(working$Fail,working$Male)
prop.table(FailandMale)
tapply(working$DOB,working$Fail,summary)
tapply(working$AdultPriors,working$Fail,summary)
tapply(working$JuvenilePriors,working$Fail,summary)
tapply(working$iViolCount,working$Fail,summary)
#now for some histograms go back and change dimensions to fit four on a page. 
Fail.subset<-subset(working,working$Fail==1)
NonFail.subset<-subset(working,working$Fail==0)
##DOB

hist(as.Date(Fail.subset$DOB,origin="1970-01-01"),breaks=50)
hist(as.Date(NonFail.subset$DOB,origin="1970-01-01"),breaks=50)
##Adult Priors
hist(Fail.subset$AdultPriors,breaks=50)
hist(NonFail.subset$AdultPriors,breaks=50)
##Juvenile Priors
hist(Fail.subset$JuvenilePriors,breaks=50)
hist(NonFail.subset$JuvenilePriors,breaks=50)
##iViol Count
hist(Fail.subset$iViolCount,breaks=50)
hist(NonFail.subset$iViolCount,breaks=50)
summary(Fail.subset)
summary(NonFail.subset)
hist(working$Male) #how to make this work?
table(working$Male,dnn='males indicated by 1s')
##################
#question 4 tree time
Index<-sample(1:43809,replace=F) # Get row numbers in random order
working2<-working[Index,] # re-order rows of data randomly
train<-working2[1:14603,] # training sample
eval<-working2[14604:29206,] # evaluation sample
test<-working2[29207:43809,]

library(rpart)
out1<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100)
plot(out1)
text(out1,use.n=T)

fitted1=predict(out1,type="class",newdata=eval) #calls everybody a zero like we'd expect 
table(eval$Fail,fitted1) # Within sample Construct Confusion Table
prop.table(table(eval$Fail,fitted1),1) # Compute model error
prop.table(table(eval$Fail,fitted1),2) #Compute use error
#also what the hell is a factor
##doesn't get beyond root node. Okay, change minbucket to be 50, denoted by out1.50
out1.50<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=50)
plot(out1.50)
text(out1.50,use.n=T)
##50 doesn't work either, try minbucket=10
out1.10<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
               data=working, method="class",minbucket=10)
plot(out1.10)
text(out1.10,use.n=T)
##yeah if 10 doesn't work that's about it
##write about why this occurs and what it means


####################
#question 5, altering the prior
### pi1 is the prior for fail, NewL1 is its new cost,
### pi2 is the prior for not fail(ie neg class), NewL2 is its new cost.
### The "prior" is the marginal distribution of the response
### here about .1653704 for fail(the positive class) and .8346296 for not fail (the negative class).

### Construct the function
NewPrior<-function(pi1,NewL1,pi2,NewL2)
{
  (pi1*NewL1)/((pi1*NewL1) + (pi2*NewL2))
}

### Use the function
NewPrior(7364/43809,4,36445/43809,1)  # For the admits class, .442134 for 4 to 1 loss
#if we're increasing the costs of missing one, that prior is increasing
out2<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.56,.44))) 
# .44 is new prior for the positive class. It yields new prior for the positives, this should increase as we increase costs
plot(out2,margin=.05)
text(out2,use.n=T)
fitted2<-predict(out2,type="class",newdata=eval)
table(eval$Fail,fitted2)
prop.table(table(eval$Fail,fitted2),1) # Compute model accuracy
prop.table(table(eval$Fail,fitted2),2)
#this makes just above 1:1 cost ratio
#now 8 to 1 cost
NewPrior(7364/43809,8,36445/43809,1) 
out3<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.383,.617)))
plot(out3,margin=.05)
text(out3,use.n=T)
fitted3<-predict(out3,type="class",newdata=eval)
table(eval$Fail,fitted3)
prop.table(table(eval$Fail,fitted3),1)
prop.table(table(eval$Fail,fitted3),2)

##5:1
NewPrior(7364/43809,5,36445/43809,1) 
out4<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.498,.502)))
plot(out4,margin=.05)
text(out4,use.n=T)
fitted4<-predict(out4,type="class",newdata=eval)
table(eval$Fail,fitted4)
prop.table(table(eval$Fail,fitted4),1)
prop.table(table(eval$Fail,fitted4),2)

#trying 5.2 
NewPrior(7364/43809,5.2,36445/43809,1)
out5<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(0.488,.512)))
plot(out5,margin=.05)
text(out5,use.n=T)
fitted5<-predict(out5,type="class",newdata=eval)
table(eval$Fail,fitted5)
prop.table(table(eval$Fail,fitted5),1)
prop.table(table(eval$Fail,fitted5),2)

##trying 5.5
NewPrior(7364/43809,5.5,36445/43809,1)
out6<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(0.474,.526)))
plot(out6,margin=.05)
text(out6,use.n=T)
fitted6<-predict(out5,type="class",newdata=eval)
table(eval$Fail,fitted6)
prop.table(table(eval$Fail,fitted6),1)
prop.table(table(eval$Fail,fitted6),2)

##Trying 5.7
NewPrior(7364/43809,5.7,36445/43809,1)
out7<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.465,.535)))
plot(out7,margin=.05)
text(out7,use.n=T)
fitted7<-predict(out7,type="class",newdata=eval)
table(eval$Fail,fitted7)
prop.table(table(eval$Fail,fitted7),1)
prop.table(table(eval$Fail,fitted7),2)

NewPrior(7364/43809,5.6,36445/43809,1)
out8<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.469,.531)))
plot(out8,margin=.05)
text(out8,use.n=T)
fitted8<-predict(out8,type="class",newdata=eval)
table(eval$Fail,fitted8)
prop.table(table(eval$Fail,fitted8),1)
prop.table(table(eval$Fail,fitted8),2)

NewPrior(7364/43809,5.1,36445/43809,1)
out9<-rpart(Fail~DOB+AdultPriors+JuvenilePriors+iViolCount+Male,
            data=train, method="class",minbucket=100,parms=list(prior=c(.493,.507)))
plot(out9,margin=.05)
text(out9,use.n=T)
fitted9<-predict(out9,type="class",newdata=eval)
table(eval$Fail,fitted9)
prop.table(table(eval$Fail,fitted9),1)
prop.table(table(eval$Fail,fitted9),2)

#this makes it jump to about 40 to 1 cost ratio, lil off
#fiddle wit hthis more
##################### 
#question 6 test data
fittedtest<-predict(out5,type="class",newdata=test)
table(test$Fail,fittedtest)
prop.table(table(test$Fail,fittedtest),1)
prop.table(table(test$Fail,fittedtest),2)

 
