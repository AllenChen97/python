#rm(list=ls())
#setwd("./case")

# library(xlsx)
# dat<-read.xlsx2("credit.xlsx",1,colClasses = NA)
library(openxlsx)
dat<-read.xlsx("credit.xlsx",1)
summary(dat)
#999替换缺失值
dat[,1:6]<-lapply(dat[,1:6],
                  function(x) {x[x==999]<-NA;return(x)} )
#去掉最后一列
dat<-dat[,-11]

library(smbinning)
library(prettyR)

#生成新列——婚姻状况列
dat1<-dat
dat1[,4]<-dat1[,4]-dat1[,3]
table(dat1[,4])
dat1[,4]<-factor(dat1[,4],levels=c(1,2),labels=c("其他","已婚"))
#重命名列
colnames(dat1)<-c("age","income","child","marital","dur_live",
                  "dur_work","housetype","nation","cardtype","loan")
summary(dat1)
dat1[,c(4,7,8,9)]<-lapply(dat1[,c(4,7,8,9)],as.factor)

##盖帽法函数
block<-function(x,lower=T,upper=T){
    if(lower){
        q1<-quantile(x,0.01)
        x[x<=q1]<-q1
    }
    if(upper){
        q99<-quantile(x,0.99)
        x[x>q99]<-q99
    }
   return(x)
}
#y中0和1互换
dat1$loan<-as.numeric(!as.logical(dat1$loan))

##对每个变量分箱，求IV
##age
boxplot(age~loan,data=dat1,horizontal=T, frame=F, 
        col="lightgray",main="Distribution")
age<-smbinning(dat1,"loan","age")
age$ivtable
par(mfrow=c(2,2))
smbinning.plot(age,option="dist",sub="年龄")
smbinning.plot(age,option="WoE",sub="年龄")
smbinning.plot(age,option="goodrate",sub="年龄")
smbinning.plot(age,option="badrate",sub="年龄")
par(mfrow=c(1,1))
age$iv
cred_iv<-c("年龄"=age$iv)

##income
boxplot(income~loan,data=dat1,horizontal=T, frame=F, 
        col="lightgray",main="Distribution")
dat1$income<-block(dat1$income)
boxplot(income~loan,data=dat1,horizontal=T, frame=F, 
        col="lightgray",main="Distribution")
income<-smbinning(dat1,"loan","income")
income$ivtable
smbinning.plot(income,option="WoE",sub="收入")
income$iv
cred_iv<-c(cred_iv,"收入"=income$iv)

##child
boxplot(child~loan,data=dat1,horizontal=T, frame=F, 
        col="lightgray",main="Distribution")
dat1$child<-block(dat1$child)
child<-smbinning(dat1,"loan","child")
child$ivtable
smbinning.plot(child,option="WoE",sub="孩子数量")
child$iv
cred_iv<-c(cred_iv,"孩子数量"=child$iv)

##marital
xtab(~marital+loan,data=dat1,chisq=T)
marital<-smbinning.factor(dat1,"loan","marital")
marital$ivtable
smbinning.plot(marital,option="WoE",sub="婚姻状态")
marital$iv
cred_iv<-c(cred_iv,"婚姻状态"=marital$iv)

##dur_live
boxplot(dur_live~loan,data=dat1,horizontal=T, 
        frame=F, col="lightgray",main="Distribution")
t.test(dur_live~loan,data=dat1)
dur_live<-smbinning(dat1,"loan","dur_live")
dur_live

##dur_work
boxplot(dur_work~loan,data=dat1,horizontal=T, 
        frame=F, col="lightgray",main="Distribution")
t.test(dur_work~loan,data=dat1)
dur_work<-smbinning(dat1,"loan","dur_work")
dur_work$ivtable
smbinning.plot(dur_work,option="WoE",sub="在现工作时间")
dur_work$iv
cred_iv<-c(cred_iv,"在现工作时间"=dur_work$iv)

##housetype
xtab(~housetype+loan,data=dat1,chisq=T)
#对分类变量最优分箱
housetype<-smbinning.factor(dat1,"loan","housetype")
housetype$ivtable
smbinning.plot(housetype,option="WoE",sub="住房类型")
housetype$iv
cred_iv<-c(cred_iv,"住房种类"=housetype$iv)

##nation
xtab(~nation+loan,data=dat1,chisq=T)
nation<-smbinning.factor(dat1,"loan","nation")
nation$ivtable
smbinning.plot(nation,option="WoE",sub="国籍")
nation$iv
cred_iv<-c(cred_iv,"国籍"=nation$iv)


##cardtype
xtab(~cardtype+loan,data=dat1,chisq=T)
cardtype<-smbinning.factor(dat1,"loan","cardtype")
cardtype$ivtable
smbinning.plot(cardtype,option="WoE",sub="信用卡类型")
cardtype$iv
cred_iv<-c(cred_iv,"信用卡类型"=cardtype$iv)


barplot(cred_iv,main="各变量信息值")
#生成分箱后的新列
dat2<-dat1
dat2<-smbinning.gen(dat2,age,"glage")
dat2<-smbinning.gen(dat2,income,"glincome")
dat2<-smbinning.gen(dat2,child,"glchild")
dat2<-smbinning.factor.gen(dat2,marital,"glmarital")
dat2<-smbinning.gen(dat2,dur_work,"gldur_work")
dat2<-smbinning.factor.gen(dat2,housetype,"glhousetype")
dat2<-smbinning.factor.gen(dat2,nation,"glnation")
dat2<-smbinning.factor.gen(dat2,cardtype,"glcardtype")

dat3<-dat2[,c(11:18,10)]

cred_mod<-glm(loan~. ,data=dat3,family=binomial())
summary(cred_mod)
coefficients(cred_mod)

#打分
cre_scal<-smbinning.scaling(cred_mod,pdo=45,score=800,odds=50)
cre_scal$minmaxscore
cre_scal$logitscaled

scaledcard<-cre_scal$logitscaled[[1]][-1,c(1,2,6)]
scaledcard[,1]<-c(rep("年龄",5),rep("收入",3),
                  rep("孩子数量",2),rep("婚否",2),rep("在现工作时间",5),
                  rep("住房类型",3),rep("国籍",8),rep("信用卡类型",7))
scaledcard
write.csv(scaledcard,"card.csv",row.names = F)

dat4<-smbinning.scoring.gen(smbscaled=cre_scal, dataset=dat3)

smbinning.metrics(dat4,"Score","loan",plot="auc")
smbinning.metrics(dat4,"Score","loan",plot="ks")

#生成每行对应的分数

boxplot(Score~loan,data=dat4,horizontal=T, frame=F, 
        col="lightgray",main="Distribution")
abline(v=540)
text(x=540,y=1.5,labels = "最大ks:0.334,对应分数:540",pos=4)



