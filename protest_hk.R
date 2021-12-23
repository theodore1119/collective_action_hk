library(haven)
library(dplyr)
library(MASS)
library(reshape2)
library(brant)
library(psych)
library(ggplot2)
library(waffle)
library(jtools)
library(effects)
library(ExtremeBounds)
library(sandwich)

WV7_Data <- read_dta("C:/Users/theod/Desktop/dissertation prospectus/postmaterialism/WV7_Data.dta")
## WVS Wave 7 HK ##
hk<-filter(WV7_Data, B_COUNTRY==344)

## Define protest, which represents the propensity to protest ##
hk$protest<-NA
hk$protest[which(hk$Q211==3)]<-0
hk$protest[which(hk$Q211==1 | hk$Q211==2)]<-1
hk$protest2<-NA
hk$protest2[which(hk$Q211==3)]<-1
hk$protest2[which(hk$Q211==2)]<-2
hk$protest2[which(hk$Q211==1)]<-3
hk$protest<-as.factor(hk$protest)
hk$protest2<-as.factor(hk$protest2)
table(hk$protest)
table(hk$protest2)

## Define boycott, which represents the propensity to join in boycotts ##
hk$boycott<-NA
hk$boycott[which(hk$Q210==3)]<-0
hk$boycott[which(hk$Q210==1 | hk$Q210==2)]<-1
hk$boycott2<-NA
hk$boycott2[which(hk$Q210==3)]<-1
hk$boycott2[which(hk$Q210==2)]<-2
hk$boycott2[which(hk$Q210==1)]<-3
hk$boycott<-as.factor(hk$boycott)
hk$boycott2<-as.factor(hk$boycott2)
table(hk$boycott)
table(hk$boycott2)

## Define strike, which represents the propensity to join strikes ##
hk$strike<-NA
hk$strike[which(hk$Q212==3)]<-0
hk$strike[which(hk$Q212==1 | hk$Q212==2)]<-1
hk$strike2<-NA
hk$strike2[which(hk$Q212==3)]<-1
hk$strike2[which(hk$Q212==2)]<-2
hk$strike2[which(hk$Q212==1)]<-3
hk$strike2<-as.factor(hk$strike2)
table(hk$strike)
table(hk$strike2)

## Define postmat, which represents the postmaterialism index for individual Hongkongers. 1=most materialist, 4=most postmaterialist ##
hk$postmat<-NA
hk$postmat[which((hk$Q154==1 & hk$Q155==3) | (hk$Q154==3 & hk$Q155==1))]<-0
hk$postmat[which((hk$Q154==1 & hk$Q155==2) | (hk$Q154==1 & hk$Q155==4) | (hk$Q154==3 & hk$Q155==2) | (hk$Q154==3 & hk$Q155==4) | (hk$Q154==2 & hk$Q155==1) | (hk$Q154==2 & hk$Q155==3) | (hk$Q154==4 & hk$Q155==1) | (hk$Q154==4 & hk$Q155==3))]<-1
hk$postmat[which((hk$Q154==2 & hk$Q155==4) | (hk$Q154==4 & hk$Q155==2))]<-2
table(hk$postmat)
round(prop.table(table(hk$postmat)),digits = 3)
mean(hk$postmat, na.rm = T)

## Define dissat, which represents dissatisfaction with the political system ##
hk$dissat<-NA
hk$Q252<- -1*(hk$Q252-11)
hk$dissat<-hk$Q252

## Define econdissat, which represents dissatisfaction with the finacial situation of his household ##
hk$econdissat<-NA
hk$Q50<- -1*(hk$Q50-11)
hk$econdissat<-hk$Q50

## Define polint, which is political interest ##
hk$polint<-NA
hk$Q199<- -1*(hk$Q199-5)
hk$polint<-hk$Q199
## Define poldis, which is political discussion ##
hk$poldis<-NA
hk$Q200<- -1*(hk$Q200-4)
hk$poldis<-hk$Q200

## Define organizational membership ##
hk$organ<-0
hk$organ[which(hk$Q94==1 | hk$Q95==1 | hk$Q96==1 | hk$Q97==1 | hk$Q98==1 | hk$Q99==1 | hk$Q100==1 | hk$Q101==1 | hk$Q102==1 | hk$Q103==1 | hk$Q104==1 | hk$Q105==1 | hk$Q94==2 | hk$Q95==2 | hk$Q96==2 | hk$Q97==2 | hk$Q98==2 | hk$Q99==2 | hk$Q100==2 | hk$Q101==2 | hk$Q102==2 | hk$Q103==2 | hk$Q104==2 | hk$Q105==2)]<-1
hk$organ<-as.factor(hk$organ)

## Define interpersonal trust ##
hk$pertrust<-NA
hk$Q59<--1*(hk$Q59-5)
hk$Q60<--1*(hk$Q60-5)
hk$Q61<--1*(hk$Q61-5)
hk$Q62<--1*(hk$Q62-5)
hk$Q63<--1*(hk$Q63-5)
hk$pertrust<-(hk$Q59+hk$Q60+hk$Q61+hk$Q62+hk$Q63)/5

## Define educ, which is education of respondent ##
hk$educ<-NA
hk$educ<-hk$Q275
hk$educ<-as.numeric(hk$educ)
## Define income, which is income of household ##
hk$income<-NA
hk$income<-hk$Q288
hk$income<-as.numeric(hk$income)
## Define age, which is age of respondent ##
hk$age<-NA
hk$age<-hk$Q262
hk$ln_age<-log(hk$age)
## Define gender of respondent. sex=0 is male, sex=1 is female ##
hk$sex<-NA
hk$Q260<-hk$Q260-1
hk$sex<-hk$Q260
hk$sex<-as.factor(hk$sex)
## Define single, which indicates if a respondent is single. single=0 is not single, single=1 if single ##
hk$single<-NA
hk$single[which(hk$Q273==1 | hk$Q273==2 | hk$Q273==3 | hk$Q273==4 | hk$Q273==5)]<-0
hk$single[which(hk$Q273==6)]<-1
hk$single<-as.factor(hk$single)

## Define postmat2, which represents the postmaterialism index for individual Hongkongers. 1=most materialist, 4=most postmaterialist ##
hk$postmat2<-NA
hk$postmat2[which((hk$Q152==1 & hk$Q153==2) | (hk$Q152==2 & hk$Q153==1))]<-0
hk$postmat2[which((hk$Q152==1 & hk$Q153==3) | (hk$Q152==1 & hk$Q153==4) | (hk$Q152==2 & hk$Q153==3) | (hk$Q152==2 & hk$Q153==4) | (hk$Q152==3 & hk$Q153==1) | (hk$Q152==4 & hk$Q153==1) | (hk$Q152==3 & hk$Q153==2) | (hk$Q152==4 & hk$Q153==2))]<-1
hk$postmat2[which((hk$Q152==3 & hk$Q153==4) | (hk$Q152==4 & hk$Q153==3))]<-2
table(hk$postmat2)
table(hk$Q152, hk$Q153)
## Define postmat3, which represents the postmaterialism index for individual Hongkongers. 1=most materialist, 4=most postmaterialist ##
hk$postmat3<-NA
hk$postmat3[which((hk$Q156==1 & hk$Q157==4) | (hk$Q156==4 & hk$Q157==1))]<-0
hk$postmat3[which((hk$Q156==1 & hk$Q157==2) | (hk$Q156==1 & hk$Q157==3) | (hk$Q156==4 & hk$Q157==2) | (hk$Q156==4 & hk$Q157==3) | (hk$Q156==2 & hk$Q157==1) | (hk$Q156==3 & hk$Q157==1) | (hk$Q156==2 & hk$Q157==4) | (hk$Q156==3 & hk$Q157==4))]<-1
hk$postmat3[which((hk$Q156==2 & hk$Q157==3) | (hk$Q156==3 & hk$Q157==2))]<-2
table(hk$postmat3)
table(hk$Q156, hk$Q157)

## Define dissat2, which represents confidence in the government ##
hk$dissat2<-NA
hk$dissat2<-hk$Q71
## Define dissat3, which represents confidence in the police ##
hk$dissat3<-NA
hk$dissat3<-hk$Q69

## Construct a dataset consisting only the relevant variables ##
fa<-as.data.frame(hk[,c("postmat","postmat2","postmat3","dissat","dissat2","dissat3")])
## Impute missing values with means ##
fa$postmat[is.na(fa$postmat)] <- mean(fa$postmat, na.rm = TRUE)
fa$postmat2[is.na(fa$postmat2)] <- mean(fa$postmat2, na.rm = TRUE)
fa$postmat3[is.na(fa$postmat3)] <- mean(fa$postmat3, na.rm = TRUE)
fa$dissat[is.na(fa$dissat)] <- mean(fa$dissat, na.rm = TRUE)
fa$dissat2[is.na(fa$dissat2)] <- mean(fa$dissat2, na.rm = TRUE)
fa$dissat3[is.na(fa$dissat3)] <- mean(fa$dissat3, na.rm = TRUE)

## Principal component analysis ##
pca<-princomp(fa)
summary(pca)
plot(pca)

## Exploratory factor analysis ##
library(psych)
fit <- factanal(fa, factors = 2, rotation="varimax")
print(fit, digits=2, cutoff=.4, sort=TRUE)
load <- fit$loadings[,1:2]
plot(load,type="n")
text(load,labels=names(fa),cex=.7)

## Confirmatory factor analysis ##
library(lavaan)
model<-'
DISSATIS = ~ dissat+dissat2+dissat3+postmat+postmat2+postmat3
'
fit2 <- cfa(model, data = fa)
summary(fit2,fit.measures=TRUE,standardized=TRUE)

model2<-'
POSTMATM = ~ postmat+postmat2+postmat3
DISSATIS = ~ dissat+dissat2+dissat3
'
fit3 <- cfa(model2, data = fa)
summary(fit3,fit.measures=TRUE,standardized=TRUE)

## Compute factor scores ##
fscore <- factanal(fa, factors = 2, rotation="varimax", scores = "regression")
fscore
head(fscore$scores)
factor<-as.data.frame(fscore$scores[,c("Factor1","Factor2")])


hk_dat<-hk[,c("postmat","postmat2","postmat3","pertrust","organ","dissat","dissat2","dissat3","econdissat","polint","poldis","educ","income","age","sex","single","ln_age","protest","protest2","boycott","boycott2","strike","strike2")]
hk_data<-cbind(hk_dat,factor)

## Waffle plot for the descriptive statistics ##
protest <- c("Would never do"=807,"Might do"=851, "Have done it before"=412)
postmat <- c("Materialist"=724,"Mixed values"=969, "Post-materialist"=365)
dissat <- c("1-2"=81,"3-4"=483,"5-6"=756,"7-8"=472,"9-10"=268)

waffle(protest/80, rows = 5, colors = c("#D8BFD8", "#FF00FF", "#800080"), xlab = "1 square ~ 80 respondents") +
  theme(legend.text = element_text(size=30))
waffle(postmat/80, rows = 5, colors = c("#D8BFD8", "#FF00FF", "#800080"), xlab = "1 square ~ 80 respondents") +
  theme(legend.text = element_text(size=30))
waffle(dissat/80, rows = 5, colors = c("#e7d7e7", "#c29ac2", "#9b5f9b", "#5e3a5e", "#150d15"), xlab = "1 square ~ 80 respondents") +
  theme(legend.text = element_text(size=30))

## Regressions for peaceful demonstrations ##
logit <- glm(protest ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
summary(logit)
ord <- polr(protest2 ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, Hess = TRUE)
summary(ord)
logit_2 <- glm(protest ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, family = "binomial")
summary(logit_2)
ord_2 <- polr(protest2 ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, Hess = TRUE)
summary(ord_2)

#Run the interaction
Inter.dissat <- effect('postmat*dissat', logit_2,
                       xlevels=list(dissat = c(1,2,3,4,5,6,7,8,9,10),
                                    postmat = c(0, 1, 2)),
                       se=TRUE, confidence.level=.95, given.values=c(organ1=0,sex1=1), typical=mean)

#Put data in data frame 
Inter.dissat <- as.data.frame(Inter.dissat)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.dissat)

#Create a factor of the IQ variable used in the interaction                   
Inter.dissat$postmat <- factor(Inter.dissat$postmat,
                               levels=c(0, 1, 2),
                               labels=c("Materialist", "Mixed values", "Post-materialist"))


ggplot(data=Inter.dissat, aes(x=dissat, y=fit, group=postmat))+
  geom_line(size=1.2, aes(color=postmat))+
  xlim(0,10)+
  ylim(0,1)+
  ylab("Probability of joining protests")+
  xlab("Dissatisfaction with the political system")+
  ggtitle("Interaction plot")+
  theme(text = element_text(size=16),
        plot.title = element_text(size = 20))+
  scale_color_discrete(name="Post-materialism index")

## Convert post-materialism to a categorical variable ##
logit <- glm(protest ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
hk_data$postmat<-factor(hk_data$postmat,levels=c(0,1,2),labels=c("Materialist", "Mixed values", "Post-materialist"))

## Plot the main effect of post-materialism ##
effect_plot(logit,postmat,interval = TRUE, x.label = "Post-materialism index", y.label = "Probability",
            main.title = "Predicted probability of joining protests") +
  ylim(0.3,1) +
  theme(text = element_text(size=18),
        plot.title = element_text(size=20))

# Plot the main effect of political dissatisfaction #
effect_plot(logit,dissat,interval = TRUE, x.label = "Dissatisfaction with the political system", y.label = "Probability",
            main.title = "Predicted probability of joining protests") +
  xlim(1,10) +
  ylim(0.3,0.8) +
  theme(text = element_text(size=18),
        plot.title = element_text(size=20))

## convert explanatory variables to categorical variables ##
hk_data$polint<-as.factor(hk_data$polint)
hk_data$poldis<-as.factor(hk_data$poldis)
hk_data$educ<-as.factor(hk_data$educ)


logit2 <- glm(protest ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
summary(logit2)
ord2 <- polr(protest2 ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, Hess = TRUE)
summary(ord2)
logit2_2 <- glm(protest ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, family = "binomial")
summary(logit2_2)
ord2_2 <- polr(protest2 ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, Hess = TRUE)
summary(ord2_2)


logit_fa <- glm(protest ~ Factor2 + Factor1 + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
summary(logit_fa)
ord_fa <- polr(protest2 ~ Factor2 + Factor1 + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, Hess = TRUE)
summary(ord_fa)

logit_bo <- glm(boycott ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
summary(logit_bo)
ord_bo <- polr(boycott2 ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, Hess = TRUE)
summary(ord_bo)
logit_bo2 <- glm(boycott ~ postmat * dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex, data = hk_data, family = "binomial")
summary(logit_bo2)
ord_bo2 <- polr(boycott2 ~ postmat * dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex, data = hk_data, Hess = TRUE)
summary(ord_bo2)

logit_st <- glm(strike ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, family = "binomial")
summary(logit_st)
ord_st <- polr(strike2 ~ postmat + dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, Hess = TRUE)
summary(ord_st)
logit_st2 <- glm(strike ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, family = "binomial")
summary(logit_st2)
ord_st2 <- polr(strike2 ~ postmat * dissat + econdissat + income + pertrust + organ + educ + ln_age + sex, data = hk_data, Hess = TRUE)
summary(ord_st2)


## Extreme Bounds Analysis ##
naive.eba <- eba(formula = protest ~ postmat * dissat + econdissat + income + polint + poldis + pertrust + organ + educ + ln_age + sex + single, data = hk_data, k = 0:8, reg.fun = glm, family=binomial(link = 'logit'))
hist(naive.eba, variables=c("postmat","dissat","postmat:dissat","econdissat","income","polint","poldis","pertrust","organ1","educ","ln_age","sex1","single1"),
     main=c(postmat="Post-materialism",dissat="Dissat.pol.system",econdissat="Dissat.fin.sit.",income="Income",polint="Political interest",poldis="Political discussion",pertrust="Interpersonal trust",organ1="Organizational member",educ="Education",ln_age="log(age)",sex1="Female",single1="Single"))
print(naive.eba)
sophisticated.eba <- eba(formula = protest ~ postmat * poleff + polint + poldis + organ + ln_age + educ | dissat + econdissat + income + pertrust + sex | dissat + econdissat + income + pertrust + sex + single, data = hk_data, k=0:3, reg.fun = glm, family=binomial(link = 'logit'))
hist(sophisticated.eba, variables=c("postmat","poleff1","postmat:poleff1","dissat","econdissat","income"), main=c(postmat="Post-materialism",poleff1="Political efficacy",dissat="Dissat.pol.system",econdissat="Dissat.fin.sit",income="Income"))
print(sophisticated.eba)
