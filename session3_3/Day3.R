mtcars

table(mtcars$cyl)
table(mtcars$vs)
table(mtcars$am)
table(mtcars$gear)

table(mtcars$cyl, mtcars$vs)
m1<-xtabs(~cyl+am, data=mtcars)
m1
summary(m1)

m2<-xtabs(~gear+carb, data=mtcars)
m2
summary(m2)

drinking <- data.frame(
  학과 = c("경영","건축","영문","경영","건축","경영","건축","영문","경영","영문","경영","영문"),
  주량 = c(2.5,0.5,2.0,3.0,2.0,2.0,1.0,1.0,1.0,0.5,3.0,1.5)
)
drinking
mean(drinking$주량)

require(dplyr)
drinking %>%
  group_by(학과) %>%
  summarise(학과평균=mean(주량))
mean(drinking$주량)

require(ggplot2)
drinking %>%
  ggplot(aes(학과, 주량)) +
  geom_boxplot()

res.aov <- aov(주량~학과, data=drinking)
res.aov
summary(res.aov)

plot(res.aov, 1)
plot(res.aov, 2)

kruskal.test(주량~학과, data=drinking)

mtcars %>%
  ggplot(aes(as.factor(cyl), mpg)) +
  geom_boxplot()
test.aov <- aov(cyl~mpg, data=mtcars)
test.aov
summary(test.aov)

options(digits=2)
cor(mtcars)
cov(mtcars)

cor(mtcars$mpg, mtcars$wt)
cor.test(mtcars$mpg, mtcars$wt)

plot(mtcars$mpg, mtcars$wt)

round(cor(mtcars), digits=2)

#install.packages("Hmisc")
require(Hmisc)
rcorr(as.matrix(mtcars))

#install.packages("corrgram")
require(corrgram)
corrgram(mtcars,
         order=TRUE,
         lower.panel = panel.ellipse,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         diag.panel = panel.minmax,
         main="Correlation Matrix"
)

cor(mtcars)
cor(mtcars$mpg, mtcars$disp)
cor.test(mtcars$mpg, mtcars$disp)

m1 <- lm(mpg ~ wt, data=mtcars)
summary(m1)

### 예측
a<-as.data.frame(x=9) # 예측을 위한 데이터 생성
colnames(a)<-"wt"
predict(m1, a) # 예측

mtcars %>%
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method="lm")

m2<-lm(mpg ~ wt + hp + disp, data=mtcars)
summary(m1)
summary(m2)

anova(m1, m2)

#install.packages("caret")
#install.packages("DAAG")
#install.packages("textreg")
require(caret)
require(DAAG)
vif(m2)

require(textreg)
screenreg(m1)
screenreg(m2)

#install.packages("aod")

library(aod)
library(ggplot2)
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
nrow(mydata)
head(mydata)

table(mydata$admit)

table(mydata$rank)

xtabs(~admit +rank, data=mydata)
summary(xtabs(~admit +rank, data=mydata))

mydata$rankfactor <- as.factor(mydata$rank)

m1 <- glm(admit ~ gre + gpa + rankfactor, data=mydata, family="binomial")
summary(m1)
exp(coef(m1))

library
library.lm <- lm(전체만족도 ~ 고객칭찬 + 고객불만 + as.factor(성별) + as.factor(학력), data=library)
summary(library.lm)

mcarts %>% 
  gglot(aes.hpg)

m3 <- lm(mpg~hp,tate,)

#install.packages("igraph")
#install.packages("statnet")
#install.packages("devtools")

require(igraph)
require(statnet)
require(devtools)

install_github("DougLuke/UserNetR")
require(UserNetR)
data(Moreno)
