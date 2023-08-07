library(tidyverse) 
library(broom)
library(readxl)
library(knitr)
library(ggplot2)
library(magrittr)
library(ggplot2)
library(knitr)
library(ggthemes)
library(dplyr)
library(forcats)
library(corrplot)

#Data Summary
data<- read_xlsx("data2.xlsx")
summary(data)
colSums(is.na(data))
names(data)
dim(data)
table(data$Attrition)

# Visualizing the Attrition variable to find imbalance
ggplot(data, aes(x=Attrition)) + ggtitle("Attrition") + xlab("Attrition") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Count") + coord_flip() + theme_minimal()

# Converting variables into factors
data$Education <- as.factor(data$Education)
data$JobLevel <- as.factor(data$JobLevel)
data$PerformanceRating <- as.factor(data$PerformanceRating)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)
summary(data)

# Visualizing the distribution of variables
# Age
ggplot(data,aes(Age))+geom_histogram(binwidth=5,aes(y=..count..),color="black",fill="pink")+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Count",title="Distribution of Age")
# Age, attrition= 1
ggplot(data, aes(Age))+geom_histogram(binwidth=5,aes(y=round(((..count..)/sum(..count..))*100,2)),color="black",fill="orange")+theme_few()+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Percentage",title="Age distribution of people who leave")+scale_y_continuous(limits=c(0,30),breaks=seq(0,30,5))+scale_x_continuous(limits=c(15,60),breaks=seq(15,60,5))
# Age, attrition= 0
ggplot(data,aes(Age))+geom_histogram(binwidth=5,aes(y=round(((..count..)/sum(..count..))*100,2)),color="black",fill="purple")+theme_few()+theme(legend.position="none",plot.title = element_text(hjust=0.5,size=15))+labs(x="Age",y="Percentage",title="Age distribution of people who stay")+scale_y_continuous(limits=c(0,30),breaks=seq(0,30,5))+scale_x_continuous(limits=c(15,60),breaks=seq(15,60,5))

# Analysis
ggplot(data,aes(Age,MonthlyIncome,size=Age,col=factor(Attrition)))+geom_point(alpha=0.3)+theme_minimal()+facet_wrap(~MaritalStatus)+labs(x="Age",y="MonthlyIncome",title="Attrition Distribution",subtitle="Distribution of Attrition in IBM based on change in Age, Income and Marital Status",col="Attrition")+theme(legend.position="bottom",plot.title=element_text(size=16,hjust=0.5),plot.subtitle = element_text(size=10))+scale_color_brewer(palette="Set1")

# Department vs. Attrition
ggplot(data,aes(x=Department,group=Attrition))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+facet_grid(~Attrition)+theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+labs(x="Department",y="Percentage",title="Attrition (%) vs. Department")+ geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) 

# Departmental pay
temp=data %>% group_by(JobRole) %>% summarise(salary=median(MonthlyIncome)) %>% arrange(desc(salary))
ggplot(temp,aes(factor(JobRole,levels=JobRole),salary))+geom_bar(stat="identity",fill="gold4")+coord_polar()+labs(x="Job Role",y="Median Salary",title="Who gets paid more?")+theme_few()+theme(axis.text.x=element_text(vjust=300),plot.title=element_text(hjust=0.5,size=16),axis.text.y=element_blank())


# Education
temp= data %>% mutate(Education=factor(Education)) %>% mutate(Education=fct_recode(Education,'Below College'='1','College'='2','Bachelor'='3','Master'='4','Doctor'='5'))
ggplot(temp,aes(Education,fill=Attrition))+geom_bar(stat="count",aes(y=..count..),position=position_dodge())+theme_few()+theme_few()+theme(legend.position="bottom",plot.title=element_text(hjust=0.5,size=16),axis.text.x = element_text(angle=90))+labs(x="Education Level",y="Count",title="Trend of Attrition with Education Level")+scale_fill_canva(palette="Fresh and bright")

# Job involvement vs Attrition
temp = data %>% mutate(JobInvolvement=factor(JobInvolvement)) %>% mutate(JobInvolvement=fct_recode(JobInvolvement,"Low"="1","Medium"="2","High"="3","Very High"="4"))
ggplot(temp,aes(x=JobInvolvement,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="Job Involvement",y="Percentage",title="Job Involvement vs. Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# Overtime vs Attrition
ggplot(data,aes(x=OverTime,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="Overtime",y="Percentage",title="Overtime vs. Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)

# WLB vs. Attrition
temp= data %>% mutate(WorkLifeBalance=factor(WorkLifeBalance)) %>% mutate(WorkLifeBalance=fct_recode(WorkLifeBalance,"Bad"="1","Good"="2","Better"="3","Best"="4"))
ggplot(temp,aes(x=WorkLifeBalance,group=Attrition))+geom_bar(stat="count",aes(y=..prop..,fill=factor(..x..)))+labs(x="WorkLifeBalance",y="Percentage",title="Work life balance vs. Attrition Rates")+facet_wrap(~Attrition)+theme_few()+theme(legend.position="none",plot.title=element_text(hjust=0.5,size=14))+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-0.5)


# ----------------------SMOTE for balancing the data set---------------------------
#Data Imbalanced Smote function
count = table(data$Attrition)


# Over Sampling
over = ( (0.6 * max(count)) - min(count) ) / min(count)

# Under Sampling
under = (0.4 * max(count)) / (min(count) * over)


over = round(over, 1) * 100
under = round(under, 1) * 100

#  Balanced dataset
install.packages('DMwR')
library(DMwR)
data1 = SMOTE(Attrition~., data, perc.over = 210, k = 5, perc.under = 100)






