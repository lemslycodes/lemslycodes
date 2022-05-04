#load packages
install.packages("ggiraphExtra")
install.packages(c("cluster", "rattle","NbClust"))
install.packages('ggfortnify')
install.packages('vip')
install.packages('caret')
install.packages('factoextra')
install.packages("ggpmisc")
install.packages("aod")
install.packages("stargazer")
install.packages("report")
##Load libraries 
library(stargazer) # making regression tables in R as text 
library(ggplot2) #plots 
library(ggthemes)
library(tidyverse) #clean data 
library(readxl) # read xl files 
library(viridis)
library(lubridate)
library(vip) # variance importance plots
library(GGally)
library(caret)
library(readxl)
library(tidyverse)
library(plotly)
library(expss)
library(factoextra)
library(RColorBrewer)
library(ggiraphExtra)
library(cluster)
library(rattle)
library(NbClust)
library(report)
library(dplyr) #data manipulation 
library(glmnet) #regularized regression 
library(car)

citation(package = "cluster")

###set working directory
setwd("~/Desktop/Stats 2")
getwd()

###Import dataset
vg_sales_ratings <- read_excel("vg_sales_ratings.xlsx") 

#Recode Year_of_Release as numeric and limit observations to 2000 - 2016
#vg_sales_ratings$Year_of_Release <- as.numeric(vg_sales_ratings$Year_of_Release)
vg_sales_ratings <- vg_sales_ratings[vg_sales_ratings$Year_of_Release >1999, ]
vg_sales_ratings <- vg_sales_ratings[vg_sales_ratings$Year_of_Release <2017, ]

##Remove NAs
vg_sales <- na.omit(vg_sales_ratings)

#remove empty rating observations
vg_sales <- vg_sales %>% filter(Rating != "") %>% droplevels()

vg_sales$NA_Sales <- vg_sales$NA_Sales * 1000000 + 1
vg_sales$EU_Sales <- vg_sales$EU_Sales * 1000000 + 1
vg_sales$JP_Sales <- vg_sales$JP_Sales * 1000000 + 1
vg_sales$Other_Sales <- vg_sales$Other_Sales * 1000000 + 1
vg_sales$Global_Sales <- vg_sales$Global_Sales * 1000000 + 1

## The summary tells us the values for sales data is waaaaaay off. 
## So, we need to use the log function to transform numeric variables that are too large!
EU.Sales.Log <- log10(vg_sales$EU_Sales)
NA.Sales.Log <- log10(vg_sales$NA_Sales)
JP.Sales.Log <- log10(vg_sales$JP_Sales)
Other.Sales.Log <- log10(vg_sales$Other_Sales)
Global.Sales.Log <-log10(vg_sales$Global_Sales)
Critic.Count.Log <- log10(vg_sales$Critic_Count)
User.Count.Log <- log10(vg_sales$User_Count)

### Combine new log variables with original variables using cbind function 
vg_sales.log <- cbind.data.frame(NA.Sales.Log, EU.Sales.Log, JP.Sales.Log, Other.Sales.Log, 
                                 Global.Sales.Log, User.Count.Log, Critic.Count.Log)
# The data we use for analysis
### Transform log variables to current data set
vg_sales <- cbind.data.frame(vg_sales, vg_sales.log)

### Divide variables by 10 to make Critic Score the same decimal as User Score
### The dataset uploaded these variables to be read as --> strings not numbers
vg_sales$Critic_Score <- as.numeric(as.character(vg_sales$Critic_Score)) / 10
vg_sales$User_Score <- as.numeric(as.character(vg_sales$User_Score))
vg_sales$Critic_Count <- as.numeric(vg_sales$Critic_Count)
vg_sales$User_Count <- as.numeric(vg_sales$User_Count)

### Make character variables as factor variables for statistical analysis 
vg_sales$Name <- as.factor(vg_sales$Name)
vg_sales$Platform <- as.factor(vg_sales$Platform)
vg_sales$Genre <- as.factor(vg_sales$Genre)
vg_sales$Publisher <- as.factor(vg_sales$Publisher)
vg_sales$Rating <- as.factor(vg_sales$Rating)

##regroup platforms as platform.type
pc <- c("PC")
xbox <- c("X360", "XB", "XOne")
nintendo <- c("Wii", "WiiU", "N64", "GC", "NES", "3DS", "DS") 
playstation <- c("PS", "PS2", "PS3", "PS4", "PSP", "PSV")
vg_sales <- vg_sales %>%
  mutate(Platform.type = ifelse(Platform %in% pc, "PC",
                                ifelse(Platform %in% xbox, "Xbox",
                                       ifelse(Platform %in% nintendo, "Nintendo",
                                              ifelse(Platform %in% playstation, "Playstation", "Others")))))

### DROP OLD VALUES 
vg_sales$NA_Sales = NULL
vg_sales$EU_Sales = NULL
vg_sales$JP_Sales = NULL
vg_sales$Other_Sales = NULL
vg_sales$Global_Sales = NULL
vg_sales$Critic_Count = NULL
vg_sales$User_Count = NULL 
vg_sales$Developer = NULL
vg_sales$Platform = NULL
vg_sales$Publisher = NULL

###use data only for correlation 
vgcor$JP.Sales = NULL
vgcor$EU.Sales = NULL
vgcor$NA.Sales = NULL
vgcor$Other.Sales = NULL
 
###Create columns 
# Format column names 
colnames(vg_sales) <- c("Name", "Year.Release", "Genre", "Critic.Score","User.Score","Rating","NA.Sales", "EU.Sales","JP.Sales", "Other.Sales", 
                        "Global.Sales","Critic.Count", "User.Count", "Platform")

#---------------------------------# 
summary(vgcor)
stargazer(vg_sales)

###look at data###
vg_sales %>% select(Global.Sales,User.Count, Critic.Score, Critic.Count, User.Score) %>% ggpairs
vg_sales %>% select(Global.Sales,User.Count,Critic.Count, User.Score, Critic.Score) %>% ggcorr(label =T)

###corrleation btw DV and IV and control variables
ggpairs(vg_sales)
vgcor <- vg_sales %>%
  select(2:12)

correlation.matrix <- cor(vg_sales[,c("Critic.Score","User.Score","Critic.Count", "User.Count", "Global.Sales")])
stargazer(correlation.matrix, title="Correlation Matrix")

###DV on IV linear reg 
### This means that critic scores can explain 19.3% of the variance in global sales. 
model1 <- lm(Global.Sales ~ Critic.Score, data = vg_sales)
##### DV on control variables 
model2 <- lm(Global.Sales ~ Critic.Count + User.Count + User.Score + Genre, data = vg_sales)
vif1 <- vif(model2)
#####DV on IV + Control variables 
model3 <- lm(Global.Sales ~ Critic.Score + User.Count + User.Score +Critic.Count + Genre, family= binomial(), data = vg_sales)

stargazer(model1, model2, model3, title="Results")

stargazer(model1, model2, model3, title="Regression Results", dep.var.labels=c("Global Sales"),
          covariate.labels=c("Critic Score", "Critic Count", "User Count", "User Score", "Adventure", 
                             "Fighting", "Misc", "Platform", "Puzzle", "Racing","Shooter", "Roleplaying", "Stimulation",
                             "Sports", "Stratgey"),
          omit.stat=c("LL","ser","f"), no.space=TRUE, ci=TRUE, ci.level=0.90, single.row=TRUE )

###fancy cute graphs
###plot of all popular genre by global sales and critic scores 
ggplot(vg_sales, aes(Global.Sales, Critic.Score, color = Platform)) + 
  geom_point()+
  facet_wrap(~ Genre)

ggplt <- ggplot(vg_sales, aes(Global.Sales, Critic.Score)) +
  geom_point() +
  geom_smooth(method= model1, se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Global Sales', y='Critic Scores', title='Linear Regression Plot') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=Genre))

###Clustering 
stats::kmeans(vg_sales.log, centers = 10, nstart = 20)
set.seed(12)
init <- sample(3, nrow(vg_sales.log), replace = TRUE)
plot(vg_sales.log, col = init)

help(log)
