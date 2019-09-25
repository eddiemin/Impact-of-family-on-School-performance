#libraries
library(lubridate)
library(tidyverse)
library(dplyr)
set.seed(1222)

# Load and explore data structure
# Placeholder
df<-"student_por.csv" %>%
  read.csv

df2<-"student_mat.csv" %>%
  read.csv

df%>%glimpse

df2%>%glimpse

# Data processing

df_port_total <- df %>%
  select(school, sex, Pstatus, famsize, Fedu, Medu, famrel, freetime, PortGrade=G3)%>%
  filter(PortGrade>1)

edu_total <- df_port_total$Medu + df_port_total$Fedu   


df_port_gp <- df %>%
  filter(school=="GP",G3>1)%>%
  select(school, sex, Pstatus, famsize, Fedu, Medu ,famrel, freetime, PortGrade=G3)

edu_gp <- df_port_gp$Medu + df_port_gp$Fedu   

df_port_ms <- df %>%
  filter(school=="MS",G3>1)%>%
  select(school, sex, Pstatus, famsize, Fedu, Medu ,famrel, freetime, PortGrade=G3)

edu_ms <- df_port_ms$Medu + df_port_ms$Fedu 

edu_total_average<-edu_total/2

df_port_total %>% glimpse

df_port_gp %>% glimpse

#Data Visualization

df_port_gp %>% ggplot(mapping = aes(x = edu_gp, y = PortGrade, color=school)) + 
  geom_jitter(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="PortGrade vs Combined Parent Education by School",
       subtitle="Points jittered+alpha blended+school color")

df_port_ms %>% ggplot(mapping = aes(x = edu_ms, y = PortGrade, color=school)) + 
  geom_jitter(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="PortGrade vs Combined Parent Education by School",
       subtitle="Points jittered+alpha blended+school color")

df_port_total %>% ggplot(mapping = aes(x = edu_total, y = PortGrade, color=school)) + 
  geom_jitter(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="PortGrade vs Combined Parent Education by School",
       subtitle="Points jittered+alpha blended+school color")
bw <- 0.5
df_port_total %>% ggplot(aes(x=PortGrade)) + 
  geom_histogram(binwidth= bw , color= "black" , fill= "blue" ) +
  theme_classic() +
  labs(title="Histogram of PortGrade of both schools",
       subtitle=str_c("Binwidth:", 0.5 ),
       x="PortGrade", y="Number of Students")
df_port_gp %>% ggplot(aes(x=PortGrade)) + 
  geom_histogram(binwidth= bw , color= "black" , fill= "blue" ) +
  theme_classic() +
  labs(title="Histogram of PortGrade of GP school",
       subtitle=str_c("Binwidth:", 0.5 ),
       x="PortGrade", y="Number of Students")
df_port_ms %>% ggplot(aes(x=PortGrade)) + 
  geom_histogram(binwidth= bw , color= "black" , fill= "blue" ) +
  theme_classic() +
  labs(title="Histogram of PortGrade of MS school",
       subtitle=str_c("Binwidth:", 0.5 ),
       x="PortGrade", y="Number of Students")

#Hypothesis Testing

#The school Gabriel Pereira has better performing students based off its Portugal 
#language scores when compared to Mousinho da Silveira. The null hypothesis is rejected. 
#The p-value is <0.05 and the confidence interval does not contain 0, so alternate hypothesis is tue. 
#The Portgual language scores are not the same for both schools.

t.test(PortGrade ~ school, data = df_port_total)

#Split into train / test sets

port.edu_gp<- lm(PortGrade~edu_gp+famrel+famsize+freetime, data=df_port_gp)

port.edu_gp%>%summary()

cat('The coefficient confidence intervals')

port.edu_gp %>% confint()

port.edu_ms<- lm(PortGrade~edu_ms+famrel+famsize+freetime, data=df_port_ms)

port.edu_ms%>%summary()

cat('The coefficient confidence intervals')
port.edu_ms %>% confint()

# Train and test models

#First Model
port.edu_gp %>% ggplot(mapping = aes(x = edu_gp++famrel, y = PortGrade, color=famsize)) + 
  geom_jitter(alpha = 0.4) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="PortGrade vs Combined Parent Education by Family relations",
       subtitle="Points jittered+alpha blended+Family relations color")

#Second Model
port.edu_ms %>% ggplot(mapping = aes(x = edu_ms+famrel, y = PortGrade, color=famsize)) + 
  geom_jitter(alpha = 0.4) + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title="PortGrade vs Combined Parent Education by Family relations",
       subtitle="Points jittered+alpha blended+Family relations color")

#Model Performance Comparison
port.edu_gp_perform <- df_port_gp %>% 
  mutate(score = predict(port.edu_gp, data = df_port_gp),
         resids = PortGrade - score,
         predicted.grade = exp(score)) 
port.edu_gp_perform %>% select(PortGrade:predicted.grade) %>% head()

port.edu_ms_perform <- df_port_ms %>% 
  mutate(score = predict(port.edu_ms, data = df_port_ms),
         resids = PortGrade - score,
         predicted.grade = exp(score)) 
port.edu_ms_perform %>% select(PortGrade:predicted.grade) %>% head()

# Analysis and Conclusions

#When observing the portugal grades of two schools (Gabriel Pereira & Mousinho da Silveira),
#we can observe that GP performs better. In order to understand why GP performs better, We can look at the family setting. 
#We looked into the parents education levels, relationships with the studen, and the family size. 
#As expected, the higher family relations and education levels had a positive correlation with grades. 
#Family size did not make much of a difference for both schools. To test for other variables, the variable freetime of students was also thrown into the regression. 
#Freetime had a negative correlation with grades as students most likely did not spend their freetime to study. 
#But we can oberserve through the visualization of the schools, that GP (on average) had a higher level of parent education and family relationship which led GP students to have higher Portugal grades than MS. 
