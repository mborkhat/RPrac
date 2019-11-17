# RPrac
Learning

library(tidyverse)
library(ggplot2)
library(dplyr)

#IRIS DATA SET

#loading
data=iris

#gathering
gather(data, key = "flower_att", value = "measurement", -Species)

#Species with highest mean Sepal width
data %>% mutate(funs(avg = mean((data$Sepal.Width),na.rm = T)))  %>% top_n(1,Sepal.Width)

#S.L >4.6= and p.w >=.5 and col starts with P
ans3=iris %>% filter(Sepal.Length>=4.6,Petal.Width>=0.5) %>% select(starts_with("P"))

# Arrange Petal Length in desc and select top 10
iris %>% arrange(desc(Petal.Length)) %>% select(Petal.Length) %>% head(10)

#Prop is L/W
iris <- iris %>% mutate(proportion=Sepal.Length/Sepal.Width)

iris[,c(5,6)] %>% tail(2)

#date format
date1 <- as.Date('20100101',format="%Y%m%d")
date1

date2 <- as.Date('20180421',format("%Y%m%d"))
date2

date3 <- as.Date('05282016',format("%m%d%Y"))
date3

#years
years <- c()
years[1] <- format(date1,"%Y")
years[2] <- format(date2,"%Y")
years[3] <- format(date3,"%Y")
years

#weekdays
weekdays <- c()
weekdays[1] <- as.numeric(format(date1,"%w"))
weekdays[2] <- as.numeric(format(date2,"%w"))
weekdays[3] <- as.numeric(format(date3,"%w"))

#mid point of first two dates
as.Date((as.numeric(date1)+as.numeric(date2))/2,origin = "1970-01-01")


#SWitch 
enter_date <- readline("Enter date in following format yearmonthday:")
date_given <- as.Date(enter_date,format("%Y%m%d"))
switch(as.numeric(format(date_given,"%w"))+1,0,6,5,4,3,2,1)

#(4B)write a function to check if a given year is a leap year or not
year = as.integer(readline(prompt="Enter a year: "))
LEAP_YEAR_FORMULA <- function(a) {
  
  if ((a %% 4) == 0)
  { 
    print("LEAP YEAR")
  } else if ((a %% 100) == 0 ) 
  {
    print("NOT A LEAP YEAR")
  } else if ((a %% 400) == 0 )   
  {
    print("LEAP YEAR")
  } else      
  {
    print("NOT A LEAP YEAR")
  }
  
}
LEAP_YEAR_FORMULA(year)


#MPG DATA SET
View(mpg)

#point using color class
ggplot(mpg,aes(x=displ,y=hwy,color=class))+geom_point()

#box plot class vs hwy
ggplot(mpg,aes(x=class,y=hwy))+geom_boxplot()#suv has highest number of outliers

#hist with 20 bins
ggplot(mpg,aes(x=hwy))+geom_histogram(bins = 20)

#barchart
ggplot(mpg,aes(x=class,y=hwy,color=class))+geom_bar(stat="identity")


#CHICKWEIGHT
view(ChickWeight)

chicks=ChickWeight

#AVG wt of all chicks during same time
avg_wt=chicks%>%group_by(Time)%>% summarise_at(vars(weight),funs(mean))

#High and lowest diet
highest_gain_Diet <- ChickWeight %>% group_by(Diet) %>% summarise_at(vars(weight),funs(max,min)) %>% arrange(desc(max)) %>% select(Diet) %>% head(1)
lowest_gain_Diet <- ChickWeight %>% group_by(Diet) %>% summarise_at(vars(weight),funs(max,min)) %>% arrange(min) %>% select(Diet) %>% head(1)

#unique list of chicks who crossed the weight of 300
ChickWeight %>% filter(weight>300) %>% select(Chick) %>%  unique()

#pass
password='pass'
input=''
n <- 3
while(n!=0){
  input <- readline(prompt = "Enter your password: ")
  if(input==password){
    print("you have successfully logged in")
    break
  }else{
    n <- n-1
    print(paste(3-n," Failed login.",n," more attempt left")) 
  }
}


#AIRQUALITY
#check for na individually and sum
colSums(is.na(airquality))
sum(is.na(airquality))

#clean by removing  na
na.omit(airquality)

#clean NA by col mean
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone,na.rm = T)
airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R,na.rm=T)

#month with highest ozone
airquality %>% arrange(desc(Ozone)) %>% select(Month) %>% head(1)
    
#plot a line chart on Ozone with filtered data of month as 5                    temp <- airquality %>% filter(Month==5)
plot(temp$Ozone,type="l",ylab = "Ozone",main = "Ozone line plot for month 5")

#18-19 RE


#DATA CENSUS POPCHANGE
f1=read.csv("~/Mtech Docs/RPracs/pop_change.csv")
headers=f1[2,]
pop_change=read.csv("~/Mtech Docs/RPracs/pop_change.csv",skip=2)

for(i in c(1:23))
{
  colnames(pop_change)[i]=paste(headers[1,i])
}

#NA SUM
colSums(is.na(pop_change)) 


#D
div <- function(x){
  if(is.numeric(x)){
    s=x/100000
  }else{
    x=as.character((x))
  }
}
temp <- as.data.frame(sapply(pop_change[,], div))

#e SUM and Mean of COLUMNS
mean_rows= summarise_all(pop_change[,1:23],funs(mean))
sum_rows=colSums(Filter(is.numeric, pop_change))

pop_change=rbind(pop_change,mean_rows)
pop_change=rbind(pop_change,sum_rows)

#perc change of population wrt 2000
ans_6= pop_change%>%mutate("Perc_change"=((X2010_POPULATION-X2000_POPULATION)/X2000_POPULATION))%>%filter(Perc_change<0)
ans_6

#order data in dec of perc change Top 10 states
ans_8=pop_change%>%mutate("Perc_change"=((X2010_POPULATION-X2000_POPULATION)/X2000_POPULATION))%>%arrange(desc(Perc_change))%>%top_n(10,Perc_change)
ans_8

#GOOGLE MICROSOFT

#GETTINg gogle data
library(quantmod)
as.data.frame(getSymbols(c("GOOG","MSFT","AAPL","TSLA"),from="2018-01-01",to="2018-12-31"))
write.csv(as.data.frame(GOOG),file="~/Mtech Docs/RPracs/google.csv")

#plot apple and tesla + labels
candleChart(GOOG)
candleChart(c(TSLA,GOOG),theme = chartTheme("black"))

plot(Delt(GOOG$GOOG.Close,type = "arithemetic"))

plot(Cl(GOOG))
barChart(GOOG)
candleChart(c(GOOG,AAPL),theme = "white")


sapply(GOOG, mean)
sapply(GOOG, max)
sapply(GOOG, min)

index(GOOG)[which.max(GOOG$GOOG.Close)]
index(GOOG)[which.min(GOOG$GOOG.Close)]


# When applying an operation to two vectors that requires them to be the same length, R automatically recycles, or repeats, elements of the shorter one, until it is long enough to match the longer Vector. 
# Example 1:
#   Suppose we have two Vectors c(1,2,4) , c(6,0,9,10,13), where the first one is shorter with only 3 elements. Now if we sum these two, we will get a warning message as follows.
# > c(1,2,4) + c(6,0,9,10,13)
# [1]  7  2 13 11 15
# Warning message:
#   In c(1, 2, 4) + c(6, 0, 9, 10, 13) :  longer object length is not a multiple of shorter object length
# 
# Here R , Sum those Vectors by Recycling or repeating the elements in shorter one, until it is long enough to match the longer one as follows..
# 
# > c(1,2,4,1,2) + c(6,0,9,10,13)
# [1]  7  2 13 11 15




#LIST OF UNIQUE WORDS
library(tm)
text = c("I am Mark I am MarkNeo4j is cool Neo4j is cool")
corpus = VCorpus(VectorSource(text))
tdm = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
tdm
