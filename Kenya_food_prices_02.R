#####Loading the required library####
setwd("C:\\Users\\user\\Desktop\\MAS 403; Practical statistics")
library(readxl)
library(tidyverse)


#####Importing the data into R##########

food_prices_data<-read_xlsx("Kenya food prices.xlsx")


###View the data###

View(food_prices_data)
str(food_prices_data)


#food_prices_data$date<-as.Date(as.numeric(food_prices_data$date),origin = "1899-12-30")

####Converting prices and Usdprice into numeric######



food_prices_data<-food_prices_data %>% 
  mutate_at(vars(price,usdprice),as.numeric)


food_prices_data<-na.omit(food_prices_data)



####Subsetting the data ####

food_prices_data<-subset(food_prices_data,food_prices_data$priceflag!="forecast")
food_prices_data<-subset(food_prices_data,food_prices_data$category!="non-food")

unique(food_prices_data$priceflag)


####Deleting the second row#####

food_prices_data2<-food_prices_data[-2,]

View(food_prices_data2)


#####converting to one unit####
food_prices_data2$price<-ifelse(food_prices_data2$unit=="90 KG",
                                food_prices_data2$price/90,ifelse(food_prices_data2$unit=="50 KG",
                                                                  food_prices_data2$price/50,
                                                                    ifelse(food_prices_data2$unit=="126 KG",food_prices_data2$price/126,
                                                                           ifelse(food_prices_data2$unit=="64 KG",food_prices_data2$price/64,
                                                                                  ifelse(food_prices_data2$unit=="13 KG",food_prices_data2$price/13,
                                                                                         ifelse(food_prices_data2$unit=="500 ML",food_prices_data2$price*2,
                                                                                                ifelse(food_prices_data2$unit=="200 ML",food_prices_data2$price*5,
                                                                                                       ifelse(food_prices_data2$unit=="400 G",food_prices_data2$price*5/2,
                                                                                                              ifelse(food_prices_data2$unit=="200 G",food_prices_data2$price*5,food_prices_data2$price)))))))))

View(food_prices_data2)

food_prices_data2$unit<-gsub("(50|126|90|13|64) KG","KG",food_prices_data2$unit)
food_prices_data2$unit<-gsub("(200|500) ML","L",food_prices_data2$unit)
food_prices_data2$unit<-gsub("(200|400) G","KG",food_prices_data2$unit)
unique(food_prices_data2$unit)

food_prices_data2R<-food_prices_data2 %>% 
  select(admin1,category,priceflag,price) %>% 
  group_by(admin1,category) %>% 
  summarise(avr_price=mean(price)) 

View(food_prices_data2R)


###Plotting bar gaph of the food price###

ggplot(food_prices_data2R,aes(x=admin1,y=avr_price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.75)+
  labs(x="Region",y="Average price",title="Average food price per region")
unique(food_prices_data2R$admin1)



####the actual food prices for the regions#####

Actual_food_prices<-food_prices_data2 %>% 
  select(admin1,category,priceflag,price) %>% 
  filter(priceflag=="actual") %>% 
  group_by(admin1,category) %>% 
  summarise(avr_price=mean(price))


ggplot(Actual_food_prices,aes(x=admin1,y=avr_price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.75)+
  labs(x="Region",y="Actual_Average price",title="Average food price per region")



#####Aggregate prices######

Aggregate_food_price<-food_prices_data2%>% 
  select(admin1,category,priceflag,price) %>% 
  filter(priceflag=="aggregate") %>% 
  group_by(admin1,category) %>% 
  summarise(avr_price=mean(price))


ggplot(Aggregate_food_price,aes(x=admin1,y=avr_price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.75)+
  labs(x="Region",y="Agregate_Average price",title="Average food price per region")

#####calculate descriptive statistics for each category#######
descriptive_statis<-food_prices_data2 %>% 
  select(category,price) %>% 
  group_by(category) %>% 
  summarize(mean_price=mean(price),
            median_price=median(price),
            sd_price=sd(price))

View(descriptive_statis)


####Testing for normality####

hist(food_prices_data2R$avr_price)
qqnorm(food_prices_data2R$avr_price)
qqline(food_prices_data2R$avr_price)



#####performing non parametric test#####

boxplot(avr_price~admin1,data = food_prices_data2R,main="Different box plot for food price in different regions",
        xlab = "Region",ylab = "Average_food_price")

kruskal.test(avr_price~admin1,data = food_prices_data2R)




#####Time series####

#View(Kenya_food_prices2)
food_data<-food_prices_data2 %>% 
  select(date,category,price)
View(food_data)

food_data$MA<-rollapply(food_data$price,width=30, mean,align="right",
                        fill=NA)

ggplot(food_data,aes(x=date,y=MA,colour=category))+
  geom_line(stat = "identity",size=0.8,alpha=1)+
  labs(x="Date",y="Moving_Average",title = "Food Prices time series")


###Prices in different market#####

Wholesale_retail_data<-food_prices_data2 %>% 
  select(pricetype,price,category) %>% 
  group_by(pricetype,category) %>% 
  summarise(Avrg_price=mean(price))

View(Wholesale_retail_data)


Summary_statistics<-food_prices_data2 %>%
  select(pricetype,category,price) %>% 
  group_by(pricetype,category) %>% 
  summarise(mean_price= mean(price),
            median_price=median(price),
            sd_price=sd(price))
View(Summary_statistics)



ggplot(Wholesale_retail_data,aes(x=pricetype,y=Avrg_price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge())+
  labs(x="Price_types",y="Average_prices",title = "Average food pricesin Retails and wholesale")

###Ploting boxplot####


boxplot(Avrg_price~pricetype,Wholesale_retail_data,col=c("blue","green"),
        main="Boxplot showing food prices in different markets",xlab = "Markets",
        ylab = "Prices")
#####Test for normality####

hist(Wholesale_retail_data$Avrg_price, main ="Histogram of price markets", xlab ="Average price", ylab = "Frequency")
qqnorm(Wholesale_retail_data$Avrg_price)
qqline(Wholesale_retail_data$Avrg_price)



#####performing non parametric test####

Retail_wholesale<-food_prices_data2 %>% 
  select(pricetype,price)
wilcox.test(price~pricetype,data = Retail_wholesale)


####Relationship between prices and commodity types####

Relation_data<-food_prices_data2 %>% 
  select(date,commodity,price) %>% 
  group_by(commodity,date) %>% 
  summarise(Avrg_price=mean(price))
View(Relation_data)

boxplot(Avrg_price~commodity,Relation_data)



