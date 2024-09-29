# set the working directory
setwd("C:\\Users\\user\\Desktop\\MAS 403; Practical statistics")

#load the required packages
library(xts)
library(tidyverse)
library(readxl)
library(writexl)
library(tidyr)

#import the data
foodprice <-  read_excel("kenya food prices.xlsx")
view(foodprice)

#know the structure of your data
str(foodprice)

#change the variables types
foodprice <- foodprice %>%
  mutate_at(vars(price, usdprice), as.numeric)
str(foodprice)

#clean the data by removing some columns and any duplication
foodprice <- foodprice %>%
  select(-c(...2)) %>%
  distinct()
foodprice <- foodprice[-1,]


#Converting the price units to a standard unit
foodprice$price<-ifelse(foodprice$unit=="90 KG",
                                 foodprice$price/90,ifelse(foodprice$unit=="50 KG",
                                                                    foodprice$price/50,
                                                                    ifelse(foodprice$unit=="126 KG",foodprice$price/126,
                                                                           ifelse(foodprice$unit=="64 KG",foodprice$price/64,
                                                                                  ifelse(foodprice$unit=="13 KG",foodprice$price/13,
                                                                                         ifelse(foodprice$unit=="500 ML",foodprice$price*2,
                                                                                                ifelse(foodprice$unit=="200 ML",foodprice$price*5,
                                                                                                       foodprice$price)))))))
foodprice$unit <- gsub("(50|126|90|13|64) KG","KG",foodprice$unit)
foodprice$unit <- gsub("200 G", "KG", foodprice$unit)
foodprice$unit <- gsub("(200|500) ML","L",foodprice$unit)

#convert the first column to date 
foodprice$date <- as.Date(as.numeric(foodprice$date), origin = "1899-12-30")

#filter the data
foodprice <- foodprice %>%
  filter(priceflag != "forecast", category != "non-food")

write_xlsx(foodprice, "C:\\Users\\user\\Desktop\\MAS 403; Practical statistics\\Kenya food prices.xlsx")


# Average prices of different commodities
commodity_prices <- foodprice %>% 
  group_by(commodity, category, unit) %>%
  summarise(avg_price = mean(price)) %>% 
  arrange(desc(avg_price))

commodity_prices <- data.frame(category = commodity_prices$category, commodity = commodity_prices$commodity, unit = commodity_prices$unit, avg_price = commodity_prices$avg_price)
view(commodity_prices)

write_xlsx(commodity_prices, "C:\\Users\\user\\Desktop\\MAS 403; Practical statistics\\commodity average prices.xlsx")


###Food prices in different regions
foodprice_region<-foodprice %>% 
  select(admin1,category,priceflag,price) %>% 
  group_by(admin1,category) %>% 
  summarise(avr_price=mean(price)) 

foodprices_regions <- data.frame(Region = foodprice_region$admin1, Category = foodprice_region$category, `Average Price` = foodprice_region$avr_price)
view(foodprice_region)
write_xlsx(foodprices_regions, "C:\\Users\\user\\Desktop\\MAS 403; Practical statistics\\Regions food prices.xlsx")



ggplot(foodprice_region,aes(x=admin1,y=avr_price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.75)+
  labs(x="Region",y="Average price",title="Average food price per region")

####Testing  if the data is normally distributed
hist(foodprice_region$avr_price)
qqnorm(foodprice_region$avr_price)
qqline(foodprice_region$avr_price)

results<-kruskal.test(avr_price~admin1,data = foodprice_region)
results

s
boxplot(avr_price~admin1,data = foodprice_region,main="Different box plot for food price in different regions",
        xlab = "Region",ylab = "Average_food_price")

#price type
food_pricetype <- foodprice %>% 
  group_by(pricetype, category) %>% 
  summarise(avr.pricetype.price = mean(price))

food_pricettypess <- data.frame(Category = food_pricetype$category, `Price type` = food_pricetype$pricetype, `Average Price` = food_pricetype$avr.pricetype.price)
view(food_pricettypess)

write_xlsx(food_pricettypess, "C:\\Users\\user\\Desktop\\MAS 403; Practical statistics\\Price types.xlsx")


#Test for normality
hist(food_pricetype$avr.pricetype.price)
qqnorm(food_pricetype$avr.pricetype.price)
qqline(food_pricetype$avr.pricetype.price)

summary(aov(avr.pricetype.price~pricetype, food_pricetype))


#Graph visualization for different price types
ggplot(food_pricetype,aes(x=pricetype,y=avr.pricetype.price,fill=category))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.75)+
  labs(x="Price Type",y="Average price",title="Average food price per Price Type")





















