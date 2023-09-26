library(readr)
#create function that will help in loading data without much repetition
load <- function(filename)
{
  read.csv(filename,header=TRUE)
}
#load file orders.csv
file="orders.csv"
orders=load(file)
head(orders)

##exploring the data
##finding dimensions`i.e the number of rows and number of columns

dim(orders)


##check if there columns containing null values
##return number of null values in each column

colSums(is.na(orders))

#remove all null values from the 
orders$promo_code_discount_percentage[is.na(orders$promo_code_discount_percentage)] <- mean(orders$promo_code_discount_percentage,na.rm=T)
orders$vendor_rating[is.na(orders$vendor_rating)] <- mean(orders$vendor_rating,na.rm=T)
orders$preparationtime[is.na(orders$preparationtime)] <- mean(orders$preparationtime,na.rm=T)
orders$item_count[is.na(orders$item_count)] <- mean(orders$item_count,na.rm=T)
orders <- na.omit(orders)

##confirm that the null values have been eliminated

colSums(is.na(orders))

##Analysing numerical columns

#selecting numerical columns from the data
library(dplyr)
new_orders <- select_if(orders, is.numeric)
new_orders <- select(new_orders,-c(akeed_order_id,payment_mode,LOCATION_NUMBER))
head(new_orders)
##summarising the numerical part of the data
summary(new_orders)

##investigate how various aspect relate to vendor ratings i.e discount, preparation time, item count and delivery distance.

library(ggplot2)
library(gridExtra)
rel1 <- ggplot(new_orders,aes(x=vendor_discount_amount,y=vendor_rating))+geom_point(color="red")
rel2 <- ggplot(new_orders,aes(x=preparationtime,y=vendor_rating))+geom_point(color="blue")
rel3 <- ggplot(new_orders,aes(x=deliverydistance,y=vendor_rating))+geom_point(color="green")
rel4 <- ggplot(new_orders,aes(x=item_count,y=vendor_rating))+geom_point(color="yellow")
grid.arrange(rel1,rel2,rel3,rel4,ncol=2)

##finding correlation between the column

cor(new_orders)


##evaluate distribution of various variables
his1 <- ggplot(new_orders,aes(x=vendor_rating))+geom_histogram(binwidth=0.5,colour="blue")
dis1 <-ggplot(new_orders,aes(x=vendor_rating))+geom_density(colour="blue")
his2 <- ggplot(new_orders,aes(x=item_count))+geom_histogram(binwidth=0.5,colour="green")
dis2 <- ggplot(new_orders,aes(x=item_count))+geom_density(colour="green")
his3 <- ggplot(new_orders,aes(x=deliverydistance))+geom_histogram(binwidth=0.5,colour="red")
dis3 <- ggplot(new_orders,aes(x=deliverydistance))+geom_density(colour="red")
his4 <- ggplot(new_orders,aes(x=preparationtime))+geom_histogram(binwidth=0.5,colour="red")
dis4 <- ggplot(new_orders,aes(x=preparationtime))+geom_density(colour="red")
grid.arrange(his1,dis1,his2,dis2,his3,dis3,his4,dis4,ncol=4)

##Grouping data based on categorical variables for this case payment mode
##understanding which mode of payment mostly used

library(dplyr)
orders %>% group_by(payment_mode) %>% 
  summarise_at(vars(item_count,grand_total),list(name=mean))

##detecting outliers on the selected features

b1 <- ggplot(new_orders,aes(x=vendor_rating))+geom_boxplot(colour="red",fill="blue")
b2 <- ggplot(new_orders,aes(x=item_count))+geom_boxplot()
b3 <- ggplot(new_orders, aes(x=preparationtime))+geom_boxplot()
b4 <- ggplot(new_orders,aes(x=deliverydistance)) + geom_boxplot()
grid.arrange(b1,b2,b3,b4,ncol=2)
##removing the outliers

#fist lets create a dataframe to store the selected features
sel_order <- data.frame(orders$vendor_rating,orders$item_count,orders$preparationtime,orders$deliverydistance)
names(sel_order) <- c("vendorratting","itemsbought","preparationtime","deliverydistance")
head(sel_order)

##Remove outliers using the z-score

#find the scores and put them in a dataframe
out <- function(data){
  as.data.frame(sapply(data, function(data) (abs(data-mean(data))/sd(data))))
}
z<-out(sel_order)
#select rows with z-score less than 1
clean_orders <- sel_order[!rowSums(z>1), ] #keep the rows with z-score less than 1
dim(clean_orders)

##test for correlation between preparation time and rating

cor.test(x=clean_orders$preparationtime,y=clean_orders$vendorratting,method = "pearson")

vendor <- load("vendors.csv")
head(vendor)

#find no of rows and columns
dim(vendor)


#number of rows that are null
colSums(is.na(vendor))

##Replace na values in commission column with the mean

vendor$commission[which(is.na(vendor$commission))] <- mean(vendor$commission,na.rm=T)

##select numeric columns while eliminating columns that may be considered categorical or other formats.

num_vendor <-select_if(vendor,is.numeric)
num_vendor <- select(num_vendor,-c(id,authentication_id,latitude,longitude,vendor_category_id,status,verified,is_open,rank,open_close_flags,country_id,city_id,device_type))
head(num_vendor)

## summary statistic
summary(num_vendor)
{r fig4 fig.height = 5 fig.width = 13 fig.align = "center"}
library(corrplot)
corrplot(cor(num_vendor))
{r fig1, fig.height = 10, fig.width = 13, fig.align = "center"}
plot(num_vendor,main="relationship between numerical variables")

##detect outliers in numerical variables
{r fig3, fig.height = 5, fig.width = 13, fig.align = "center"}
boxplot(num_vendor)
##Removing column with outliers using


sel_vendor <- select(num_vendor,-c(prepration_time,discount_percentage))
head(sel_vendor)

#check if delierydistance is normally distributed using qqplot
qqnorm(sel_order$deliverydistance)
qqline(sel_order$deliverydistance)


#check if servingdistance is normally distributed using qqplot
qqnorm(sel_vendor$serving_distance)
qqline(sel_vendor$serving_distance)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
delivery <- normalize(sel_order$deliverydistance)
serving <- normalize(sel_order$deliverydistance)```
H0: mu1=mu2
H1: mu1 not equal mu2

library(BSDA)
res <- z.test(sel_order$deliverydistance, y = sel_vendor$serving_distance, alternative = "two.sided", mu = 0, sigma.x = var(sel_order$deliverydistance), sigma.y = var(sel_vendor$serving_distance), conf.level = 0.95)
res

library(data.table)
train_full <- fread("train_full.csv",select=c("delivery_charge","serving_distance","vendor_rating","vendor_category_en","vendor_category_id"),showProgress= F)
head(train_full)

##test if the mean of delivery_charge,serving distance and that of vendor rating, in full dataset is greater to that in in vendor dataset 
let s1 be mean in vendor dataset and m1 that of whole dataset
H0: s1=m1
H0: s1 < m1
at 95% confidence level

##testing for significance difference for delivery charge 
z.test(x=normalize(sel_vendor$delivery_charge),y=NULL,alternative="less",mu=mean(normalize(train_full$delivery_charge)),sigma.x=var(normalize(sel_vendor$delivery_charge)),conf.level=0.95)

##testing for difference in mean for serving distance
z.test(x=sel_vendor$serving_distance,y=NULL,alternative="less",mu=mean(normalize(train_full$serving_distance)),sigma.x=var(sel_vendor$serving_distance),conf.level=0.95)

##graphing categorical variables 
##status and verified describing account status
##is_akeed_delivering stating if the the vendor has delivered

library(ggplot2)
library(gridExtra)
bar1 <- ggplot(data=vendor,aes(x=vendor_category_en))+geom_bar(fill="red")
bar2 <- ggplot(data=vendor,aes(x=is_akeed_delivering))+geom_bar(fill="blue")
bar3 <- ggplot(data=vendor,aes(x=as.factor(status)))+geom_bar(fill="green")
bar4 <- ggplot(data=vendor,aes(x=as.factor(verified)))+geom_bar(fill="yellow")
grid.arrange(bar1,bar2,bar3,bar4,ncol=2)


##status and verified accounts based on vendors category

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r") #load when connected to internet
vendor$vendor_category_en <- as.factor(vendor$vendor_category_en)
vendor$verified <- as.factor(vendor$verified)
vendor$status <- as.factor(vendor$status)
crosstab(vendor,row.vars="vendor_category_en",col.vars=c("status","verified"),type="f")

##load location data

location <- load('train_locations.csv')
head(location)

dim(location)

location$longitude[is.na(location$longitude)] <- mean(location$longitude,na.rm=T)
location$latitude[is.na(location$latitude)] <- mean(location$latitude,na.rm=T)

##check for outliers in the the coordinates using a scatter plot

ggplot(data = location) + 
  geom_point(aes(x = longitude, y = latitude))

## Taking coordinates for the upper half

Q1 <- quantile(location$longitude, .50)
Q3 <- quantile(location$longitude, .1)
IQR <- IQR(location$longitude)

no_outliers <- subset(location, location$longitude > (Q1 - 1.5*IQR) & location$longitude < (Q3 + 1.5*IQR))
dim(no_outliers)

##clustering customers based on location to identify closest vendor i.e resturant based on vendor category id and category

cord <- data.frame(location$longitude,location$latitude)
names(cord) <- c("long","lat")
set.seed(123)
clus.res <- kmeans(cord,2, iter.max=100)
clus.res

clus.dat <- cbind(cord,clusters=clus.res$cluster)
head(clus.dat)

#load the testfull file
library(data.table)
test_id <- fread("test_full.csv",select=c("serving_distance","delivery_charge","status_y","verified_y","language","vendor_category_id","vendor_category_en",""),showProgress = F)
test_id

test_x <- select(test_id,-c("vendor_category_id","vendor_category_en","language"))#remove targets from the set
test_x <- cbind(test_x,language=as.numeric(as.factor(test_id$language)))
test_x <- data.frame(delivery_charge=as.numeric(as.factor(test_x$delivery_charge)),serving_distance=as.numeric(as.factor(test_x$serving_distance)),
                     status=as.numeric(as.factor(test_x$status_y)),verified=as.numeric(as.factor(test_x$verified_y)),language=as.numeric(as.factor(test_x$language)))
head(test_x)

#create target sample sample size as length of the clustaers dataframe.

clus.data<- cbind(clus.dat, restaurant=as.factor(test_id$vendor_category_id[1:nrow(clus.dat)]))
head(clus.data)

##visualising the clusters

ggplot() +
  geom_point(data = clus.data, 
             mapping = aes(x = long, 
                           y = lat, 
                           colour = clusters))+  
  geom_point(mapping = aes_string(x = clus.res$centers[, "long"], 
                                  y = clus.res$centers[, "lat"]),
             color = "red", size = 4)

customer <- load('train_customers.csv')
head(customer)

colSums(is.na(customer))




##evaluate customer gender by selecting 100 first samples

barplot(table(customer$gender[1:100]))
##convert gender to binaries

library(dplyr)

new_customer <- select(customer,-c(dob,created_at,updated_at,akeed_customer_id)) #eliminate column dob,created_at and updated_at
head(new_customer)

##convert the categorical variables to numeric for purpose of training

num <- function(col){
  as.numeric(as.factor(col))
}
sel_customer <- data.frame(num(new_customer$gender),num(new_customer$status),num(new_customer$verified),num(new_customer$language))
names(sel_customer) <- names(new_customer)
head(sel_customer)


##combine all the clean data for training with only 50000 rows

set.seed(123)
sample_orders <- train_full[1:50000,] #restaurant order information
sample_customers <- sel_customer[1:50000,]
train <- cbind(sample_orders,sample_customers)
train <- select(train,-c("vendor_rating","vendor_category_en","vendor_category_id"))
head(train)

new_train <- cbind(train,restaurant=as.factor(train_full$vendor_category_en[1:50000]))#attach the target
new_train <- select(new_train,-c("gender")) #remove categorical variable gender
head(new_train)

##perform the svm to categorise restaurants

library(caret)
library(e1071) 
model <- svm(restaurant~.,data=new_train,method="c-classification",kernal="radial")
summary(model)

##finding the predictions

library(caret)
preds <- predict(model,test_x)
preds

res <- table(as.factor(test_id$vendor_category_en),preds)
confusionMatrix(res)

##naive bayes algorithm

library(e1071)
library(caTools) 
library(caret)
naiv <- naiveBayes(restaurant ~ ., data = new_train) 
naiv

##find naiv predictions

rest_pred <- predict(naiv, newdata = test_x) 

##evaluate naive bayes performance

perfm <- table(as.factor(test_id$vendor_category_en),rest_pred)
confusionMatrix(perfm)


