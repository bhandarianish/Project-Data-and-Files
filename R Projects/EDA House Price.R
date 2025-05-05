


# Required Libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(caret)
library(janitor)
library(doParallel)


# Reads the data
house <- read.csv("https://raw.githubusercontent.com/NickSager/DS_6372_Ames2/master/Data/train.csv")

house <- data.frame(house)
summary(house)
str(house)


# Convert chr into factors
house[c(2,3,6:17,22:26,28:34,36,40:43,54,56,58,59,61,64:66,73:75,79:80)] <- lapply(house[c(2,3,6:17,22:26,28:34,36,40:43,54,56,58,59,61,64:66,73:75,79:80)],as.factor)


# visualize missing data
install.packages("naniar")
library(naniar)
vis_miss(house[c(1:40)])
vis_miss(house[c(41:81)])

# take out Fence(missing 91.2%), fireplace (50%), and Alley (93%), no needed

#house1 <- house[,-c(7,58,74)]
#vis_miss(house1[c(1:40)])
#vis_miss(house1[c(41:77)])

# to assign levels to missing values


#list column names
colnames(house)
# Adress missing values, 

house$MasVnrType[is.na(house$MasVnrType)] <- "None"

house$MasVnrArea[is.na(house$MasVnrArea)] <- "0"
house$MasVnrArea = as.integer(house$MasVnrArea)


house$LotFrontage[is.na(house$LotFrontage)] <- "0"
house$LotFrontage = as.integer(house$LotFrontage)

#basement quality
levels(house$BsmtQual) <- c(levels(house$BsmtQual), "No Basement")
house$BsmtQual[is.na(house$BsmtQual)] <- as.factor("No Basement")

#basement condition
levels(house$BsmtCond) <- c(levels(house$BsmtCond), "No Basement")
house$BsmtCond[is.na(house$BsmtCond)] <- as.factor("No Basement")

#basement exposure
levels(house$BsmtExposure) <- c(levels(house$BsmtExposure), "No Basement")
house$BsmtExposure[is.na(house$BsmtExposure)] <- as.factor("No Basement")

#basement finnsih type 1
levels(house$BsmtFinType1) <- c(levels(house$BsmtFinType1), "No Basement")
house$BsmtFinType1[is.na(house$BsmtFinType1)] <- as.factor("No Basement")


#basement finnish type 2
levels(house$BsmtFinType2) <- c(levels(house$BsmtFinType2), "No Basement")
house$BsmtFinType2[is.na(house$BsmtFinType2)] <- as.factor("No Basement")

levels(house$BsmtFinType2) <- c(levels(house$BsmtFinType2), "No Basement")
house$BsmtFinType2[is.na(house$BsmtFinType2)] <- as.factor("No Basement")

levels(house$BsmtFinType2) <- c(levels(house$BsmtFinType2), "No Basement")
house$BsmtFinType2[is.na(house$BsmtFinType2)] <- as.factor("No Basement")



#If garage-related variables are NA, assume there is no garage and assign to 0
house <- house %>%
  mutate(
    GarageType = ifelse(is.na(GarageType), "None", GarageType),
    #GarageYrBlt = ifelse(is.na(GarageYrBlt), 0, GarageYrBlt), #These will be changed to the mean because of large year values
    GarageFinish = ifelse(is.na(GarageFinish), "None", GarageFinish),
    GarageCars = ifelse(is.na(GarageCars), 0, GarageCars),
    GarageArea = ifelse(is.na(GarageArea), 0, GarageArea),
    GarageQual = ifelse(is.na(GarageQual), "None", GarageQual),
    GarageCond = ifelse(is.na(GarageCond), "None", GarageCond)
)


# add a column called sales price in thousands for easy interpretation
house$saleprice.t = house$SalePrice/10000

# ggpairs to view relationship

library(GGally)
house %>%  select(c(2:4,82)) %>% ggpairs(aes())
house %>%  select(c(5:9,82)) %>% ggpairs(aes())
house %>%  select(c(10:12,14,15,82)) %>% ggpairs(aes())
house %>%  select(c(16:20,82)) %>% ggpairs(aes()) #4
house %>%  select(c(21:24,81)) %>% ggpairs(aes()) #5
house %>%  select(c(26:31,81)) %>% ggpairs(aes()) #6
house %>%  select(c(32:38,81)) %>% ggpairs(aes()) #7
house %>%  select(c(39:45,81)) %>% ggpairs(aes()) #8
house %>%  select(c(46:52,81)) %>% ggpairs(aes()) #9
house %>%  select(c(53:60,81)) %>% ggpairs(aes()) #10
# 11
house %>%  select(c(61:67,81)) %>% ggpairs(aes())
house %>%  select(c(68:74,81)) %>% ggpairs(aes())
house %>%  select(c(75:80,81)) %>% ggpairs(aes())


# display corelation plot
#install.packages("corrplot")
library(corrplot)
corrplot(cor(house[, unlist(lapply(data, is.numeric))]))


# scatterplot of Sales Price with other variables
plot(house$saleprice.t)
boxplot(house$saleprice.t)
summary(house$SalePrice.t)
plot(house$LotArea, house$saleprice.t)

plot(house$LandContour, house$saleprice.t)
plot(house$LotShape, house$saleprice.t)
plot(house$LotConfig, house$saleprice.t)
plot(house$LandSlope, house$saleprice.t)
plot(house$Condition1, house$saleprice.t)
plot(house$Condition2,house$saleprice.t)
plot(house$OverallQual,house$saleprice.t)
plot(house$RoofStyle,house$saleprice.t)
plot(house$MasVnrType,house$saleprice.t)
plot(house$Foundation,house$saleprice.t)

plot((house$BsmtFinSF1+house$BsmtFinSF2),house$saleprice.t)
cor(((house$BsmtFinSF1+house$BsmtFinSF2)>1000),house$saleprice.t)
cor(house$BsmtFinSF1 ,house$saleprice.t)
plot(house$BsmtFinSF1 ,house$saleprice.t)

plot((house$GrLivArea)^2, house$saleprice.t)
cor(house$GrLivArea, house$saleprice)
cor((house$GrLivArea)^2, house$saleprice.t)

plot(house$YrSold, house$saleprice.t)

# Model chosen for question 1 was SalesPrice Vs  GrLivArea for 3 common Housestyles - 1 story, 1.5 story and 2 story
hist(house$GrLivArea)
plot(house$GrLivArea)
quantile(house$GrLivArea, probs=0.987)
# for better interpretability and based on the histogram, 3000 sq ft used as upper limit for the model. Using the quantile, 3000 sfft covers close to 99% of the data.

hist(house$SalePrice)
plot(house$SalePrice)

# plot for GrLivArea vs salesprice with HouseStyle category, few other categories for comparison
ggplot(data= filter(house, GrLivArea<=3000),aes(x=GrLivArea,y=saleprice.t,colour=BldgType))+geom_point() + geom_smooth()
ggplot(data= filter(house, GrLivArea<=3000),aes(x=GrLivArea,y=saleprice.t,colour=BedroomAbvGr))+geom_point() + geom_smooth()
ggplot(data= filter(house),aes(x=GrLivArea,y=saleprice.t,colour=HouseStyle))+geom_point() + geom_smooth()
ggplot(data= filter(house,GrLivArea<=3000 ),aes(x=GrLivArea,y=saleprice.t,colour=HouseStyle))+geom_point() + geom_smooth()
ggplot(data= filter(house, (HouseStyle =="1.5Fin"| HouseStyle =="2Story" |HouseStyle=="1Story") & GrLivArea<=3000  ),aes(x=GrLivArea,y=saleprice.t,colour=HouseStyle))+geom_point() + geom_smooth()

# The 3 styles visually look to have same slope based on the plot


# model 1 without transformation
model1 <- lm(SalePrice~GrLivArea + HouseStyle ,data=filter(house,GrLivArea<=3000 &  HouseStyle =="1.5Fin"| HouseStyle =="2.5Fin" |HouseStyle=="1Story"))

summary(model1)

plot(model1)

# Residual constant variation


# model 2 with lof transformation on response variable

model2 <- lm(log(SalePrice)~GrLivArea + HouseStyle,data=filter(house, (HouseStyle =="1.5Fin"| HouseStyle =="2Story" |HouseStyle=="1Story") & GrLivArea<=3000))
summary(model2)

plot(model2)

# residual good, interpretation good
