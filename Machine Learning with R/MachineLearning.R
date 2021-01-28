library(tidyverse)
library(reshape2)

housing = read.csv('housing.csv')
head(housing)
summary(housing)

par(mfrow=c(2,5)) # Create a 2 x 5 plotting matrix

colnames(housing)
ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')


# Step 2. Clean the data
#inpute missing values
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

#Fix the total columns - make them means
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms', 'total_rooms')
housing = housing[ , !(names(housing) %in% drops)]

head(housing)


# Turn categoricals into booleans
categories = unique(housing$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)
for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}
head(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))

tail(cat_housing)

#Add distance to nearest city of at least 1M population
majorCities = read.csv('worldcities.csv')
majorCities <- subset(majorCities, population >= 1000000) #limit to population over 1M

#Find lon and lat spread in housing
minLong = min(housing_num$longitude, na.rm=T)
maxLong = max(housing_num$longitude, na.rm=T)
minLat = min(housing_num$latitude, na.rm=T)
maxLat = max(housing_num$latitude, na.rm=T)

#Limit Major Cities to the same spread with margin around the edges
majorCities <- subset(majorCities, lng > minLong - 1)
majorCities <- subset(majorCities, lng < maxLong + 1)
majorCities <- subset(majorCities, lat > minLat - 1)
majorCities <- subset(majorCities, lat < maxLat + 1)


head(majorCities)

#Function calulates the distance between two pints given their lat and long
distance <- function(lat1, lon1, lat2, lon2) {
  radius = 7917.5 #in miles
  p = pi / 180
  a = 0.5 - cos((lat2 - lat1) * p)/2 + 
    cos(lat1 * p) * cos(lat2 * p) * 
    (1 - cos((lon2 - lon1) * p))/2;
  
  return(radius * asin(sqrt(a)))
}

#Function returns the distance between a point and the nearest Major City
shortestDistanceToMajorCity <- function(lat, lon) {
  shortestDistance <- 12500 # initialize distance to max
  for (i in 1:nrow(majorCities)) {
    distance = distance(lat, lon, majorCities[i, 3], majorCities[i, 4])
    if (distance < shortestDistance) {
      shortestDistance <- distance
    }
  }
  return (shortestDistance)
}
 
head(majorCities) 


distance(32.54, -117.04, 32.6633, -115.4678)

shortestDistanceToMajorCity(32.54, -117.04)


housing$distance_nearest_city <- apply(housing_num, 1, FUN = function(x) shortestDistanceToMajorCity(as.numeric(x[2]), as.numeric(x[1])) )

head(housing)

#Scale the numerical variables
drops = c('ocean_proximity','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]

head(housing_num)

summary(housing_num)

scaled_housing_num = scale(housing_num)
head(scaled_housing_num)

#Merge the altered numerical and categorical dataframes
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)






#Step 3. Create a test set of data
set.seed(1738) # Set a random seed so that same sample can be reproduced in future runs

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples
head(train)

nrow(train) + nrow(test) == nrow(cleaned_housing)


#Step 4. Test some predictive models
library('boot')
?cv.glm # note the K option for K fold cross validation

glm_house = glm(median_house_value~median_income+mean_rooms+population+latitude+longitude, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing , glm_house, K=5)

k_fold_cv_error$delta
glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse
summary(housing)

names(glm_house) #what parts of the model are callable?
glm_house$coefficients


# Random forest model
library('randomForest')

?randomForest

set.seed(1738)

train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']

head(train_y)
head(train_x)

rf_model = randomForest(train_x, y = train_y , ntree = 500, importance = TRUE)

names(rf_model) #these are all the different things you can call from the model

rf_model$importance


oob_prediction = predict(rf_model)
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse


