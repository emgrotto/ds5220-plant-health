##### Data Prepping

data = read.csv("plant_moniter_health_data.csv") ## Reading in the data after
								 ## downloading from github
data = data[,-c(1,8)] ## Removing the Plant_ID
data$Health_Status = as.factor(data$Health_Status)

##### Train test splitting

set.seed(4172025)
split = sample.int(n = nrow(data), size = floor(.80*nrow(data)), replace = F)

train = data[split,]
test = data[-split,]

##### GLM

glmPlant = glm(Health_Status ~ Temperature_C *  Humidity_. * Soil_Moisture_.*
			Soil_pH * Nutrient_Level * Light_Intensity_lux, 
            	family='binomial', data=data)

anova(glmPlant)

## Interactions with a significance of 0.05: 
## Humidity_.:Soil_Moisture_.:Nutrient_Level:Light_Intensity_lux 
## Humidity_.:Soil_Moisture_.:Soil_pH:Light_Intensity_lux

## Interactions with a significance of 0.1: 
## Temperature_C                                    
## Temperature_C:Humidity_.:Soil_pH:Nutrient_Level:Light_Intensity_lux

##### Descision Tree
# library(tidymodels)

# dt = decision_tree(mode = "classification")
# fittedDT = fit(dt,Health_Status ~ Temperature_C * Humidity_. * Soil_Moisture_. * Soil_pH * Nutrient_Level * Light_Intensity_lux,data)

## Trees can not handle interation terms, as it so happens. 

##### Prediction with interaction terms. 

withInteract = glm(Health_Status ~ Temperature_C *  Humidity_. * Soil_Moisture_.*
			Soil_pH * Nutrient_Level * Light_Intensity_lux, 
            	family='binomial', data=train)

pred = predict(withInteract,test)

predCat = ifelse(pred > 0.5, 1, 0)
table(predCat,test[,7])

## predCat   0   1
##       0   1   4
##       1  33 162

## Compated to our previous models, a glm with interactions does seem to preform
## better. This is likely because it appears there is some statistically
## significant interaction between the external enviromental factors on both
## the soil_pH and nutrient levels. 