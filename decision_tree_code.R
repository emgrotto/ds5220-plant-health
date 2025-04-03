library(tidymodels)

set.seed(432025)

plant = read.csv("plant_moniter_health_data.csv")

plant$Health_Status = as.factor(plant$Health_Status)

dt = decision_tree(mode = "classification")

split = sample.int(n = nrow(plant), size = floor(.80*nrow(plant)), replace = F)

train = plant[split,]
test = plant[-split,]

fittedDT = fit(dt,Health_Status ~ Temperature_C + Humidity_. + Soil_Moisture_. + Soil_pH + Nutrient_Level + Light_Intensity_lux,train)

pred = predict(fittedDT,new_data = test)

table(pred$.pred_class,test[,9])

##       0   1
##   0   0   1
##   1  24 175


