library(tidymodels)
library(tidyverse)
library(naniar)

#####################################################################################
#reading the data
insurance_data <- read_csv("D:/George Mason University/5. Spring 2021/GBUS 720/insurance.csv")

#checking whether any missing data exist
any_na(insurance_data)

#check duplicate row
insurance_data[duplicated(insurance_data), ]

#remove duplicate row
insurance_data <- insurance_data[!duplicated(insurance_data), ]

#check outliers
ggplot(insurance_data) +
  aes(x = "", y = charges) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(insurance_data) +
  aes(x = sex, y = charges) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(insurance_data) +
  aes(x = region, y = charges) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(insurance_data) +
  aes(x = smoker, y = charges) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#remove outliers
insurance_data <- insurance_data %>% 
  filter(charges <= 50000)
#####################################################################################
#DATA EXPLORATORY
#####################################################################################
#see the skewness for the children data
ggplot(data = insurance_data, mapping = aes(x = children)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 6)

#see the skewness for the bmi data
ggplot(data = insurance_data, mapping = aes(x = bmi)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 35)

#see the skewness for the age data
ggplot(data = insurance_data, mapping = aes(x = age)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 45)
#####################################################################################
#Data Visualization

ggplot(insurance_data) +
  aes(x = region, y = bmi) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(insurance_data) +
  aes(x = smoker, y = age) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(insurance_data) +
  aes(x = region, y = charges) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(data=insurance_data, 
       aes(x=children, y=charges)) +
  geom_bar(stat="identity") +
  facet_wrap(~ region, nrow = 2)

ggplot(data=insurance_data, 
       aes(x=age, y=charges)) +
  geom_bar(stat="identity") +
  facet_wrap(~ region, nrow = 2)

ggplot(data=insurance_data, 
       aes(x=smoker, y=age, fill = smoker)) +
  geom_boxplot() +
  facet_wrap(~ region, nrow = 2)


library(psych)
pairs.panels(insurance_data, main="Relationships Between Predictors")

#relation between age and charge in every region
ggplot(data = insurance_data) + 
  geom_point(mapping = aes(x = age, y = charges)) + 
  facet_wrap(~ region, nrow = 2)

ggplot(data = insurance_data) + 
  geom_smooth(mapping = aes(x = age, y = charges))

#relation between age and charge while the insuers is a smokers or non smokers
ggplot(data = insurance_data) + 
  geom_point(mapping = aes(x = age, y = charges)) + 
  facet_wrap(~ smoker, nrow = 2)

#relation between bmi and charge
ggplot(data = insurance_data) + 
  geom_smooth(mapping = aes(x = bmi, y = charges))

# qplot 
qplot(age,charges, data = insurance_data,color=bmi,geom = c("point", "smooth"))
qplot(age,charges, data = insurance_data,color=bmi,geom = c("point", "smooth"),facets = . ~region )

qplot(bmi,charges, data = insurance_data,color=bmi,geom = c("point", "smooth") )

# sex, region, charges
sw_male <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="male"&&region=="southwest")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
sw_female <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="female"&&region=="southwest")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
nw_male <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="male"&&region=="northwest")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
nw_female <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="female"&&region=="northwest")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
se_male <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="male"&&region=="southeast")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
se_female <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="famle"&&region=="southeast")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
ne_male <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="male"&&region=="northeast")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))
ne_female <- insurance_data %>% select(sex,region,charges) %>% filter(sex=="female"&&region=="northeast")%>%group_by(sex,region)%>%summarise(count = n(),avg_charges = mean(charges))

sex_charges <- rbind(sw_male,sw_female,nw_male,nw_female,se_male,se_female,ne_male,ne_female)
combines <- paste(sex_charges$region,sex_charges$sex)
sex_charges<- cbind(sex_charges,combines)

ggplot(data = sex_charges, aes(x = ...5, fill = sex)) + 
  geom_histogram(stat='identity',aes(y = avg_charges))+
  labs(title = "genger Distribution by Region (Gender - Female/male)",
       x = "Region", y = "Average of Charges")

# smoker, region, charges
sw_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="yes"&&region=="southwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
sw_non_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="no"&&region=="southwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
nw_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="yes"&&region=="northwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
nw_non_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="no"&&region=="northwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
se_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="yes"&&region=="southeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
se_non_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="no"&&region=="southeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
ne_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="yes"&&region=="northeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))
ne_non_smoker <- insurance_data %>% select(smoker,region,charges) %>% filter(smoker=="no"&&region=="northeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_charges = mean(charges))

smoker_charges <- rbind(sw_smoker,sw_non_smoker,nw_smoker,nw_non_smoker,se_smoker,se_non_smoker,ne_smoker,ne_non_smoker)
ggplot(data = smoker_charges, aes(x = region, fill = smoker)) + 
  geom_histogram(stat='identity',aes(y = avg_charges),color = "blue") +
  facet_wrap(~ smoker, nrow = 2) +
  labs(title = "Smoker Distribution by Region (Smoker - Yes/No)",
       x = "Region", y = "Average of Charges")

# smoker, region, ages
sw_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="yes"&&region=="southwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
sw_non_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="no"&&region=="southwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
nw_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="yes"&&region=="northwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
nw_non_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="no"&&region=="northwest")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
se_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="yes"&&region=="southeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
se_non_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="no"&&region=="southeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
ne_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="yes"&&region=="northeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))
ne_non_smoker <- insurance_data %>% select(smoker,region,age) %>% filter(smoker=="no"&&region=="northeast")%>%group_by(smoker,region)%>%summarise(count = n(),avg_age = mean(age))

smoker_charges <- rbind(sw_smoker,sw_non_smoker,nw_smoker,nw_non_smoker,se_smoker,se_non_smoker,ne_smoker,ne_non_smoker)
ggplot(data = smoker_charges, aes(x = region, fill = smoker)) + 
  geom_histogram(stat='identity',aes(y = avg_age),color = "blue") +
  facet_wrap(~ smoker, nrow = 2) +
  labs(title = "Smoker Distribution by Region (Smoker - Yes/No)",
       x = "Region", y = "Average of Ages")
#####################################################################################
## Data Splitting

set.seed(314)
insurance_split <- initial_split(insurance_data, prop = 0.80, strata = charges)

## Extracting Training and Test Sets
insurance_training <- insurance_split %>% training()
insurance_test <- insurance_split %>% testing()
#####################################################################################
## Specify a Recipe
insurance_recipe <- recipe(charges ~ .,
                           data = insurance_training)
summary(insurance_recipe)
#####################################################################################
#original skewness before the step_YeoJohnson process for the age data
ggplot(data = insurance_data, mapping = aes(x = age)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 45)

#skewness after the step_YeoJohnson process for the age data
insurance_recipe %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  prep() %>% 
  bake(new_data = insurance_training) %>% 
  ggplot(mapping = aes(x = age)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 45) +
  labs(title = 'Distribution of Transformed Age',
       x = 'Age',
       y = 'Number of Insuers')
#####################################################################################
#original skewness before the step_YeoJohnson process for the bmi data
ggplot(data = insurance_training, mapping = aes(x = bmi)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 35)

#skewness after the step_YeoJohnson process for the bmi data
insurance_recipe %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  prep() %>% 
  bake(new_data = insurance_training) %>% 
  ggplot(mapping = aes(x = bmi)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 40) +
  labs(title = 'Distribution of Transformed bmi',
       x = 'bmi',
       y = 'Number of Insuers')
#####################################################################################
#original skewness before the step_YeoJohnson process for the children data
ggplot(data = insurance_training, mapping = aes(x = children)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 6)

#skewness after the step_YeoJohnson process for the children data
insurance_recipe %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  prep() %>% 
  bake(new_data = insurance_training) %>% 
  ggplot(mapping = aes(x = children)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 4) +
  labs(title = 'Distribution of Transformed children',
       x = 'children',
       y = 'Number of Insuers')
#####################################################################################
#preparing train data
insurance_transformation <- insurance_recipe %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  #step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()

# Apply to insurance_training
insurance_transformation_train <- insurance_transformation %>% 
  bake(new_data = insurance_training)

# Apply to insurance_test
insurance_transformation_test <- insurance_transformation %>% 
  bake(new_data = insurance_test)
#####################################################################################
#Separation for the X and Y variables for the train and the test set
#####################################################################################
#Train X
insurance_transformation_train
insuranceTrainX <- insurance_transformation_train %>% 
  select(-charges) %>% 
  as.data.frame()
class(insuranceTrainX)

#Train Y
insuranceTrainY <- insurance_transformation_train %>% 
  select(charges) %>% 
  unlist() %>% 
  as.numeric()
class(insuranceTrainY)

#Test X
insurance_transformation_test
insuranceTestX <- insurance_transformation_test %>% 
  select(-charges) %>% 
  as.data.frame()
class(insuranceTestX)

#Test Y
insuranceTestY <- insurance_transformation_test %>% 
  select(charges) %>% 
  unlist() %>% 
  as.numeric()
class(insuranceTestY)

#####################################################################################
#Transform the nominal data into dummy variables and omit the first level
#####################################################################################
#Categorical data feature engineering
library(caret)
dmy <- dummyVars(" ~ .", data = insuranceTrainX, fullRank = T)
insuranceTrainX_dmy <- data.frame(predict(dmy, newdata = insuranceTrainX))
insuranceTestX_dmy <- data.frame(predict(dmy, newdata = insuranceTestX))

#####################################################################################
#Final data that will be used for the whole model
#####################################################################################
#X predictors for the train set
insuranceTrainX_dmy
#X Predictors for the test set
insuranceTestX_dmy

#Y variable for the train set
insuranceTrainY
#Y variable for the test set
insuranceTestY

#####################################################################################
#control function for 10 fold cross validation that will be used accross the whole model
ctrl <- trainControl(method = "cv", number = 10)
#####################################################################################
#Linear Model
#####################################################################################
#linear regression
set.seed(100)
lmFit <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "lm", trControl = ctrl)
lmFit
summary(lmFit)

set.seed(100)
lmFit2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY), method = "lm", trControl = ctrl)
lmFit2

# Compute the new test samples 
lmPred = predict(lmFit, insuranceTestX_dmy)
lmPred2 = predict(lmFit2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
lmValues = data.frame(obs = insuranceTestY, pred = lmPred)
defaultSummary(lmValues)

lmValues2 = data.frame(obs = log(insuranceTestY), pred = lmPred2)
defaultSummary(lmValues2)
#####################################################################################
#robust linear regression
set.seed(100)
rlmFit <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "rlm", trControl = ctrl)
rlmFit

set.seed(100)
rlmFit2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY), method = "rlm", trControl = ctrl)
rlmFit2

rlmImp <- varImp(rlmFit2, scale = FALSE)
plot(rlmImp, top = 7, scales = list(y = list(cex = .95)))

# Compute the new test samples 
rlmPred = predict(rlmFit, insuranceTestX_dmy)
rlmPred2 = predict(rlmFit2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
rlmValues = data.frame(obs = insuranceTestY, pred = rlmPred)
defaultSummary(rlmValues)

rlmValues2 = data.frame(obs = log(insuranceTestY), pred = rlmPred2)
defaultSummary(rlmValues2)
#####################################################################################
#robust linear regression with preprocess by PCA
set.seed(100)
rlmPCA <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "rlm", preProcess = "pca", trControl = ctrl)
# The results are not as good as the one with lm and without PCA
rlmPCA 

set.seed(100)
rlmPCA2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY), method = "rlm", preProcess = "pca", trControl = ctrl)
# The results are not as good as the one with lm and without PCA
rlmPCA2 

# Compute the new test samples 
rlmPCAPred = predict(rlmPCA, insuranceTestX_dmy)
rlmPCAPred2 = predict(rlmPCA2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
rlmPCAValues = data.frame(obs = insuranceTestY, pred = rlmPCAPred)
defaultSummary(rlmPCAValues)

rlmPCAValues2 = data.frame(obs = log(insuranceTestY), pred = rlmPCAPred2)
defaultSummary(rlmPCAValues2)
#####################################################################################
#Partial Least Squares (PLS)
set.seed(100)
plsFit <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "pls", tuneGrid = expand.grid(ncomp = 20), trControl = ctrl)
plsFit

set.seed(100)
plsFit2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY), method = "pls", tuneGrid = expand.grid(ncomp = 20), trControl = ctrl)
plsFit2

# Compute the new test samples 
plsFitPred = predict(plsFit, insuranceTestX_dmy)
plsFitPred2 = predict(plsFit2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
plsFitValues = data.frame(obs = insuranceTestY, pred = plsFitPred)
defaultSummary(plsFitValues)

plsFitValues2 = data.frame(obs = log(insuranceTestY), pred = plsFitPred2)
defaultSummary(plsFitValues2)
#####################################################################################
#Partial Least Squares (PLS) with tuning parameter
set.seed(100)
plsTuneFit <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "pls", tuneGrid = expand.grid(ncomp = 1:8), trControl = ctrl)
plsTuneFit

set.seed(100)
plsTuneFit2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY), method = "pls", tuneGrid = expand.grid(ncomp = 1:8), trControl = ctrl)
plsTuneFit2

# Compute the new test samples 
plsTuneFitPred = predict(plsTuneFit, insuranceTestX_dmy)
plsTuneFitPred2 = predict(plsTuneFit2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
plsTuneFitValues = data.frame(obs = insuranceTestY, pred = plsTuneFitPred)
defaultSummary(plsTuneFitValues)

plsTuneFitValues2 = data.frame(obs = log(insuranceTestY), pred = plsTuneFitPred2)
defaultSummary(plsTuneFitValues2)
#####################################################################################
#Elastic net
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = insuranceTrainX_dmy, y = insuranceTrainY,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl)
enetTune

enetGrid <- expand.grid(lambda = c(0, 0.01, .1), fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune2 <- train(x = insuranceTrainX_dmy, y = log(insuranceTrainY),
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl)
enetTune2

# Compute the new test samples 
enetTunePred = predict(enetTune, insuranceTestX_dmy)
enetTunePred2 = predict(enetTune2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
enetTuneValues = data.frame(obs = insuranceTestY, pred = enetTunePred)
defaultSummary(enetTuneValues)

enetTuneValues2 = data.frame(obs = log(insuranceTestY), pred = enetTunePred2)
defaultSummary(enetTuneValues2)
#####################################################################################
#Non Linear Model
#####################################################################################
#K-NN model
set.seed(100)
knnModel = train(x=insuranceTrainX_dmy, 
                 y=insuranceTrainY, 
                 method="knn",
                 tuneLength=10,
                 trControl = ctrl)
knnModel

set.seed(100)
knnModel2 = train(x=insuranceTrainX_dmy, 
                 y=log(insuranceTrainY), 
                 method="knn",
                 tuneLength=10,
                 trControl = ctrl)
knnModel2

# Compute the new test samples 
knnModelPred = predict(knnModel, insuranceTestX_dmy)
knnModelPred2 = predict(knnModel2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
knnModelValues = data.frame(obs = insuranceTestY, pred = knnModelPred)
defaultSummary(knnModelValues)

knnModelValues2 = data.frame(obs = log(insuranceTestY), pred = knnModelPred2)
defaultSummary(knnModelValues2)
#####################################################################################
#Neural Network model:
nnGrid = expand.grid( .decay=c(0,0.01,0.1), .size=1:10 )
set.seed(100)
# MaxNWts: The maximum allowable number of weights. There is no intrinsic limit in the code, 
# but increasing MaxNWts will probably allow fits that are very slow and time-consuming. We restrict it to 10 hidden units.
# linout: switch for linear output units. Default logistic output units.
nnetModel = train(x=insuranceTrainX_dmy, 
                  y=insuranceTrainY, 
                  method="nnet", 
                  linout=TRUE, 
                  trace=FALSE, 
                  MaxNWts=10 * (ncol(insuranceTrainX_dmy)+1) + 10 + 1, 
                  maxit=500, 
                  tuneGrid = nnGrid)
nnetModel

set.seed(100)
nnetModel2 = train(x=insuranceTrainX_dmy, 
                  y=log(insuranceTrainY), 
                  method="nnet", 
                  linout=TRUE, 
                  trace=FALSE, 
                  MaxNWts=10 * (ncol(insuranceTrainX_dmy)+1) + 10 + 1, 
                  maxit=500, 
                  tuneGrid = nnGrid)
nnetModel2

# Compute the new test samples 
nnetModelPred = predict(nnetModel, insuranceTestX_dmy)
nnetModelPred2 = predict(nnetModel2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
nnetModelValues = data.frame(obs = insuranceTestY, pred = nnetModelPred)
defaultSummary(nnetModelValues)
nnetModelValues2 = data.frame(obs = log(insuranceTestY), pred = nnetModelPred2)
defaultSummary(nnetModelValues2)
#####################################################################################
#Averaged Neural Network models
set.seed(100)
avNNetModel = train(x=insuranceTrainX_dmy, 
                    y=insuranceTrainY, 
                    method="avNNet", 
                    linout=TRUE,
                    trace=FALSE,
                    MaxNWts=10 * (ncol(insuranceTrainX_dmy)+1) + 10 + 1, 
                    maxit=500)
avNNetModel

set.seed(100)
avNNetModel2 = train(x=insuranceTrainX_dmy, 
                    y=log(insuranceTrainY), 
                    method="avNNet", 
                    linout=TRUE,
                    trace=FALSE,
                    MaxNWts=10 * (ncol(insuranceTrainX_dmy)+1) + 10 + 1, 
                    maxit=500)
avNNetModel2

# Compute the new test samples 
avNNetModelPred = predict(avNNetModel, insuranceTestX_dmy)
avNNetModelPred2 = predict(avNNetModel2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
avNNetModelValues = data.frame(obs = insuranceTestY, pred = avNNetModelPred)
defaultSummary(avNNetModelValues)
avNNetModelValues2 = data.frame(obs = log(insuranceTestY), pred = avNNetModelPred2)
defaultSummary(avNNetModelValues2)
#####################################################################################
#MARS model
library(earth)
marsGrid = expand.grid(.degree=1:2, .nprune=2:38)
set.seed(100)
marsModel = train(x=insuranceTrainX_dmy, 
                  y=insuranceTrainY, 
                  method="earth", 
                  tuneGrid=marsGrid)
marsModel

set.seed(100)
marsModel2 = train(x=insuranceTrainX_dmy, 
                  y=log(insuranceTrainY), 
                  method="earth", 
                  tuneGrid=marsGrid)
marsModel2

marsImp <- varImp(marsModel, scale = FALSE)
plot(marsImp, top = 4, scales = list(y = list(cex = .95)))

# Compute the new test samples 
marsModelPred = predict(marsModel, insuranceTestX_dmy)
marsModelPred2 = predict(marsModel2, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
marsModelValues = data.frame(obs = insuranceTestY, pred = marsModelPred)
marsModelValues2 = data.frame(obs = insuranceTestY, pred = marsModelPred2)

marsPR = postResample(pred=marsModelPred, obs=insuranceTestY)
marsPR
marsPR2 = postResample(pred=marsModelPred2, obs=log(insuranceTestY))
marsPR2
#####################################################################################
#Support Vector Machine
#set.seed(100)

# tune against the cost C
#svmRModel = train(x=insuranceTrainX_dmy, 
#                  y=insuranceTrainY, 
#                  method="svmRadial", 
#                  tuneLength=20)
#svmRModel

# Compute the new test samples 
#svmRModelPred = predict(svmRModel, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
#svmRModelValues = data.frame(obs = insuranceTestY, pred = svmRModelPred)

#svmRModelPR = postResample(pred=svmRModelPred, obs=insuranceTestY)
#svmRModelPR
#####################################################################################
#tree regression
#####################################################################################
#set.seed(100)

## rpart2 is used to tune max depth 
#rpartTune <- train(x = insuranceTrainX_dmy, y = insuranceTrainY, method = "rpart2",tuneLength = 20, trControl = ctrl)
#rpartTune

#FinalTree = rpartTune$finalModel
#FinalTree

# Compute the new test samples 
#rpartTunePred = predict(rpartTune, insuranceTestX_dmy)

# Evaluate the test performance using a caret function
#rpartTuneValues = data.frame(obs = insuranceTestY, pred = rpartTunePred)

#rpartTunePR = postResample(pred=rpartTunePred, obs=insuranceTestY)
#rpartTunePR
#####################################################################################
# Boosted tree: library(gbm)
#library(gbm)
#set.seed=100
#gbmModel = gbm.fit( insuranceTrainX_dmy, insuranceTrainY, distribution="gaussian", n.trees =100, interaction.depth=7, shrinkage=0.1)
#summary(gbmModel)


###############################################################################################################################################################################################################################################################