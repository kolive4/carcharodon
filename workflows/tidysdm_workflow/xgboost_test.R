# load data
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
# fit model
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nrounds = 2,
               nthread = 2, objective = "binary:logistic")

pd = effectplots::partial_dependence(object = bst, v = names(train$data), data = train$data)
fe = effectplots::feature_effects(object = bst, v = names(train$data), data = train$data)

# predict
pred <- predict(bst, test$data)