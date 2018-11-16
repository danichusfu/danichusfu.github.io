###### Homework for BMA presentation by Matt and Dani
###### Due November 22nd
###### Name:
###### There is 1 question and F parts to this homework
######
###### Note: install appropriate packages and
###### run code above for loop
###### all questions will be completing information inside the for loop
###### if you want to test your code before everything is complete
###### set i to 1 and step through the for loop (without running the whole thing)
###### to test each part of the question



# Install package manager package called pacman
# install.packages(pacman)
pacman::p_load(tidyverse, BMS, leaps, glmnet)

# attach data
data(datafls)

# make it a tibble for nicer console output
data_fls <- 
  datafls %>% 
  as_tibble()

# make empty list
rmse <- list()

# Split your data for 5 fold validation
# each fold is the sample we will hold out on each loop
data_fls <- 
  data_fls %>% 
  # make a column for fold for cv
  mutate(fold = map_dbl(.x = y, .f = ~ sample(1:5, size = 1, replace = T)))


for(i in 1:5){
  
  # Move everything that is not in the current hold out fold to the train set
  data_fls_train <- 
    data_fls %>% 
    filter(fold != i) %>%
    select(-fold)
    
  # move everything in the hold out fold into the test set
  data_fls_test <- 
    data_fls %>% 
    filter(fold == i) %>%
    select(-fold)
  
  # fit a stepwise linear regression model
  # save the best one of each size
  models <- regsubsets(y ~ ., 
                       data = data_fls_train, 
                       nvmax = 41,
                       method = "seqrep")
  
  # choose one of these models based on a criteria 
  # either adjr2 or bic
  summary(models)$adjr2
  summary(models)$bic
  
  # extract the coefficients for the model with the best adjusted r^2
  step_model_adjr2 <- coef(models, which.max(summary(models)$adjr2))
  
  # make a prediction on your test set
  # get the model matrix
  test_mat <- model.matrix(y ~ ., data = data_fls_test)
  
  # make the prediction
  step_adjr2_pred <- test_mat[, names(step_model_adjr2)] %*% step_model_adjr2
  
  # get your out of sample rmse
  step_adjr2_rmse <- sqrt( mean( (data_fls_test$y - step_adjr2_pred)^2 ) )
  
  ################
  ### PART A #####
  ################
  
  # Calculate the rmse if we were to select the model based on BIC instead of 
  # adjusted R^2
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################################################################
  
  # set lambdas for glmnet cv
  lambdas <- 10^seq(3, -2, by = -.1)
  
  # create data matrix x
  data_fls_train_x <- 
    data_fls_train %>% 
    select(-y) %>%
    as.matrix()
  
  
  ################
  ### PART B #####
  ################
  
  # fit a ridge regression model using cross validation to choose the best lambda
  # and the data in data_fls_train_x
  # save the model in an object called ridge_model
  # use the code below to assess its rmse
  
  # get optimal lambda
  
  
  
  # fit ridge model
  ridge_model <- 
  
  
  
  
  
  
  
  
  
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  ####################################################################################################
  
  # create data matrix x for test set
  data_fls_test_x <- 
    data_fls_test %>% 
    select(-y) %>%
    as.matrix()
  
  # make prediction for test set ridge model
  ridge_test_pred <- 
    data_fls_test %>%
    mutate(prediction = predict(ridge_model, data_fls_test_x))
  
  # calculate rmse for ridge test set
  ridge_rmse <-  
    ridge_test_pred %>%  
    summarise(rmse = sqrt( mean( (y - prediction)^2 )) ) %>%
    pull(rmse)
  
  
  ################
  ### PART C #####
  ################
  
  # Fill in the missing parameter needed to fit the bayesian model averaging
  
  # bayesian model averaging
  mfls <- 
    bms(data_fls_train, 
        # burn in, number of iterations to run 
        # before calculating statistics
        burn = 100000, 
        # number of iterations for which to save 
        # information for
        iter = 200000, 
        # g-prior for Betas
        g = "BRIC", 
        # model prior is BLANK
        mprior = "", 
        # only save the top 2000 models
        nmodel = 2000, 
        # use a birth death metorpolis sampler
        mcmc = "bd", 
        # see output in console
        user.int = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############################################################################
  
  # exact gets the information for the top 2000 models
  # coef(mfls, include.constant = TRUE)
  
  # predict for the test set for the model using the average posterior density 
  # value for each coefficient 
  # this for averaging over all models
  bma_avg_pred <- predict(mfls, data_fls_test)
  
  # calculate the out of sample rmse
  bma_avg_rmse <- sqrt( mean( (data_fls_test$y - bma_avg_pred)^2 ) )
  
  
  # make predictions on the test set for only the top model
  bma_best_pred <- predict(mfls, data_fls_test, topmodels = 1)
  
  # get out of sample rmse for test set
  bma_best_rmse <- sqrt( mean( (data_fls_test$y - bma_best_pred)^2 ) )
  
  
  # make predictions on the test set for only the top model
  bma_100_pred <- predict(mfls, data_fls_test, topmodels = 1:100)
  
  # get out of sample rmse for test set
  bma_100_rmse <- sqrt( mean( (data_fls_test$y - bma_100_pred)^2 ) )
  
  ################
  ### PART D #####
  ################
  
  # Create predictions for bma using the average of the top 1000 models and the top 2000 models
  # calculate the rmse for these 2 new models 
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################################################################
  
  ################
  ### PART E #####
  ################
  
  # add any new rmse values that you've calculated and add them to this tibble
  
  rmse[i] <-
    list(tibble(step_adjr2_rmse, ridge_rmse, bma_avg_rmse, bma_best_rmse, bma_100_rmse, bma_1000_rmse ))
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################################################################
}


################
### PART F #####
################

# Run the for loop above 
# Then run the code below
# write a short paragraph about your results

# Look at each run individually
tibble(rmse) %>%
  unnest()

# compare average rmse values for all methods
tibble(rmse) %>%
  unnest() %>%
  summarise_all(mean)













#########################################################################################################
