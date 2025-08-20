# Run cross-validation using a neural network

# If running this script separately (not as part of the general report) load the data
if (!exists("D_wsl")) {
    library(dplyr)
    # library(caret)

    # Response variable
    RESPONSE <- "species"

    # Columns with ID information that should not be used for training models
    idcols_wsl <- c("Source", "event.id", "call.id", "Year", "Month", "UTC")
    vars4table <- c("duration", "freqBeg", "freqEnd", "freqMax", "freqMin",
                    "freqSpread", "freqSweepUpPercent", "numInflections")

    # Outputs of dataprocess.R
    D_wsl <- readRDS("dataderived/dataprocess_D_wsl.rds")
    predictors_wsl <- readRDS("dataderived/dataprocess_predictors_wsl.rds")

    # Create event table as for Banter
    ev <- D_wsl %>%
        group_by(event.id) %>%
        summarise(species = unique(species)[1]) %>%
        # left_join(tabevent0 %>% select(event.id, WhistlePerMinute), by = "event.id") %>%
        as.data.frame()

    set.seed(123)

    # Number of folds
    K <- 5
    Ktimes <- 5

    # Create folds
    FOLDS <- caret::createMultiFolds(ev$species, k = K, times = Ktimes)
}

library(tidymodels)
library(brulee)

RES_train <- tibble()
RES_test <- tibble()
PREDS <- tibble()

set.seed(123)
for (k in 1:length(FOLDS)) { # k = 1
    ev_train <- ev[-FOLDS[[k]], ]
    ev_test <- ev[FOLDS[[k]], ]

    # Balance
    # ev_train <- ROSE::ovun.sample(species ~ ., data = ev_train,
    #                               N = nrow(ev_train),
    #                               method = "both")$data

    D_wsl_train <- D_wsl %>%
        filter(event.id %in% ev_train$event.id) %>%
        # select(-species) %>%
        select(-any_of(idcols_wsl %>% setdiff(c("call.id", "event.id")))) %>%
        as.data.frame()
    D_wsl_test <- D_wsl %>%
        filter(event.id %in% ev_test$event.id) %>%
        # select(-species) %>%
        select(-any_of(idcols_wsl %>% setdiff(c("call.id", "event.id")))) %>%
        as.data.frame()

    # D_train <- list(events = ev_train,
    #                 detectors = list(dw = D_wsl_train))
    # D_test <- list(events = ev_test,
    #                detectors = list(dw = D_wsl_test))

    # Select predictors for whistle classification
    # B <- Boruta::Boruta(species ~ .,
    #                     doTrace = 0, maxRuns = 100,
    #                     data = D_wsl_train[, c(RESPONSE, predictors_wsl)])
    # st <- attStats(B)
    # varRej <- rownames(st)[st$decision == "Rejected"]
    # # v <- rownames(st)[st$decision == "Confirmed"]
    # v <- base::setdiff(predictors_wsl, varRej)
    v <- predictors_wsl

    # Model 1, for whistles
    # Create a recipe to preprocess the data
    biv_rec <- recipe(species ~ ., data = D_wsl_train[, c(RESPONSE, v)]) %>%
        step_normalize(all_predictors())

    # Define the neural network model
    # https://brulee.tidymodels.org/reference/brulee_mlp.html
    nnet_spec <- mlp(
        epochs = 10000,
        hidden_units = c(30, 10),
        penalty = 0.001,
        learn_rate = 0.126
        # ,activation = "leaky_relu"
    ) %>%
        set_engine("brulee", validation = 0.2) %>%
        set_mode("classification")

    # Create a workflow
    nnet_wflow <- biv_rec %>% workflow(nnet_spec)

    # Fit the model
    m1 <- fit(nnet_wflow, D_wsl_train[, c(RESPONSE, v)])

    # Extract model details
    # nnet_fit %>% extract_fit_engine()

    # Predict
    m1_fitted <- predict(m1,
                         new_data = D_wsl_train[, c(RESPONSE, v)],
                         type = "prob")

    # Add m1 predictions to the event database
    ev_train <- D_wsl_train %>%
        select(event.id) %>%
        mutate(ProbCommon = m1_fitted %>% pull(.pred_Common)) %>%
        group_by(event.id) %>%
        summarise(PropCommon = mean(ProbCommon > 0.5),
                  ProbCommon_avg = mean(ProbCommon),
                  ProbCommon_med = quantile(ProbCommon, probs = 0.5),
                  ProbCommon_q1 = quantile(ProbCommon, probs = 0.25),
                  ProbCommon_q3 = quantile(ProbCommon, probs = 0.75)) %>%
        right_join(ev_train, by = "event.id")

    # Model for events
    m2 <- decision_tree() %>%
        set_engine("rpart") %>%
        set_mode("classification") %>%
        fit(species ~ PropCommon, data = ev_train)
    # tidy(m2)
    m2_fitted <- predict(m2, ev_train, type = "class") %>%
        pull(.pred_class)

    # Performance on the training set
    x_acc <- caret::confusionMatrix(m2_fitted,
                                    ev_train %>% pull(species))
    RES_train <- bind_rows(RES_train, c(x_acc$overall, x_acc$byClass, Fold = k))

    # Predict on the testing set
    m1_pred <- predict(m1, new_data = D_wsl_test, type = "prob")
    ev_test <- D_wsl_test %>%
        select(event.id) %>%
        mutate(ProbCommon = m1_pred %>% pull(.pred_Common)) %>%
        group_by(event.id) %>%
        summarise(PropCommon = mean(ProbCommon > 0.5),
                  ProbCommon_avg = mean(ProbCommon),
                  ProbCommon_med = quantile(ProbCommon, probs = 0.5),
                  ProbCommon_q1 = quantile(ProbCommon, probs = 0.25),
                  ProbCommon_q3 = quantile(ProbCommon, probs = 0.75)) %>%
        right_join(ev_test, by = "event.id")
    m2_pred <- predict(m2, ev_test, type = "class") %>% pull(.pred_class)

    x_acc <- caret::confusionMatrix(m2_pred,
                                    ev_test %>% pull(species) )
    RES_test <- bind_rows(RES_test, c(x_acc$overall, x_acc$byClass,
                                      Fold = k, nvars = length(v)))

    print(c(k, mean(RES_test$`Balanced Accuracy`)))

    PREDS <- bind_rows(PREDS,
                       tibble(ev_test %>%
                                  select(event.id) %>%
                                  mutate(Predicted = m2_pred,
                                         Fold = k)
                       )
    )
}

CV <- list(RES_train = RES_train,
           RES_test = RES_test,
           PREDS = PREDS)

saveRDS(CV, file = "dataderived/mod_cv_nn_out.rds")
