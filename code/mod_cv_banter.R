# Run cross-validation for BANTER

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

library(banter)

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

    D_wsl_train <- lapply(ev_train$event.id, function(eid)
        D_wsl %>% filter(event.id == eid)) %>%
        bind_rows() %>%
        select(-species) %>%
        select(-any_of(idcols_wsl %>% setdiff(c("call.id", "event.id")))) %>%
        as.data.frame()
    D_wsl_test <- D_wsl %>%
        filter(event.id %in% ev_test$event.id) %>%
        select(-species) %>%
        select(-any_of(idcols_wsl %>% setdiff(c("call.id", "event.id")))) %>%
        as.data.frame()

    D_train <- list(events = ev_train,
                    detectors = list(dw = D_wsl_train))
    D_test <- list(events = ev_test,
                   detectors = list(dw = D_wsl_test))

    # Initialize BANTER model
    bant.mdl_train <- initBanterModel(D_train$events)

    # Summarize BANTER model
    # summary(bant.mdl_train)

    # Add BANTER Detectors and Run Detector Models
    bant.mdl_train <- addBanterDetector(
        bant.mdl_train,
        data = D_train$detectors,
        ntree = 1000,
        importance = FALSE,
        sampsize = 0.5
    )
    # summary(bant.mdl_train)
    # plotDetectorTrace(bant.mdl_train)

    # Second-stage BANTER event model based on output from the Detector Models
    bant.mdl2_train <- runBanterModel(bant.mdl_train, ntree = 1000, sampsize = 1)
    # rfPermute::confusionMatrix(getBanterModel(bant.mdl2_train))
    # rfPermute::plotImportance(getBanterModel(bant.mdl2_train), plot.type = "heatmap")

    # Performance on the training set
    RES_train <- bind_rows(RES_train,
                           modelPctCorrect(bant.mdl2_train) %>% mutate(Fold = k))

    # Predict on the testing set
    x <- predict(bant.mdl2_train, D_test)
    x_acc <- caret::confusionMatrix(x$predict.df$predicted %>% as.factor(),
                                    ev_test %>% pull(species) )
    RES_test <- bind_rows(RES_test, c(x_acc$overall, x_acc$byClass, Fold = k))

    preds <- ev_test %>%
        as_tibble() %>%
        select(event.id) %>%
        mutate(Predicted = x$predict.df$predicted %>% as.factor(),
               Fold = k)
    PREDS <- bind_rows(PREDS, preds)

    print(c(k, mean(RES_test$`Balanced Accuracy`)))
}

CV <- list(RES_train = RES_train,
           RES_test = RES_test,
           PREDS = PREDS)

saveRDS(CV, file = "dataderived/mod_cv_banter_out.rds")
