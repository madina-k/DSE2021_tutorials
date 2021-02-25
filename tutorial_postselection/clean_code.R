library("tidyverse")
library("glmnet") # for lasso
library("hdm") # for high-dimensional metrics
library("broom") # for tidying up regression outputs

# SImulate data ----

set.seed(456)
N <- 100 # Number of observations
P <- 100 # Number of potential confounders
nSim <- 100 # Number of simulations

# Simulate the potential confounders from V1 to V100, then create d and y
simulated100 <-  matrix(
  rnorm(n = N*P*nSim), # all of the potential confounders ~N(0,1)
  nrow = N*nSim, # total number of observations
  ncol = P) %>%
  as.data.frame() %>%
  mutate(
    d = V1 + V2 + V3 + 0.2*rnorm(N*nSim),
    y = 1*d + 10*V1 + 10*V2 + 10*V3 +
      10*V4 + 10*V5 + 10*V6 +
      10*V7 + 10*V8 + 10*V9 + rnorm(N*nSim),
    SimNumber = rep(c(1:nSim), each = N) # This stores the simulation number (e.g., the first 100 observations will have SimNumber = 1, the next 100 will have SimNumber = 2, and so on)
  ) %>%
  select(y, d, everything()) # This puts y and d as the first two columns.


simulated100nested <- simulated100 %>%
  group_by(SimNumber) %>%
  nest()


# True OLS ----
estimate_correctols <- function(df){
  lm(y ~ d + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9, data = df) %>%
    tidy() %>%
    filter(term == "d") %>% # We only want to store the coefficient for d
    return(.)
}


results_true <- map_dfr(.x = c(1:nSim), ~estimate_correctols(simulated100nested$data[[.x]]))

# Naive post-lasso ----
estimate_naiveLasso <- function(data){
  # Create X matrix
  X <- data %>%  select(-y)  %>%  as.matrix()

  # Cross-validation
  cv_lasso <- cv.glmnet(X, data$y, alpha = 1)
  bestlam <- cv_lasso$lambda.min
  # Run Lasso
  lasso_reg <- glmnet(X, data$y, alpha = 1, lambda=bestlam)

  # Get Lasso coefficients
  lasso_coefficients <- coef(lasso_reg) %>%   as.matrix() %>%
    as.data.frame() %>%  mutate(coef_names = row.names(.)) %>%
    rename(est = s0) %>%
    filter(est != 0)

  # Run OLS y on d and selected control variables
  data %>% select(y, d, lasso_coefficients$coef_names[-1]) %>%
    lm(y ~ d + ., data = .) %>%  tidy() %>% filter(term == "d") %>%
    return(.)
}

set.seed(456)
# Apply estimate_naiveLasso() function using map_dfr, just like we did for OLS
results_naive_lasso <- map_dfr(.x = c(1:nSim), ~estimate_naiveLasso(data = simulated100nested$data[[.x]]))

# CV DS ----
get_residuals <- function(dependent_var, data){
  # Create a matrix of predictors
  X <- as.matrix(data)
  # Cross-validation stage
  cv_lasso <- cv.glmnet(X, dependent_var, alpha = 1)
  bestlam <- cv_lasso$lambda.min
  # Run Lasso at best lambda
  lasso_reg <- glmnet(X, dependent_var, alpha = 1, lambda=bestlam)
  # Get Lasso coefficients
  lasso_coefficients <- coef(lasso_reg) %>%   as.matrix() %>%
    as.data.frame() %>%  mutate(coef_names = row.names(.)) %>%
    rename(est = s0) %>%
    filter(est != 0)

  # Run OLS: dependent variable  on the selected control variables
  data%>%
    mutate(dependent_var = dependent_var) %>% #add the dependent variable to the data
    select(dependent_var, lasso_coefficients$coef_names[-1]) %>% # select only the selected predictors
    lm(dependent_var ~ ., data = .) %>%  #be careful, not to use y or d notation here
    residuals(.) %>% #obtain the residuals
    return(.)
}

results_cvDS <- numeric(0)


set.seed(123)
for (i in c(1:nrow(simulated100nested))) {

  # Step 1 y on z. Apply the function get_residuals()

  resid_y <- get_residuals(dependent_var= simulated100nested$data[[i]]$y,
                           data = simulated100nested$data[[i]] %>%  select(-y, -d))

  # Step 2 d on z. Apply the function get_residuals()
  resid_d <- get_residuals(dependent_var= simulated100nested$data[[i]]$d,
                           data = simulated100nested$data[[i]] %>%  select(-y, -d))

  # Step 3 resid of y on resid of d
  # Collect the coefficients
  step3_coefs <- lm(resid_y ~ resid_d) %>% coefficients()

  # Append the coefficient corresponding to resid_d to the numerical vector with all the results
  results_cvDS <- append(results_cvDS, step3_coefs[2])

  # Print information on which iteration is complete
  # cat("iteration ", i, "complete\n")
}

results_cvDS <- data.frame(estimate=results_cvDS)

# RLasso DS ----

estimate_rlassoDS <- function(data){
  # Apply rlassoEffect function to the data
  res <- rlassoEffect(
    x = data %>%  select(-y, -d) %>%  as.matrix(),
    y = data$y,
    d = data$d,
    method = "partialling out")

  # Return the matrix of estimated coefficients as a dataframe
  summary(res)$coefficients %>%
    as.data.frame() %>%
    return(.)
}


# Estimate the post-double-selection estimator on all of the 100 samples stores in the `data` column of the `simulated100nested` dataframe
results_rle <- map_dfr(.x = c(1:nSim), ~estimate_rlassoDS(data = simulated100nested$data[[.x]]))

# Rename column `Estimate.` into `estimate`
results_rle <- results_rle %>% rename(estimate = Estimate.)

# Final graph ----
results_all <- bind_rows(
  results_naive_lasso     %>%  select(estimate) %>% mutate(estimator = "single Lasso"),
  results_cvDS      %>%  select(estimate) %>% mutate(estimator = "cv DS"),
  results_rle  %>%  select(estimate) %>% mutate(estimator = "rlasso DS"),
  results_true      %>%  select(estimate) %>% mutate(estimator = "true OLS"),
) %>%
  mutate(estimator = factor(estimator, levels = c("true OLS", "single Lasso", "cv DS", "rlasso DS")))


# results_all %>%
#   ggplot(aes(x = estimate, fill = estimator, color = estimator)) +
#   geom_density(alpha = 0.2) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
#   theme_bw() + xlim(-5,15)
#
# results_all %>%
#   group_by(estimator) %>%
#   summarise(mean(estimate), sd(estimate))


# DML ----
set.seed(123)
I  <- sample(x = c(1:N), size = N/2) %>% sort(.)
IC <- setdiff(c(1:N),I)

# # Create a matrix of predictors
# dataset %>% select(-y,-d) %>%  as.matrix() -> X
# y_vec <- dataset$y
# d_vec <- dataset$d
#
#
# # Cross-validation stage
#
# # Train for y
#
# cv_lasso_y <- cv.glmnet(X[I,], y_vec[I], alpha = 1)
# bestlam_y <- cv_lasso_y$lambda.min
# lasso_y <- glmnet(X[I,], y_vec[I], alpha = 1, lambda = bestlam_y)
#
# # Train for d
#
# cv_lasso_d <- cv.glmnet(X[I,], d_vec[I], alpha = 1)
# bestlam_d <- cv_lasso_d$lambda.min
# lasso_d <- glmnet(X[I,], d_vec[I], alpha = 1, lambda = bestlam_d)
#
#
# # Prediction stage
# predict_y <- predict(lasso_y, newx = X[IC,], s = bestlam_y)
# predict_d <- predict(lasso_d, newx = X[IC,], s = bestlam_d)
#
# # Residuals
#
# resid_y <- y_vec[IC] - predict_y
# resid_d <- d_vec[IC] - predict_d
#
#
# mean(resid_d*resid_y)/mean(resid_d*resid_d)


DML_estimator <- function(data, I, IC){
  # Create a matrix of predictors
  data %>% select(-y,-d) %>%  as.matrix() -> X
  y_vec <- data$y
  d_vec <- data$d

  # Train for y

  cv_lasso_y <- cv.glmnet(X[I,], y_vec[I], alpha = 1)
  bestlam_y <- cv_lasso_y$lambda.min
  lasso_y <- glmnet(X[I,], y_vec[I], alpha = 1, lambda = bestlam_y)

  # Train for d

  cv_lasso_d <- cv.glmnet(X[I,], d_vec[I], alpha = 1)
  bestlam_d <- cv_lasso_d$lambda.min
  lasso_d <- glmnet(X[I,], d_vec[I], alpha = 1, lambda = bestlam_d)


  # Prediction stage
  predict_y <- predict(lasso_y, newx = X[IC,], s = bestlam_y)
  predict_d <- predict(lasso_d, newx = X[IC,], s = bestlam_d)

  # Residuals

  resid_y <- y_vec[IC] - predict_y
  resid_d <- d_vec[IC] - predict_d

  mean(resid_d*resid_y)/mean(resid_d*resid_d) %>%
    return(.)
}


DML_crossfit <- function(data, I, IC){
  est1 <- DML_estimator(data, I, IC)
  est2 <- DML_estimator(data, IC, I)

  data.frame(
    estimate = 0.5*est1 + 0.5*est2
  ) %>%
    return(.)
}




set.seed(456)

results_DMLcf <- map_dfr(.x = c(1:nSim), ~DML_crossfit(data = simulated100nested$data[[.x]], I=I, IC=IC))

# Final graph ----
results_all <- bind_rows(
  results_naive_lasso     %>%  select(estimate) %>% mutate(estimator = "single Lasso"),
  results_cvDS      %>%  select(estimate) %>% mutate(estimator = "cv DS"),
  results_rle  %>%  select(estimate) %>% mutate(estimator = "rlasso DS"),
  results_DMLcf  %>%  select(estimate) %>% mutate(estimator = "DML partialling-out"),
  results_true      %>%  select(estimate) %>% mutate(estimator = "true OLS"),
) %>%
  mutate(estimator = factor(estimator, levels = c("true OLS", "single Lasso", "cv DS", "rlasso DS",  "DML partialling-out")))


results_all %>%
  ggplot(aes(x = estimate, fill = estimator, color = estimator)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  theme_bw() + xlim(-5,15)

results_all %>%
  group_by(estimator) %>%
  summarise(mean(estimate), sd(estimate))

