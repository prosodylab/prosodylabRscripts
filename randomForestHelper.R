library(tidymodels)
library(ranger)   # The fast random forest engine
library(themis)   # For downsampling/balancing classes
library(vip)      # For calculating and plotting variable importance
library(ggplot2)  # For custom plotting

# Set a seed for reproducibility across all steps
set.seed(1745)

# --- 1. MOCK DATA SETUP (Mimics your subsFour data balancing) ---

# # Mock data set for testing
# data_full <- iris %>%
#   as_tibble() %>%
#   # Make one class much larger to demonstrate downsampling
#   bind_rows(filter(iris, Species == "setosa") %>% slice(rep(1:n(), 5))) %>%
#   mutate(Species = factor(Species))

predictors <- c("duration.1", "meanPitch.1", "maxPitch.1", "maxPitTime.1",
                "meanIntensity.1", "maxIntensity.1",
                "duration.2", "meanPitch.2", "maxPitch.2", "maxPitTime.2",
                "meanIntensity.2", "maxIntensity.2",
                "duration.3", "meanPitch.3", "maxPitch.3", "maxPitTime.3",
                "meanIntensity.3", "maxIntensity.3",
                "rPitch", "rDuration", "rIntensity")

data_full <- d %>%
  filter(trialPart == "question1") %>%
  select(Focus, all_of(predictors)) %>%
  mutate(Focus = factor(Focus))

# Calculate the minimum class size
min_n <- data_full %>%
  count(Focus) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

# Downsample all classes to the minimum size (your subsFour equivalent)
data_balanced <- data_full %>%
  group_by(Focus) %>%
  slice_sample(n = min_n) %>%
  ungroup()

# Split data into training and testing sets
data_split <- initial_split(data_balanced, strata = Focus)
data_train <- training(data_split)
data_test  <- testing(data_split)

# --- 2. DEFINE RECIPE, MODEL, AND WORKFLOW ---

# Define the recipe (preprocessing steps)
rf_recipe <-
  recipe(Focus ~ ., data = data_train) %>%
  # Example: removing unnecessary ID columns if they existed
  step_rm(starts_with("ID"))

# Define the model using the ranger engine
rf_spec_ranger <-
  rand_forest(
    mtry  = 4,
    trees = 1000
  ) %>%
  set_engine(
    "ranger",
    # CRITICAL: Request permutation importance during the fit
    importance = "permutation", 
    seed = 1745 
  ) %>%
  set_mode("classification")

# Create the workflow
rf_workflow_ranger <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec_ranger)

# --- 3. FIT THE MODEL ---

rf_fit_ranger <- workflows::fit(rf_workflow_ranger, data = data_train)

# --- 4. VARIABLE IMPORTANCE PLOT (ggplot2) ---


# Extract the RAW variable importance scores (class 'vi' object)
vi_scores_ranger <- vi(rf_fit_ranger, num_features = 10, method = "model")

# Convert the scores to a tibble and create the custom ggplot plot
vip_plot_ranger <-
  vi_scores_ranger %>%
  as_tibble() %>%
  ggplot(aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_point() +
  # Add the reference line at zero
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  labs(
    title = "Ranger Random Forest Variable Importance (Permutation)",
    x = "Permutation Importance",
    y = NULL
  ) +
  theme_minimal()

print(vip_plot_ranger)

# To see the actual importance scores:
print(vi_scores_ranger)