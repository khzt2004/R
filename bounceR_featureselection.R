library(bounceR)
# devtools::install_github("STATWORX/bounceR")

# DATA GENERATION ---------------------------------------------------------
# We start by simulating a dataset where we can divide the feature space into relevant and irrelevant features
# simulating a dataset
train_df <- sim_data(n = 1000,
                     modelvars = 30,
                     noisevars = 2000,
                     model_sd = 4,
                     noise_sd = 4,
                     epsilon_sd = 4,
                     outcome = "regression",
                     cutoff = NULL)

# FILTER METHODS ----------------------------------------------------------
# To reduce the dimensionality of the feature space, we can filter out irrelevant features using simple correlation,
# information criteria and near zero variance metrics.
# Correlation Collinearity Filter
test_cc <- featureFiltering(data = train_df,
                            target = "y",
                            method = "cc",
                            returning = "names")

# Maximum Relevance Minimum Redundancy Filter
test_mr <- featureFiltering(data = train_df,
                            target = "y",
                            method = "mrmr",
                            returning = "names")

# WRAPPER METHODS ---------------------------------------------------------
# For a rather rigorous and more importantly model oriented selection, we can use wrapper methods to produce optimal
# model equations, based on stability criteria.

test_ge <- featureSelection(data = train_df,
                            target = "y",
                            selection = selectionControl(n_rounds = 100,
                                                         n_mods = 1000,
                                                         p = 30,
                                                         penalty = 0.3,
                                                         reward = 0.2),
                            bootstrap = "regular",
                            early_stopping = "none",
                            n_cores = 1)