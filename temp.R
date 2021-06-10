# library(mlr3extralearners)
# create_learner(
#   pkg = ".",
#   classname = "Nnet",
#   algorithm = "neural net",
#   type = "regr",
#   key = "nnet",
#   package = "nnet",
#   caller = "nnet",
#   feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
#   predict_types = c("response"),
#   properties = c(),
#   references = FALSE,
#   gh_name = "0livier1O1"
# )


library(mlr)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
source("~/uni/ra/dml/src/dml/r/tools.r")

k = 50
n = 100

learner.nnet   <- lrn("regr.nnet", size=12,  maxit=1000, decay=0.01, MaxNWts=10000, trace=FALSE)


ml_g = learner.nnet$clone()
ml_m = learner.nnet$clone()

data     <- as.data.frame(generate.data.ra(k, n, theta=theta, dgp='linear'))
dml_data <- double_ml_data_from_data_frame(data, y_col="y", d_cols="d")

dml_plr  <- DoubleMLPLR$new(dml_data, ml_g, ml_m, n_rep=10, n_folds = 2)
dml_plr$fit()
