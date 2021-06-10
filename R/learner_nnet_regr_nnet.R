#' @title Regression Neural Net Learner
#' @author 0livier1O1
#' @name mlr_learners_regr.nnet
#'
#' @template class_learner
#' @templateVar id regr.nnet
#' @templateVar caller nnet
#'
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrNnet = R6Class("LearnerRegrNnet",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "size", default = 3L, lower = 0L, tags = "train"),
          ParamUty$new(id = "subset", tags = "train"),
          ParamUty$new(id = "na.action", tags = "train"),
          ParamUty$new(id = "contrasts", default = NULL, tags = "train"),
          ParamUty$new(id = "Wts", tags = "train"),
          ParamUty$new(id = "mask", tags = "train"),
          ParamLgl$new(id = "linout", default = FALSE, tags = "train"),
          ParamLgl$new(id = "entropy", default = FALSE, tags = "train"),
          ParamLgl$new(id = "softmax", default = FALSE, tags = "train"),
          ParamLgl$new(id = "censored", default = FALSE, tags = "train"),
          ParamLgl$new(id = "skip", default = FALSE, tags = "train"),
          ParamDbl$new(id = "rang", default = 0.7, tags = "train"),
          ParamDbl$new(id = "decay", default = 0, tags = "train"),
          ParamInt$new(id = "maxit", default = 100L, lower = 1L, tags = "train"),
          ParamLgl$new(id = "Hess", default = FALSE, tags = "train"),
          ParamLgl$new(id = "trace", default = TRUE, tags = "train"),
          ParamInt$new(id = "MaxNWts", default = 1000L, lower = 1L, tags = "train"),
          ParamDbl$new(id = "abstol", default = 1.0e-4, tags = "train"),
          ParamDbl$new(id = "reltol", default = 1.0e-8, tags = "train")
        )
      )

      ps$values = list(size = 3L)
      ps$add_dep("linout", "entropy", CondEqual$new(FALSE))
      ps$add_dep("linout", "softmax", CondEqual$new(FALSE))
      ps$add_dep("linout", "censored", CondEqual$new(FALSE))
      ps$add_dep("entropy", "linout", CondEqual$new(FALSE))
      ps$add_dep("entropy", "softmax", CondEqual$new(FALSE))
      ps$add_dep("entropy", "censored", CondEqual$new(FALSE))
      ps$add_dep("softmax", "linout", CondEqual$new(FALSE))
      ps$add_dep("softmax", "entropy", CondEqual$new(FALSE))
      ps$add_dep("softmax", "censored", CondEqual$new(FALSE))
      ps$add_dep("censored", "linout", CondEqual$new(FALSE))
      ps$add_dep("censored", "entropy", CondEqual$new(FALSE))
      ps$add_dep("censored", "softmax", CondEqual$new(FALSE))

      super$initialize(
        id = "regr.nnet",
        packages = "nnet",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_regr.nnet"
      )
    }


  ),

  private = list(

    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      # set column names to ensure consistency in fit and predict
      self$state$feature_names = task$feature_names

      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      f = task$formula()
      data = task$data()

      # use the mlr3misc::invoke function (it's similar to do.call())
      mlr3misc::invoke(nnet::nnet.formula, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = task$data(cols = self$state$feature_names)

      response = mlr3misc::invoke(predict, self$model, newdata = newdata,
                              type = 'raw', .args = pars)

      return(list(response = response))
    }
  )
)

.extralrns_dict$add("regr.nnet", LearnerRegrNnet)

