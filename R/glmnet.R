#' @rdname vetiver_create_description
#' @export
vetiver_create_description.glmnet <- function(model) {
    glue("A generalized linear model with lasso or elasticnet regularization
         from the glmnet package")
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.glmnet <- function(model) {
    #model <- butcher::axe_data(model)
    model
}

#' glmnet expects a numeric matrix, with factors as dummy variables
#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.glmnet <- function(model, ...) {
    #pred_names <- preds_lm_ish(model)
    #pred_names <- rownames(model$beta)
    #prototype <- vctrs::vec_ptype(model$model[pred_names])
    #tibble::as_tibble(prototype)
    NULL
}

#' @rdname handler_startup
#' @export
handler_predict.glmnet <- function(vetiver_model, ...) {
    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }
        ret <- glmnet::predict.glmnet(vetiver_model$model,
                       newx = as.matrix(newdata),
                       type='response',
                       ...)
        ret <- unname(rowMeans(ret))
        list(.pred = ret)
    }
}

