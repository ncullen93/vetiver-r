#' @rdname vetiver_create_description
#' @export
vetiver_create_description.coxph <- function(model) {
    "A Cox regression model"
}

#' @rdname vetiver_create_description
#' @export
vetiver_prepare_model.coxph <- function(model) {
    #model <- butcher::axe_data(model)
    model
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.coxph <- function(model, ...) {
    pred_names <- preds_lm_ish(model)
    prototype <- vctrs::vec_ptype(model.frame(model)[pred_names])
    tibble::as_tibble(prototype)
}

#' @rdname handler_startup
#' @export
handler_predict.coxph <- function(vetiver_model, ...) {

    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!is_null(ptype)) {
            newdata <- vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }

        pred <- predict(vetiver_model$model,
                        newdata=newdata,
                        type='survival',
                        se.fit=T)

        pred$conf_lo <- 100 * pmax(pred$fit - 1.96*pred$se.fit, 0)
        pred$conf_hi <- 100 * pmin(pred$fit + 1.96*pred$se.fit, 1)
        pred$fit <- 100 * pred$fit

        list(.pred = pred$fit,
             .conf_lo = pred$conf_lo,
             .conf_hi = pred$conf_hi)
    }

}
