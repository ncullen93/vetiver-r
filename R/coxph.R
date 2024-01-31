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
    pred_names <- all.vars(stats::formula(model)[[3]])
    data <- model.frame(model)
    cnames <- colnames(data)
    colnames(data)[grepl('strata\\(',colnames(data))] <-
        stringr::str_extract(colnames(data)[grepl('strata\\(',colnames(data))],
                             "(?<=\\().+?(?=\\))")
    prototype <- vctrs::vec_ptype(data[pred_names])
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

        res <- survival::survfit(vetiver_model$model, newdata=newdata)

        list(.pred = 100 * res$surv,
             .conf_lo = pmax(100 * res$lower, 0),
             .conf_hi = pmin(100 * res$upper, 100),
             .time = res$time)
    }

}
