


#' @rdname vetiver_create_description
#' @export
vetiver_create_description.lmerMod <- function(model) {
    "A mixed effects model fit with the lme4 package"
}

#' @rdname vetiver_prepare_model
#' @export
vetiver_prepare_model.lmerMod <- function(model) {
    model <- butcher::butcher(model)
    return(model)
}

#' this is mainly need for keeping track of all the levels for factor variables
#' which itself is mainly needed for generating confidence intervals
#' @rdname vetiver_ptype
#' @export
vetiver_ptype.lmerMod <- function(model, ...) {
    pred_names <- all.vars(formula(model, fixed.only=T)[-2])
    pred_labels <- labels(terms(model))

    # convert pred_names to correct pred_label
    converted_names <- c()
    for (i in seq_along(pred_names)) {
        pred_name <- pred_names[i]
        matched_labels <- pred_labels[grepl(pred_name, pred_labels)]
        matched_labels <- matched_labels[!grepl(':', matched_labels)]
        right_label <- matched_labels[1]
        for (matched_label in matched_labels) {
            if (matched_label == pred_name) right_label <- matched_label
        }
        converted_names <- c(converted_names, right_label)
    }

    prototype <- vctrs::vec_ptype(model@frame[converted_names])
    colnames(prototype) <- pred_names

    if (is.null(prototype)) {
        return(NULL)
    }
    tibble::as_tibble(prototype)
}




#' Title
#'
#' @param vetiver_model
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
handler_predict.lmerMod <- function(vetiver_model, ...) {

    ptype <- vetiver_model$prototype

    function(req) {
        newdata <- req$body
        if (!purrr::is_null(ptype)) {
            newdata <- vetiver::vetiver_type_convert(newdata, ptype)
            newdata <- hardhat::scream(newdata, ptype)
        }

        model <- vetiver_model$model

        newdata$pred <- lme4:::predict.merMod(model,
                                               newdata=newdata,
                                               type='response',
                                               re.form=NA)

        if (!purrr::is_null(ptype)) {

            res <- tryCatch({
                Designmat <- model.matrix(formula(model, fixed.only=TRUE)[-2], newdata)
                predvar <- diag(as.matrix(Designmat %*% lme4:::vcov.merMod(model) %*% t(Designmat)))
                newdata$SE <- sqrt(predvar + sigma(model)^2)
                newdata$conf_lo = newdata$pred - 1.96*newdata$SE
                newdata$conf_hi = newdata$pred + 1.96*newdata$SE

                list(.pred = newdata$pred,
                            .conf_lo = newdata$conf_lo,
                            .conf_hi = newdata$conf_hi)
            }, error = function(cond) {
                list(.pred = newdata$pred)
            })

        } else {
            res <- list(.pred = newdata$.pred)
        }

        return(res)
    }
}

