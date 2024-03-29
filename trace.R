manageparam <- function(start.arg, fix.arg, obs, distname) {
    #if clause with 3 different cases:
    #start.arg : NULL | named list | a function

    if (is.null(start.arg)) {
        trystart <- try(start.arg.default(obs, distname), silent = TRUE)
        if (class(trystart) == "try-error") {
            cat("Error in computing default starting values.\n")
            stop(trystart)
        }
        lstart <- trystart
        #lstart should be a named list but check it
        hasnoname <- is.null(names(lstart)) || !is.list(lstart)
        if (hasnoname)
            stop("Starting values must be a named list, error in default starting value.")

    } else if (is.list(start.arg)) {
        hasnoname <- is.null(names(start.arg))
        if (hasnoname)
            stop("Starting values must be a named list (or a function returning a named list).")
        lstart <- start.arg
    } else if (is.function(start.arg)) {
        trystart <- try(start.arg(obs), silent = TRUE)
        if (class(trystart) == "try-error") {
            cat("Error in computing starting values with your function.\n")
            stop(trystart)
        }
        lstart <- trystart
        hasnoname <- is.null(names(lstart)) || !is.list(lstart)
        if (hasnoname)
            stop("Starting values must be a named list, your function does not return that.")
        } else
            stop("Wrong type of argument for start")

    #if clause with 3 different cases:
    #fix.arg : NULL | named list | a function
    if (is.null(fix.arg)) {
        lfix <- NULL
    } else if (is.list(fix.arg)) {
        hasnoname <- is.null(names(fix.arg))
        if (hasnoname)
            stop("Fixed parameter values must be a named list (or a function returning a named list).")
        lfix <- fix.arg
    } else if (is.function(fix.arg)) {
        tryfix <- try(fix.arg(obs), silent = TRUE)
        if (class(tryfix) == "try-error") {
            cat("Error in computing fixed parameter values with your function.\n")
            stop(tryfix)
        }
        lfix <- tryfix
        hasnoname <- is.null(names(lfix)) || !is.list(lfix)
        if (hasnoname)
            stop("Fixed parameter values must be a named list, your function does not return that.")
        } else
            stop("Wrong type of argument for fix.arg")

    #eliminate arguments both in lstart and lfix (when start.arg was NULL)
    if (is.null(start.arg) && !is.null(lfix)) {
        lstart <- lstart[!names(lstart) %in% names(lfix)]
        if (length(lstart) == 0)
            stop("Don't need to use fitdist() if all parameters have fixed values")
        }

    list("start.arg" = lstart, "fix.arg" = lfix)
}

checkparamlist <- function(start.arg, fix.arg, argdistname, hasnodefaultval) {
    errtxt <- list(t3 = "'start' must specify names which are arguments to 'distr'.",
          t4 = "'fix.arg' must specify names which are arguments to 'distr'.",
          t5 = "A distribution parameter cannot be specified both in 'start' and 'fix.arg'.",
          t6 = "'start' should not have NA or NaN values.",
          t7 = "'fix.arg' should not have NA or NaN values.",
          t8 = "Some parameter names have no starting/fixed value: ",
          t9 = "Some parameter names have no starting/fixed value but have a default value: ")

    vstart <- unlist(start.arg)
    #check unexpected names
    m <- match(names(vstart), argdistname)
    if (any(is.na(m)))
        stop(errtxt$t3)
    #check NA/NaN values
    if (any(is.na(vstart)) || any(is.nan(vstart)))
        stop(errtxt$t6)
    if (!is.null(fix.arg)) {
        vfix <- unlist(fix.arg)
        #check unexpected names
        mfix <- match(names(vfix), argdistname)
        if (any(is.na(mfix)))
            stop(errtxt$t4)

        # check that some parameters are not both in fix.arg and start
        minter <- match(names(vstart), names(vfix))
        if (any(!is.na(minter)))
            stop(errtxt$t5)

        #check NA/NaN values
        if (any(is.na(vfix)) || any(is.nan(vfix)))
            stop(errtxt$t7)
        allparname <- names(c(vstart, vfix))
    } else
        allparname <- names(vstart)

    theoparam <- computegetparam(argdistname)
    #special case where both scale and rate are allowed, see ?dgamma
    if ("scale" %in% theoparam && "rate" %in% theoparam) {
        errt8 <- any(!allparname %in% theoparam) || length(allparname) != length(theoparam) - 1
        #special case where both prob and mu are allowed, see ?dnbinom
    } else if (length(theoparam) == 3 && all(c("size", "prob", "mu") %in% theoparam)) {
        errt8 <- any(!allparname %in% theoparam) || length(allparname) != length(theoparam) - 1
    } else
        errt8 <- any(!theoparam %in% allparname)
    #only make a warning if unset arguments have a default value
    if (errt8) {
        unsetarg <- theoparam[!theoparam %in% allparname]
        if (any(hasnodefaultval[unsetarg]))
            stop(paste0(errtxt$t8, unsetarg, "."))
        else
            warning(paste0(errtxt$t9, unsetarg, "."))
        }

    list("start.arg" = start.arg, "fix.arg" = fix.arg)
}

computegetparam <- function(argdistname) {
    #remove first argument, that should be "x", "p", "q", or "n", see ?dgamma, pgamma, qgamma
    argdistname <- argdistname[-1]
    nonparaminR <- c("x", "p", "q", "n") #defensive programming
    #remove other arguments, see ?dgamma, pgamma, qgamma, dbeta
    nonparaminR <- c(nonparaminR, "log", "log.p", "lower.tail", "ncp")
    nonparaminActuar <- c("limit", "order", "t")
    nonparaminGamlssdist <- "fast"
    nonparamspecial <- c("...", "..1", "..2")
    #see ?dnig, dhyperb, dskewlap, dgig,...
    nonparaminGenHyperbolic <- c("param", "KOmega", "ibfTol", "nmax", "method", "intTol",
                               "valueOnly", "nInterpol", "uniTol", "subdivisions", "logPars")
    #see ?dsn
    nonparamsn <- "dp"

    plist <- setdiff(argdistname, nonparaminR)
    plist <- setdiff(plist, nonparaminActuar)
    plist <- setdiff(plist, nonparaminGamlssdist)
    plist <- setdiff(plist, nonparamspecial)
    plist <- setdiff(plist, nonparaminGenHyperbolic)
    plist <- setdiff(plist, nonparamsn)

    plist
}


trace= function(data, distr, start = NULL, fix.arg = NULL, optim.method = "default",
    lower = -Inf, upper = Inf, custom.optim = NULL, weights = NULL,
    silent = TRUE, gradient = NULL, checkstartfix = FALSE, ...) {

    if (!is.character(distr))
        stop("distr must be a character string naming a distribution")
    else distname <- distr
    ddistname <- paste("d", distname, sep = "")
    argddistname <- names(formals(ddistname))
    if (!exists(ddistname, mode = "function"))
        stop(paste("The ", ddistname, " function must be defined"))
    if (is.null(custom.optim))
        optim.method <- match.arg(optim.method, c("default",
            "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
            "Brent"))
    start.arg <- start
    if (is.vector(start.arg))
        start.arg <- as.list(start.arg)
    txt1 <- "data must be a numeric vector of length greater than 1 for non censored data"
    txt2 <- "or a dataframe with two columns named left and right and more than one line for censored data"
    if (!is.null(weights)) {
        if (any(weights < 0))
            stop("weights should be a vector of integers greater than 0")
        if (!is.allint.w(weights))
            stop("weights should be a vector of (strictly) positive integers")
        if (length(weights) != NROW(data))
            stop("weights should be a vector with a length equal to the observation number")
        warning("weights are not taken into account in the default initial values")
    }
    if (is.vector(data)) {
        cens <- FALSE
        if (!(is.numeric(data) & length(data) > 1))
            stop(paste(txt1, txt2))
        }
    else {
        cens <- TRUE
        censdata <- data
        if (!(is.vector(censdata$left) & is.vector(censdata$right) &
            length(censdata[, 1]) > 1))
                stop(paste(txt1, txt2))
        pdistname <- paste("p", distname, sep = "")
        if (!exists(pdistname, mode = "function"))
            stop(paste("The ", pdistname, " function must be defined to apply maximum likelihood to censored data"))
        }
    if (cens) {
        dataformat <- cens2pseudo(censdata)
        data <- dataformat$pseudo
        rcens <- dataformat$rcens
        lcens <- dataformat$lcens
        icens <- dataformat$icens
        ncens <- dataformat$ncens
        irow <- cens2idxrow(censdata)
        irow.rcens <- irow$rcens
        irow.lcens <- irow$lcens
        irow.icens <- irow$icens
        irow.ncens <- irow$ncens
    }
    if (!checkstartfix) {
        arg_startfix <- manageparam(start.arg = start, fix.arg = fix.arg,
            obs = data, distname = distname)
        hasnodefaultval <- sapply(formals(ddistname), is.name)
        arg_startfix <- checkparamlist(arg_startfix$start.arg,
            arg_startfix$fix.arg, argddistname, hasnodefaultval)
        if (is.function(fix.arg))
            fix.arg.fun <- fix.arg
        else fix.arg.fun <- NULL
        }
    else {
        arg_startfix <- list(start.arg = start, fix.arg = fix.arg)
        fix.arg.fun <- NULL
    }
    vstart <- unlist(arg_startfix$start.arg)
    if (is.null(vstart))
        stop("Starting values could not be NULL with checkstartfix=TRUE")
    fix.arg <- arg_startfix$fix.arg
    if (distname == "unif") {
        par <- c(min = min(data), max = max(data))
        res <- list(estimate = par[!names(par) %in% names(fix.arg)],
            convergence = 0, loglik = NA, hessian = NA, optim.function = NA,
            fix.arg = fix.arg)
        return(res)
    }
    if (!cens && is.null(weights)) {
        if ("log" %in% argddistname) {
            fnobj <- function(par, fix.arg, obs, ddistnam) {
                -sum(do.call(ddistnam, c(list(obs), as.list(par),
                  as.list(fix.arg), log = TRUE)))
            }
        }
        else {
            fnobj <- function(par, fix.arg, obs, ddistnam) {
                -sum(log(do.call(ddistnam, c(list(obs), as.list(par),
                  as.list(fix.arg)))))
            }
        }
    }
    else if (cens && is.null(weights)) {
        argpdistname <- names(formals(pdistname))
        if (("log" %in% argddistname) & ("log.p" %in% argpdistname))
            fnobjcens <- function(par, fix.arg, rcens, lcens,
                icens, ncens, ddistnam, pdistnam) - sum(do.call(ddistnam,
                c(list(ncens), as.list(par), as.list(fix.arg),
                  list(log = TRUE)))) - sum(do.call(pdistnam,
                c(list(lcens), as.list(par), as.list(fix.arg),
                  list(log = TRUE)))) - sum(do.call(pdistnam,
                c(list(rcens), as.list(par), as.list(fix.arg),
                  list(lower.tail = FALSE), list(log = TRUE)))) -
                      sum(log(do.call(pdistnam, c(list(icens$right),
                  as.list(par), as.list(fix.arg))) - do.call(pdistnam,
                  c(list(icens$left), as.list(par), as.list(fix.arg)))))
            else fnobjcens <- function(par, fix.arg, rcens, lcens,
            icens, ncens, ddistnam, pdistnam) - sum(log(do.call(ddistnam,
            c(list(ncens), as.list(par), as.list(fix.arg))))) -
                sum(log(do.call(pdistnam, c(list(lcens), as.list(par),
                as.list(fix.arg))))) - sum(log(1 - do.call(pdistnam,
            c(list(rcens), as.list(par), as.list(fix.arg))))) -
                sum(log(do.call(pdistnam, c(list(icens$right), as.list(par),
                as.list(fix.arg))) - do.call(pdistnam, c(list(icens$left),
                as.list(par), as.list(fix.arg)))))
            }
    else if (!cens && !is.null(weights)) {
        fnobj <- function(par, fix.arg, obs, ddistnam) {
            -sum(weights * log(do.call(ddistnam, c(list(obs),
                as.list(par), as.list(fix.arg)))))
        }
    }
    else if (cens && !is.null(weights)) {
        fnobjcens <- function(par, fix.arg, rcens, lcens, icens,
            ncens, ddistnam, pdistnam) {
            p1 <- log(do.call(ddistnam, c(list(ncens), as.list(par),
                as.list(fix.arg))))
            p2 <- log(do.call(pdistnam, c(list(lcens), as.list(par),
                as.list(fix.arg))))
            p3 <- log(1 - do.call(pdistnam, c(list(rcens), as.list(par),
                as.list(fix.arg))))
            p4 <- log(do.call(pdistnam, c(list(icens$right),
                as.list(par), as.list(fix.arg))) - do.call(pdistnam,
                c(list(icens$left), as.list(par), as.list(fix.arg))))
            - sum(weights[irow.ncens] * p1) - sum(weights[irow.lcens] *
                p2) - sum(weights[irow.rcens] * p3) - sum(weights[irow.icens] *
                p4)
        }
    }
    owarn <- getOption("warn")
    if (is.null(custom.optim)) {
        hasbound <- any(is.finite(lower) | is.finite(upper))
        if (optim.method == "default") {
            meth <- ifelse(length(vstart) > 1, "Nelder-Mead",
                "BFGS")
        }
        else meth <- optim.method
        if (meth == "BFGS" && hasbound && is.null(gradient)) {
            meth <- "L-BFGS-B"
            txt1 <- "The BFGS method cannot be used with bounds without provided the gradient."
            txt2 <- "The method is changed to L-BFGS-B."
            warning(paste(txt1, txt2))
        }
        options(warn = ifelse(silent, -1, 0))
        if (hasbound) {
            if (!is.null(gradient)) {
                opt.fun <- "constrOptim"
            }
            else {
                if (meth == "Nelder-Mead")
                    opt.fun <- "constrOptim"
                else if (meth %in% c("L-BFGS-B", "Brent"))
                    opt.fun <- "optim"
                else {
                    txt1 <- paste("The method", meth, "cannot be used by constrOptim() nor optim() without gradient and bounds.")
                    txt2 <- "Only optimization methods L-BFGS-B, Brent and Nelder-Mead can be used in such case."
                    stop(paste(txt1, txt2))
                }
            }
            if (opt.fun == "constrOptim") {
                npar <- length(vstart)
                lower <- as.double(rep_len(lower, npar))
                upper <- as.double(rep_len(upper, npar))
                haslow <- is.finite(lower)
                Mat <- diag(npar)[haslow,]
                hasupp <- is.finite(upper)
                Mat <- rbind(Mat, - diag(npar)[hasupp,])
                colnames(Mat) <- names(vstart)
                rownames(Mat) <- paste0("constr", 1:NROW(Mat))
                Bnd <- c(lower[is.finite(lower)], - upper[is.finite(upper)])
                names(Bnd) <- paste0("constr", 1:length(Bnd))
                initconstr <- Mat %*% vstart - Bnd
                if (any(initconstr < 0))
                    stop("Starting values must be in the feasible region.")
                if (!cens) {
                    opttryerror <- try(opt <- constrOptim(theta = vstart,
                    f = fnobj, ui = Mat, ci = Bnd, grad = gradient,
                    fix.arg = fix.arg, obs = data, ddistnam = ddistname,
                    hessian = !is.null(gradient), method = meth,
                    ...), silent = TRUE)
                }
                else opttryerror <- try(opt <- constrOptim(theta = vstart,
                  f = fnobjcens, ui = Mat, ci = Bnd, grad = gradient,
                  ddistnam = ddistname, rcens = rcens, lcens = lcens,
                  icens = icens, ncens = ncens, pdistnam = pdistname,
                  fix.arg = fix.arg, hessian = !is.null(gradient),
                  method = meth, ...), silent = TRUE)
                if (!inherits(opttryerror, "try-error"))
                    if (length(opt$counts) == 1)
                        opt$counts <- c(opt$counts, NA)
                    }
            else {
                if (!cens)
                    opttryerror <- try(opt <- optim(par = vstart,
                    fn = fnobj, fix.arg = fix.arg, obs = data,
                    gr = gradient, ddistnam = ddistname, hessian = TRUE,
                    method = meth, lower = lower, upper = upper,
                    ...), silent = TRUE)
                else opttryerror <- try(opt <- optim(par = vstart,
                  fn = fnobjcens, fix.arg = fix.arg, gr = gradient,
                  rcens = rcens, lcens = lcens, icens = icens,
                  ncens = ncens, ddistnam = ddistname, pdistnam = pdistname,
                  hessian = TRUE, method = meth, lower = lower,
                  upper = upper, ...), silent = TRUE)
                }
        }
        else {
            opt.fun <- "optim"
            if (!cens)
                opttryerror <- try(opt <- optim(par = vstart,
                  fn = fnobj, fix.arg = fix.arg, obs = data,
                  gr = gradient, ddistnam = ddistname, hessian = TRUE,
                    method = method, lower = lower, upper = upper, control=list(trace=6),
                  ...), silent = TRUE)
            else opttryerror <- try(opt <- optim(par = vstart,
                fn = fnobjcens, fix.arg = fix.arg, gr = gradient,
                rcens = rcens, lcens = lcens, icens = icens,
                ncens = ncens, ddistnam = ddistname, pdistnam = pdistname,
                hessian = TRUE, method = meth, lower = lower,
                upper = upper, ...), silent = TRUE)
            }
        options(warn = owarn)
        if (inherits(opttryerror, "try-error")) {
            warnings("The function optim encountered an error and stopped.")
            if (getOption("show.error.messages"))
                print(attr(opttryerror, "condition"))
            return(list(estimate = rep(NA, length(vstart)), convergence = 100,
                loglik = NA, hessian = NA, optim.function = opt.fun,
                fix.arg = fix.arg, optim.method = meth, fix.arg.fun = fix.arg.fun,
                counts = c(NA, NA)))
        }
        if (opt$convergence > 0) {
            warnings("The function optim failed to converge, with the error code ",
                opt$convergence)
        }
        if (is.null(names(opt$par)))
            names(opt$par) <- names(vstart)
        res <- list(estimate = opt$par, convergence = opt$convergence,
            value = opt$value, hessian = opt$hessian, optim.function = opt.fun,
            optim.method = meth, fix.arg = fix.arg, fix.arg.fun = fix.arg.fun,
            weights = weights, counts = opt$counts, optim.message = opt$message,
            loglik = -opt$value)
    }
    else {
        options(warn = ifelse(silent, -1, 0))
        if (!cens)
            opttryerror <- try(opt <- custom.optim(fn = fnobj,
                fix.arg = fix.arg, obs = data, ddistnam = ddistname,
                par = vstart, ...), silent = TRUE)
        else opttryerror <- try(opt <- custom.optim(fn = fnobjcens,
            fix.arg = fix.arg, rcens = rcens, lcens = lcens,
            icens = icens, ncens = ncens, ddistnam = ddistname,
            pdistnam = pdistname, par = vstart, ...), silent = TRUE)
        options(warn = owarn)
        if (inherits(opttryerror, "try-error")) {
            warnings("The customized optimization function encountered an error and stopped.")
            if (getOption("show.error.messages"))
                print(attr(opttryerror, "condition"))
            return(list(estimate = rep(NA, length(vstart)), convergence = 100,
                loglik = NA, hessian = NA, optim.function = custom.optim,
                fix.arg = fix.arg, fix.arg.fun = fix.arg.fun,
                counts = c(NA, NA)))
        }
        if (opt$convergence > 0) {
            warnings("The customized optimization function failed to converge, with the error code ",
                opt$convergence)
        }
        if (is.null(names(opt$par)))
            names(opt$par) <- names(vstart)
        argdot <- list(...)
        method.cust <- argdot$method
        res <- list(estimate = opt$par, convergence = opt$convergence,
            value = opt$value, hessian = opt$hessian, optim.function = custom.optim,
            optim.method = method.cust, fix.arg = fix.arg, fix.arg.fun = fix.arg.fun,
            weights = weights, counts = opt$counts, optim.message = opt$message,
            loglik = -opt$value)
    }
    return(res)
}

z= trace(x.1972, 'gb2', start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1), lower = c(0, 0, 0, 0), optim.method = 'Nelder-Mead')
