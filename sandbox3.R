
#myfunc = function(x, z, weights = NULL) {
data = cps[year == 1968 & complete.cases(incwage, occ1950), .(incwage, occ1950)]

#data = cps[year == 1982 & complete.cases(inctot, educ)]



x = data[, incwage]
x[x<=0] = 1
z = data[, occ1950]
z = droplevels(z)

weights = NULL

table(z)


    if (is.null(weights)) {
        weights <- rep(1, length(x))
    }
    note = paste(sum(x <= 0, na.rm = TRUE), "negative or zero x's deleted (unweighted)")
    if (!all(weights >= 0, na.rm = TRUE))
        stop("At least one weight is negative", call. = FALSE)
    if (all(weights == 0, na.rm = TRUE))
        stop("All weights are zero", call. = FALSE)
    z <- factor(z)
    df <- data.frame(x = as.numeric(x[x > 0]), z = as.factor(z[x >
        0]), w = as.numeric(weights[x > 0]))
    df <- df[stats::complete.cases(df),, drop = FALSE]
    n <- as.numeric(nrow(df))
    n_weighted <- sum(df[, "w"])
    dfSplit <- split(df[, c("x", "w")], df[, "z"])
    n_group <- table(df[, "z"])
    n_group_weighted <- sapply(dfSplit, function(df) sum(df[,
        "w"]), simplify = TRUE)
    df[, "w"] <- df[, "w"] / sum(df[, "w"])
    xMean <- stats::weighted.mean(df[, "x"], df[, "w"])
    xMean_group <- sapply(dfSplit, function(df) stats::weighted.mean(df[,
        "x"], df[, "w"]), simplify = TRUE)
    share_group <- n_group_weighted / n_weighted
share_group_income <- share_group * xMean_group / xMean

    mld_group <- sapply(dfSplit, function(df) mld.wtd(df[, "x"],
        df[, "w"]), simplify = TRUE)



    mld_group_contribution <- mld_group * (share_group_income ^ 0) *
        (share_group)
    mld_within <- sum(mld_group_contribution)
    mld_between <- mld.wtd(xMean_group, share_group)
    index <- mld_within + mld_between
    mld_total <- mld.wtd(df[, "x"], df[, "w"])
    return(list(mld_decomp = list(mld_total = mld_total, mld_within = mld_within,
        mld_between = mld_between), mld_group = list(mld_group = mld_group,
        mld_group_contribution = mld_group_contribution), mean = list(mean_total = xMean,
        mean_group = xMean_group), share_groups = share_group,
        share_income_groups = share_group_income, number_cases = list(n_unweighted = n,
            n_weighted = n_weighted, n_group_unweighted = n_group,
            n_group_weighted = n_group_weighted), note = note))
#}

#x = cps[year == 1982, inctot]
#z = cps[year == 1982, educ]
#weights = NULL
#myfunc(x, z)
#mld_decomp(x,y)


> mld.wtd
function(x, weights = NULL) {
    if (is.null(weights)) {
        weights <- rep(1, length(x))
    }
    missing <- !(is.na(x) | is.na(weights))
    x <- x[missing]
    weights <- weights[missing]
    if (!all(weights >= 0))
        stop("At least one weight is negative", call. = FALSE)
    if (all(weights == 0))
        stop("All weights are zero", call. = FALSE)
    x_sel <- x[x > 0]
    weights_sel <- weights[x > 0]
    weights_sel <- weights_sel / sum(weights_sel)
    mean <- stats::weighted.mean(x_sel, weights_sel)
    x_sel <- x_sel / mean
    mld <- (-sum(weights_sel * log(x_sel)))
    return(mld)
}