library(stats4)
library(fitdistrplus)
library(actuar)
setproj(5)

acs = fread('wrkdata/ipums_cps.csv')

#mat = varname X year X dataversion
#list(mat1, mat2, mat3, etc)



x = acs[year == 1980, incwage]
x = x[!is.na(x) & x > 0]
x.rescaled = x/sd(x)
censor.thresh = max(x.rescaled)
x.fit = data.frame(left = x.rescaled, right = x.rescaled)
x.fit$right[x.fit$right >= censor.thresh] = NA

fit = list()

# Fit pareto
fit$pareto$nocens = fitdist(x.rescaled, 'pareto',
    start = list(shape = 1, scale = 5), lower = c(0,0))
fit$pareto$cens = fitdistcens(x.fit, 'pareto',
    start = list(shape = 1, scale = 5), lower = c(0,0))


# Fit generalized beta  -------------------

fit$genbeta$nocens = fitdist(x.rescaled, distr = 'genbeta',
              start = list(shape1 = 10, shape2 = 10, shape3 = 10, rate = 10, scale = 10),
              lower = c(0, 0, 0, 0, 0), control = list(maxit = 500))

fit$genbeta$cens = fitdistcens(x.fit, distr = 'genbeta',
              start = list(shape1 = 10, shape2 = 10, shape3 = 10, rate = 10, scale = 10),
              lower = c(0, 0, 0, 0, 0), control = list(maxit = 500))


# Fit genbeta on rich only -----------------------
cutoff = quantile(x.rescaled,.95)
x.rescaled.rich = x.rescaled[x.rescaled >= cutoff]

shift = min(x.rescaled.rich)

x.rescaled.rich = x.rescaled.rich - shift

x.rescaled.rich.censordrop = x.rescaled.rich[x.rescaled.rich != max(x.rescaled.rich)]


x.fit.rich = data.frame(left = x.rescaled.rich, right = x.rescaled.rich)
x.fit.rich$right[x.fit.rich$right >= censor.thresh - shift] = NA
#x.fit.rich$left[x.fit.rich$right >= cutoff] = NA

fit$genbeta.rich$nocens = fitdist(x.rescaled.rich.censordrop, distr = 'genbeta',
              start = list(shape1 = 1, shape2 = 3, shape3 = 1, rate = 1, scale = 10),
              lower = c(0, 0, 0, 0, 0), control = list(maxit = 100, trace = 1))

fit$genbeta.rich$nocens = fitdist(x.rescaled.rich.censordrop, distr = 'beta',
              start = list(shape1 = 1, shape2 = 10),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))


fit$genbeta.rich$cens = fitdistcens(x.fit.rich, distr = 'genbeta',
              start = list(shape1 = 1, shape2 = 1, shape3 = 100, rate = 100, scale = 150),
              lower = c(0, 0, 0, 0, 0), control = list(maxit = 100, trace = 1,report=1))


fit$genbeta.rich$nocens = fitdist(x.rescaled.rich, distr = 'pareto',
              start = list(shape = 1, scale = 2),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))

fit$genbeta.rich$cens = fitdistcens(x.fit.rich, distr = 'pareto',
              start = list(shape = 1, scale = 2),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))


cdf.fit = function(y, fits, plot = T, xlim = NULL, ylim = NULL, scalefac = 1) {
    y = sort(y)
    wrkdata = data.table(data = y, pct = rank(y, ties.method = 'first') / length(y))
    for (fit in names(fits)) {
        wrkdata[, paste0('theo.', fit) := do.call(paste0('q', fits[[fit]]$distname),
            c(list(wrkdata$pct), as.list(fits[[fit]]$estimate)))]
    }
    setcolorder(wrkdata, 'pct')
    wrkdata[, 2:ncol(wrkdata)] = wrkdata[, 2:ncol(wrkdata)] * scalefac
    plotdata = melt(wrkdata, id.vars = 'pct')
    if (plot == T) {
        plot = ggplot() +
            geom_point(data = plotdata[variable == 'data'], aes(x = pct, y = value)) +
            geom_line(data = plotdata[variable != 'data'], aes(x = pct, y = value, color = variable)) + coord_cartesian(xlim, ylim)

        print(plot)
    }
    return(wrkdata)
}


#z = cdf.fit(x.rescaled, fit$pareto, plot = T, scalefac = sd(x))
#z = cdf.fit(x.rescaled, fit$genbeta, plot = T, scalefac = sd(x))
z = cdf.fit(x.rescaled.rich, fit$genbeta.rich, plot = T, scalefac = 1, ylim = c(0, 10))

z = cdf.fit(x.rescaled.rich, fit$genbeta.rich, plot = T, scalefac = 1, ylim = c(0, 10))


# Fit gen beta on rich only, validating using recent data
