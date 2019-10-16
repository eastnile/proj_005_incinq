library(stats4)
library(fitdistrplus)
library(actuar)
library(GB2)
setproj(5)
cps = fread('wrkdata/ipums_cps_small.csv')

fakecens = function(x, censpt) {
    wrkdata = data.table(x = x, x.cens = x, cens = 0)
    wrkdata[x.cens >= censpt, x.cens := censpt]
    wrkdata[x.cens >= censpt, cens := 1]

    fitdata = data.frame(left = x, right = x)
    fitdata$right[fitdata$right >= censpt] = NA
    fitdata$left[fitdata$left >= censpt] = censpt

    output = list(wrkdata, fitdata)
    names(output) = c('wrkdata', 'fitdata')
    return(output)
}
#fwrite(cps.income, paste0(p$data$local, 'ipums_cps_incvar_orig.csv'))
#fwrite(cps.income, paste0(p$data$local, 'ipums_cps_incvar_swap1.csv'))
#fwrite(cps.income, paste0(p$data$local, 'ipums_cps_incvar_swap2.csv'))

x.unscaled = cps[year == 2015, incwage]
x.unscaled = x.unscaled[!is.na(x.unscaled) & x.unscaled > 0]
scalefac = sd(x.unscaled)
x = x.unscaled / scalefac
censpt = quantile(x,.99)
cutoff = quantile(x, .6)
x.rich = x[x >= cutoff]
shift = min(x.rich)
x.rich.shift = x.rich - shift

# functions for seeing results ----------------

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
            geom_point(data = plotdata[variable != 'data'], aes(x = pct, y = value, color = variable)) + coord_cartesian(xlim, ylim)

        print(plot)
    }
    return(wrkdata)
}

cdf.fit.cens = function(x.cens.dt, fits, plot = T, xlim = NULL, ylim = NULL, scalefac = 1) {

    #x.cens.dt = x.cens.plot
    #fits = fit$genbeta
    wrkdata = x.cens.dt[order(x)]
    wrkdata[, pct := rank(x, ties.method = 'first') / length(x)]

    for (fit in names(fits)) {
        wrkdata[, paste0('theo.', fit) := do.call(paste0('q', fits[[fit]]$distname),
            c(list(wrkdata$pct), as.list(fits[[fit]]$estimate)))]
    }
    setcolorder(wrkdata, 'pct')
    wrkdata[, 2:ncol(wrkdata)] = wrkdata[, 2:ncol(wrkdata)] * scalefac
    plotdata = melt(wrkdata, id.vars = c('pct', 'cens'))
    plotdata$variable = as.character(plotdata$variable)
    if (plot == T) {
        plot = ggplot() +
        geom_line(data = plotdata[variable %like% 'theo.'], aes(x = pct, y = value, color = variable)) +
        geom_point(data = plotdata[variable == 'x'], aes(x = pct, y = value), color = 'black') +
        geom_point(data = plotdata[variable == 'x.cens'], aes(x = pct, y = value), color = 'red') +

        coord_cartesian(xlim, ylim)
        print(plot)
    }
    return(wrkdata)
}

mse = function(x, y) {
    x[is.infinite(x)] = NA
    y[is.infinite(y)] = NA
    return(sum((x-y)^2, na.rm=T))
}

apd = function(x, y) {
    x[is.infinite(x)] = NA
    y[is.infinite(y)] = NA
    return(mean(abs((x - y)/y), na.rm = T))
}


fit = list()

# Fit pareto ---------------------
#fit$pareto = fitdist(x.rich.shift, 'pareto',
    #start = list(shape = 1, scale = 5), lower = c(0, 0))


#fit$pareto$cens = fitdistcens(x.fit, 'pareto',
#start = list(shape = 1, scale = 5), lower = c(0,0))

# Fit generalized beta  -------------------

#fit$genbeta = fitdist(x, distr = 'genbeta',
              #start = list(shape1 = 1, shape2 = 1, shape3 = 1, rate = 1, scale = 100),
              #lower = c(0, 0, 0, 0, 0), control = list(maxit = 500))


#z = cdf.fit(x.rich.shift, fit)
#fit$genbeta$cens = fitdistcens(x.cens, distr = 'genbeta',
              #start = list(shape1 = 1, shape2 = 1, shape3 = 1, rate = 1, scale = 100),
#lower = c(0, 0, 0, 0, 0), control = list(maxit = 500))

# Fit beta2 -----------------------
# install.packages('GB2')
library(GB2)

# Censored preformance with fake data: looks decent

# On all x
x = rgb2(n = 10000, shape1 = 1, shape2 = 1, shape3 = 1, scale = 1)
cutoff = quantile(x, .6)
x.rich = x[x >= cutoff]
shift = min(x.rich)
x.rich.shift = x.rich - shift

censpt = quantile(x, .95)
data = fakecens(x, censpt)$wrkdata
data.cens = fakecens(x, censpt)$fitdata

fit = list()
fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 100))
mse(q$x, q$theo.cens) # 23150072
apd(q$x, q$theo.cens) # 0.01362894


# On rich only
censpt = quantile(x.rich, .95)
data = fakecens(x.rich, censpt)$wrkdata
data.cens = fakecens(x.rich, censpt)$fitdata

fit = list()
fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 100))
mse(q$x, q$theo.cens) # 892498773
apd(q$x, q$theo.cens) # 0.06220193

# On rich only, shifted
censpt = quantile(x.rich.shift, .95)
data = fakecens(x.rich.shift, censpt)$wrkdata
data.cens = fakecens(x.rich.shift, censpt)$fitdata

fit = list()
fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 100))
mse(q$x, q$theo.cens) #58988979
apd(q$x, q$theo.cens) #0.03755299

# Now try uncensored preformance with real data
x.unscaled = cps[year == 2018, inctot]
x.unscaled = x.unscaled[!is.na(x.unscaled) & x.unscaled > 0]
scalefac = sd(x.unscaled)
x = x.unscaled / scalefac

fit = list()
fit$gb2$nocens = fitdist(x, distr = 'gb2', optim.method = 'Nelder-Mead',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit(x, fit$gb2, xlim = c(0, 1), ylim = c(0, 10))

# Now try censored preformance with real data
censpt = quantile(x, .95)
cutoff = quantile(x, .6)
x.rich = x[x >= cutoff]
shift = min(x.rich)
x.rich.shift = x.rich - shift


# On all X:

data = fakecens(x, censpt)$wrkdata
data.cens = fakecens(x, censpt)$fitdata

fit = list()
fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 10))
mse(q$x, q$theo.cens) # 9782.315
apd(q$x, q$theo.cens) # 0.03333699, seems pretty good


# On rich only
data = fakecens(x.rich, censpt)$wrkdata
data.cens = fakecens(x.rich, censpt)$fitdata

fit = list()
fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 10))
mse(q$x, q$theo.cens) # 159218.5
apd(q$x, q$theo.cens) # 0.05114381 # Poor results, esp. for higher incomes

# On rich only, shifted
data = fakecens(x.rich.shift, censpt)$wrkdata
data.cens = fakecens(x.rich.shift, censpt)$fitdata 

fit$genbeta.rich$nocens = fitdist(data$x.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))
fit$genbeta.rich$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

q = cdf.fit.cens(data, fit$genbeta.rich, xlim = c(0, 1), ylim = c(0, 10))
mse(q$x, q$theo.cens) # Doesn't converge
apd(q$x, q$theo.cens) # 



# Try other fitting methods: Method of Moments -----------------------------

x = rgb2(n = 10000, shape1 = 1, shape2 = 1, shape3 = 1, scale = 1)

data = fakecens(x, censpt)$wrkdata
data.cens = fakecens(x, censpt)$fitdata

fit = list()

fit$mle$nocens = fitdist(data$x, distr = 'gb2', method = 'mle',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 100, trace = 1))

fit$mle$cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 100, trace = 1))

cdf.fit.cens(data,fit$mle)

memp <- function(x, order) mean(x ^ order)
# Theoretical moment
mgb2 = function(order, shape1, scale, shape2, shape3) {
    return(moment.gb2(order, shape1, scale, shape2, shape3))
}

fit$mme$nocens = fitdist(data$x, distr = 'gb2', method = 'mme',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 5),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1),
              order = c(1,2,3,4), memp = memp)

fit$mme$cens = fitdist(data$x.cens, distr = 'gb2', method = 'mle',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1),
              order = c(1, 2), memp = memp))


fit$pareto.rich$cens_off = fitdist(data$x.cens, distr = 'pareto',
              start = list(shape = 5, scale = 1),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))

fit$pareto.rich$cens_on = fitdistcens(data.cens, distr = 'pareto',
              start = list(shape = 5, scale = 1),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))

fit$pareto.rich$nocens = fitdist(data$x, distr = 'pareto',
              start = list(shape = 5, scale = 1),
              lower = c(0, 0), control = list(maxit = 100, trace = 1))

q = cdf.fit.cens(data, fit$pareto.rich, plot = T, xlim = c(0.8,1), ylim = c(0,15))


require(actuar)
#simulate a sample
x4 <- rpareto(1000, 6, 2)

#empirical raw moment
memp <- function(x, order) mean(x ^ order)
# Theoretical moment
mgb2 = function(order, shape1, scale, shape2, shape3) {
    return(moment.gb2(k, shape1, scale, shape2, shape3))
}

#fit by MME
mmedist(x4, "pareto", order = c(1, 2), memp = memp,
    start = list(shape = 10, scale = 10), lower = 1, upper = Inf)

# Try other fitting methods: Distance Estimation (curve fitting the emperical CDF)------

x = rgb2(n = 10000, shape1 = 1, shape2 = 1, shape3 = 1, scale = 1)

fit = list()
fit$mge = fitdist(x, distr = 'gb2', method = 'mge',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 100, trace = 1))
fit$mle = fitdist(x, distr = 'gb2', method = 'mle',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 200, trace = 1))
cdf.fit(x, fit)

censpt = quantile(x,.75) # Now try censored data
data = fakecens(x, censpt)$wrkdata
data.cens = fakecens(x, censpt)$fitdata

fit = list()
fit$mle.nocens = fitdist(data$x.cens, distr = 'gb2', method = 'mle',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 200, trace = 1))

fit$mle.cens = fitdistcens(data.cens, distr = 'gb2',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 200, trace = 1))

fit$mge = fitdist(data$x.cens, distr = 'gb2', method = 'mge',
              start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
              lower = c(0, 0, 0, 0), control = list(maxit = 100, trace = 1))

cdf.fit.cens(data,fit, xlim = c(0.75,1), ylim = c(0,50))