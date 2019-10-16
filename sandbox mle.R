library(stats4)
library(fitdistrplus)
library(actuar)

# Fit a normal distribution with MLE -----------------
x = rnorm(10000, mean = 3, sd = 2)
ll = function(mu, sigma) {
    r = dnorm(x, mu, sigma)
    return(-sum(log(r)))
}
#ll(4,2)
library(stats4)
mle(ll, start = list(mu = 1, sigma = 1))

# Fit a pareto distribution with closed-form MLE ----------------
# Reference: https://stats.stackexchange.com/questions/27426/how-do-i-fit-a-set-of-data-to-a-pareto-distribution-in-r
# Graph the pdf
x = (100:1000)/100
pdf = dpareto(x, location = 1, shape = 1)
qplot(x, pdf)

# Fit distribution
x = rpareto(10000, 1, 2)
pareto.MLE <- function(X) {
    n <- length(X)
    m <- min(X)
    a <- n / sum(log(X) - log(m))
    return(c(m, a))
}
pareto.MLE(x)

# Fit a pareto distribution with actual MLE ---------------
x = rpareto(10000, scale = 1, shape = 2)
fit <- fitdist(x, distr = "pareto", method = 'mle', start = list(scale = 1, shape = 2))
summary(fit)


# Fit a censored normal distr with fitdistcens ---------------
x = rnorm(1000, mean = 5, sd = 1) # Normal distribution
x.censored = x
x.censored[x.censored > 5] = 5 # Censor data
#hist(x.censored)

# Create censored dataset in correct format for fitdistcens
x.censored.fitdist = data.frame(left = x, right = x)
x.censored.fitdist$right[x.censored.fitdist$right > 5] = NA
x.censored.fitdist$left[x.censored.fitdist$left > 5] = 5

fitnorm = list()
# Regular fit, not very accurate:
fitnorm$nocens = fitdist(x.censored, 'norm', start = list(mean = 5, sd = 1))
#summary(fit)
#plot(fit)

# Fit with fitdistcens: Much better
fitnorm$cens = fitdistcens(x.censored.fitdist, 'norm', start = list(mean = 5, sd = 1))
#summary(fit)
#plot(fit)


cdf.fit = function(y, fits, plot=T,xlim=NULL,ylim=NULL) {
    y = sort(y)
    wrkdata = data.table(data = y, pct = rank(y, ties.method = 'first') / length(y))
    for (fit in names(fits)) {
        wrkdata[, paste0('theo.', fit) := do.call(paste0('q', fits[[fit]]$distname),
            c(list(wrkdata$pct), as.list(fits[[fit]]$estimate)))]
    }
    plotdata = melt(wrkdata, id.vars = 'pct')
    if (plot==T) {
        plot = ggplot() +
            geom_point(data = plotdata[variable == 'data'], aes(x = pct, y = value)) +
            geom_line(data = plotdata[variable != 'data'], aes(x = pct, y = value, color = variable)) +
        coord_cartesian(xlim, ylim)
        print(plot)
    }
    return(wrkdata)
}

cdf.fit(x.censored,fitnorm)

#old code--------------------------

#plotfit.cdf = function(y, para, distname, xlim = NULL, ylim = NULL) {
    ##y = data
    ##para = fit$estimate
    ##distname = 'norm'
    ##if (length(y) > 1000) {
    ##y = sort(sample(y, size = 10000))
    ##} else {
    #y = sort(y)
    ##}
    #y = data.table(y = y, pct = rank(y, ties.method = 'first') / length(y))
    #qdistname = paste0('q', distname)
    #pct = 1:1000 / 1000
    #yhat = data.table(yhat = do.call(qdistname, c(list(pct), as.list(para))), pct = pct)
    #plot = ggplot() + geom_point(data = y, aes(x = y, y = pct)) +
                      #geom_line(data = yhat, aes(x = yhat, y = pct, color = 'red')) +
                      #coord_cartesian(xlim = xlim, ylim = ylim)
    #return(plot)
#}

##p = plotfit.cdf(data, fit$estimate, 'norm', xlim = c(10,12))
##print(p)

#mse.qq = function(y, para, distname) {
    #y = x
    #para = fit$pareto$nocens$estimate
    #distname = 'pareto'
    #pdistname = paste0('p', distname)
    #qdistname = paste0('q', distname)

    #data = sort(y)
    ##z = data.table(y = data, pct = trunc(rank(data)) / length(data))
    #z = data.table(y = y, rank = rank(y, ties.method = 'first'), pct = rank(y, ties.method = 'first') / length(y))
    #z[, lorenz := cumsum(y) / sum(y)]

    #z[, tester := do.call(qdistname, c(list(z$pct), as.list(fit$pareto$nocens$estimate)))]
    #z[, tester2 := do.call(qdistname, c(list(z$pct), as.list(fit$pareto$cens$estimate)))]
    ##pct_theo = do.call(pdistname, c(list(z$y), as.list(para)))
    ##z = cbind(z, pct_theo)
    ##mse = sum((z$pct - z$pct_theo) ^ 2)
    ##aad = sum(abs(z$pct - z$pct_theo))
    ##cor = cor(z$pct, z$pct_theo, use = 'complete.obs')
    ##return(c(mse=mse,aad=aad,cor=cor))
    #return(z)
#}







# Fit a censored pareto distribution with fitdist package
x = rpareto(1000, shape = 5, scale = 1)



censor.thresh = quantile(x, .75)

#hist(x)
x.censored = x
x.censored[x.censored > censor.thresh] = censor.thresh
#hist(x.censored)

x.censored.fitdist = data.frame(left = x, right = x)
x.censored.fitdist$right[x.censored.fitdist$right > censor.thresh] = NA
x.censored.fitdist$left[x.censored.fitdist$left > censor.thresh] = censor.thresh

#x.censored.fitdist = data.cens
#x.censored = data$x.cens

fitpareto=list()
fitpareto$cens = fitdistcens(x.censored.fitdist, 'pareto', start = list(shape = 5, scale = 1))
#summary(fit)
#plot(fit)

fitpareto$nocens = fitdist(x.censored, 'pareto', start = list(shape = 5, scale = 1))
#summary(fit)
#plot(fit)

cdf.fit(x.censored, fitpareto, ylim = c(0,10))