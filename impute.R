setproj(5)
library(fitdistrplus)
library(GB2)

# Load data --------------
cps = fread('wrkdata/ipums_cps_incvar_orig.csv')
cps.swap1 = fread('wrkdata/ipums_cps_incvar_swap1.csv')
cps.swap2 = fread('wrkdata/ipums_cps_incvar_swap2.csv')

# Examine Data ---------------------
#options(scipen = 10000)
#data = cps.swap1[year == 2015, .(incwage)]
#data = data[order(incwage)]
#data = data[,id:=.I]

#rich = data[incwage >= quantile(incwage, .95, na.rm = T)]
#qplot(data = rich, id, incwage) + scale_x_continuous(labels = comma)
#data = cps.swap1[year == 2015, (incwage)]

# Perform imputation ---------------------

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

fit.yr = function(x,cens,...) {
    x.unscaled = x[!is.na(x) & x != 0] ; scalefac = sd(x.unscaled) ; x = x.unscaled / scalefac 
    censpt = max(x) ; x.cens = data.table(left = x,right = x) ; x.cens[right == censpt, right := NA]
    #fit = list()
    if (cens == 'nocens') {
        fit = tryCatch(fitdist(x, ...),
            error = function(e) e
        )
    } else {
        fit = tryCatch(fitdistcens(x.cens, ...),
            error = function(e) e
        )
    }
    return(fit)
}


# Do Fits -------------
datasets = c('cps', 'cps.swap1', 'cps.swap2')
vars = c('inctot', 'incwage')
fits = c('cens', 'nocens')
distrs = c('gb2')
years = 1962:2018

fit = array(data = list(),
            dim = c(length(datasets), length(vars), length(distr), length(fits),length(years)),
            dimnames = list(datasets, vars, distrs, fits, years)
            ) 

system.time({
for (dataset in datasets) {
    for (var in vars) {
        for (distr in distrs) {
            for (i in 1:length(years)) {

                #dataset = datasets[1]
                #var = vars[1]
                #fit = fits[1]
                #distr = distrs[1]
                #i = 1
                print(paste(dataset,var,distr,years[i]))

                data = get(dataset)[year == years[i], get(var)]
                # data = sample(data, 1000)
                z = fit.yr(data, control = list(maxit = 250, trace = 0), optim.method = 'L-BFGS-B', distr = 'gb2',
                    start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1), lower = c(0, 0, 0, 0))

                cps.fit[[dataset, var, distr, 'nocens', i]] = z$nocens
                cps.fit[[dataset, var, distr, 'cens', i]] = z$cens

            }
        }
    }
}
})

# saveRDS(fit,file = 'wrkdata/fitcps.rds')


# Redo erroneous fits ------------------
for (dataset in datasets) {
    for (var in vars) {
        for (distr in distrs) {
            for (fit in fits) {
                for (i in 1:length(years)) {
                    print(paste(dataset, var, distr, fit, years[i]))
                    print(fit)
                    data = get(dataset)[year == years[i], get(var)]
                    if ('error' %in% class(cps.fit[[dataset, var, distr, fit, i]])) {
                        print('error')
                        z = fit.yr(data, fit, control = list(maxit = 500, trace = 0), optim.method = 'Nelder-Mead', distr = 'gb2',
                            start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1), lower = c(0, 0, 0, 0))
                        cps.fit[[dataset, var, distr, fit, i]] = z
                    } else {
                        print('no error')
                    }
                }
            }
        }
    }
}

#saveRDS(cps.fit,file = 'wrkdata/fitcps2.rds')

# Check for failures --------------
fit.sum = array(data = integer(),
            dim = c(length(datasets), length(vars), length(distr), length(fits), length(years)),
            dimnames = list(datasets, vars, distrs, fits, years)
            )
for (dataset in datasets) {
    for (var in vars) {
        for (distr in distrs) {
            for (fit in fits) {
                for (i in 1:length(years)) {
                    if ('error' %in% class(cps.fit[[dataset, var, distr, fit, i]])) {
                        #fit.sum[[dataset, var, distr, fit, i]] = as.character(cps.fit[[dataset, var, distr, fit, i]])
                        fit.sum[[dataset, var, distr, fit, i]] = 1
                    } else {
                        #fit.sum[[dataset, var, distr, fit, i]] = paste(cps.fit[[dataset, var, distr, fit, i]])
                        fit.sum[[dataset, var, distr, fit, i]] = 0
                    }
                    #dataset = datasets[1]
                    #var = vars[1]
                    #fit = fits[1]
                    #distr = distrs[1]
                    #i = 1
                    # data = sample(data, 1000)
                }
            }
        }
    }
}


# Examine imputed data
cps.fit = readRDS('wrkdata/fitcps2.rds')

cps.sim = data.table(NULL)
for (year in as.character(1962:2018)) {

    #year = '1980'
    #print(cps.fit['cps', 'incwage', 'gb2', 'nocens', year])
    z = cps.fit[['cps.swap1', 'incwage', 'gb2', 'nocens', year]]$estimate
    z$n = 10000

    incwage = do.call(rgb2, z)
    yearnum = as.numeric(year)

    dt = data.table(incwage,year=yearnum)
    cps.sim = rbind(cps.sim,dt)

}

incshare = function(data, percentile) {
    z = quantile(data, percentile, na.rm = T)
    sum(data[data >= z], na.rm = T) / sum(data, na.rm = T)
}
#incshare(acs$inctot, .90)
incinq.calc = function(data, stat, ...) {
    if (stat == 'incshare') {
        return(incshare(data, ...))
    } else {
        return(ineq(data, type = stat, na.rm = T))
    }
}


datasets = c('cps', 'cps.swap1', 'cps.swap2', 'cps.sim')
vars = c('incwage')
stats = c('Gini', 'incshare')
years = 1940:2018
incinq = array(data = NA,
               dim = c(length(datasets), length(vars), length(stats), length(years)),
               dimnames = list(datasets, vars, stats, years)) # [dataset,var,stat,year]

for (data in datasets) {
    for (var in vars) {
        for (stat in stats) {
            #data = datasets[1]
            #var = vars[1]
            #stat = stats[1]
            z = get(data)
            z = z[!is.na(get(var)) & get(var) >0]
            z = z[, .(val = incinq.calc(get(var), stat = stat, percentile = .99)), by = year]
            z2 = merge(data.table(year = 1940:2018), z, all.x = T)
            incinq[data, var, stat,] = z2$val
        }
    }
}

incinq.long = as.data.table(plyr::adply(incinq, .margins = c(1, 2, 3, 4), .id = c('dataset', 'var', 'stat', 'year')))

plotdata = incinq.long[, info := paste(dataset, var, stat)]
plotdata$year = as.integer(as.character(plotdata$year))

plotdata = plotdata[year >= 1960 & stat == 'Gini',]

ggplot(plotdata, aes(x = year, y = V1, group = info, color = info)) + geom_line(size = 2)








#LL2 <- function(shape1,shape2,shape3,scale) {
    #R = dgb2(data, shape1,shape2,shape3,scale)
    #return(-sum(log(R)))
#}

#LL2(1,1,1,1)

#LL <- function(mu, sigma) {
    #R = dnorm(data, mu, sigma)
    #return(R)
    ##return(-sum(log(R)))
#}

#LL(2,3)

#mle(LL2, start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1))




# cps.fit = readRDS(file='wrkdata/fitcps.rds')

# saveRDS(fit,file = 'wrkdata/fitcps.rds')



#fit['cps','inctot','gb2','nocens',]

#for (i in 1:length(years)) {
    #data = sample(cps.swap1[year == years[i], incwage], 10000)
    #z = quote (
    #fit.yr(data, control = list(maxit = 100, trace = 0), optim.method = 'L-BFGS-B', distr = 'gb2',
        #start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1), lower = c(0, 0, 0, 0))
    #)
    #results[[i]] = tryCatch (
        #eval(z),
        #error = function(e) e,
        #finally = print('hello')
    #)
#}



#z1 = fitdist(data, distr = 'gb2', optim.method = 'L-BFGS-B',
              #start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1.5),
              #lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))

#z2 = fitdist(data, distr = 'gb2', optim.method = 'Nelder-Mead',
              #start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1.5),
              #lower = c(0, 0, 0, 0), control = list(maxit = 500, trace = 1))


#tryCatch(stop('jo'),
         #finally = print('hello'))

#tryCatch(1, finally = print("Hello"))


#stop(e)
#tryCatch(stop(e), finally = print("Hello"))

#tryCatch(stop(e), error = function(e) assign(,, finally = print("Hello"))



#omega = tryCatch(q=3,
         #error = function(e) e,
         #finally = print("Hello"))




expr = quote(stop(e))
tryCatch(eval(stop(e)),
         error = function(e) print(e),
         finally = print("Hello"))




withCallingHandlers({ warning("A"); 1 + 2 }, warning = function(w) { })



require(graphics)
(s.e <- substitute(expression(a + b), list(a = 1))) #> expression(1 + b)
(s.s <- substitute(a + b, list(a = 1))) #> 1 + b
c(mode(s.e), typeof(s.e)) #  "call", "language"
c(mode(s.s), typeof(s.s)) #   (the same)
# but:
(e.s.e <- eval(s.e)) #>  expression(1 + b)
c(mode(e.s.e), typeof(e.s.e)) #  "expression", "expression"

myplot <- function(x, y)
    plot(x, y, xlab = deparse(substitute(x)),
         ylab = deparse(substitute(y)))