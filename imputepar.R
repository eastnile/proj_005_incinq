## init -------------
setproj(5)
libs = c('foreach', 'iterators', 'fitdistrplus', 'GB2', 'doSNOW', 'actuar', 'abind') # doParallel
libif(libs)
rm(libs)
# Load data --
cps = fread('wrkdata/ipums_cps_incvar_orig.csv')
cps.swap1 = fread('wrkdata/ipums_cps_incvar_swap1.csv')
# cps.swap2 = fread('wrkdata/ipums_cps_incvar_swap2.csv')

## Fill arrays ---------------------
dims = c('datasets', 'vars', 'fits', 'distrs', 'years')
datasets = c('cps','cps.swap1') # c('cps', 'cps.swap1', 'cps.swap2')
vars = names(cps)[-1:-3] # c('inctot', 'incwage', 'incwelfr')
fits = c('cens', 'nocens')
distrs = c('gb2', 'pareto') # distrs = c('gb2', 'pareto')
years = as.character(1962:2018) # years = 1962:2018
#--
#dims = c('datasets','vars','fits','distrs','years')
#datasets = c('cps')
#vars = c('inctot')
#fits = c('nocens')
#distrs = c('gb2')
#years = 2017:2018

wrkdata = array.new(data = list(), dimnames = list(datasets, vars, fits, distrs, years),
                    dimnames2 = c('datasets', 'vars', 'fits', 'distrs', 'years'))
iddata = array.new(data = list(), dimnames = list(datasets, years),
                   dimnames2 = c('datasets', 'years'))

for (dataset in datasets) { 
    setkey(get(dataset), year, serial, pernum)
    for (yr in years) {
        z = get(dataset)[list(as.integer(yr))]
        iddata[[dataset, yr]] = z[,.(year,serial,pernum)]
        # print(yr)
        for (var in vars) {
            for (fit in fits) {
                for (distr in distrs) {
                    wrkdata[[dataset, var, fit, distr, yr]] = z[[var]]
                }
            }
        }
    }
}

## Fit distributions (first pass) -----------
# Set opts --
opts.dir = as.data.table(array.dir(wrkdata))
opts = array.new(data = list(), dim = c(4, nrow(opts.dir)), dimnames = list(c('control', 'method', 'start', 'lower'), NULL))
opts['control',] = rep(list(list(maxit = 500, trace = 0)), nrow(opts.dir))
opts['method',] = rep('Nelder-Mead')
opts['start',] = ifelse(opts.dir$distrs == 'gb2', list(list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1)), list(list(shape = 1, scale = 1)))
opts['lower',] = ifelse(opts.dir$distrs == 'gb2', list(c(0, 0, 0, 0)), list(c(0, 0)))
#par.dir.small = par.dir[1:50]
fitcps = function(cps.array, ncore = 1, opts) { # most of the requisite parameters are in the arraynames
    # Function for yearly imputation
    fit.yr.par = function(x, fit, ...) {
        x = x[!is.na(x) & x != 0]
        x = x / sd(x)
        #print(start)
        #print(distr)
        if (fit == 'nocens') {
            output = fitdist(x, ...)
        } else {
            censpt = max(x);
            x.cens = data.table(left = x, right = x);
            x.cens[right == censpt, right := NA]
            output = fitdistcens(x.cens, ...)
        }
        return(output)
    }
    # Main Loop
    time.start = Sys.time()
    pb <- txtProgressBar(max = length(cps.array), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    snowopts <- list(progress = progress)
    #cl = makeCluster(ncore)
    #registerDoParallel(cl)
    cl <- makeSOCKcluster(ncore)
    registerDoSNOW(cl)

    cps.fit = foreach(dataset = iter(cps.array),
                      control = iter(opts['control',]),
                      method = iter(opts['method',]),
                      start = iter(opts['start',]),
                      lower = iter(opts['lower',]),
                      fit = array.dir(cps.array)$fits,
                      distr = array.dir(cps.array)$distrs,
                      .packages = c('data.table', 'fitdistrplus', 'GB2', 'actuar'),
                      .errorhandling = 'pass',
                      .options.snow = snowopts
                      ) %dopar% {

    fit.yr.par(dataset, fit = fit, control = control, optim.method = method, start = start, lower = lower, distr = distr)
    }
    stopCluster(cl)
    time.end = Sys.time()
    print(paste(time.start, time.end, time.end - time.start))
    return(array.mimic(cps.fit, cps.array))
}
#fit.cps = fitcps(wrkdata, 10, opts)
saveRDS(cps.fit, 'wrkdata/cps_fit.rds')

## Project CPS data using fit -----------
cps.fit = readRDS('wrkdata/cps_fit.rds')
cps.imp = array.mimic(list(), cps.fit)
qgb2 = function(pr, shape1, scale, shape2, shape3) {
    z = ifelse(pr <= 0.5,
               qbeta(pr, shape2, shape3) / (1 - qbeta(pr, shape2, shape3)),
               (1 - qbeta(1 - pr, shape3, shape2)) / qbeta(1 - pr, shape3, shape2))
    return(scale * z ^ (1 / shape1))
}
get.theo = function(prob, fit, scalefac = 1) {
    # Note: vectorized over prob
    #pct = c(0.1, 0.2, 1)
    #fit = fitarray[[1, 1, 1]]
    #scalefac = 10
    if (any(str_detect(class(fit), 'error'))) {
        warning('no fit found')
        return(as.double(rep(NA, length(prob))))
    }
    arglist = c(list(prob), fit$estimate)
    res = scalefac * do.call(paste0('q', fit$distname), arglist)
    res[is.infinite(res)] = NA
    res = as.double(res)
    return(res)
}
mypctrank = function(x) {
    z = ifelse(x==0,NA,x)
    z = ifelse(is.na(z), NA, frank(z, ties.method = 'first'))
    z = z / length(z[!is.na(z) & z != 0])
    z[x == 0] = 0
    return(z)
}


for (i in 1:length(cps.fit)) { # 999:999) { # length(fit.cps)
    print(i)
    z = mypctrank(wrkdata[[i]])
    sd = sd(wrkdata[[i]], na.rm = T)
    #print(z)
    #print(fit.cps[[i]])
    #print(sd)
    #print(get.theo(z, fit.cps[[i]], sd))
    cps.imp[[i]] = get.theo(z, cps.fit[[i]], sd)
}

saveRDS(cps.imp, 'wrkdata/cps_imp_array.rds')

# Get stats ----------
cps.imp = readRDS('wrkdata/cps_imp_array.rds')
stats = c('mean', 'median', 'sd', 'max', 'min', 'atmax', 'pctinc.atmax', 'not.na', 'nobs', 'incshare90', 'gini')
cps.stats = array.new(numeric(), dimnames = list(datasets, vars, fits, distrs, years, stats),
                    dimnames2 = c('datasets', 'vars', 'fits', 'distrs', 'years', 'stats'))

f.summary = function(x) {
    not.na = function(x) {
        return(sum(!is.na(x)))
    }
    atmax = function(x) {
        return(sum(x == max(x, na.rm = T), na.rm = T) / sum(!is.na(x)))
    }
    pctinc.atmax = function(x) {
        sum(x[x == max(x, na.rm = T)], na.rm = T) / sum(x, na.rm = T)
    }
    incshare = function(data, percentile = .9) {
        z = quantile(data, percentile, na.rm = T)
        sum(data[data >= z], na.rm = T) / sum(data, na.rm = T)
    }
    incinq.calc = function(data, stat, ...) {
        data = data[!is.infinite(data)]
        if (stat == 'incshare') {
            return(incshare(data, ...))
        } else {
            return(ineq(data, type = stat, na.rm = T))
        }
    }
    return(list(mean = mean(x, na.rm = T),
                    median = median(x, na.rm = T),
                    sd = sd(x, na.rm = T),
                    max = max(x, na.rm = T),
                    min = min(x, na.rm = T),
                    atmax = atmax(x),
                    pctinc.atmax = pctinc.atmax(x),
                    not.na = not.na(x),
                    nobs = length(x),
                    incshare90 = incinq.calc(x, stat = 'incshare', percentile = .9),
                    gini = incinq.calc(x, stat = 'Gini')
                    ))
}
for (dataset in datasets) {
    for (var in vars) {
        for (fit in fits) {
            for (distr in distrs) {
                for (yr in years) {
                    z = f.summary(cps.imp[[dataset,var,fit,distr,yr]])
                    for (stat in stats) {
                        cps.stats[[dataset, var, fit, distr, yr, stat]] = z[[stat]]
                    }
                }
            }
        }
    }
}
saveRDS(cps.stats, 'wrkdata/cps_stats.rds')

# Ethan out --------
# Export data
cps.imp = readRDS('wrkdata/cps_imp_array.rds')
for (dataset in datasets) {
    for (fit in fits) {
        for (distr in distrs) {
            #dataset = 'cps'
            #fit = 'cens'
            #distr = 'gb2'
            z = list()
            #for (var in vars) {
            for (yr in years) {
                z[[yr]] = cbind(as.data.table(iddata[[dataset, yr]]),
                                as.data.table(do.call(cbind, cps.imp[dataset,, fit, distr, yr])))
                #z3 = data.frame(dataset = dataset, var = var, fit = fit, distr = distr, z1, z2)
                #cps.imp.dt = rbind(cps.imp.dt, z3)
                print(yr)
            }
            z = do.call(rbind, z)
            fname = paste0(dataset, '_imp_', fit, '_', distr,'.csv')
            print(fname)
            fwrite(z,file = paste0('wrkdata/',fname))
        }
    }
}

# Export fitted coefficents
cps.fit.gb2 = cps.fit[,,,'gb2',]
fitGetParms = function(x) {
    if (any(str_detect(class(x), 'error')))
        return(NULL)
    else return(x$estimate)
}
z = array.mimic(lapply(cps.fit.gb2, fitGetParms), cps.fit.gb2)

parm.arr = list()
dts = list()
for (name in names(z[[6]])) {
    # Get array
    z1 = lapply(z, '[[', name)
    parm.arr[[name]] = array.mimic(as.numeric(as.character(z1)), cps.fit.gb2)
    # Convert to dt
    dts[[name]] = as.data.table(plyr::adply(parm.arr[[name]], .margins = c(1, 3, 4)))
    dts[[name]][, parm := name]
    }
z2 = do.call('rbind',dts)
setcolorder(z2, c('datasets', 'fits', 'years', 'parm'))
z2 = z2[order(datasets,fits,years,parm)]
fwrite(z2,'wrkdata/cps_fit_gb2_parms.csv')




z = fread('wrkdata/cps.swap1_imp_cens_gb2.csv')
z2 = merge(z, cps, by = c('year', 'serial', 'pernum'))


plot(sort(sample(z2[year == 1968, inctot.x], 40000)))


View(z2[year == 1968, .(inctot.x,inctot.y)])

summary(cps.imp[['cps.swap1','inctot','cens','gb2','1982']])

cps.stats.dt = as.data.table(plyr::adply(cps.stats, .margins = c(1, 2, 3, 4, 5, 6)))
cps.stats.allyrs = cps.stats.dt[, mean(V1, na.rm = T), by = .(datasets, vars, fits, distrs, stats)]

z = cps.stats.allyrs[datasets == 'cps.swap1' & fits == 'cens' & distrs == 'gb2']

plotdata = cps.stats.dt[datasets %in% c('cps.swap1') & fits %in% c('cens') & distrs %in% c('gb2','pareto') & stats == 'gini' & vars %in% c('inctot'),]

ggplot(data = plotdata) + geom_line(aes(x = years, y = V1, group = distrs, color = distrs))


ggplot(plotdata[stat == 'incshare']) +
    geom_line(aes(x = year, y = V1, group = info, color = dataset), size = 1)
cps.stats.dt = as.data.table(plyr::adply())

incinq.long = as.data.table(plyr::adply(incinq, .margins = c(1, 2, 3, 4), .id = c('dataset', 'var', 'stat', 'year')))
plotdata = incinq.long[, info := paste(dataset, var, stat)]
plotdata$year = as.integer(as.character(plotdata$year))


cps.stats['cps.swap1', 'incwage', 'cens', 'gb2',, 'gini']
cps.imp['cps.swap1', 'incwage', 'cens', 'gb2','2016']
fit.cps['cps.swap1', 'incwage', 'cens', 'gb2', '2016']
z = cps.imp[['cps.swap1', 'incwage', 'cens', 'gb2', '2016']]



# Produce .csv file

# Visualize


View(cpsdata[year == 2018] %>% dplyr::select(year, matches('incwage')))
plot(cpsdata[year == 2018, incwage], cpsdata[year == 2018, incwage.theo.gb2.cens])
plotdata = cpsdata[year == 2018] %>% dplyr::select(matches('^incwage'))
plotdata.long = melt(plotdata, id.vars = 'incwage.pct')

library(ggplot2)
ggplot(data = plotdata.long) + geom_line(aes(x = incwage.pct, y = value, color = variable))





#z = lapply(cps.imp[1:4], stats[2], na.rm = T)

#projection.fit = function(cpsdata, fitarray) {
    ##cpsdata = cps.swap1
    ##fitarray = adrop(fit.cps, 1)

    #cpsdata = copy(cpsdata)
    #counter = 1
    #for (var in dimnames(fitarray)$vars) {
        #cpsdata[, paste0(var, '.pct') := rank(get(var), ties.method = 'first') / length(get(var)), by = year]
        #cpsdata = cpsdata[order(year, get(paste0(var, '.pct')))]
        #for (fit in dimnames(fitarray)$fits) {
            #for (distr in dimnames(fitarray)$distrs) {
                #for (yr in as.character(dimnames(fitarray)$years)) {
                    ##var = 'incss'
                    ##fit = 'cens'
                    ##distr = 'gb2'
                    ##yr = '1968'

                    ##print(fitarray[[var, fit, distr, yr]])


                    ##View(get.theo(cpsdata[year == as.integer(yr), get(paste0(var, '.pct'))], fitarray[[var, fit, distr, yr]], sd.yr))

                    ##print(head(
                        ##mean(get.theo(cpsdata[year == as.integer(yr), get(paste0(var, '.pct'))], fitarray[[var, fit, distr, yr]], sd.yr),na.rm=T)
                        ##)
                    ##)

                    #sd.yr = sd(cpsdata[year == as.integer(yr), get(var)], na.rm = T)

                    #cpsdata[year == as.integer(yr), paste0(var, '.imp.', distr, '.', fit) :=
                        #get.theo(get(paste0(var, '.pct')), fitarray[[var, fit, distr, yr]], sd.yr)]

                    #counter = counter + 1
                    #if (counter %% 5 == 0) {
                        #print(counter)
                        #print(paste(var,fit,distr,yr))
                        #print(fitarray[[var, fit, distr, yr]])
                        #print(head(cpsdata[year == as.integer(yr), paste0(var, '.imp.', distr, '.', fit), with = F]))
                        ##test[[counter]] = cpsdata[year == as.integer(yr), paste0(var, '.imp.', distr, '.', fit), with = F]
                    #}
                #}
            #}
        #}
    #}
    #return(cpsdata)
#}

#system.time({cps.swap1.imp = projection.fit(cps.swap1,adrop(fit.cps,1))})
## cps.swap1.imp = dplyr::select(cps.swap1.imp, -contains('.pct'))
#fwrite(cps.swap1.imp, 'wrkdata/cps_swap1_imp.csv')

## Get stats
#fread('wrkdata/cps.swap1.imp.csv')

#sumByYear = function(DT, cols = names(DT)) {
    ## cols = names(DT)
    #stats = c('mean', 'median', 'sd', 'max', 'min', 'atmax', 'pctinc.atmax', 'not.na', 'nobs', 'incshare90','gini')

    #not.na = function(x) {
        #return(sum(!is.na(x)))
    #}

    #atmax = function(x) {
        #return(sum(x == max(x, na.rm = T), na.rm = T) / sum(!is.na(x)))
    #}

    #pctinc.atmax = function(x) {
        #sum(x[x == max(x, na.rm = T)], na.rm = T) / sum(x, na.rm = T)
    #}

    #incshare = function(data, percentile=.9) {
        #z = quantile(data, percentile, na.rm = T)
        #sum(data[data >= z], na.rm = T) / sum(data, na.rm = T)
    #}

    ##incshare(acs$inctot, .90)
    #incinq.calc = function(data, stat, ...) {
        #if (stat == 'incshare') {
            #return(incshare(data, ...))
        #} else {
            #return(ineq(data, type = stat, na.rm = T))
        #}
    #}


    #f.summary = function(x) {
        #return(list(mean = mean(x, na.rm = T),
                    #median = median(x, na.rm = T),
                    #sd = sd(x, na.rm = T),
                    #max = max(x, na.rm = T),
                    #min = min(x, na.rm = T),
                    #atmax = atmax(x),
                    #pctinc.atmax = pctinc.atmax(x),
                    #not.na = not.na(x),
                    #nobs = length(x),
                    #incshare90 = incinq.calc(x, stat = 'incshare', percentile = .9),
                    #gini = incinq.calc(x, stat = 'Gini')
                    #))
    #}

    #sum = DT[, lapply(.SD, f.summary), by = year, .SDcols = setdiff(cols, 'year')]
    #sum = sum[, lapply(.SD, unlist)] # Unlist, otherwise each column is a list
    #sum[, stat := rep(stats, nrow(sum) / length(stats))]
    #setcolorder(sum, 'stat')
    #return(sum)
#}

#varnames = str_subset(names(cps.swap1.imp)[-2:-3],'.pct',negate = T)
#cps.swap1.imp.sum = sumByYear(cps.swap1.imp[, varnames, with = F])



#cps = projection.fit(cps, cps.fit.array['cps',,,,])
#cps.swap1 = projection.fit(cps.swap1, cps.fit.array['cps.swap1',,,,])
#cps.swap2 = projection.fit(cps.swap2, cps.fit.array['cps.swap2',,,,])

#View(cpsdata[year == 2018] %>% dplyr::select(year,matches('incwage')))


#plot(sort(cpsdata[year == 1980, inctot]))
#par(new = TRUE)
#plot(sort(cpsdata[year == 1980, inctot.theo.gb2.nocens]))
### Examine imputed data


#cdf.fit = function(y, fits, plot = T, xlim = NULL, ylim = NULL, scalefac = 1) {
    #y = sort(y)
    #wrkdata = data.table(data = y, pct = rank(y, ties.method = 'first') / length(y))
    #for (fit in names(fits)) {
        #wrkdata[, paste0('theo.', fit) := do.call(paste0('q', fits[[fit]]$distname),
            #c(list(wrkdata$pct), as.list(fits[[fit]]$estimate)))]
    #}
    #setcolorder(wrkdata, 'pct')
    #wrkdata[, 2:ncol(wrkdata)] = wrkdata[, 2:ncol(wrkdata)] * scalefac
    #plotdata = melt(wrkdata, id.vars = 'pct')
    #if (plot == T) {
        #plot = ggplot() +
            #geom_point(data = plotdata[variable == 'data'], aes(x = pct, y = value)) +
            #geom_line(data = plotdata[variable != 'data'], aes(x = pct, y = value, color = variable)) + coord_cartesian(xlim, ylim)
        #print(plot)
    #}
    #return(wrkdata)
#}




#cps.sim = data.table(NULL)
#for (year in as.character(1962:2018)) {

    ##year = '1980'
    ##print(cps.fit['cps', 'incwage', 'gb2', 'nocens', year])
    #z = cps.fit[['cps.swap1', 'incwage', 'gb2', 'nocens', year]]$estimate
    #z$n = 10000

    #incwage = do.call(rgb2, z)
    #yearnum = as.numeric(year)

    #dt = data.table(incwage, year = yearnum)
    #cps.sim = rbind(cps.sim, dt)

#}

#incshare = function(data, percentile) {
    #z = quantile(data, percentile, na.rm = T)
    #sum(data[data >= z], na.rm = T) / sum(data, na.rm = T)
#}
##incshare(acs$inctot, .90)
#incinq.calc = function(data, stat, ...) {
    #if (stat == 'incshare') {
        #return(incshare(data, ...))
    #} else {
        #return(ineq(data, type = stat, na.rm = T))
    #}
#}


#datasets = c('cps', 'cps.swap1', 'cps.swap2', 'cps.sim')
#vars = c('incwage')
#stats = c('Gini', 'incshare')
#years = 1940:2018
#incinq = array(data = NA,
               #dim = c(length(datasets), length(vars), length(stats), length(years)),
               #dimnames = list(datasets, vars, stats, years)) # [dataset,var,stat,year]

#for (data in datasets) {
    #for (var in vars) {
        #for (stat in stats) {
            ##data = datasets[1]
            ##var = vars[1]
            ##stat = stats[1]
            #z = get(data)
            #z = z[!is.na(get(var)) & get(var) > 0]
            #z = z[, .(val = incinq.calc(get(var), stat = stat, percentile = .99)), by = year]
            #z2 = merge(data.table(year = 1940:2018), z, all.x = T)
            #incinq[data, var, stat,] = z2$val
        #}
    #}
#}

#incinq.long = as.data.table(plyr::adply(incinq, .margins = c(1, 2, 3, 4), .id = c('dataset', 'var', 'stat', 'year')))

#plotdata = incinq.long[, info := paste(dataset, var, stat)]
#plotdata$year = as.integer(as.character(plotdata$year))

#plotdata = plotdata[year >= 1960 & stat == 'Gini',]

#ggplot(plotdata, aes(x = year, y = V1, group = info, color = info)) + geom_line(size = 2)

## Second pass -----------
#res.dir = array.dir(fit.cps)
#res.dir$result1 = as.character(lapply(fit.cps, class), recursive = F)

#par.dir$result1 = as.character(lapply(cps.fit,class), recursive = F)
#index.error1 = str_which(par.dir$result1,'error')
#system.time({
#cl = makeCluster(10)
#registerDoParallel(cl)
#iter.datasets = iter(par.in[index.error1])
#cps.fit2 = foreach(dataset = iter.datasets,
#fit = par.dir$fit[index.error1],
#distr = par.dir$distr[index.error1],
#.packages = c('data.table', 'fitdistrplus', 'GB2'),
#.errorhandling = 'pass') %dopar% {

#fit.yr.par(dataset, cens = fit, control = list(maxit = 1000, trace = 0),
#optim.method = 'Nelder-Mead', distr = distr,
#start = list(shape1 = 1, shape2 = 1, shape3 = 1, scale = 1),
#lower = c(0, 0, 0, 0))
#}
#stopCluster(cl)
#})

#cps.fit[index.error1] = cps.fit2
#par.dir$result2 = as.character(lapply(cps.fit, class), recursive = F)
#saveRDS(cps.fit,'wrkdata/cpsfitpar.rds')

## Examine fits
#cps.fit2 = readRDS('wrkdata/fitcps2.rds')
#cps.fit.array['cps', 'inctot', 'nocens', 'gb2', '2010']
#cps.fit.array['cps', 'incwage', 'nocens', 'gb2', '2018']
#z=list()
#z$fit1 = cps.fit.array[['cps', 'incwage', 'cens', 'gb2', '2018']]
#cdf.fit(par.in[[682]], z, ylim = c(0,10))



