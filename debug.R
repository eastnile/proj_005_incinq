fitcps = function(cps.array, ncore = 1, opts) {
    # most of the requisite parameters are in the arraynames
    fit.yr.par = function(x, cens, ...) {
        print(start)
        print(distr)
        # Function for yearly imputation
        if (cens == 'nocens') {
            output = fitdist(x, ...)
        } else {
            censpt = max(x);
            x.cens = data.table(left = x, right = x);
            x.cens[right == censpt, right := NA]
            output = fitdistcens(x.cens, ...)
        }
        return(output)
    }
    #time.start = Sys.time()
    #cl = makeCluster(ncore)
    #registerDoParallel(cl)
    #iter.datasets = iter(cps.array)
    #iter.control = iter(opts['control',])
    #iter.method = iter(opts['method',])
    #iter.start = iter(opts['start',])
    #iter.lower = iter(opts['lower',])


    cps.fit = foreach(dataset = iter(cps.array),
                      control = iter(opts['control',]),
                      method = iter(opts['method',]),
                      start = iter(opts['start',]),
                      lower = iter(opts['lower',]),
                      fit = array.dir(cps.array)$fits,
                      distr = array.dir(cps.array)$distrs,
                      .packages = c('data.table', 'fitdistrplus', 'GB2','actuar'),
                      .errorhandling = 'pass') %do% {

                      fit.yr.par(dataset, cens = fit, control = control, optim.method = method, start = start, lower = lower, distr = distr)

                      }
    #stopCluster(cl)
    #time.end = Sys.time()
    #time.wait = time.end - time.start
    #print(paste(time.start,time.end,time.wait))
    return(array.mimic(cps.fit, cps.array))
}

fit.cps = fitcps(wrkdata, 4, opts)


#fitcps = function(cps.array, ncore = 1, opts) {
    ## most of the requisite parameters are in the arraynames
    #fit.yr.par = function(x, cens, control = control, method = method, start = start, lower = lower, distr = distr) {
        #print(start)
        #print(distr)
        ## Function for yearly imputation
        #if (cens == 'nocens') {
            #output = fitdist(x, control = control, optim.method = method, start = start, lower = lower, distr = distr)
        #} else {
            #censpt = max(x);
            #x.cens = data.table(left = x, right = x);
            #x.cens[right == censpt, right := NA]
            #output = fitdistcens(x.cens, control = control, optim.method = method, start = start, lower = lower, distr = distr)
        #}
        #return(output)
    #}
    ##time.start = Sys.time()
    ##cl = makeCluster(ncore)
    ##registerDoParallel(cl)
    ##iter.datasets = iter(cps.array)
    ##iter.control = iter(opts['control',])
    ##iter.method = iter(opts['method',])
    ##iter.start = iter(opts['start',])
    ##iter.lower = iter(opts['lower',])


    #cps.fit = foreach(dataset = iter(cps.array),
                      #control = iter(opts['control',]),
                      #method = iter(opts['method',]),
                      #start = iter(opts['start',]),
                      #lower = iter(opts['lower',]),
                      #fit = array.dir(cps.array)$fits,
                      #distr = array.dir(cps.array)$distrs,
                      #.packages = c('data.table', 'fitdistrplus', 'GB2', 'actuar'),
                      #.errorhandling = 'pass') %do% {

                          #fit.yr.par(dataset, cens = fit, control = control, method = method, start = start, lower = lower, distr = distr)

                      #}
    ##stopCluster(cl)
    ##time.end = Sys.time()
    ##time.wait = time.end - time.start
    ##print(paste(time.start,time.end,time.wait))
    #return(array.mimic(cps.fit, cps.array))
#}

#fit.cps = fitcps(wrkdata, 4, opts)