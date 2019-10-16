setproj(5)
cps = fread('wrkdata/ipums_cps.csv')
acs = fread('wrkdata/ipums_acs_100k.csv')

# Import topcode replacement values and handle them  ------------
swap1 = fread('wrkdata/swapvalues.txt')
swap2 = fread('wrkdata/topcodes.txt')

names(swap1) = str_replace(names(swap1), '_swap', '')
vars.swap1 = setdiff(intersect(names(swap1), names(cps)), c('year', 'serial', 'pernum'))
vars.swap2 = setdiff(intersect(names(swap2), names(cps)), c('year', 'serial', 'pernum'))
allvars = union(union(c('year', 'serial', 'pernum'), vars.swap1), vars.swap2)

cps = cps[, allvars, with = F] # Shrink dataset
cps.swap1 = copy(cps)
cps.swap2 = copy(cps)

for (var in vars.swap1) {
    z = swap1[!is.na(get(var)) & get(var) != 0]
    cps.swap1[z, on = .(year, serial, pernum), (vars.swap1) := get(paste0('i.', vars.swap1))]
}

for (var in vars.swap2) {
    z = swap2[!is.na(get(var)) & get(var) != 0]
    cps.swap2[z, on = .(year, serial, pernum), (vars.swap2) := get(paste0('i.', vars.swap2))]
}


# Summarize Data ---------------
sumByYear = function(DT, cols = names(DT)) {
    cols = names(DT)
    stats = c('mean', 'median', 'sd', 'max', 'min', 'atmax','pctinc.atmax', 'not.na', 'nobs')

    not.na = function(x) {
        return(sum(!is.na(x)))
    }

    atmax = function(x) {
        return(sum(x == max(x, na.rm = T), na.rm = T) / sum(!is.na(x)))
    }

    pctinc.atmax = function(x) {
        sum(x[x == max(x, na.rm = T)],na.rm = T)/sum(x,na.rm = T)
    }

    f.summary = function(x) {
        return(list(mean = mean(x, na.rm = T),
                                 median = median(x, na.rm = T),
                                 sd = sd(x, na.rm = T),
                                 max = max(x, na.rm = T),
                                 min = min(x, na.rm = T),
                                 atmax = atmax(x),
                                 pctinc.atmax = pctinc.atmax(x),
                                 not.na = not.na(x),
                                 nobs = length(x)))
    }

    sum = DT[, lapply(.SD, f.summary), by = year, .SDcols = setdiff(cols,'year')]
    sum = sum[, lapply(.SD, unlist)] # Unlist, otherwise each column is a list
    sum[, stat := rep(stats, nrow(sum) / length(stats))]
    setcolorder(sum, 'stat')
    return(sum)
}

cps.sum = sumByYear(cps)
cps.swap1.sum = sumByYear(cps.swap1)
cps.swap2.sum = sumByYear(cps.swap2)
acs.sum = sumByYear(acs)

## Examine effect of topcodes --------------
cps.sum$info = 'cps'
cps.swap1.sum$info = 'cps.swap1'
cps.swap2.sum$info = 'cps.swap2'
acs.sum$info = 'acs'

vars = Reduce(intersect, list(names(cps.sum), names(acs.sum), names(cps.swap1.sum), names(cps.swap2.sum)))

z = rbind(cps.sum[, vars, with = F],
          cps.swap1.sum[, vars, with = F],
          cps.swap2.sum[, vars, with = F],
          acs.sum[, vars, with = F])

ggplot(z[stat == 'max' & year >= 1960], aes(x = year, y = incwage, color = info)) +
    geom_line(size =2)

 #View(select(cps.sum, stat, year, starts_with('inc'))[stat %in% c('atmax', 'max', 'not.na'),])

# Compute Gini --------------------
incshare = function(data, percentile) {
    z = quantile(data, percentile, na.rm = T)
    sum(data[data >= z], na.rm = T) / sum(data, na.rm = T)
}
#incshare(acs$inctot, .90)
incinq.calc = function(data, stat, ...) {
    if (stat == 'incshare') {
        return(incshare(data,...))
    } else {
        return(ineq(data, type = stat, na.rm = T))
    }
}
#incinq.calc(acs$incwage, 'incshare', percentile=.9)

datasets = c('acs','cps','cps.swap1','cps.swap2')
vars = c('incwage')
stats = c('Gini', 'incshare')
years = 1960:2018
incinq = array(data = NA,
               dim = c(length(datasets), length(vars), length(stats), length(years)),
               dimnames = list(datasets, vars, stats, years)) # [dataset,var,stat,year]

for (data in datasets) {
    for (var in vars) {
        for (stat in stats) {
            #data = datasets[1]
            #var = vars[1]
            #stat = stats[1]
            z = get(data)[, .(val = incinq.calc(get(var), stat = stat, percentile = .99)), by = year]
            z2 = merge(data.table(year = 1940:2018), z, all.x = T)
            incinq[data,var,stat,] = z2$val
        }
    }
}

incinq.long = as.data.table(plyr::adply(incinq, .margins = c(1, 2, 3, 4), .id = c('dataset', 'var', 'stat', 'year')))
plotdata = incinq.long[, info := paste(dataset, var, stat)]
plotdata$year = as.integer(as.character(plotdata$year))
plotdata = plotdata[year>=1960 & dataset == 'cps' & stat == 'incshare',]

ggplot(plotdata, aes(x = year, y = V1, group = info, color = info)) + geom_line(size=2)

    #+
    #geom_vline(xintercept = 1996 - 1940) +
    #geom_vline(xintercept = 1996 - 1940) +
    #geom_vline(xintercept = 1996 - 1940) 



#z = data.table(val = incinq['acs', 'incwage', 'Gini',], year = as.integer(names(incinq['acs', 'incwage', 'Gini',])))
#+ geom_line(data = z[!is.na(val),])

# Old code --------------------------
## Compute Gini
#library(ineq)
#vars = c('incwage') #, 'inctot.nom','incwage.nom')
#gini = data.table(year = unique(cps.sum$year))
##gini = data.table(year=years,gini=NA)
#for (var in vars) {
##var = vars[1]
#years = sum.cps.wide[varname == var & not.na != 0, year]
#z = double(length(years))
#for (i in 1:length(years)) {
##print(i)
##print(years[i])
##print(head(acs[year == years[i]][[var]]))#[['var']]))
#z[i] = ineq(cps[year == years[i]][[var]], type = 'Gini')
#}
#z = data.table(years, z)
#names(z) = c('year', var)
#gini = merge(gini, z)
#}

##gini.long = melt(gini,id='year')
#ggplot(gini.long, aes(x = year,y = value,color=variable)) + geom_line()
# Compute Ineq
#ineq.ts = function(data, vars, years) {
##data = acs
##vars = c('incwage', 'inctot')
##years = unique(acs$year)
##lookup = data.table(year = years)

#stats = c('incshare', 'gini')
#z = matrix(nrow = years, ncol = length(stats))

#for (var in vars) {

#z1 = double(length = length(years))

#for (i in 1:length(years)) {
##print(i)
##print(years[i])
##print(head(acs[year == years[i]][[var]]))#[['var']]))
#z = data[,.(gini = ineq())]

#z[i] = ineq(data[year == years[i]][[var]], type = 'Gini')
#}
#z = data.table(years, z)
#names(z) = c('year', var)
#gini = merge(gini, z)
#}
#gini.long = melt(gini, id = 'year')
#return(gini.long)




#data = acs
#vars = c('incwage', 'inctot')
#years = unique(acs$year)
#wrkdata = data[year == years[20],vars[1],with=F]

#}

#gini=list()
#gini$cps = ineq.ts(cps, c('incwage', 'inctot'), years = 1962:2018)
#gini$cps[, plotvar := paste(variable, 'cps')]
#gini$acs = ineq.ts(acs, c('incwage', 'inctot'), years = unique(acs$year))
#gini$acs[, plotvar := paste(variable, 'acs')]
#ggplot(rbind(gini$cps,gini$acs)[year>=1940], aes(x = year, y = value, color = plotvar)) + geom_line()
