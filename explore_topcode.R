# Study topcode pattern

setproj(5)
cps = fread('wrkdata/ipums_cps_incvar_orig.csv')
cps.swap1 = fread('wrkdata/ipums_cps_incvar_swap1.csv')
cps.swap2 = fread('wrkdata/ipums_cps_incvar_swap2.csv')

sumByYear = function(DT, cols = names(DT)) {
    cols = names(DT)
    stats = c('mean', 'median', 'sd', 'max', 'min', 'atmax', 'pctinc.atmax', 'not.na', 'nobs')

    not.na = function(x) {
        return(sum(!is.na(x)))
    }

    atmax = function(x) {
        return(sum(x == max(x, na.rm = T), na.rm = T) / sum(!is.na(x)))
    }

    pctinc.atmax = function(x) {
        sum(x[x == max(x, na.rm = T)], na.rm = T) / sum(x, na.rm = T)
    }

    f.summary = function(x) {
        return(list(mean = mean(x, na.rm = T),
                                 median = median(x, na.rm = T),
                                 sd = sd(x, na.rm = T),
                                 max = ifelse(is.infinite(max(x, na.rm = T)), NA, max(x, na.rm = T)),
                                 max = ifelse(is.infinite(min(x, na.rm = T)), NA, min(x, na.rm = T)),
                                 atmax = atmax(x),
                                 pctinc.atmax = pctinc.atmax(x),
                                 not.na = not.na(x),
                                 nobs = length(x)))
    }

    sum = DT[, lapply(.SD, f.summary), by = year, .SDcols = setdiff(cols, 'year')]
    sum = sum[, lapply(.SD, unlist)] # Unlist, otherwise each column is a list
    sum[, stat := rep(stats, nrow(sum) / length(stats))]
    setcolorder(sum, 'stat')
    return(sum)
}

# Report largest income components -------------
z = cps[, lapply(.SD, mean, na.rm = T)]
z = select(z, inctot:oincwage)
z = t(z / z$inctot)
inc.mean = data.table(rownames(z), z);
names(inc.mean) = c('inc', 'pct')
inc.mean = inc.mean[order(pct, decreasing = T)]

# Summarize data ---------
cps.sum = list()
for (dataset in c('cps', 'cps.swap1', 'cps.swap2')) {
    cps.sum[[dataset]] = sumByYear(get(dataset))
    cps.sum[[dataset]][, info := dataset]
}

# Analyze ---------
vars.common = Reduce(intersect, list(names(cps.sum$cps), names(cps.sum$cps.swap1), names(cps.sum$cps.swap2)))

#vars.id = c('year','serial','pernum')
#vars.common = setdiff(vars.common, c('serial', 'pernum', 'hhincome', 'taxinc', 'adjginc', 'incretir', 'incsurv', 'incdisab', 'incdivid'))

#vars.common = setdiff(vars.common, c('serial', 'pernum'))

vars.common = c('stat', 'year', 'info', inc.mean$inc[3:3])


cps.sum.long = rbind(cps.sum$cps[, vars.common, with = F],
                     cps.sum$cps.swap1[, vars.common, with = F],
                     cps.sum$cps.swap2[, vars.common, with = F])

cps.sum.longer = melt(id.vars = c('stat', 'year', 'info'), data = cps.sum.long)

ggplot(cps.sum.longer[stat == 'atmax' & year >= 1960]) +
    geom_line(aes(x = year, y = value, color = info), size = 1) +
    geom_point(aes(x = year, y = value, shape = info), size = 2) +
    scale_shape_manual(values = c(1:33)) +
    ggtitle('mean income components')

# Compute ineq for each ts --------------------
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
#incinq.calc(acs$incwage, 'incshare', percentile=.9)

datasets = c('cps', 'cps.swap1', 'cps.swap2')
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
            z2 = merge(data.table(year = 1960:2018), z, all.x = T)
            incinq[data, var, stat,] = z2$val
        }
    }
}

incinq.long = as.data.table(plyr::adply(incinq, .margins = c(1, 2, 3, 4), .id = c('dataset', 'var', 'stat', 'year')))
plotdata = incinq.long[, info := paste(dataset, var, stat)]
plotdata$year = as.integer(as.character(plotdata$year))


ggplot(plotdata[stat == 'incshare']) +
    geom_line(aes(x = year, y = V1, group = info, color = dataset), size = 1)
