# Copy data into local work directory
setproj(5)
## Prep meta -----------
cps.meta <- read_ipums_ddi(paste0(p$data$remote$cps,'cps_00006.xml'))
cps.meta$var_info$var_name = tolower(cps.meta$var_info$var_name)
#saveRDS(cps.meta, paste0(p$data$local, 'cpsmeta.rds'))

## write metadata to gsheets --------
#to.gs = cps.meta$var_info
#to.gs$val_labels = as.character(lapply(cps.meta$var_info$val_labels,
                                #function(x) paste(x$val, ': ', x$lbl, collapse = '; ', sep = '')))
#to.gs$val_labels[to.gs$val_labels == ': '] = NA #convert val lbls into character
#gs_edit_cells(ss = gs$cps$meta, ws = 'meta', input = to.gs, anchor = "A1", trim = T, verbose = F)

## read data -----------
cps <- as.data.table(read_ipums_micro(cps.meta))
names(cps) = tolower(names(cps))

# Relabel values
# Read from gsheets info to remap variables
recodelist = data.table(gs_read(gs$cps$meta, ws = 'recode', col_types = 'cccc'))
for (i in 1:nrow(recodelist)) {
    #i=1
    varname = recodelist$varname.raw[i]
    z.in = unlist(strsplit(recodelist$recode.in[i], split = ','))
    z.out = unlist(strsplit(recodelist$recode.out[i], split = ','))
    print(paste('mapping values for',varname,'from:',paste(z.in,collapse = ', '),'to:',paste0(z.out,collapse =', ')))
    z.out[z.out == 'na'] = NA
    cps[[varname]] = as.numeric(plyr::mapvalues(cps[[varname]], from = z.in, to = z.out))
}

# z = cps[, .(max = max(incwage, na.rm = T)), by = year]



# Read from gsheets to rescale variables
rescalelist = data.table(gs_read(gs$cps$meta, ws = 'rescale', col_types = 'cc'))
for (var in rescalelist$varname.raw) { # set negatives to zero and correct for inflation
    if ('noneg' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) { #laborious but works
        cps[get(var) < 0, paste0(var) := NA]
    }
    #if ('inflation' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) {
        #cps[, paste0(var, '.nom') := get(var)]
        #cps[, paste0(var) := get(var) * cpi99]
    #}
}

fwrite(cps, paste0(p$data$local,'ipums_cps.csv'))




#z = acs[, lapply(.SD, mean, na.rm = TRUE), by = year]
#q = complete.cases(z[, year, inctot])
#q = z[complete.cases(year, inctot)]
#plot(z$year,z$inctot)

# Compute Ineq----------------
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
