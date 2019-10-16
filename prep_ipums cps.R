# Copy data into local work directory
setproj(5)
gs.connect()
# Income data:
## Prep meta -----------
cps.meta <- read_ipums_ddi(paste0(p$data$remote$cps,'cps_00009.xml'))
cps.meta$var_info$var_name = tolower(cps.meta$var_info$var_name)
# gsheets: https://docs.google.com/spreadsheets/d/1Sg1bYFqSMg9K3CastjVluEiOxjtUFTnGbRJZ09uFGNw/edit#gid=0
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
cps[, id := .I]

# Import topcode replacement values and handle them  ------------
swap1 = fread('wrkdata/swapvalues.txt') # row swapping
swap2 = fread('wrkdata/topcodes.txt') # cell means

names(swap1) = str_replace(names(swap1), '_swap', '')
vars.orig = str_subset(names(cps),'inc')
vars.swap1 = setdiff(intersect(names(swap1), names(cps)), c('year', 'serial', 'pernum'))
vars.swap2 = setdiff(intersect(names(swap2), names(cps)), c('year', 'serial', 'pernum'))
allvars = Reduce(union, list(c('year', 'serial', 'pernum'), vars.orig, vars.swap1, vars.swap2))

cps.income = cps[, allvars, with = F] # Shrink dataset
cps.swap1 = copy(cps.income)
cps.swap2 = copy(cps.income)

for (var in vars.swap1) {
    z = swap1[!is.na(get(var)) & get(var) != 0]
    cps.swap1[z, on = .(year, serial, pernum), (vars.swap1) := get(paste0('i.', vars.swap1))]
}

for (var in vars.swap2) {
    z = swap2[!is.na(get(var)) & get(var) != 0]
    cps.swap2[z, on = .(year, serial, pernum), (vars.swap2) := get(paste0('i.', vars.swap2))]
}

# Relabel values
# Read from gsheets info to remap variables
recodelist = data.table(gs_read(gs$cps$meta, ws = 'recode', col_types = 'cccc'))
for (dataset in c('cps', 'cps.income', 'cps.swap1', 'cps.swap2')) {
    for (i in 1:nrow(recodelist)) {
        #i=1
        varname = recodelist$varname.raw[i]
        z.in = unlist(strsplit(recodelist$recode.in[i], split = ','))
        z.out = unlist(strsplit(recodelist$recode.out[i], split = ','))
        z.out[z.out == 'na'] = NA

        print(paste('mapping values for',varname,'from:',paste(z.in,collapse = ', '),'to:',paste0(z.out,collapse =', ')))

        #print(head(get(dataset)[[varname]]))
        try( get(dataset)[,(varname) := as.numeric(plyr::mapvalues(get(varname), from = z.in, to = z.out))] ) # may return error if variable is missing, but moves on
    }
}

#myDT = data.table(a = 1:5, b = 1:5)

#dtname = 'myDT'
#varname = 'a'
#myDT[, a]
#get(dtname)[, (varname) := plyr::mapvalues(get(varname), from = 1:5, to = 6:10)]

#myDT
# z = cps[, .(max = max(incwage, na.rm = T)), by = year]


# Read from gsheets to rescale variables
rescalelist = data.table(gs_read(gs$cps$meta, ws = 'rescale', col_types = 'cc'))
for (dataset in c('cps','cps.income','cps.swap1','cps.swap2')) {
    for (var in rescalelist$varname.raw) { # set negatives to zero and correct for inflation
        if ('noneg' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) { #laborious but works
            try(get(dataset)[get(var) < 0, (var) := NA]) # may return error if variable is missing, but moves on
        }
        #if ('inflation' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) {
            #cps[, paste0(var, '.nom') := get(var)]
            #cps[, paste0(var) := get(var) * cpi99]
        #}
    }
}

#fwrite(cps, paste0(p$data$local, 'ipums_cps.csv'))
fwrite(cps.income, paste0(p$data$local, 'ipums_cps_incvar_orig.csv'))
fwrite(cps.swap1, paste0(p$data$local, 'ipums_cps_incvar_swap1.csv'))
fwrite(cps.swap2, paste0(p$data$local, 'ipums_cps_incvar_swap2.csv'))

# Prep small
cps.small = cps[,.(id,year,serial,pernum,inctot,incwage,incbus,incfarm,incunern,cpi99)]
fwrite(cps.small, paste0(p$data$local, 'ipums_cps_small.csv'))

# Other CPS data
library(labelled)
cps.meta <- read_ipums_ddi(paste0(p$data$remote$cps, 'cps_00011.xml'))
cps <- as.data.table(read_ipums_micro(cps.meta))
names(cps) = tolower(names(cps))

byvars = c('sex', 'race', 'bpl', 'occ', 'occ2010', 'occ1990', 'occ1950', 'ind', 'ind1990', 'ind1950')

for (var in byvars) {
    print(var)
    try({cps[[var]] = haven::as_factor(cps[[var]], levels = 'label')})
}

fwrite(cps, 'wrkdata/cps_othervars.csv')
