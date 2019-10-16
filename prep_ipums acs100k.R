# Copy data into local work directory
setproj(5)
## Prep meta -----------
acs.meta <- read_ipums_ddi(paste0(p$data$remote$acs100k,'ipums_acs_100k.xml'))
acs.meta$var_info$var_name = tolower(acs.meta$var_info$var_name)
saveRDS(acs.meta, paste0(p$data$local, 'acsmeta.rds'))

## write metadata to gsheets --------
to.gs = acs.meta$var_info
to.gs$val_labels = as.character(lapply(acs.meta$var_info$val_labels,
                                function(x) paste(x$val, ': ', x$lbl, collapse = '; ', sep = '')))
to.gs$val_labels[to.gs$val_labels == ': '] = NA #convert val lbls into character
gs_edit_cells(ss = gs$acs$meta, ws = 'meta', input = to.gs, anchor = "A1", trim = T, verbose = F)

## read data -----------
acs <- as.data.table(read_ipums_micro(acs.meta))
names(acs) = tolower(names(acs))

# Data contains two samples from 1970, drop one of them
acs = acs[!(year==1970 & datanum ==4)]

# Relabel values
# Read from gsheets info to remap variables
recodelist = data.table(gs_read(gs$acs$meta, ws = 'recode', col_types = 'ccc'))
for (i in 1:nrow(recodelist)) {
    #i=1
    varname = recodelist$varname.raw[i]
    z.in = unlist(strsplit(recodelist$recode.in[i], split = ','))
    z.out = unlist(strsplit(recodelist$recode.out[i], split = ','))
    print(paste('mapping values for',varname,'from:',paste(z.in,collapse = ', '),'to:',paste0(z.out,collapse =', ')))
    z.out[z.out == 'na'] = NA
    acs[[varname]] = as.numeric(plyr::mapvalues(acs[[varname]], from = z.in, to = z.out))
}

# Read from gsheets to rescale variables
rescalelist = data.table(gs_read(gs$acs$meta, ws = 'rescale', col_types = 'cc'))
for (var in rescalelist$varname.raw) { # set negatives to zero and correct for inflation
    if ('nozeros' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) { #laborious but works
        acs[get(var) < 0, paste0(var) := NA]
    }
    if ('inflation' %in% unlist(strsplit(rescalelist[varname.raw == var, method], split = ','))) {
        acs[, paste0(var, '.nom') := get(var)]
        acs[, paste0(var) := get(var) * cpi99]
    }
}

fwrite(acs, paste0(p$data$local,'ipums_acs_100k.csv'))

#z = acs[, lapply(.SD, mean, na.rm = TRUE), by = year]
#q = complete.cases(z[, year, inctot])
#q = z[complete.cases(year, inctot)]
#plot(z$year,z$inctot)