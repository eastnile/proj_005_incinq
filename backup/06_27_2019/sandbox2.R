setproj(5)
swap1 = fread('wrkdata/swapvalues.txt')
swap2 = fread('wrkdata/topcodes.txt')
cps = fread('wrkdata/ipums_cps.csv')


names(swap1) = str_replace(names(swap1), '_swap', '')
vars.swap1 = setdiff(intersect(names(swap1), names(cps)), c('year', 'serial', 'pernum'))
vars.swap2 = setdiff(intersect(names(swap2), names(cps)), c('year', 'serial', 'pernum'))
allvars = union(union(c('year', 'serial', 'pernum'), vars.swap1), vars.swap2)

cps = cps[, allvars, with = F] # Shrink dataset
cps.swap1 = copy(cps)
cps.swap2 = copy(cps)

for (var in vars.swap1) {
    cps.swap1[swap1, on = .(year,serial,pernum), (vars.swap1) := get(paste0('i.',vars.swap1))]
}

for (var in vars.swap2) {
    cps.swap2[swap2, on = .(year, serial, pernum), (vars.swap2) := get(paste0('i.', vars.swap2))]
}

#z = data.table(year = cps$year, serial = cps$serial, pernum = cps$pernum, orig = cps$incwage, swap1 = cps.swap1$incwage, swap2 = cps.swap2$incwage)
#View(z[swap1!=orig]) # Check to see if it worked


cps.sum = sumByYear(cps)
cps.swap1.sum = sumByYear(cps.swap1)
cps.swap2.sum = sumByYear(cps.swap2)

cps.sum$info = 'orig'
cps.swap1.sum$info = 'swap1'
cps.swap2.sum$info = 'swap2'
# varnames = c('year','info','stat',names.orig)
z = rbind(cps.sum, cps.swap1.sum, cps.swap2.sum)

ggplot(z[stat == 'pctinc.atmax'], aes(x = year, y = incwage, color=info)) + geom_line()

#View(data.table(cps.sum[stat == 'max', year, incwage], cps.swap.sum[stat == 'max', year,incwage]))