# install.packages('dineq')
# install.packages('IC2')
setproj(5)
library(dineq)
library(IC2)

# Get data ----------
wrkdata = fread('wrkdata/cps.swap1_imp_cens_gb2.csv')
othervars = fread('wrkdata/cps_othervars.csv',stringsAsFactors = T)
names.othervars = setdiff(names(othervars), intersect(names(wrkdata), names(othervars)))
cps = merge(wrkdata, othervars, by = c('year', 'serial', 'pernum'), all.x = T)
colnames(cps)[colnames(cps) == "hhincome.y"] <- "hhincome"
cps$hhincome.x = NULL
rm(wrkdata,othervars)
#inequality_decomp = dineq_rb(inctot ~ age + sex + race + educ + ind1990, data = cps[year==1982])
#z1=mld_decomp(cps[year==1982,inctot], cps[year==1982,educ])
#z2=gini_decomp(cps[year == 1982, inctot], cps[year == 1982, sex])
#z = decompGEI(cps[year == 1982, inctot], cps[year == 1982, sex], alpha = .99)

# Decomp non regression --------------
incvars = c('inctot','incwage')
byvars = c('sex', 'race', 'bpl', 'occ', 'occ2010', 'occ1990', 'occ1950', 'ind', 'ind1990', 'ind1950')
f.decomp = function(x, y) {
    # browser()
    #x = 1:10
    #y = as.factor(c(rep(1, 5), rep(2, 5)))
    x[x <= 0] = 1 # replace zero incomes with 1 dollar, otherwise can't compute entropy
    y = as.factor(y) # Make sure y is a factor, otherwise can get error
    y = droplevels(y) # drop unused factor levels as these can lead to an error
    tryCatch({
        gei = decompGEI(x, y, alpha = .99)
        #print('gei done')
        names(gei$ineq$index) = 'gei_total'
        names(gei$decomp) = c('gei_within', 'gei_between', 'gei_betweenELMO')


        return(c(mld_decomp(x, y)$mld_decomp, gini_decomp(x, y)$gini_decomp, gei$ineq$index, gei$decomp))
        },
        error = function(e) { # On error, print error, and return -1
            print(e)
            as.list(rep(-1, length = 11))
        }
    )
}


#z = cps[year == 1968,.(incwage,occ1950)]
#mld_decomp(z$incwage,z$occ1950)

cps.decomp = NULL
for (incvar in incvars) {
    for (byvar in byvars) {
        #i = 2
        #byvar = byvars[i]
        #incvar = incvars[i]
        # --
        print(paste(incvar,byvar))
        z = cps[complete.cases(get(incvar), get(byvar)), f.decomp(get(incvar), get(byvar)), by = year]
        z[, incvar := incvar]
        z[, byvar := byvar]
        cps.decomp = rbind(cps.decomp,z, use.names=F)
        }
    }

setcolorder(cps.decomp, c('year', 'incvar', 'byvar'))
cps.decomp[cps.decomp == -1] = NA
fwrite(cps.decomp, file = 'wrkdata/cps_decomp.csv')

# Explore -----------
cps.decomp = fread(file = 'wrkdata/cps_decomp.csv')
var = 'incwage'
plotdata = cps.decomp[incvar == var]
plotdatalong = melt(plotdata, id = c('year','incvar','byvar'))
#plotdatalong = plotdatalong[incvar == 'incwage']
#plotdatalong = plotdatalong[incvar == 'inctot' & variable == 'mld_between']

plotdatalong = plotdatalong[variable %in% c('mld_between','gini_between','gei_between','gei_betweenELMO')]

ggplot(plotdatalong) + aes(x = year, y = value, shape = byvar, color = variable, group = interaction(byvar, variable)) + geom_line() + geom_point() +
   scale_shape_manual(values = seq(1:9))

ggplot(plotdatalong) +
    aes(x = year, y = value, shape = byvar, color = byvar, group = byvar) +
    geom_line() + geom_point() +
    scale_shape_manual(values = seq(1:9))

dt = data.table(x = 1:10, y = 1:10, a = c(rep(1, 5), rep(2, 5)))
ggplot(dt) +
    aes(x = x, y = y, shape = a) + geom_line()


# Regression based:

#library(dineq)
#library(IC2)
#data(mex_inc_2008)
#inequality_change <- dineq_change_rb(formula1 = income ~ hh_structure + education + domicile_size + age_cat,
#weights1 = "factor", data1 = mex_inc_2008, formula2 = income ~ hh_structure + education +
#domicile_size + age_cat, weights2 = "factor", data2 = mex_inc_2016)
##selection of output: change in variance of log income decomposed in variables split into price
##and quantity effect and residual.
#inequality_change["decomposition_change_absolute"]
##selection of output: relatieve change in variance of log income decomposed in variables split
##into price and quantity effect and residual. Because of negative change in variance of log
##income, the negative contributuon of education (quantity) becomes a positive number.
#inequality_change["decomposition_change_relative"]

#inequality_decomp = dineq_rb(income ~ hh_structure + education + domicile_size + age_cat,
#weights = "factor", data = mex_inc_2008)

#z = inequality_decomp$regression_results

#list.mimic = function(mylist, data, fill = T)
#if (fill) {
#z = rep(data, length(unlist(mylist)))
#relist(z, skeleton = mylist)
#} else {
#relist(data,skeleton = mylist)
#}