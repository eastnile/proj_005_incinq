# set cran repos
local({
    r <- getOption('repos')
    r['CRAN'] <- 'http://cran.r-project.org'
    options(repos = r)
})

# pkgs
libstr = c('ipumsr','ineq','tidyverse','data.table','googlesheets')
installif(libstr)
lib(libstr)
rm(libstr)

## Paths
p = list()
p$proj$local = paste0(gdrivepath(), 'research/proj_005_incinq/')
p$data$local = paste0(p$proj$local, 'wrkdata/')
p$data$remote$root = 'F:/data/'
p$data$remote$acs100k = paste0(p$data$remote$root, 'ipums acs 100k/')
p$data$remote$cps = paste0(p$data$remote$root, 'ipums cps income data/')
p$res = paste0(p$proj$local, 'results/')
setwd(p$proj$local)

# opts
options(max.print = 1000)

# functions
tools = new.env()

tools$loadmain = function() {
    assign('acs', fread('ipums_acs_100k.csv'), envir = parent.frame())
    assign('acs.meta', readRDS('acsmeta.rds'), envir = parent.frame())
}

tools$gs.connect = function() {
    gs = list()
    gs$acs$meta = gs_key('1LGRdYF7D5ZEBDjI1u-sKr3ibL23noam-HiORbarQgvk')
    gs$cps$meta = gs_key('1Sg1bYFqSMg9K3CastjVluEiOxjtUFTnGbRJZ09uFGNw')
    assign('gs', gs, envir = parent.frame())
}


# Misc Functions
tools$array.mimic = function(data, oldarray) {
    newarray = array(data = data, dim = dim(oldarray), dimnames = dimnames(oldarray))
    names(dimnames(newarray)) = names(dimnames(oldarray))
    return(newarray)
}

tools$array.dir = function(array) {
    as.data.frame(expand.grid(dimnames(array), stringsAsFactors = F))
}

tools$array.new = function(data = NULL, dim = NULL, dimnames = NULL, dimnames2 = NULL) {
    if (missing(dim) & !missing(dimnames)) {
        # if missing dims, use length of dimnames
        dim = unlist(lapply(dimnames, length))
    }
    newarray = array(data = data, dim = dim, dimnames = dimnames)
    if (!missing(dimnames2)) {
        # name the dimensions themselves
        names(dimnames(newarray)) = dimnames2
    }
    return(newarray)
}

attach(tools)