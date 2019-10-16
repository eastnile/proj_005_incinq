# Generate income distribution
n = 1000
dist = data.table(id = 1:n, inc = rlnorm(n, meanlog = 10, sdlog = 1))
dist[, pct := percent_rank(inc)]

# Set up tax brackets
taxbrac = data.table(bracid = c(0:10))
taxbrac[, pct := bracid * 10]
taxbrac$thresh = quantile(dist$inc, taxbrac$pct / 100)
taxbrac$taxrate = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .5)

# Lookup each individual's bracket
dist$bracid = as.numeric(cut(dist$inc, breaks = taxbrac$thresh, include.lowest = T))
dist = merge(dist,taxbrac[,.(bracid,taxrate)],by='bracid')

# apply taxes/redistribution
dist[, inctax := inc * taxrate]
dist[, inc.post := inc - inctax]
# z.n = dist[, .N, by = taxrate][taxrate==0.0,2][[1]]
taxrev = sum(dist$inctax) 
dist[,inc.post := inc + taxrev/n]

# Compare results
plot(Lc(dist$inc))
plot(Lc(dist$inc.post))
ineq(dist$inc, type = 'Gini')
ineq(dist$inc.post, type = 'Gini')