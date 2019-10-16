# 20 industries, 2 sexes, 6 education levels

# Alternative 1: Keep it long, use a list, and use a dictionary

industries = 1:20
sexes = c('m', 'f')
education = 1:6
x = rnorm(240)
dir.reg = expand.grid(industries, sexes, education)
reg = list()

# Alternative 2: Use an array

name.industries = letters[1:20]
name.sex = c('m', 'f')
name.educ = as.character(1:6)

myarray = array(data = NA,
                dim = c(20, 2, 6),
                dimnames = list(name.industries, name.sex, name.educ))

myarray[1,1,1] = data.frame(x=1,y=2)
# Array of lists:
x = vector(mode = 'list', length = 240)
myarray = array(data = x,
                dim = c(20, 2, 6),
                dimnames = list(name.industries, name.sex, name.educ))

myarray[[1, 1, 1]] = data.frame(x = 1, y = 2)