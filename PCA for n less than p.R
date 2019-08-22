
library(ISLR)
data("NCI60")
?NCI60

X = NCI60$data
y = NCI60$labs

View(X)
dim(X)

?NCI60

class(NCI60)
class(NCI60$data)


NCI.pc = prcomp(X)
round(NCI.pc$sdev^2/sum(apply(X,2,var))*100,4)


c = cov(X)
e = eigen(c)
round(e$values,4)
dim(c)

require(Matrix)
r = rankMatrix(c)
r[1]


#REFERENCE
#https://stats.stackexchange.com/questions/46475/is-pca-appropriate-when-np