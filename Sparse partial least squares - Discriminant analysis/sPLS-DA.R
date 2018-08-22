###data. 
data<-CAZymesdata
###Y levels create factors
Y <- factor(c("Musa","Musa","Musa","Sol","Musa","Sol","Sol"))
###unsupervised clustering
pca.res = pca(data, ncomp = 4, logratio = 'ILR')
plot(pca.res)
###plot first component
plotIndiv(pca.res,  comp = c(1,2),ind.names = T, group = Y, col.per.group = color.mixo(1:2), legend = TRUE)

###Supervised analysis and selection of discriminant variables with sPLS-DA
###To choose the number of components for sPLS-DA
host.plsda <- plsda(X = data, Y, ncomp = 2)
host.perf.plsda <- perf(host.plsda, validation = 'Mfold', folds = 5,progressBar = FALSE, nrepeat = 10)
plot(host.perf.plsda, overlay = 'measure', sd=TRUE)

###First two components
plotIndiv(host.plsda , comp = c(1,2), group = Y, ind.names = FALSE,ellipse = TRUE, legend = TRUE, title = 'Host-specificity, PLSDA comp 1 - 2')

splsda.tune = tune.splsda(data, 
                          Y = Y, 
                          ncomp = 2, 
                          multilevel = NULL, 
                          logratio = 'none',
                          test.keepX = c(seq(5,20, 5)), 
                          validation = c('Mfold'), 
                          folds = 5, 
                          dist = 'max.dist', # prediction distance can be chosen according to tune.plsda results
                          nrepeat = 10)


splsda.tune$choice.keepX

kable(head(splsda.tune$error.rate))

### Input parameters for sPLS-DA

choice.keepX = c(20, 20) # optimal keepX values according to the tuning criterion above.

choice.ncomp = length(choice.keepX) # the number of components

# The sPLS-DA
res.splsda = splsda(X = data, Y = Y, ncomp = choice.ncomp,keepX = choice.keepX)


###Plots for component 1 & 2
plotIndiv(res.splsda, ind.names = F, col.per.group = color.mixo(1:2), comp = c(1,2),  pch = 16, ellipse = TRUE, legend = TRUE)

###Selected variables
selectVar(res.splsda, comp = 1)$value
cim(res.splsda, comp = 1, row.sideColors = color.mixo(Y))
plotLoadings(res.splsda, comp = 1, method = 'mean', contrib = 'max')

###Validation

splsda.perf = perf(res.splsda, validation = 'Mfold', folds = 5, progressBar = FALSE, nrepeat = 10)
splsda.perf$error.rate
plot(splsda.perf$features$stable[[1]], type = 'h', 
     xlab = 'variables selected across CV folds', ylab = 'Stability frequency', title='Feature stability for comp = 1')

head(selectVar(res.splsda, comp = 1)$value)


### Save the name of selected var + stability from perf:
select.name <- selectVar(res.splsda, comp = 1)$name
stability <-splsda.perf$features$stable[[1]][select.name]
# Head of the stability of the selected var:
head(cbind(selectVar(res.splsda, comp = 1)$value, stability))
