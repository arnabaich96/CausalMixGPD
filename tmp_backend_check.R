pkgload::load_all('d:/Git-Repos/CausalMixGPD-Package/CausalMixGPD', quiet=TRUE, export_all=FALSE)
data('lalonde', package='MatchIt')
covars <- within(lalonde, {
  race <- factor(race, levels = c('white', 'black', 'hispan'))
  married <- factor(married, levels = c(0, 1), labels = c('no', 'yes'))
  nodegree <- factor(nodegree, levels = c(0, 1), labels = c('no', 'yes'))
})
X <- stats::model.matrix(~ age + educ + race + married + nodegree + re74 + re75, data = covars)[, -1, drop = FALSE]
y <- covars$re78 + 0.1
A <- as.integer(covars$treat)
b <- build_causal_bundle(y = y, X = X, A = A, backend = 'sb', kernel = 'gamma', components = 10, PS = 'logit')
cat('con backend:', b$outcome$con$spec$meta$backend, '\n')
cat('trt backend:', b$outcome$trt$spec$meta$backend, '\n')
