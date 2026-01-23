models <- data.frame(
  model=factor(c("BIDE", "Geometric", "Logistic", "Lotka-Volterra", "Age", "Metapopulation")),
  density_dependence=factor(c("No", "No", "Yes", "Yes", "No", "No")),
  structure=factor(c("No", "No", "No", "Yes", "Yes", "Yes")))

tree <- rpart(model~structure+density_dependence, models, control=list(minsplit=1, minbucket=1))

plot(tree)
text(tree, use.n=TRUE)
