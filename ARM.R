#detach("package:arulesViz", unload=TRUE)
#detach("package:arules", unload=TRUE)
library(arules)
library(arulesViz)

data<-read.csv("arm.csv")

support_seq <- seq(0.1, 0.8, by = 0.05)
confidence_seq <- seq(0.2, 0.9, by = 0.1)
results <- data.frame(support = numeric(), confidence = numeric(), lift = numeric())
for (i in 1:length(support_seq)) {
  for (j in 1:length(confidence_seq)) {
    # Fit the model with the current support and confidence values
    model <- apriori(data, parameter = list(support = support_seq[i], confidence = confidence_seq[j], target = "rules"))
    
    # Evaluate the model with the lift metric
    lift <- quality(model)$lift[1]
    
    results <- rbind(results, data.frame(support = support_seq[i], confidence = confidence_seq[j], lift = lift))
  }
}
optimal_support <- results$support[which.max(results$lift)]
optimal_confidence <- results$confidence[which.max(results$lift)]

# Print the optimal threshold values
cat(paste("Optimal support:", optimal_support, "\n"))
cat(paste("Optimal confidence:", optimal_confidence, "\n"))

rules <- apriori(data, parameter = list(support = optimal_support , confidence = optimal_confidence, minlen = 2))
inspect(rules)
plot(rules, jitter = 0)


transactions <- as(lhs(rules), "transactions")


itemFrequencyPlot(transactions,topN=20, type="absolute", main="Item Frequency Plot")

# Get the top 15 rules for support
top_support <- head(sort(rules, by = "support", decreasing = TRUE), 15)
# Inspecting the top 15 rules for support
inspect(top_support)

# Get the top 15 rules for confidence
top_confidence <- head(sort(rules, by = "confidence", decreasing = TRUE), 15)
# Inspecting the top 15 rules for confidence
inspect(top_confidence)

# Get the top 15 rules for lift
top_lift <- head(sort(rules, by = "lift", decreasing = TRUE), 15)
# Inspecting the top 15 rules for lift
inspect(top_lift)

plot(top_support, method="graph", engine="htmlwidget")
plot(top_confidence, method="graph", engine="htmlwidget")
plot(top_lift, method="graph", engine="htmlwidget")
