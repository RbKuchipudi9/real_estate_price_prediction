library(magrittr)
library(dplyr)
library(arules)
library(arulesViz)
data <- read.csv("armdata.csv")
head(data)

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

Bins <- data %>%
  mutate(sqftBin = cut(as.numeric(sqft), breaks = c(0, 500, 1000, 1500, 2000, 3000, Inf), labels = c("very small", "small", "medium", "large", "very large", "huge")))
Bins <- Bins %>%
  mutate(priceBin=cut(as.numeric(price), breaks=c( 0,700000,1500000,3500000,Inf), labels=c("cheap", "average", "high", "expensive")))
Bins <- Bins %>%
  mutate(bathBin=cut(as.numeric(bathrooms), breaks=c( 0,1,2,3,4,5, Inf), labels=c("1 Bath", "2 Bath", "3 Bath", "4 Bath","5 Bath","5+ Bath")))
Bins <- Bins %>%
  mutate(bedBin=cut(as.numeric(bedrooms), breaks=c( 0,1,2,3,4,5,Inf), labels=c("1 BHK","2 BHK","3 BHK","4 BHK","5 BHK","5+BHK")))

head(Bins)

# Create bins of prices
Bins$floors[Bins$floors==1] <- "Level 1"
Bins$floors[Bins$floors==2] <- "Level 2"
Bins$floors[Bins$floors==3] <- "Level 3"


Arules <- Bins %>% select(priceBin,bedBin,bathBin,sqftBin,floors)

head(Arules)


write.csv(Arules,"ARM_dataset.csv", row.names = FALSE)
ARM = read.transactions("ARM_dataset.csv", rm.duplicates = TRUE, format = "basket", sep = ",")
rules = arules::apriori(ARM, parameter = list(support=.10, 
                                              confidence=0.9, minlen=2))
inspect(rules)
transactions <- as(lhs(rules), "transactions")
itemFrequencyPlot(transactions,topN=20, type="absolute", main="Item Frequency Plot")

plot(rules, jitter = 0)

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