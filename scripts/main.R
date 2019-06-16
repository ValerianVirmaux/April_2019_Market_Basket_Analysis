library(arules)
library(arulesViz)

transactions <- read.transactions("datasets/ElectronidexTransactions2017.csv", 
                                  format = "basket", 
                                  sep = ",",
                                  rm.duplicates = TRUE)

####Creating categories####
#Categories from Eletronidex
transactionsCategories <- addCategory(transactions)
#Categories similar to Blackwell's categories
transactionsBlackwellCat <- addCategory(transactions, "blackwells")

#Calculating rules for each kind of transactions
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.1, minlen = 2))
rulesCateg <- apriori(transactionsCategories, parameter = list(supp = 0.01, conf = 0.6, minlen = 2))
rulesBW <- apriori(transactionsBlackwellCat, parameter = list(supp = 0.01, conf = 0.1, minlen = 2))

#Subsetting 
rulesBWAccessories <- subset(rulesBW, rhs %in% c("Accessories"))
rulesBWPrinter <- subset(rulesBW, rhs %in% c("Printers"))
rulesBWPrinterInk <- subset(rulesBW, rhs %in% c("Printer Ink"))
rulesBWMonitors <- subset(rulesBW, rhs %in% c("Monitors"))
#Removing Accessories from the rules
rulesBWNotAccessories <- subset(rulesBW, !(rhs %in% c("Accessories")) & !(lhs %in% c("Accessories")))

inspect(sort(rules, by = "confidence")[1:20])
inspect(sort(rulesBW, by = "support")[1:20])
inspect(sort(rulesBW, by = "confidence")[1:20])
inspect(sort(rulesBW, by = "lift")[1:20])
inspect(sort(rulesBWAccessories, by = "support"))
inspect(sort(rulesBWPrinter, by = "support"))
inspect(sort(rulesBWMonitors, by = "support"))


ggplot(data = rules@quality, aes(x = support)) + geom_histogram(col = "white")

plot(sort(rulesCateg, by = "support")[1:15], method="paracoord", control=list(type="items")) 

plot(sort(rules, by = "support")[1:20], method="graph", control=list(type="items"))

plot(sort(rulesBW, by = "support")[1:15], method="paracoord", control=list(type="items"))

plot(sort(rulesBWAccessories, by = "support")[1:15], method="paracoord", control=list(type="items"))

plot(sort(rulesBWMonitors, by = "support")[1:15], method="paracoord", control=list(type="items"))

plot(sort(rulesBWPrinter, by = "support")[1:15], method="paracoord", control=list(type="items"))

itemFrequencyPlot(transactions, topN = 10)
itemFrequencyPlot(transactionsCategories, topN = 10)
itemFrequencyPlot(transactionsBlackwellCat, topN = 10)
