#loading necessary data
leveladv.df <- read.csv("LEVELADV3.csv")

#CORRELATION ANALYSIS
heatmap(cor(leveladv.df), Rowv = NA, Colv = NA)
## heatmap with values
library(gplots)
heatmap.2(cor(leveladv.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(leveladv.df),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# alternative plot with ggplot
library(ggplot2)
library(reshape) # to generate input for the plot
cor.mat <- round(cor(leveladv.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
  geom_tile() +
  geom_text(aes(x = X1, y = X2, label = value))