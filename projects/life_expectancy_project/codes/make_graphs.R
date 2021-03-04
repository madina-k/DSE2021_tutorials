require(tidyverse)

dataset <- read_csv("./data/life_exp.csv")

ggplot(dataset, aes(x=male, y = female, label = country)) + geom_point() + geom_text(alpha = 0.4) + geom_abline(a= 0, b=1)

ggsave(filename = "./figures/scatterplot.pdf")
