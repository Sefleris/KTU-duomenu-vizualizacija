library(readr)
library(dplyr)
library(readr)
library(ggplot2)
lab_sodra = read_csv("laboratorinis/data/lab_sodra.csv")



data = lab_sodra %>%
  filter(lab_sodra$ecoActCode == '451100')

p = ggplot(data, aes(x = avgWage)) +
  geom_histogram(binwidth = 10, fill = "#0072B2", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Values", x = "Vid. atlyginimas", y = "Dažnumas")
theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )
print(p)
