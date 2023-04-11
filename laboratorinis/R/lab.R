library(readr)
library(dplyr)
library(ggplot2)
lab_sodra = read_csv("KTU-duomenu-vizualizacija/laboratorinis/data/lab_sodra.csv")



data = lab_sodra %>%
  filter(lab_sodra$ecoActCode == '451100')

data = filtered_data

ggplot(data, aes(x = value)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Values", x = "Values", y = "Frequency")