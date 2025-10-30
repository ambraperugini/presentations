rm(list = ls())

x <- rnorm(150, mean = 10, sd = 4)
y <- x*.5 + rnorm(150, mean = 2, sd = 15)
z <- runif(150, min = 5, max = 15)
k <- z*2 + rnorm(150, mean = 4, sd = 2)

df_sim <- data.frame(x,y,z,k)

library(writexl)
write_xlsx(df_sim, "/Users/ambraperugini/Library/CloudStorage/OneDrive-UniversitàdegliStudidiPadova/Lavoro/Conferences_2024/Presentazioni/AIP_Sper/df_sim")

str(df_sim)
