#-----------------------------
# Nonparametric Bootstrap in R
#-----------------------------


# dataset
x <- c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2, 7.01, 8.74, 7.78,
       7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52, 6.21, 12.28, 5.6, 5.38, 6.6, 8.74)

# coefficient of variation (CV)
cv <- sd(x) / mean(x)
cv # 0.2524712

# bootstrap
num_bootstraps <- 10000
bootstrap_cvs <- numeric(num_bootstraps)
set.seed(2023)  
for (i in 1:num_bootstraps) {
  resample <- sample(x, replace = TRUE)
  bootstrap_cvs[i] <- sd(resample) / mean(resample)
}

# bias and standard error of the CV estimator
bias <- mean(bootstrap_cvs) - cv
bias # [1] -0.01216963
standard_error <- sd(bootstrap_cvs)
standard_error # [1] 0.04484109

# 2. plot the bootstrap distribution
library(ggplot2)
bootstrap_df <- data.frame(CVs = bootstrap_cvs)

ggplot(bootstrap_df, aes(x = CVs)) +
  geom_histogram(binwidth = 0.01, fill = "darkred", color = "black") +
  geom_vline(xintercept = cv, color = "black", linetype = "dashed", lwd = 2) +
  labs(title = 'Bootstrap distribution of the Coefficient of Variation (cv)',
       y="Frequency", x="cv") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


