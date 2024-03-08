# Install httpgd and tidyverse from their github repositories (there were errors downloading from CRAN)
remotes::install_github("nx10/httpgd")
remotes::install_github("tidyverse/tidyverse")
install.packages("latex2exp")

# Libraries
library(ggplot2)
library(httpgd)
library(latex2exp)

# Gathering S-box results and attaching for ease of use
results <- read.csv("./results/sboxes_results.csv")
attach(results)

# Note that linear_probability and nonlinearity are directly related, so linear prob is removed
fit_sm1 <- lm(X10_cwlitearm_tvla_ECB ~ nonlinearity + linear_probability)
summary(fit_sm)
# Note that nonlinearity and linearity are directly related so linearity is removed
fit_sm2 <- lm(X10_cwlitearm_tvla_ECB ~ nonlinearity + linearity )
summary(fit_sm2)

# Open plot in separate window
hgd_browse()
pos_nl <- results[results$nonlinearity > 0, ]
plot(pos_nl$nonlinearity, pos_nl$X10_cwlitearm_tvla_ECB, xlab="Nonlinearity", ylab="Average Leakage Ratio",
    main="Average Leakage from CW-Lite on ECB Mode with 250 Traces")

fit <- lm(X10_cwlitearm_tvla_ECB ~ 
            nonlinearity + 
            differential_probability +
            boomerang_uniformity +
            diff_branch + 
            linear_branch +
            bic +
            sac)
summary(fit)
step(fit)


fit_sq <- lm(X10_cwlitearm_tvla_ECB ~ 
            (nonlinearity + 
            differential_probability +
            boomerang_uniformity +
            diff_branch + 
            linear_branch +
            bic +
            sac)^2+
            I(nonlinearity^2) +
            I(differential_probability^2) +
            I(boomerang_uniformity^2)+
            I(diff_branch^2)+
            I(linear_branch^2)+
            I(bic^2)+
            I(sac^2))

summary(fit_sq)
step(fit_sq)
