install.packages("httpgd")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("patchwork")
install.packages("car")
install.packages("rgl")
install.packages("effects")
install.packages("estimability")


# Libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(httpgd)
library(reshape2)
library(car)
library(effects)
library(estimability)

# Import functions defined in functions.r
source("functions.r")


# Open plot in separate window
hgd()

# Gathering S-box results and attaching for ease of use
results <- read.csv("./results/sboxes_results.csv")
dependent_vars <- colnames(results)[11:ncol(results)]
attach(results)

# Note that linear_probability and nonlinearity are directly related, so linear prob is removed
fit_sm1 <- lm(X30_cwlitearm_tvla_CBC_1000 ~ nonlinearity + linearity)
summary(fit_sm1)
# Note that nonlinearity and linearity are directly related so linearity is removed
fit_sm2 <- lm(X30_cwlitearm_tvla_CBC_1000 ~ linear_branch + I(linear_branch^2) )
summary(fit_sm2)


plot_densities(results, dependent_vars)

## Plot DPA dists
# NANO
plot1 <- ggplot(data = results, aes(x = X10_cwnano_dpa_ECB, color = "CW-Nano")) +
         geom_density() +
         labs(x = "", y = "Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Number of Traces Distribution with DPA") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))


# LITE
plot2 <- ggplot(data = results, aes(x = X10_cwlitearm_dpa_ECB, color = "CW-Lite")) +
         geom_density() +
         labs(x = "Minimum Number of Traces to break AES >= 90% of the time",
            y = "Density") +
        theme(plot.title = element_text(hjust = 0.5),
            # panel.background = element_rect(fill = "transparent", color = NA),
            axis.title.x = element_text(margin = margin(t = 20))) + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                name = "ChipWhisperer Device") + 
        guides(color = guide_legend(override.aes = list(fill = c("red"))))

# Put in SubPlots
plot1 + plot2  + plot_layout(nrow = 2)

## CPA Number traces distribution
# NANO
# TODO: Need to generate data for 30 runs
plot1 <- ggplot(data = results, aes(x = X10_cwnano_cpa_ECB, color = "CW-Nano")) +
         geom_density() +
         labs(x = "", y = "Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Number of Traces Distribution with CPA") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))


# LITE
plot2 <- ggplot(data = results, aes(x = X30_cwlitearm_cpa_ECB, color = "CW-Lite")) +
         geom_density() +
         labs(x = "Minimum Number of Traces to break AES >= 90% of the time",
            y = "Density") +
        theme(plot.title = element_text(hjust = 0.5),
            # panel.background = element_rect(fill = "transparent", color = NA),
            axis.title.x = element_text(margin = margin(t = 20))) + 
            scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") + 
        guides(color = guide_legend(override.aes = list(fill = c("red"))))

# Put in SubPlots
plot1 + plot2  + plot_layout(nrow = 2)


pos_nl <- results[results$nonlinearity > 0, ]

# Formula string (has all interactions except those that are singularities)
formula <- "(nonlinearity + 
            linear_probability+
            differential_probability +
            boomerang_uniformity +
            diff_branch + 
            linear_branch + 
            bic +
            sac)^2 - 
            diff_branch:linear_branch -
            diff_branch:sac -
            linear_branch:sac - 
            linear_branch:bic -
            linear_probability:diff_branch - 
            linear_probability:linear_branch - 
            linear_branch:boomerang_uniformity"
# Count how many times each property was significant
count_df <- count_terms(dependent_vars2, formula)
# Plot a barplot of each significant count
plot_count_df(count_df)
# Summary of each linear model
create_models(dependent_vars, formula)




## Examples for showing Regression formulae and interaction plots
install.packages("modeldata")
library(modeldata)

data(penguins)

View(penguins)
penguins <- na.omit(penguins)

pen_fit <- lm(body_mass_g ~ bill_length_mm * bill_depth_mm, data=penguins)
summary(pen_fit)

# Here is the regression formula
print_regression(pen_fit)

# Show predicted v. actual for penguins
plot_model(pen_fit, penguins)
# Show each term's interaction plot
source("functions.r")
plot_terms(pen_fit, penguins)


