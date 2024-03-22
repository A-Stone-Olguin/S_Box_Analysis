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
library(rgl)

# Import functions defined in functions.r
source("functions.r")


# Open plot in separate window
hgd()

# Gathering S-box results and attaching for ease of use
results <- read.csv("./results/sboxes_results.csv")
dependent_vars <- colnames(results)[11:ncol(results)]
dependent_vars <- setdiff(dependent_vars, "cwnano_CTR_avg_leaks")
dependent_vars <- setdiff(dependent_vars, "cwlitearm_CTR_avg_leaks")
attach(results)

# Note that linear_probability and nonlinearity are directly related, so linear prob is removed
fit_sm1 <- lm(cwnano_ECB_avg_leaks ~ nonlinearity + linearity)
summary(fit_sm1)


plot_densities(results, dependent_vars)

## Plot DPA dists
# NANO
plot1 <- ggplot(data = results, aes(x = cwnano_DPA_n_traces, color = "CW-Nano")) +
         geom_density() +
         labs(x = "", y = "Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Number of Traces Distribution with DPA") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))


# LITE
plot2 <- ggplot(data = results, aes(x = cwlitearm_DPA_n_traces, color = "CW-Lite")) +
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
plot1 <- ggplot(data = results, aes(x = cwnano_CPA_n_traces, color = "CW-Nano")) +
         geom_density() +
         labs(x = "", y = "Density") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Number of Traces Distribution with CPA") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))


# LITE
plot2 <- ggplot(data = results, aes(x = cwlitearm_CPA_n_traces, color = "CW-Lite")) +
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

# Plot all tvla plots (not ctr)
plot1 <- ggplot(data = results, aes(x = cwnano_ECB_avg_leaks, color = "CW-Nano")) +
         geom_density() +
         labs(x = "", y = "ECB Mode Density") +
        theme(legend.position = "top") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))

plot2 <- ggplot(data = results, aes(x = cwlitearm_ECB_avg_leaks, color = "CW-Lite")) +
         geom_density() +
         labs(x = "", y = "") +
        theme( legend.position = "top") +
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("red"))))

plot3 <- ggplot(data = results, aes(x = cwnano_CBC_avg_leaks)) +
         geom_density(color = "blue") +
         labs(x = "", y = "CBC Mode Density")

plot4 <- ggplot(data = results, aes(x = cwlitearm_CBC_avg_leaks)) +
         geom_density(color = "red") +
         labs(x = "", y = "")

plot5 <- ggplot(data = results, aes(x = cwnano_CTR_avg_leaks)) +
         geom_density(color = "blue") +
         labs(x = "Average Leakage Ratio", y = "CTC Mode Density")

plot6 <- ggplot(data = results, aes(x = cwlitearm_CTR_avg_leaks)) +
         geom_density(color = "red") +
         labs(x = "Average Leakage Ratio", y = "")

plot1 + plot2  + plot3 + plot4 + plot5 + plot6 +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Density Plots for \"Detected Leakage\" Metric") &
  theme(plot.title = element_text(hjust = 0.5))

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
            linear_branch:boomerang_uniformity -
            differential_probability:linear_branch - 
            nonlinearity:linear_branch"
# Count how many times each property was significant
count_df <- count_terms(dependent_vars, formula)
# Plot a barplot of each significant count
plot_count_df(count_df)
# Summary of each linear model
create_models(dependent_vars, formula)


nano_dpa_df <- results[results$cwnano_DPA_n_traces > 0, ]
dependent_str <- "cwnano_DPA_n_traces ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = nano_dpa_df)
reg_lm <- regression_step(lm, dependent_str, nano_dpa_df)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = nano_dpa_df[[dependent_variable]])
plot1 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values, color = "CW-Nano")) +
         geom_point() +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "Actual Values") +
        theme(legend.position = "top") + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("blue"))))

lite_dpa_df <- results[results$cwlitearm_DPA_n_traces > 0, ]
dependent_str <- "cwlitearm_DPA_n_traces ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = lite_dpa_df)
reg_lm <- regression_step(lm, dependent_str, lite_dpa_df)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = lite_dpa_df[[dependent_variable]])
plot2 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values, color = "CW-Lite")) +
         geom_point() +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "DPA") +
        theme(legend.position = "top",
              axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1)) + 
        scale_color_manual(values = c("CW-Nano" = "blue", "CW-Lite" = "red"),
                                    name = "ChipWhisperer Device") +
        guides(color = guide_legend(override.aes = list(fill = c("red"))))


dependent_str <- "cwnano_CPA_n_traces ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot3 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "blue") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "Actual Values")

dependent_str <- "cwlitearm_CPA_n_traces ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
plot_terms(reg_lm, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot4 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "red") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "CPA") +
         theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1))

dependent_str <- "cwnano_ECB_avg_leaks ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot5 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "blue") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "Actual Values ")

dependent_str <- "cwlitearm_ECB_avg_leaks ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
plot_terms(reg_lm, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot6 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "red") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "", y = "ECB") +
         theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1))

dependent_str <- "cwnano_CBC_avg_leaks ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot7 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "blue") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "Predicted Values", y = "Actual Values")

dependent_str <- "cwlitearm_CBC_avg_leaks ~"
lm <- lm(as.formula(paste(dependent_str, formula)), data = results)
reg_lm <- regression_step(lm, dependent_str, results)
dependent_variable <- as.character(formula(lm)[[2]])
plot_data <- data.frame(fitted_values = fitted(lm), actual_values = results[[dependent_variable]])
plot8 <- ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
         geom_point(color = "red") +       
        geom_abline(slope = 1, intercept = 0) + 
         labs(x = "Predicted Values", y = "CBC") +
         theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 1))


plot1 + plot2 + plot3 + plot4 + plot5 + plot6 + plot7 + plot8 +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Predicted Values vs. Actual Values for Each Regression Model",
                  subtitle = "The center labels show how each metric changed") &
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))





create_models(dependent_vars, formula)


plot_model(reg_lm, nano_dpa_df)
plot_terms(reg_lm, nano_dpa_df)

## Examples for showing Regression formulae and interaction plots
install.packages("modeldata")
library(modeldata)

data(penguins)

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


