# Install httpgd and tidyverse from their github repositories (there were errors downloading from CRAN)
remotes::install_github("nx10/httpgd")
remotes::install_github("tidyverse/tidyverse")
install.packages("latex2exp")
install.packages("reshape2")

# Libraries
library(ggplot2)
library(httpgd)
library(latex2exp)
library(reshape2)


# Open plot in separate window
hgd()

# Gathering S-box results and attaching for ease of use
results <- read.csv("./results/sboxes_results.csv")
results <- subset(results, select = -X10_cwnano_tvla_CTR_250)
dependent_vars <- colnames(results)[(ncol(results)-12):ncol(results)]
attach(results)

# Note that linear_probability and nonlinearity are directly related, so linear prob is removed
fit_sm1 <- lm(X10_cwlitearm_tvla_ECB_250 ~ nonlinearity + linear_probability)
summary(fit_sm)
# Note that nonlinearity and linearity are directly related so linearity is removed
fit_sm2 <- lm(X10_cwlitearm_tvla_ECB_250 ~ nonlinearity + linearity )
summary(fit_sm2)

regression_step <- function(linear_model, dependent_var) {
    summ_fit <- summary(linear_model)
    terms <- attr(linear_model$terms, "term.labels")
    pvals <- coef(summ_fit)[-1, "Pr(>|t|)"]


    # Clean out the NAs
    non_na <- !is.na(pvals)
    terms <- terms[non_na]
    pvals <- pvals[non_na]

    max_p_value <- which.max(pvals)
    max_p <- max(pvals)
    if (max_p < 0.05) {
        return(linear_model)
    }

    remove_term <- names(pvals)[max_p_value]
    ind_terms <- paste(setdiff(terms, remove_term), collapse= " + ")
    formula_string <- paste(dependent_var, ind_terms)
                     
    new_formula <- as.formula(formula_string)
    new_lm <- update(linear_model, new_formula)

    return(regression_step(new_lm, dependent_var))
}

plot_deps <- function(df, dependent_vars) {
    for (dependent_var in dependent_vars) {
        p <- ggplot(df, aes(x=.data[[dependent_var]])) +
            # geom_histogram()
            geom_density() +
            labs(x = dependent_var)
        print(p)
    }
}
plot_deps(results, dependent_vars)

p <- ggplot(results, aes(x=X10_cwlitearm_tvla_ECB_250)) +
    # geom_histogram()
    geom_density(color="blue") + 
    geom_density(aes(x=X30_cwlitearm_tvla_ECB_1000), color="red")
print(p)

pos_nl <- results[results$nonlinearity > 0, ]
plot(pos_nl$nonlinearity, pos_nl$X10_cwlitearm_tvla_ECB_250, xlab="Nonlinearity", ylab="Average Leakage Ratio",
    main="Average Leakage from CW-Lite on ECB Mode with 250 Traces")

fit <- lm(X10_cwlitearm_tvla_ECB_250 ~ 
            nonlinearity + 
            differential_probability +
            boomerang_uniformity +
            diff_branch + 
            linear_branch +
            bic +
            sac)
summary(fit)
fit_sq <- lm(X10_cwlitearm_tvla_ECB_250 ~
            (nonlinearity +
            differential_probability +
            boomerang_uniformity +
            diff_branch +
            linear_branch +
            bic +
            sac)^2 +
            I(nonlinearity^2) +
            I(differential_probability^2) +
            I(boomerang_uniformity^2) +
            I(diff_branch^2) +
            I(linear_branch^2) +
            I(bic^2) +
            I(sac^2))
summary(fit_sq)

create_models <- function(dependent_vars) {
    base_string <- "(nonlinearity +
            differential_probability +
            boomerang_uniformity +
            diff_branch +
            linear_branch +
            bic +
            sac)^2 +
            I(nonlinearity^2) +
            I(differential_probability^2) +
            I(boomerang_uniformity^2) +
            I(diff_branch^2) +
            I(bic^2) +
            I(sac^2)"
    for (dependent_var in dependent_vars) {
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(paste("The formula for: ", dependent_var))
        dep_var_str <- paste(dependent_var, "~")
        formula_string <- paste(dep_var_str, base_string)
        # print(base_string)
        form <- as.formula(formula_string)
        lm <- lm(form, data=results)
        print(regression_step(lm, dep_var_str))
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    }
}
create_models(dependent_vars)

regression_step(fit_sq, "X10_cwlitearm_tvla_ECB_250 ~")
step(fit_sq, direction="backward")