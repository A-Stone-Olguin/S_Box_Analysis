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


# Open plot in separate window
hgd()

# Gathering S-box results and attaching for ease of use
results <- read.csv("./results/sboxes_results.csv")
dependent_vars <- colnames(results)[(ncol(results)-6):ncol(results)]
attach(results)

# Note that linear_probability and nonlinearity are directly related, so linear prob is removed
fit_sm1 <- lm(X30_cwlitearm_tvla_CBC_1000 ~ nonlinearity + linearity)
summary(fit_sm1)
# Note that nonlinearity and linearity are directly related so linearity is removed
fit_sm2 <- lm(X30_cwlitearm_tvla_CBC_1000 ~ linear_branch + I(linear_branch^2) )
summary(fit_sm2)

regression_step <- function(linear_model, dependent_var, mode = "l") {
    summ_fit <- summary(linear_model)
    terms <- attr(linear_model$terms, "term.labels")
    if (mode == "g") {
        pvals <- coef(summ_fit)[-1, "Pr(>|z|)"]
    }
    else {
        pvals <- coef(summ_fit)[-1, "Pr(>|t|)"]
    }

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

    return(regression_step(new_lm, dependent_var, mode))
}

fit1 <- lm(X30_cwlitearm_tvla_ECB_1000 ~
            nonlinearity +
            differential_probability +
            boomerang_uniformity +
            diff_branch +
            linear_branch +
            bic +
            sac +
            I(nonlinearity^2) +
            I(differential_probability^2) +
            I(boomerang_uniformity^2) +
            I(diff_branch^2) +
            I(linear_branch^2) +
            I(bic^2) +
            I(sac^2) + 
            I(nonlinearity^3) +
            I(differential_probability^3) +
            I(boomerang_uniformity^3) +
            I(diff_branch^3) +
            I(linear_branch^3) +
            I(bic^3) +
            I(sac^3))

fitnano <- lm(X30_cwlitearm_tvla_ECB_1000 ~
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
            I(bic^2) +
            I(sac^2))

res <- regression_step(fit1, "X30_cwlitearm_tvla_ECB_1000 ~")
summary(res)

ptvla <- ggplot(data=results, aes(x = bic, y = X30_cwnano_tvla_ECB_1000)) + 
        geom_point() + 
        geom_smooth(method =  "lm", se = FALSE)
ptvla
ptvla <- ggplot(data=results, aes(x = differential_probability, y = X30_cwnano_tvla_ECB_1000)) + 
        geom_point() + 
        geom_smooth(method =  "lm", se = FALSE)
ptvla

crPlots(res)
# Create partial regression plots
avPlots(res)

results$factor <- as.factor(X10_cwnano_dpa_ECB)
fit_sq_glm <- glm(factor ~
            nonlinearity +
            differential_probability +
            boomerang_uniformity +
            diff_branch +
            linear_branch +
            bic +
            sac +
            I(nonlinearity^2) +
            I(differential_probability^2) +
            I(boomerang_uniformity^2) +
            I(diff_branch^2) +
            I(linear_branch^2) +
            I(bic^2) +
            I(sac^2), 
            data = results,
            family = "binomial")
summary(fit_sq_glm)
summary(regression_step(fit_sq_glm, "factor ~", mode = "g"))




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


# p <-  ggplot(data = results, aes(x = X10_cwnano_dpa_ECB)) +
#          geom_density(aes(color = "CW-Nano"), fill = NA) +
#          geom_density(data = results, aes(x = X10_cwlitearm_dpa_ECB, color = "CW-Lite"), fill = NA) +
#          labs( x = "Minimum Number of Traces to break AES >= 90% of the time",
#             y = "Density") +
#         # theme(plot.title = element_text(hjust = 0.5),
#         #         # panel.background = element_rect(fill = "transparent", color = NA),
#         #         axis.title.x = element_text(margin = margin(t = 20)))
#         ggtitle( "Number of Traces Distribution with DPA on CW-Nano")
# print(p)


p <- ggplot(results, aes(x=X10_cwlitearm_tvla_ECB_250)) +
    # geom_histogram()
    geom_density(color="blue") + 
    geom_density(aes(x=X30_cwlitearm_tvla_ECB_1000), color="red")
print(p)

pos_nl <- results[results$nonlinearity > 0, ]
plot(pos_nl$nonlinearity, pos_nl$X30_cwlitearm_tvla_ECB_1000, xlab="Nonlinearity", ylab="Average Leakage Ratio",
    main="Average Leakage from CW-Lite on ECB Mode with 1000 Traces")

fit <- lm(X30_cwlitearm_tvla_CBC_1000 ~ 
            nonlinearity + 
            linear_probability+
            differential_probability +
            boomerang_uniformity +
            diff_branch + 
            linear_branch +
            linearity + 
            bic +
            sac)
summary(fit)
fit_sq <- lm(X30_cwlitearm_tvla_CBC_1000 ~
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

count_terms <- function(dependent_vars, base_string) {
  independent_vars <- c("nonlinearity", "linear_probability",
                      "differential_probability", "boomerang_uniformity",
                      "diff_branch", "linear_branch", "bic", "sac")

  count_df <- data.frame(
    terms = independent_vars,
    Number_Significant = rep(0, length(independent_vars)),
    Interaction_Count = rep(0, length(independent_vars)),
    Number_Positive = rep(0, length(independent_vars))
  )

  for (dependent_var in dependent_vars) {
    dep_var_str <- paste(dependent_var, "~")
    formula_string <- paste(dep_var_str, base_string)
    form <- as.formula(formula_string)
    lm <- lm(form, data=results)
    linear_model <- regression_step(lm, dep_var_str)
    
    # Currently recomputes a linear model each time

    terms <- attr(terms(linear_model), "term.labels")

    linear_terms <- terms[!grepl(":", terms)]
    interaction_terms <- terms[grepl(":", terms)]
    for (term in linear_terms) {
        if (term %in% independent_vars) {
            count_df[count_df$terms == term, "Number_Significant"] <- 
                count_df[count_df$terms == term, "Number_Significant"] + 1
            if (coef(linear_model)[[term]] > 0) {
                count_df[count_df$terms == term, "Number_Positive"] <- 
                    count_df[count_df$terms == term, "Number_Positive"] + 1
            }
        }
    }
    for (int_term in interaction_terms) {
        single_terms <- strsplit(int_term, ":")[[1]]
        for (term in single_terms) {
            if (term %in% independent_vars) {
                count_df[count_df$terms == term, "Number_Significant"] <- 
                    count_df[count_df$terms == term, "Number_Significant"] + 1
                count_df[count_df$terms == term, "Interaction_Count"] <- 
                    count_df[count_df$terms == term, "Interaction_Count"] + 1
                if (coef(linear_model)[[int_term]] > 0) {
                    count_df[count_df$terms == term, "Number_Positive"] <- 
                        count_df[count_df$terms == term, "Number_Positive"] + 1
                }
            }
        }
    }
    }
    return(count_df)
}
linear_string <- "nonlinearity +
        differential_probability +
        boomerang_uniformity +
        diff_branch +
        linear_branch +
        bic +
        linear_probability +
        sac"
# interaction_string <-  "(nonlinearity +
#         differential_probability +
#         boomerang_uniformity +
#         diff_branch +
#         linear_branch +
#         bic +
#         linear_probability +
#         sac)^2"
interaction_string <- "(nonlinearity + 
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

count_df <- count_terms(dependent_vars2, interaction_string)

plot_count_df <- function(count_df) {
    # Plot without interactions if there aren't any
    if(all(count_df$Interaction_Count == 0)) {
        ggplot(data = count_df, aes(x = terms, y = Number_Significant)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            labs(title = "Barplot of Counts", x = "Category", y = "Number of Times as Significant Predictor") +
            theme(plot.title = element_text(hjust = 0.5),
                axis.title.x = element_text(margin = margin(t = 20)))
    } 
    else {
        ggplot(data = count_df, aes(x = terms, y = Number_Significant,  fill = c("Linear and Interaction", "Interaction"))) +
            geom_bar(aes(y = Number_Significant, fill = "Linear and Interaction"), stat = "identity") +
            geom_bar(aes(y = Interaction_Count, fill = "Interaction"), stat = "identity") +
            labs(title = "Barplot of Counts with Interactions", x = "Category", y = "Number of Times as Significant Predictor") + 
            theme(plot.title = element_text(hjust = 0.5),
                axis.title.x = element_text(margin = margin(t = 20)),
                legend.position = "top") + 
            scale_fill_manual(values = c("Linear and Interaction" = "skyblue", "Interaction" = "lightgreen"),
                                name = "Type of Term Count:")
    } 
}
plot_count_df(count_df)

create_models <- function(dependent_vars) {
    base_string <- "nonlinearity +
            differential_probability +
            boomerang_uniformity +
            diff_branch +
            linear_branch +
            bic +
            linear_probability +
            sac"
    for (dependent_var in dependent_vars) {
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(paste("The formula for: ", dependent_var))
        dep_var_str <- paste(dependent_var, "~")
        formula_string <- paste(dep_var_str, base_string)
        form <- as.formula(formula_string)
        lm <- lm(form, data=results)
        fit_result <- regression_step(lm, dep_var_str)
        print(fit_result)
        print(summary(fit_result))
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    }
}
dependent_vars2 <- c("X30_cwlitearm_cpa_ECB", "X30_cwlitearm_tvla_CBC_1000", 
"X30_cwnano_tvla_ECB_1000", "X10_cwlitearm_dpa_ECB", "X10_cwnano_cpa_ECB", 
"X10_cwnano_dpa_ECB")

create_models(dependent_vars2)

fit2 <- lm(X30_cwlitearm_tvla_ECB_1000 ~ 
            (nonlinearity + 
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
            linear_branch:boomerang_uniformity)

p_fit <- regression_step(fit2, "X30_cwlitearm_tvla_ECB_1000 ~")
summary(p_fit)
summary(step(fit2, direction = "backward"))

residuals <- resid(p_fit)
plot(fitted(p_fit), residuals)

plot_data <- data.frame(fitted_values = fitted(p_fit), actual_values = X30_cwlitearm_tvla_ECB_1000)

# Create a scatterplot with a 1:1 line
ggplot(plot_data, aes(x = fitted_values, y = actual_values)) +
    geom_point() +   
    theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 20)))   +                      
    geom_abline(slope = 1, intercept = 0) +  
    labs(x = "Fitted Values", y = "Actual Values") +  
    ggtitle("Actual versus Fitted Values")


crPlot3d(model = p_fit, terms =  ~ ., var1 = "linear_probability" ,
 var2 = "sac", data = results,
         main = "3D Scatterplot with Regression Plane",
         smoother = "none")



  par3d(userMatrix = rotationMatrix(0, 1, 0, 0))
  par3d(userMatrix = rotationMatrix(pi/2, 0, 1, 0))
  par3d(userMatrix = rotationMatrix(pi, 0, 1, 0))
  par3d(zoom = .7)
rgl.snapshot('3dplot.png', fmt = 'png')
rgl::title3d("Your Title", cex = 5)
rgl::par3d("title", adj = 0.1)

# Print the regression formula
print_regression <- function(linear_model) {
    # Extract coefficients from the model
  coef <- coef(linear_model)
  
  # Extract variable names
  variables <- names(coef)[-1]
  
  dependent_variable <- as.character(formula(linear_model)[[2]])
  # Create the function string
  function_string <- paste(dependent_variable, " = ")
  
  # Add each coefficient to the function string
  for (i in seq_along(variables)) {
    function_string <- paste(function_string, ifelse(coef[i+1] >= 0, "+", "-"), abs(coef[i+1]), "*", variables[i], collapse = "")
  }
  
  # Print the function
  cat(function_string, "\n")
}

create_terms_dataframe <- function(linear_model, data) {
  # Extract formula and terms from the linear model
  formula <- formula(linear_model)
  terms <- attr(terms(formula), "term.labels")
  
  # Initialize an empty data frame to store the terms
  terms_df <- data.frame(matrix(NA, nrow = nrow(data), ncol = length(terms)))
  colnames(terms_df) <- terms

  # Linear terms and interaction terms
  linear_terms <- terms[!grepl(":", terms)]
  interaction_terms <- terms[grepl(":", terms)]

 for (l_t in linear_terms) {
    terms_df[[l_t]] <- data[[l_t]]
 }
 for (i_t in interaction_terms) {
    l_terms <- strsplit(i_t, ":")[[1]]
    terms_df[[i_t]] <- data[[l_terms[1]]] * data[[l_terms[2]]]
 }

  dependent_variable <- as.character(formula(linear_model)[[2]])
  terms_df$dependent_var <- data[[dependent_variable]]
  
  return(terms_df)
}

save_3d_fig <- function(plot_3d, filename) {
  # Display the 3D plot
  plot_3d

  x_filename <- paste(filename, "_xview.png", sep = "")
  y_filename <- paste(filename, "_yview.png", sep = "")

  # Rotate the plot using par3d
  par3d(zoom = .7)
  par3d(userMatrix = rotationMatrix(pi/2, 0, 1, 0))

  rgl.snapshot(x_filename, fmt = 'png')

  
  par3d(userMatrix = rotationMatrix(pi, 0, 1, 0))
  rgl.snapshot(y_filename, fmt = 'png')

}

# Show each term's fit
plot_terms <- function(linear_model, data) {
  # Get the terms from the model formula
  terms <- attr(terms(linear_model), "term.labels")
  
  # Identify linear terms and interaction terms
  linear_terms <- terms[!grepl(":", terms)]
  interaction_terms <- terms[grepl(":", terms)]

  new_df <- create_terms_dataframe(linear_model, data)
  new_linear_model <- lm(dependent_var ~ ., data= new_df)
  
  # Create crplots for linear terms
  for (term in linear_terms) {
    crPlot(new_linear_model, term, main = paste("crplot for", term), smooth =FALSE)
  }
  
    dependent_variable <- as.character(formula(linear_model)[[2]])
    filename <- paste("3d_images/", dependent_variable, sep ="")
  # Create 3dcrplots for interaction terms
  for (term in interaction_terms) {
    var_names <- strsplit(term, ":")[[1]]
    plot_3d <- crPlot3d(model = linear_model, var1 = var_names[1],
             var2 = var_names[2], data = data,
         main = "3D CR Plot with Regression Plane",
         smoother = "none")

    save_3d_fig(plot_3d, filename)
  }
}

# Plot the whole model's fit
plot_model <- function(linear_model, data) {
    dependent_variable <- as.character(formula(linear_model)[[2]])

    plot_data <- data.frame(fitted_values = fitted(linear_model), actual_values = data[[dependent_variable]])

    xlab <- paste("Predicted Values of", dependent_variable)
    ylab <- paste("Actual Values of", dependent_variable)
    title <- paste("Comparison Between Predicted Values and Actual Values for", dependent_variable)

    # Create a scatterplot with a 1:1 line
    ggplot(data = plot_data, aes(x = fitted_values, y = actual_values)) +
        geom_point() +   
        theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 20)))   +                      
        geom_abline(slope = 1, intercept = 0) +  
        labs(x = xlab, y = ylab) +  
        ggtitle(title)
}


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
plot_terms(pen_fit, penguins)

