calculate_cis <- function(pvals, linear_model) {
  # Calculate the test statistic for each p-value
#   z <- -0.862 + sqrt(0.743 - 2.404 * log(pvals))
  df <- linear_model$df.residual
  test_stat <- qt(1- pvals/2, df)
  
  # Calculate the standard error
  SE <- abs(1 / test_stat)
  
  # Calculate the 95% confidence interval
  lower_bound <- 1 - 1.96 * SE
  upper_bound <- 1 + 1.96 * SE
  
  # Return the confidence interval
  return(data.frame(p_value = pvals, lower_bound = lower_bound, upper_bound = upper_bound, diff=upper_bound-lower_bound))

}

# Function that finds the best linear model stepwise considering the p-value
regression_step <- function(linear_model, dependent_var, pos_df) {
    summ_fit <- summary(linear_model)
    terms <- attr(linear_model$terms, "term.labels")
    pvals <- coef(summ_fit)[-1, "Pr(>|t|)"]
    # Clean out the NAs
    non_na <- !is.na(pvals)
    terms <- terms[non_na]
    pvals <- pvals[non_na]

    # Calculate 95% Confidence Interval
    ci_vals <- calculate_cis(pvals, linear_model)
    largest_ci_difference <- which.max(ci_vals$diff)

    # Make sure bounds don't contain zero
    if (!(ci_vals$lower_bound[largest_ci_difference] <=0 && ci_vals$upper_bound[largest_ci_difference] >=0 )) {
        View(ci_vals)
        return(linear_model)
    }

    remove_term <- rownames(ci_vals)[largest_ci_difference]
    ind_terms <- paste(setdiff(terms, remove_term), collapse= " + ")
    formula_string <- paste(dependent_var, ind_terms)
                     
    new_formula <- as.formula(formula_string)
    new_lm <- update(linear_model, new_formula)
    return(regression_step(new_lm, dependent_var, pos_df))
}

# Given a list of dependent vars for a dataframe, plot all density functions
plot_densities <- function(df, dependent_vars) {
    for (dependent_var in dependent_vars) {
        p <- ggplot(df, aes(x=.data[[dependent_var]])) +
            geom_density() +
            labs(x = dependent_var)
        print(p)
    }
}

# Function that counts each term given a base_formula string. Returns a dataframe of counts
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
    pos_df <- results[results[[dependent_var]] > 0, ]
    lm <- lm(form, data=pos_df)
    linear_model <- regression_step(lm, dep_var_str, pos_df)
    
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

# Given a count_df from coutn_terms, make a barplot
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

# Create linear regression models given a list of dependent variables
create_models <- function(dependent_vars, base_string) {
    for (dependent_var in dependent_vars) {
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(paste("The formula for: ", dependent_var))
        dep_var_str <- paste(dependent_var, "~")
        formula_string <- paste(dep_var_str, base_string)
        form <- as.formula(formula_string)
        pos_df <- results[results[[dependent_var]] > 0, ]
        lm <- lm(form, data=pos_df)
        fit_result <- regression_step(lm, dep_var_str, pos_df)
        print(fit_result)
        print_regression(fit_result)
        # print(summary(fit_result))
        print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    }
}

# Print the regression formula of a provided linear model
print_regression <- function(linear_model) {
  # Extract coefficients from the model, and then round them
  coef <- coef(linear_model)
  coef <- round(coef, 3)
  
  # Extract variable names
  variables <- names(coef)[-1]
  dependent_variable <- as.character(formula(linear_model)[[2]])

  # Create the function with dependent variable
  function_string <- paste(dependent_variable, " = ")
  
  # Add intercept
  intercept <- round(coef(linear_model)["(Intercept)"], 3)
  function_string <- paste(function_string, as.character(intercept))
  
  # Add each coefficient to the function string
  for (i in seq_along(variables)) {
    function_string <- paste(function_string, ifelse(coef[i+1] >= 0, "+", "-"), abs(coef[i+1]), "*", variables[i], collapse = "")
  }
  
  # Print the function
  cat(function_string, "\n")
}

# Given a linear model and data, create a new dataframe that has interaction terms as columns
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

# Given a 3dplot and filename, save the 3d plot from x and y angles
save_3d_fig <- function(plot_3d, filename) {
  # Display the 3D plot
  plot_3d

  x_filename <- paste(filename, "_xview.png", sep = "")
  y_filename <- paste(filename, "_yview.png", sep = "")

  # Rotate the plot using par3d
  par3d(zoom = 1)
  par3d(windowRect = c(20,30,600,600))
  par3d(userMatrix = rotationMatrix(pi/2, 0, 1, 0))
  movie3d( spin3d(rpm=3), duration=20,dir="./3d_movie", clean=FALSE )

  rgl.snapshot(x_filename, fmt = 'png')

  
  par3d(userMatrix = rotationMatrix(pi, 0, 1, 0))
  rgl.snapshot(y_filename, fmt = 'png')
}

# Plot each term's fit in a given linear model.
#   makes 2d plots for linear terms, 3d plots for interaction terms
plot_terms <- function(linear_model, data) {
  # Get the terms from the model formula
  terms <- attr(terms(linear_model), "term.labels")
  
  # Identify linear terms and interaction terms
  linear_terms <- terms[!grepl(":", terms)]
  interaction_terms <- terms[grepl(":", terms)]

  new_df <- create_terms_dataframe(linear_model, data)
  new_linear_model <- lm(dependent_var ~ ., data= new_df)
  
  # Create crplots for linear terms
  linear_form <- as.formula(paste("~", paste(linear_terms, collapse = " + ")))
#   crPlots(new_linear_model, terms = linear_form, smooth = FALSE)

  dependent_variable <- as.character(formula(linear_model)[[2]])
  for (term in linear_terms) {
    crPlot(new_linear_model, term, main = paste("C+R Plot for", dependent_variable), smooth = FALSE)
  }
  
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

# Plot the predicted vs. actual values of a provided linear model
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