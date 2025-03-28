# --- Libraries ---
library(shiny)
library(DiagrammeR) # This package contains both grViz and mermaid
library(shinyjs)    # For conditional panels

# --- Data Structure (Mermaid Diagram and Questions) ---
# (No changes needed in this structure itself, as it already contains Mermaid code)
decision_data <- list(
    start = list(
        diagram = "graph LR\n  A[Start: Quantitative or Categorical?] --> B((Quantitative))\n  A --> C((Categorical))",
        question = "Is your data quantitative (numerical measurements) or categorical (grouping variables)?\n\nExamples of Quantitative Data:\n* Height\n* Weight\n* Temperature\n\nExamples of Categorical Data:\n* Eye color\n* Brand name\n* Yes/No responses",
        answers = c("Quantitative", "Categorical"),
        id= "start"
    ),
    Quantitative = list(
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> E((One))\n  D --> F((Two))\n  D --> G((Three or More))",
        question = "How many means are you comparing?",
        answers = c("One", "Two", "Three or More"),
        id = "quan"
    ),
    Categorical = list(
        diagram = "graph LR\n  A[Start] --> C((Categorical))\n  C --> H[How many proportions?]\n  H --> I((One))\n  H --> J((Two))\n  H --> K((Three or More))",
        question = "How many proportions are you comparing?",
        answers = c("One", "Two", "Three or More"),
        id = "cat"
    ),
    One = list(
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> E((One))\n  E --> L[Is sigma known?]\n  L --> M((Yes))\n  L --> N((No))",
        question = "Is the population standard deviation (sigma) known?",
        answers = c("Yes", "No"),
        id = "one_mean"
    ),
    Two = list(
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> F((Two))\n  F --> O[Are the data paired?]\n  O --> P((Yes))\n  O --> Q((No))",
        question = "Are the two sets of data paired (e.g., before and after measurements on the same subjects)?",
        answers = c("Yes", "No"),
        id = "paired"
    ),
    # --- Updated Result Nodes with R code examples ---
    `One_Sigma_Known` = list( # One Mean, Sigma Known
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> E((One))\n  E --> L[Is sigma known?]\n  L --> M((Yes))\n  M --> S[One Sample Z-Test]",
        question = "Result: One Sample Z-Test",
        result = "One Sample Z-Test",
        formula = "<b>Hypotheses:</b><br>H₀: μ = μ₀<br>Hₐ: μ ≠ μ₀ (or < or >)<br><br><b>Test Statistic:</b><br>z = (x̄ - μ₀) / (σ / √n)<br><br><b>R Function (Requires package like BSDA or teachingstats):</b><br><code>library(BSDA)</code><br><code>z.test(your_data_vector, mu = mu0, sigma.x = known_sigma, alternative = 'two.sided')</code><br><i>Note: Base R doesn't have a built-in z.test.</i><br><br><b>Confidence Interval:</b><br>x̄ ± z*(σ / √n)<br>Calculated within `z.test` output."
    ),
    `One_Sigma_Unknown` = list( # One Mean, Sigma Unknown
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> E((One))\n  E --> L[Is sigma known?]\n  L --> N((No))\n  N --> T[One Sample t-Test]",
        question = "Result: One Sample t-Test",
        result = "One Sample t-Test",
        formula = "<b>Hypotheses:</b><br>H₀: μ = μ₀<br>Hₐ: μ ≠ μ₀ (or < or >)<br><br><b>Test Statistic:</b><br>t = (x̄ - μ₀) / (s / √n)<br><br><b>R Function (Base R):</b><br><code>t.test(your_data_vector, mu = mu0, alternative = 'two.sided')</code><br><br><b>Confidence Interval:</b><br>x̄ ± t*(s / √n)<br>Calculated within `t.test` output."
    ),
    `Paired_Two_Sample` = list( # Two Means, Paired
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> F((Two))\n  F --> O[Are the data paired?]\n  O --> P((Yes))\n  P --> U[Paired t-Test]",
        question = "Result: Paired t-Test",
        result = "Paired t-Test",
        formula = "<b>Hypotheses:</b><br>H₀: μ_d = 0<br>Hₐ: μ_d ≠ 0 (or < or >)<br><i>where μ_d is the mean of the differences</i><br><br><b>Test Statistic:</b><br>t = d̄ / (s_d / √n)<br><br><b>R Function (Base R):</b><br><code>t.test(vector1, vector2, paired = TRUE, alternative = 'two.sided')</code><br><i>Ensure vector1 and vector2 correspond pair-wise.</i><br><br><b>Confidence Interval:</b><br>d̄ ± t*(s_d / √n)<br>Calculated within `t.test` output for the difference."
    ),
    `Independent_Two_Sample` = list( # Two Means, Independent
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> F((Two))\n  F --> O[Are the data paired?]\n  O --> Q((No))\n  Q --> V[Independent Samples t-Test]",
        question = "Result: Independent Samples t-Test",
        result = "Independent Samples t-Test",
        formula = "<b>Hypotheses:</b><br>H₀: μ₁ = μ₂<br>Hₐ: μ₁ ≠ μ₂ (or < or >)<br><br><b>Test Statistic (Welch's t-test is default in R):</b><br>t ≈ (x̄₁ - x̄₂) / √(s₁²/n₁ + s₂²/n₂)<br><br><b>R Function (Base R):</b><br><code>t.test(vector1, vector2, paired = FALSE, alternative = 'two.sided')</code><br><i>Or using formula:</i><br><code>t.test(dependent_variable ~ grouping_variable, data = your_data)</code><br><br><b>Confidence Interval:</b><br>(x̄₁ - x̄₂) ± t*√(s₁²/n₁ + s₂²/n₂)<br>Calculated within `t.test` output for the difference."
    ),
    `Three_or_More_(Means)` = list(
        diagram = "graph LR\n  A[Start] --> B((Quantitative))\n  B --> D[How many means?]\n  D --> G((Three or More))\n  G --> R[ANOVA Test]",
        question = "Result: ANOVA",
        result = "ANOVA",
        formula = "<b>Hypotheses:</b><br>H₀: All means are equal.<br>Hₐ: At least one mean is different.<br><br><b>R Function:</b><br><code>model <- aov(dependent_variable ~ grouping_variable, data = your_data)</code><br><code>summary(model)</code><br><br><b>Confidence Interval:</b> Not typically calculated directly from standard ANOVA output; use post-hoc tests (e.g., TukeyHSD) if needed.<br><code>TukeyHSD(model)</code>"
    ),
    `One_Prop` = list( # One Proportion
        diagram = "graph LR\n  A[Start] --> C((Categorical))\n  C --> H[How many proportions?]\n  H --> I((One))\n  I --> W[One Proportion Z-Test]",
        question = "Result: One Proportion Z-Test",
        result = "One Proportion Z-Test",
        formula = "<b>Hypotheses:</b><br>H₀: p = p₀<br>Hₐ: p ≠ p₀ (or < or >)<br><br><b>Test Statistic:</b><br>z = (p̂ - p₀) / √(p₀(1-p₀)/n)<br><br><b>R Function (Base R):</b><br><code>prop.test(x = number_of_successes, n = sample_size, p = p0, alternative = 'two.sided', correct = FALSE)</code><br><i>Use `correct = FALSE` for standard Z-test, `TRUE` (default) adds continuity correction.</i><br><br><b>Confidence Interval:</b><br>Calculated within `prop.test` output (Wilson score interval is default)."
    ),
    `Two_Prop` = list( # Two Proportions
        diagram = "graph LR\n  A[Start] --> C((Categorical))\n  C --> H[How many proportions?]\n  H --> J((Two))\n  J --> X[Two Proportion Z-Test]",
        question = "Result: Two Proportion Z-Test",
        result = "Two Proportion Z-Test",
        formula = "<b>Hypotheses:</b><br>H₀: p₁ = p₂<br>Hₐ: p₁ ≠ p₂ (or < or >)<br><br><b>Test Statistic:</b><br>z = (p̂₁ - p̂₂) / √(p̂_pooled(1-p̂_pooled)(1/n₁ + 1/n₂))<br><i>where p̂_pooled = (x₁ + x₂) / (n₁ + n₂)</i><br><br><b>R Function (Base R):</b><br><code>prop.test(x = c(successes1, successes2), n = c(n1, n2), alternative = 'two.sided', correct = FALSE)</code><br><i>Use `correct = FALSE` for standard Z-test.</i><br><br><b>Confidence Interval:</b><br>Calculated within `prop.test` output for the difference in proportions."
    ),
    `Three_or_More_Prop` = list( # Three+ Proportions (Chi-Square)
        diagram = "graph LR\n  A[Start] --> C((Categorical))\n  C --> H[How many proportions?]\n  H --> K((Three or More))\n  K --> Y[Chi-Square Test of Homogeneity/Independence]",
        question = "Result: Chi-Square Test",
        result = "Chi-Square Test",
        formula = "<b>Use Case:</b> Comparing proportions across 3+ groups (Homogeneity) or testing association between two categorical variables (Independence).<br><br><b>Hypotheses (Homogeneity):</b><br>H₀: p₁ = p₂ = ... = p<0xE2><0x82><0x99><br>Hₐ: At least one proportion is different.<br><b>Hypotheses (Independence):</b><br>H₀: The two variables are independent.<br>Hₐ: The two variables are not independent.<br><br><b>Test Statistic:</b><br>χ² = Σ [(Observed - Expected)² / Expected]<br><br><b>R Function (Base R):</b><br><code>chisq.test(your_table_or_matrix)</code><br><i>Data should be in a contingency table format.</i><br><code># Example table creation:</code><br><code># tbl <- table(data$variable1, data$variable2)</code><br><code># chisq.test(tbl)</code><br><br><b>Confidence Interval:</b> Not directly applicable for the overall test. Examine residuals or perform post-hoc tests if needed."
    ),
    # --- Add Regression Node (as per original image) ---
    Regression = list( # Assuming Regression is a distinct starting point or path not fully shown
        diagram = "graph LR\n Z[Regression]", # Simple diagram for now
        question = "Result: Regression",
        result = "Regression",
        formula = "<b>Use Case:</b> Modeling the relationship between a dependent variable (usually continuous) and one or more independent variables.<br><br><b>Simple Linear Regression Model:</b><br>Y = β₀ + β₁X + ε<br><br><b>Hypotheses (for slope β₁):</b><br>H₀: β₁ = 0 (No linear relationship)<br>Hₐ: β₁ ≠ 0 (Linear relationship exists)<br><br><b>R Function (Base R):</b><br><code>model <- lm(dependent_variable ~ independent_variable, data = your_data)</code><br><code>summary(model) # Provides coefficients, t-stats, p-values</code><br><br><b>Confidence Interval (for slope β₁):</b><br>Calculated within `summary(model)` output or using `confint(model)`."
    )
)

# --- Server ---
server <- function(input, output, session) {
    
    current_state <- reactiveVal("start")
    final_result <- reactiveVal(NULL)
    # --- Initialize History Stack ---
    history_stack <- reactiveVal(c("start")) # Use a character vector
    # --- Track last button ---
    last_button_info <- reactiveVal(list(id = "None", time = Sys.time()))
    
    # --- Store observer handles in an environment ---
    # Using an environment is efficient for adding/removing named objects
    answer_observers <- new.env(parent = emptyenv())
    
    # Dynamic Question UI (No changes needed here from the fixed version)
    output$questionUI <- renderUI({
        state <- current_state()
        if (!state %in% names(decision_data)) {
            current_state("start")
            history_stack(c("start")) # Reset history if state invalid
            state <- "start"
        }
        state_data <- decision_data[[state]]
        if (!is.null(state_data$result)) {
            # This should ideally not happen if logic elsewhere is correct, but handle it
            if(is.null(final_result())) final_result(state_data$result)
            return(NULL)
        }
        if (is.null(state_data) || is.null(state_data$id) || is.null(state_data$answers)) {
            return(p("Error: Invalid state definition or missing ID/answers."))
        }
        wellPanel(
            h4(state_data$question),
            lapply(state_data$answers, function(answer) {
                sanitized_answer <- gsub("[^[:alnum:]_]", "_", answer)
                button_id <- paste0(state_data$id, "_", sanitized_answer)
                actionButton(inputId = button_id, label = answer, class = "btn-info")
            })
        )
    })
    
    # Diagram Output (No changes needed)
    output$diagram <- renderDiagrammeR({
        state_data <- decision_data[[current_state()]]
        if (!is.null(state_data$diagram) && nzchar(state_data$diagram)) {
            mermaid(state_data$diagram)
        } else {
            mermaid("graph LR\n error[Diagram not found]")
        }
    })
    
    # Observe Answer Buttons (Update to modify history)
    # --- Observe and Manage Answer Button Listeners ---
    observe({
        # 1. Destroy existing answer observers from the previous state
        # This prevents accumulating listeners when current_state changes
        # print(paste("State changed to:", current_state(), "Destroying old observers...")) # Debug
        for (obs_name in ls(answer_observers)) {
            # print(paste("  Destroying:", obs_name)) # Debug
            if (!is.null(answer_observers[[obs_name]])) {
                try(answer_observers[[obs_name]]$destroy(), silent = TRUE)
            }
            # Remove the reference from the environment
            rm(list = obs_name, envir = answer_observers)
        }
        # print("Old observers destroyed.") # Debug
        
        
        # 2. Create new observers for the current state
        current_state_val <- current_state()
        state_data <- decision_data[[current_state_val]]
        
        # Only proceed if it's a state that should have answer buttons
        if (!is.null(state_data) && !is.null(state_data$id) && !is.null(state_data$answers) && is.null(state_data$result)) {
            state_id_for_buttons <- state_data$id # Capture state ID for use in observer creation
            # print(paste("Creating observers for state:", current_state_val, "ID:", state_id_for_buttons)) # Debug
            
            for (answer in state_data$answers) {
                # Use local to ensure 'ans' and 'button_id_to_observe' are correctly scoped for each observer
                local({
                    ans <- answer
                    sanitized_answer <- gsub("[^[:alnum:]_]", "_", ans)
                    button_id_to_observe <- paste0(state_id_for_buttons, "_", sanitized_answer)
                    # print(paste("  Setting up observer for:", button_id_to_observe)) # Debug
                    
                    # Create the observer and store its handle in the environment
                    answer_observers[[button_id_to_observe]] <- observeEvent(input[[button_id_to_observe]], {
                        # print(paste("Button clicked:", button_id_to_observe)) # Debug
                        
                        # --- Inside the button click handler ---
                        last_button_info(list(id = button_id_to_observe, time = Sys.time()))
                        
                        # --- Determine Next State ---
                        # It's crucial to know which state *this listener was created for*.
                        # We use the state_id_for_buttons captured when the listener was defined.
                        originating_state_name <- names(which(sapply(decision_data, function(x) !is.null(x$id) && x$id == state_id_for_buttons)))
                        if (length(originating_state_name) != 1) {
                            # Fallback, though should not happen with unique IDs
                            current <- current_state() # Use live value as fallback
                            warning(paste("Could not uniquely determine originating state for ID:", state_id_for_buttons))
                        } else {
                            current <- originating_state_name[1]
                        }
                        # print(paste("  Originating state:", current)) # Debug
                        # print(paste("  Answer selected:", ans)) # Debug
                        
                        
                        potential_next_state <- gsub("[^[:alnum:]_]", "_", ans)
                        next_state <- potential_next_state # Default assumption
                        
                        # Key Mapping Logic
                        next_state <- potential_next_state
                        if (current == "One" && ans == "Yes") next_state <- "One_Sigma_Known"
                        if (current == "One" && ans == "No") next_state <- "One_Sigma_Unknown"
                        if (current == "Two" && ans == "Yes") next_state <- "Paired_Two_Sample"
                        if (current == "Two" && ans == "No") next_state <- "Independent_Two_Sample"
                        if (current == "Quantitative" && ans == "Three or More") next_state <- "Three_or_More_(Means)"
                        if (current == "Categorical" && ans == "One") next_state <- "One_Prop"
                        if (current == "Categorical" && ans == "Two") next_state <- "Two_Prop"
                        if (current == "Categorical" && ans == "Three or More") next_state <- "Three_or_More_Prop"
                        # Add specific mappings if you have other "Yes"/"No" or reused answers
                        
                        if (!next_state %in% names(decision_data)) {
                            showNotification(paste("Error: Decision path not defined for:", ans, "from:", current), type = "error", duration=10)
                            return()
                        }
                        
                        # --- Update History ---
                        current_history <- history_stack()
                        # Check if we are actually moving forward (next state is different from current last state)
                        if (tail(current_history, 1) != next_state) {
                            new_history <- c(current_history, next_state)
                            history_stack(new_history)
                            # print(paste("  History updated:", paste(new_history, collapse=", "))) # Debug
                        } else {
                            # Optional: Handle potential duplicate clicks if needed
                            # print("  Duplicate state detected, history not updated.") # Debug
                            # return() # Maybe stop if it's a duplicate? Depends on desired behavior.
                        }
                        
                        # --- Update Current State ---
                        current_state(next_state)
                        
                        # Check if the new state is a final result
                        new_state_data <- decision_data[[next_state]]
                        if (!is.null(new_state_data$result)) {
                            final_result(new_state_data$result)
                            # print(paste("  Final result reached:", new_state_data$result)) # Debug
                        } else {
                            final_result(NULL) # Clear result if moving to non-final state
                        }
                        
                    }, ignoreInit = TRUE) # ignoreInit=TRUE prevents firing on creation
                }) # end local
            } # end for loop
        } # end if state_data is valid for creating observers
    }) # end outer observe
    
    # --- Back Button Logic ---
    observeEvent(input$backButton, {
        current_history <- history_stack()
        if (length(current_history) > 1) {
            # Remove last state
            new_history <- head(current_history, -1)
            history_stack(new_history)
            
            # Get the new latest state
            new_current <- tail(new_history, 1)
            current_state(new_current)
            
            # We are no longer in a final state after going back
            final_result(NULL)
            
            # Update debug info
            last_button_info(list(id = "Back Button Pressed", time = Sys.time()))
        }
    })
    
    # Formula Information (No changes needed)
    output$formulaInfo <- renderUI({
        result_key <- current_state()
        if (!is.null(final_result()) && result_key %in% names(decision_data)) {
            state_data <- decision_data[[result_key]]
            if (!is.null(state_data$formula)) {
                HTML(state_data$formula)
            } else {
                HTML(paste("<b>", state_data$result, "</b><br>No detailed formula/code information available."))
            }
        } else {
            NULL
        }
    })
    
    # --- Reactive Flags for Conditional UI ---
    output$isFinal <- reactive({ !is.null(final_result()) })
    outputOptions(output, "isFinal", suspendWhenHidden = FALSE)
    
    output$canGoBack <- reactive({ length(history_stack()) > 1 })
    outputOptions(output, "canGoBack", suspendWhenHidden = FALSE)
    
    
    # --- Reset Button (Update to reset history) ---
    observeEvent(input$resetButton, {
        current_state("start")
        final_result(NULL)
        history_stack(c("start")) # Reset history
        last_button_info(list(id = "Reset Pressed", time = Sys.time()))
    })
    
    
    # --- Debugging Output (Update to show history) ---
    output$debugOutput <- renderPrint({
        cat("--- Current Reactive Values ---\n")
        cs <- current_state()
        cat("Current State (current_state()):", cs, "\n")
        cat("Final Result (final_result()):", if(is.null(final_result())) "NULL" else final_result(), "\n")
        cat("History Stack (history_stack()):\n")
        print(history_stack()) # Print the history vector
        
        last_btn <- last_button_info()
        cat("\n--- Last Button Event ---\n")
        cat("ID:", last_btn$id, "\n")
        cat("Time:", format(last_btn$time, "%Y-%m-%d %H:%M:%S"), "\n")
        
        cat("\n--- Data for Current State ('", cs, "') ---\n")
        current_data <- tryCatch({ decision_data[[cs]] }, error = function(e) { list(Error = e$message) })
        
        if (is.list(current_data)) {
            str(current_data, max.level = 2)
            if (!is.null(current_data$id) && !is.null(current_data$answers)) {
                cat("\n--- Expected Button IDs for this state ---\n")
                for(answer in current_data$answers) {
                    sanitized_answer <- gsub("[^[:alnum:]_]", "_", answer)
                    cat(paste0("  ", current_data$id, "_", sanitized_answer, "\n"))
                }
            }
        } else {
            print(current_data)
        }
    })
    
}