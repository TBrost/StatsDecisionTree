# --- Libraries ---
library(shiny)
library(DiagrammeR) # This package contains both grViz and mermaid
library(shinyjs)    # For conditional panels

# --- UI ---
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Statistical Test Decision Tool - ALPHA"),
    sidebarLayout(
        sidebarPanel(
            width = 4, # Give sidebar a bit more space if needed
            # Show Back button ONLY if not on the first step AND not on final result
            conditionalPanel(
                condition = "output.canGoBack",
                actionButton("backButton", "Back", icon = icon("arrow-left"), class = "btn-warning")
            ),
            # Show questions ONLY if not on the final result page
            conditionalPanel(
                condition = "!output.isFinal",
                uiOutput("questionUI")
            ),
            # Show Reset button ONLY on the final result page
            conditionalPanel(
                condition = "output.isFinal",
                actionButton("resetButton", "Start Over", icon = icon("refresh"), class = "btn-primary btn-lg"),
                hr()
            ),
            # --- Debugging Section (Optional) ---
            hr(),
            tags$h5("Debugging Info:"), # Use h5 for smaller debug title
            verbatimTextOutput("debugOutput")
            # --- End Debugging ---
        ),
        mainPanel(
            width = 8,
            tags$h4("Decision Path Diagram:"),
            DiagrammeROutput("diagram", height = "450px"),
            hr(),
            conditionalPanel(
                condition = "output.isFinal",
                tags$h4("Test Information:"),
                wellPanel(
                    htmlOutput("formulaInfo")
                )
            )
        )
    )
)