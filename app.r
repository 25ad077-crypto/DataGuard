# ============================================================
# DataGuard - Automated Data Quality and Drift Detection
# Web Application using R Shiny
# ============================================================

library(shiny)
library(ggplot2)

# -------------------- UI --------------------

ui <- fluidPage(
  
  titlePanel("DataGuard | Data Quality Monitor"),
  
  tags$style(HTML("
    body { background-color: #f4f7fb; }
    .well { background-color: white; border-radius: 10px; }
    h2, h3 { color: #183153; }
    .metric {
      background: white;
      padding: 18px;
      border-radius: 10px;
      margin-bottom: 15px;
      box-shadow: 0 2px 8px #dce3ed;
      text-align: center;
    }
    .metric h2 { color: #1677b8; }
  ")),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Upload datasets"),
      
      fileInput(
        "reference",
        "Reference CSV",
        accept = ".csv"
      ),
      
      fileInput(
        "current",
        "New CSV",
        accept = ".csv"
      ),
      
      checkboxInput(
        "allow_extra",
        "Allow additional columns",
        value = TRUE
      ),
      
      numericInput(
        "missing_limit",
        "Missing-value warning threshold (%)",
        value = 5, min = 0, max = 100
      ),
      
      numericInput(
        "drift_limit",
        "Categorical drift threshold",
        value = 0.15, min = 0, max = 1
      ),
      
      actionButton(
        "validate",
        "Validate Data",
        class = "btn-primary"
      ),
      
      br(), br(),
      
      downloadButton(
        "download_issues",
        "Download issue report"
      ),
      
      br(), br(),
      
      downloadButton(
        "download_drift",
        "Download drift report"
      ),
      
      br(), br(),
      
      downloadButton(
        "download_score",
        "Download scorecard"
      ),
      
      br(), br(),
      
      helpText(
        "Upload a baseline CSV and a new CSV to compare them."
      )
    ),
    
    mainPanel(
      
      h2("DataGuard Dashboard"),
      
      textOutput("status"),
      
      fluidRow(
        column(
          4,
          div(
            class = "metric",
            h4("Quality Score"),
            h2(textOutput("score", inline = TRUE)),
            p("Out of 100")
          )
        ),
        
        column(
          4,
          div(
            class = "metric",
            h4("Missing Values"),
            h2(textOutput("missing", inline = TRUE)),
            p("Percentage of cells")
          )
        ),
        
        column(
          4,
          div(
            class = "metric",
            h4("Duplicate Rows"),
            h2(textOutput("duplicates", inline = TRUE)),
            p("In new dataset")
          )
        )
      ),
      
      tabsetPanel(
        
        tabPanel(
          "Quality Issues",
          br(),
          h3("Validation Results"),
          tableOutput("issues")
        ),
        
        tabPanel(
          "Missing Values",
          br(),
          h3("Missing Values by Column"),
          plotOutput("missing_plot"),
          tableOutput("missing_table")
        ),
        
        tabPanel(
          "Data Drift",
          br(),
          h3("Reference vs New Dataset"),
          p("Numeric drift uses the Kolmogorov-Smirnov test. Categorical drift uses changes in category proportions."),
          tableOutput("drift_table")
        ),
        
        tabPanel(
          "Schema",
          br(),
          h3("Schema Validation"),
          tableOutput("schema_table")
        ),
        
        tabPanel(
          "Data Preview",
          br(),
          h3("New Dataset Preview"),
          tableOutput("preview")
        )
      )
    )
  )
)

# -------------------- SERVER --------------------

server <- function(input, output, session) {
  
  result <- eventReactive(input$validate, {
    
    req(input$reference, input$current)
    
    ref <- read.csv(
      input$reference$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    dat <- read.csv(
      input$current$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    required <- names(ref)
    
    missing_cols <- setdiff(required, names(dat))
    extra_cols <- setdiff(names(dat), required)
    
    schema_ok <- length(missing_cols) == 0 &&
      (input$allow_extra || length(extra_cols) == 0)
    
    issues <- data.frame(
      Check = character(),
      Column = character(),
      Severity = character(),
      Details = character(),
      stringsAsFactors = FALSE
    )
    
    add_issue <- function(check, column, severity, details) {
      issues <<- rbind(
        issues,
        data.frame(
          Check = check,
          Column = column,
          Severity = severity,
          Details = details,
          stringsAsFactors = FALSE
        )
      )
    }
    
    # 1. Schema validation
    
    if (length(missing_cols) > 0) {
      add_issue(
        "Schema", "Multiple", "CRITICAL",
        paste("Missing columns:",
              paste(missing_cols, collapse = ", "))
      )
    }
    
    if (length(extra_cols) > 0 && !input$allow_extra) {
      add_issue(
        "Schema", "Multiple", "WARNING",
        paste("Unexpected columns:",
              paste(extra_cols, collapse = ", "))
      )
    }
    
    # 2. Missing values
    
    missing_counts <- colSums(is.na(dat))
    
    missing_table <- data.frame(
      Column = names(dat),
      Missing_Count = as.integer(missing_counts),
      Missing_Percent = round(
        100 * missing_counts / max(1, nrow(dat)), 2
      )
    )
    
    for (col in names(dat)) {
      
      count <- sum(is.na(dat[[col]]))
      
      if (count > 0) {
        
        pct <- 100 * count / max(1, nrow(dat))
        
        severity <- if (pct >= input$missing_limit) {
          "WARNING"
        } else {
          "INFO"
        }
        
        add_issue(
          "Missing Values", col, severity,
          paste(count, "missing values (",
                round(pct, 2), "%)")
        )
      }
    }
    
    # 3. Duplicate rows
    
    duplicate_count <- sum(duplicated(dat))
    
    if (duplicate_count > 0) {
      add_issue(
        "Duplicates", "All columns", "WARNING",
        paste(duplicate_count, "duplicate rows")
      )
    }
    
    # 4. Data type and numeric range checks
    
    invalid_count <- 0
    
    for (col in intersect(names(ref), names(dat))) {
      
      old <- ref[[col]]
      new <- dat[[col]]
      
      if (is.numeric(old)) {
        
        if (!is.numeric(new)) {
          
          add_issue(
            "Data Type", col, "CRITICAL",
            "Expected numeric data"
          )
          
          invalid_count <- invalid_count +
            sum(!is.na(new))
          
        } else {
          
          limits <- range(old, na.rm = TRUE)
          
          if (all(is.finite(limits))) {
            
            bad <- !is.na(new) &
              (new < limits[1] | new > limits[2])
            
            if (any(bad)) {
              
              invalid_count <- invalid_count +
                sum(bad)
              
              add_issue(
                "Range Check", col, "WARNING",
                paste(sum(bad),
                      "values outside reference range",
                      paste(limits, collapse = " to "))
              )
            }
          }
        }
      }
    }
    
    # 5. Drift detection
    
    drift <- data.frame(
      Column = character(),
      Test = character(),
      Difference = numeric(),
      P_Value = numeric(),
      Alert = character(),
      stringsAsFactors = FALSE
    )
    
    for (col in intersect(names(ref), names(dat))) {
      
      old <- ref[[col]]
      new <- dat[[col]]
      
      if (is.numeric(old) && is.numeric(new)) {
        
        old_clean <- old[is.finite(old)]
        new_clean <- new[is.finite(new)]
        
        if (length(old_clean) >= 2 &&
            length(new_clean) >= 2) {
          
          test <- tryCatch(
            suppressWarnings(
              ks.test(old_clean, new_clean)
            ),
            error = function(e) NULL
          )
          
          if (!is.null(test)) {
            
            alert <- test$p.value < 0.05
            
            drift <- rbind(
              drift,
              data.frame(
                Column = col,
                Test = "KS test",
                Difference = round(
                  mean(new_clean) - mean(old_clean), 4
                ),
                P_Value = test$p.value,
                Alert = if (alert) "ALERT" else "OK"
              )
            )
            
            if (alert) {
              add_issue(
                "Distribution Drift", col, "WARNING",
                paste("KS test p-value:",
                      signif(test$p.value, 4))
              )
            }
          }
        }
        
      } else if (
        (is.character(old) || is.factor(old)) &&
        (is.character(new) || is.factor(new))
      ) {
        
        old <- as.character(old)
        new <- as.character(new)
        
        old <- old[!is.na(old)]
        new <- new[!is.na(new)]
        
        categories <- union(unique(old), unique(new))
        
        if (length(old) > 0 && length(new) > 0) {
          
          old_prop <- prop.table(
            table(factor(old, levels = categories))
          )
          
          new_prop <- prop.table(
            table(factor(new, levels = categories))
          )
          
          difference <- max(
            abs(as.numeric(old_prop) -
                  as.numeric(new_prop))
          )
          
          alert <- difference > input$drift_limit
          
          drift <- rbind(
            drift,
            data.frame(
              Column = col,
              Test = "Category proportion",
              Difference = round(difference, 4),
              P_Value = NA_real_,
              Alert = if (alert) "ALERT" else "OK"
            )
          )
          
          if (alert) {
            add_issue(
              "Category Drift", col, "WARNING",
              paste("Maximum proportion change:",
                    round(difference, 4))
            )
          }
        }
      }
    }
    
    # 6. Missingness drift
    
    for (col in intersect(names(ref), names(dat))) {
      
      old_rate <- mean(is.na(ref[[col]]))
      new_rate <- mean(is.na(dat[[col]]))
      
      if (abs(new_rate - old_rate) > 0.05) {
        add_issue(
          "Missingness Drift", col, "WARNING",
          paste("Missing rate changed from",
                round(100 * old_rate, 2), "% to",
                round(100 * new_rate, 2), "%")
        )
      }
    }
    
    # 7. Quality score
    
    missing_pct <- 100 * sum(is.na(dat)) /
      max(1, length(dat))
    
    duplicate_pct <- 100 * duplicate_count /
      max(1, nrow(dat))
    
    schema_penalty <- if (schema_ok) 0 else 20
    
    score <- max(
      0,
      round(
        100 - missing_pct - duplicate_pct -
          2 * invalid_count - schema_penalty
      )
    )
    
    list(
      ref = ref,
      dat = dat,
      issues = issues,
      drift = drift,
      missing = missing_table,
      score = score,
      missing_pct = missing_pct,
      duplicates = duplicate_count,
      schema_ok = schema_ok,
      missing_cols = missing_cols,
      extra_cols = extra_cols
    )
  })
  
  # -------------------- OUTPUTS --------------------
  
  output$status <- renderText({
    if (is.null(result())) {
      "Upload both CSV files and click Validate Data."
    } else {
      "Validation completed successfully."
    }
  })
  
  output$score <- renderText({
    req(result())
    result()$score
  })
  
  output$missing <- renderText({
    req(result())
    paste0(round(result()$missing_pct, 2), "%")
  })
  
  output$duplicates <- renderText({
    req(result())
    result()$duplicates
  })
  
  output$issues <- renderTable({
    req(result())
    result()$issues
  }, striped = TRUE, bordered = TRUE)
  
  output$missing_table <- renderTable({
    req(result())
    result()$missing
  }, striped = TRUE)
  
  output$missing_plot <- renderPlot({
    req(result())
    
    ggplot(
      result()$missing,
      aes(
        x = reorder(Column, Missing_Percent),
        y = Missing_Percent
      )
    ) +
      geom_col(fill = "#2185d0") +
      coord_flip() +
      labs(
        title = "Missing Values by Column",
        x = "Column",
        y = "Missing (%)"
      ) +
      theme_minimal()
  })
  
  output$drift_table <- renderTable({
    req(result())
    result()$drift
  }, striped = TRUE, bordered = TRUE)
  
  output$schema_table <- renderTable({
    req(result())
    
    data.frame(
      Check = c(
        "Required columns",
        "Unexpected columns",
        "Overall schema"
      ),
      Result = c(
        if (length(result()$missing_cols) == 0) {
          "PASS"
        } else {
          paste("Missing:",
                paste(result()$missing_cols,
                      collapse = ", "))
        },
        if (length(result()$extra_cols) == 0) {
          "None"
        } else {
          paste(result()$extra_cols, collapse = ", ")
        },
        if (result()$schema_ok) "PASS" else "FAIL"
      )
    )
  }, striped = TRUE)
  
  output$preview <- renderTable({
    req(result())
    head(result()$dat, 10)
  }, striped = TRUE)
  
  # -------------------- DOWNLOAD REPORTS --------------------
  
  output$download_issues <- downloadHandler(
    filename = function() "dataguard_issues.csv",
    content = function(file) {
      req(result())
      write.csv(result()$issues, file, row.names = FALSE)
    }
  )
  
  output$download_drift <- downloadHandler(
    filename = function() "dataguard_drift.csv",
    content = function(file) {
      req(result())
      write.csv(result()$drift, file, row.names = FALSE)
    }
  )
  
  output$download_score <- downloadHandler(
    filename = function() "dataguard_scorecard.csv",
    content = function(file) {
      req(result())
      
      x <- result()
      
      scorecard <- data.frame(
        Run_Time = as.character(Sys.time()),
        Rows = nrow(x$dat),
        Columns = ncol(x$dat),
        Quality_Score = x$score,
        Missing_Percent = round(x$missing_pct, 2),
        Duplicate_Rows = x$duplicates,
        Schema_Status = if (x$schema_ok) {
          "PASS"
        } else {
          "FAIL"
        },
        Drift_Alerts = sum(x$drift$Alert == "ALERT")
      )
      
      write.csv(scorecard, file, row.names = FALSE)
    }
  )
}

# -------------------- RUN APP --------------------

shinyApp(ui = ui, server = server)
