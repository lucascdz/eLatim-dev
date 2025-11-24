# Complete Semantic Tagging Module for lexicographR
# Supports three methods: Zero-shot, With Translations, With Training Examples
# VERSION: Fixed for "ID: " prefix parsing issue + namespace-safe

# Encoding/Decoding functions for safe filenames
SafeEncodeUTF8 <- function(string){
  paste(utf8ToInt(string), collapse = "-")
}

SafeDecodeUTF8 <- function(dashedNumstring){
  intToUtf8(as.numeric(unlist(strsplit(dashedNumstring,"-"))))
}

MakeSafeForFilename <- function(string){
  paste0("_xx",SafeEncodeUTF8(string), "xx_")
}

DecodeSafeFilename <- function(SafeFilename){
  string <- gsub(".*(_xx[\\d|-]+xx_).*", "\\1", SafeFilename)
  string <- gsub("_xx|xx_","",string)
  return(SafeDecodeUTF8(string))
}

# UI Function
SemanticTaggingUI <- function(id){
  tagList(
    div(HTML("
      <h4>Semantic Sense Tagging with AI</h4>
      <p>This tool uses AI to automatically tag corpus sentences with appropriate word senses.</p>
      <hr>
    ")),
    
    fluidRow(
      column(width=6,
             h5("Processing Method Selection"),
             radioButtons(NS(id, "processingMethod"), "Choose Tagging Method",
                          choices = list("Zero-shot (no additional data)" = "zero_shot",
                                         "With Translations" = "with_translations", 
                                         "With Training Examples" = "with_training"),
                          selected = "zero_shot"),
             
             conditionalPanel(condition = "input.processingMethod == 'zero_shot'", ns = NS(id),
                              div(class = "alert alert-secondary", HTML("<strong>Zero-shot:</strong> Uses only sense definitions."))),
             
             conditionalPanel(condition = "input.processingMethod == 'with_translations'", ns = NS(id),
                              div(class = "alert alert-warning", HTML("<strong>With Translations:</strong> Tags sentences FROM translation files."))),
             
             conditionalPanel(condition = "input.processingMethod == 'with_training'", ns = NS(id),
                              div(class = "alert alert-info", HTML("<strong>With Training:</strong> Tags sentences FROM examples files."))),
             
             hr(),
             h5("Data Input"),
             fileInput(NS(id, "senseDefsFile"), "Upload sense definitions (CSV)", accept = c(".csv")),
             selectInput(NS(id, "headwordColumn"), "Headword column name", choices = NULL),
             selectInput(NS(id, "definitionColumn"), "Definition column name", choices = NULL),
             
             conditionalPanel(condition = "output.senseFileUploaded", ns = NS(id),
                              hr(), h5("Lemma Selection"),
                              selectInput(NS(id, "lemmaSelect"), "Select lemma to process:", choices = NULL)),
             
             conditionalPanel(condition = "input.processingMethod == 'with_training' && output.senseFileUploaded", ns = NS(id),
                              selectInput(NS(id, "trainingFileSelect"), "Select training data file:", choices = NULL)),
             
             hr(),
             h5("Processing Scope"),
             radioButtons(NS(id, "processingScope"), "Scope",
                          choices = list("Test (sample sentences)" = "TEST", "All sentences" = "ALL"), selected = "TEST"),
             
             conditionalPanel(condition = "input.processingScope == 'TEST'", ns = NS(id),
                              numericInput(NS(id, "sampleSize"), "Number of sample sentences", value = 50, min = 10, max = 500))
      ),
      
      column(width=6,
             h5("AI Provider Configuration"),
             radioButtons(NS(id, "aiProvider"), "Choose AI Provider",
                          choices = list("Google Gemini (Free)" = "gemini", "Anthropic Claude (Paid)" = "claude"), selected = "gemini"),
             
             conditionalPanel(condition = "input.aiProvider == 'claude'", ns = NS(id),
                              passwordInput(NS(id, "claudeApiKey"), "Anthropic API Key", placeholder = "sk-ant-..."),
                              selectInput(NS(id, "claudeModel"), "Claude Model",
                                          choices = list("Claude Sonnet 4.5" = "claude-sonnet-4-5-20250929",
                                                         "Claude Opus 4.1" = "claude-opus-4-1-20250805",
                                                         "Claude Sonnet 4" = "claude-sonnet-4-20250514"),
                                          selected = "claude-sonnet-4-5-20250929")),
             
             conditionalPanel(condition = "input.aiProvider == 'gemini'", ns = NS(id),
                              passwordInput(NS(id, "geminiApiKey"), "Google AI Studio API Key"),
                              selectInput(NS(id, "geminiModel"), "Gemini Model",
                                          choices = list("Gemini 1.5 Flash" = "gemini-1.5-flash",
                                                         "Gemini 1.5 Pro" = "gemini-1.5-pro"),
                                          selected = "gemini-1.5-flash")),
             
             numericInput(NS(id, "temperature"), "Temperature", value = 0, min = 0, max = 1, step = 0.1),
             numericInput(NS(id, "maxTokens"), "Max tokens", value = 4000, min = 1000, max = 8000),
             hr(),
             h5("Processing Options"),
             numericInput(NS(id, "confidenceThreshold"), "Confidence threshold", value = 0.79, min = 0, max = 1, step = 0.01),
             numericInput(NS(id, "batchSize"), "Sentences per batch", value = 100, min = 10, max = 200)
      )
    ),
    
    hr(),
    fluidRow(column(width=12, h5("Data Availability Status"), htmlOutput(NS(id, "dataStatus")))),
    hr(),
    
    fluidRow(
      column(width=4, actionButton(NS(id, "validateSetup"), "Validate Setup", class = "btn-info"), br(), br(),
             conditionalPanel(condition = "output.setupValid", ns = NS(id),
                              actionButton(NS(id, "runTest"), "Run Test", class = "btn-warning"))),
      column(width=4, conditionalPanel(condition = "output.testComplete", ns = NS(id),
                                       actionButton(NS(id, "processAll"), "Process All Data", class = "btn-success")))
    ),
    
    hr(),
    conditionalPanel(condition = "output.hasResults", ns = NS(id),
                     fluidRow(column(width=12, h4("Results Preview"),
                                     DT::dataTableOutput(NS(id, "resultsPreview")), br(),
                                     downloadButton(NS(id, "downloadResults"), "Download Results", class = "btn-primary")))),
    
    htmlOutput(NS(id, "statusMessage"))
  )
}

# Server Function
SemanticTaggingServer <- function(id, DictData, HeadwordVar, Cores) {
  moduleServer(id, function(input, output, session) {
    
    # Check for required packages - NO require() or library() calls to avoid conflicts
    if (!requireNamespace("httr", quietly = TRUE)) {
      showModal(modalDialog(title = "Missing Package",
                            "httr required. Install with: install.packages('httr')", easyClose = TRUE))
      return()
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      showModal(modalDialog(title = "Missing Package",
                            "jsonlite required. Install with: install.packages('jsonlite')", easyClose = TRUE))
      return()
    }
    
    # DO NOT USE require() or library() - all calls use :: notation to prevent conflicts
    
    values <- reactiveValues(senseData = NULL, setupValid = FALSE, testComplete = FALSE,
                             processingActive = FALSE, results = NULL, examplesData = NULL, 
                             translationsData = NULL, trainingData = NULL, selectedLemma = NULL)
    
    if (!dir.exists("./data/SemanticTagging/with_translation")) {
      dir.create("./data/SemanticTagging/with_translation", recursive = TRUE)
    }
    if (!dir.exists("./data/SemanticTagging/with_trainingdata")) {
      dir.create("./data/SemanticTagging/with_trainingdata", recursive = TRUE)
    }
    if (!dir.exists("./data/SemanticTagging/zero_shot")) {
      dir.create("./data/SemanticTagging/zero_shot", recursive = TRUE)
    }
    
    refreshTrigger <- reactiveVal(0)
    output$senseFileUploaded <- reactive({ !is.null(values$senseData) })
    outputOptions(output, "senseFileUploaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$senseDefsFile, {
      req(input$senseDefsFile)
      tryCatch({
        values$senseData <- read.csv(input$senseDefsFile$datapath, stringsAsFactors = FALSE, encoding = "UTF-8")
        updateSelectInput(session, "headwordColumn", choices = colnames(values$senseData))
        updateSelectInput(session, "definitionColumn", choices = colnames(values$senseData))
        output$statusMessage <- renderUI({
          div(class = "alert alert-success", paste("Loaded", nrow(values$senseData), "sense definitions"))
        })
      }, error = function(e) {
        output$statusMessage <- renderUI({
          div(class = "alert alert-danger", "Error loading sense definitions: ", e$message)
        })
      })
    })
    
    observeEvent(list(values$senseData, input$headwordColumn, input$processingMethod), {
      req(values$senseData, input$headwordColumn)
      all_lemmas <- unique(values$senseData[[input$headwordColumn]])
      
      if (input$processingMethod == "with_translations") {
        available_lemmas <- getAvailableLemmasForTranslation(all_lemmas)
        updateSelectInput(session, "lemmaSelect", choices = available_lemmas)
      } else if (input$processingMethod == "with_training") {
        available_lemmas <- getAvailableLemmasForTraining(all_lemmas)
        updateSelectInput(session, "lemmaSelect", choices = available_lemmas)
      } else {
        available_lemmas <- getAvailableLemmasForExamples(all_lemmas)
        updateSelectInput(session, "lemmaSelect", choices = available_lemmas)
      }
    })
    
    observeEvent(input$lemmaSelect, {
      req(input$lemmaSelect)
      values$selectedLemma <- input$lemmaSelect
      
      if (input$processingMethod == "with_training") {
        training_files <- getTrainingFilesForLemma(input$lemmaSelect)
        if (length(training_files) > 0) {
          updateSelectInput(session, "trainingFileSelect", choices = training_files)
        }
      }
      refreshTrigger(refreshTrigger() + 1)
    })
    
    observe({
      refreshTrigger()
      status_msgs <- c()
      
      if (dir.exists("./data/Outputs/Examples")) {
        example_files <- dir("./data/Outputs/Examples", pattern = "_ExampleSents\\.csv$")
        if (length(example_files) > 0) {
          status_msgs <- c(status_msgs, paste0("<span style='color: green;'>&#10004;</span> Examples: ", length(example_files), " files"))
        } else {
          status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Examples: None found")
        }
      } else {
        status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Examples: Folder missing")
      }
      
      if (dir.exists("./data/Other/Translations")) {
        trans_files <- dir("./data/Other/Translations", pattern = "_Translation\\.csv$")
        if (length(trans_files) > 0) {
          status_msgs <- c(status_msgs, paste0("<span style='color: green;'>&#10004;</span> Translations: ", length(trans_files), " files"))
        } else {
          status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Translations: None found")
        }
      } else {
        status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Translations: Folder missing")
      }
      
      if (dir.exists("./data/SemanticTagging/with_translation")) {
        training_files <- dir("./data/SemanticTagging/with_translation", pattern = "^_xx.*xx_Tagged.*\\.csv$")
        if (length(training_files) > 0) {
          status_msgs <- c(status_msgs, paste0("<span style='color: green;'>&#10004;</span> Training Data: ", length(training_files), " files"))
        } else {
          status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Training Data: None found")
        }
      } else {
        status_msgs <- c(status_msgs, "<span style='color: red;'>&#10008;</span> Training Data: None")
      }
      
      output$dataStatus <- renderUI({ HTML(paste(status_msgs, collapse = "<br/>")) })
    })
    
    observeEvent(input$validateSetup, {
      req(values$senseData, input$headwordColumn, input$definitionColumn)
      valid <- TRUE
      errors <- c()
      
      if (is.null(input$lemmaSelect) || input$lemmaSelect == "") {
        valid <- FALSE
        errors <- c(errors, "Please select a lemma")
      }
      
      if (input$processingMethod == "with_translations") {
        translation_file <- getTranslationFileForLemma(input$lemmaSelect)
        if (is.null(translation_file)) {
          valid <- FALSE
          errors <- c(errors, "Translation file not found")
        } else {
          tryCatch({
            values$translationsData <- read.csv(translation_file, stringsAsFactors = FALSE)
          }, error = function(e) {
            valid <- FALSE
            errors <- c(errors, paste("Error loading translation:", e$message))
          })
        }
      } else {
        example_file <- getExamplesFileForLemma(input$lemmaSelect)
        if (is.null(example_file)) {
          valid <- FALSE
          errors <- c(errors, "Examples file not found")
        } else {
          tryCatch({
            values$examplesData <- read.csv(example_file, stringsAsFactors = FALSE)
          }, error = function(e) {
            valid <- FALSE
            errors <- c(errors, paste("Error loading examples:", e$message))
          })
        }
      }
      
      if (input$processingMethod == "with_training") {
        if (is.null(input$trainingFileSelect) || input$trainingFileSelect == "") {
          valid <- FALSE
          errors <- c(errors, "Please select training file")
        } else {
          tryCatch({
            selected_file <- paste0("./data/SemanticTagging/with_translation/", input$trainingFileSelect)
            values$trainingData <- read.csv(selected_file, stringsAsFactors = FALSE)
            if (!"confidence" %in% colnames(values$trainingData)) {
              valid <- FALSE
              errors <- c(errors, "Training file missing confidence column")
            }
          }, error = function(e) {
            valid <- FALSE
            errors <- c(errors, paste("Error loading training:", e$message))
          })
        }
      }
      
      apiKey <- if(input$aiProvider == "claude") input$claudeApiKey else input$geminiApiKey
      if (is.null(apiKey) || nchar(apiKey) < 10) {
        valid <- FALSE
        errors <- c(errors, "Valid API key required")
      }
      
      if (!all(c(input$headwordColumn, input$definitionColumn) %in% colnames(values$senseData))) {
        valid <- FALSE
        errors <- c(errors, "Selected columns not found")
      }
      
      if (valid) {
        values$setupValid <- TRUE
        output$statusMessage <- renderUI({ div(class = "alert alert-success", "Setup validated!") })
      } else {
        values$setupValid <- FALSE
        output$statusMessage <- renderUI({ div(class = "alert alert-danger", "Errors: ", paste(errors, collapse = "; ")) })
      }
    })
    
    output$setupValid <- reactive({ values$setupValid })
    output$testComplete <- reactive({ values$testComplete })
    output$processingActive <- reactive({ values$processingActive })
    output$hasResults <- reactive({ !is.null(values$results) })
    outputOptions(output, "setupValid", suspendWhenHidden = FALSE)
    outputOptions(output, "testComplete", suspendWhenHidden = FALSE)
    outputOptions(output, "processingActive", suspendWhenHidden = FALSE)
    outputOptions(output, "hasResults", suspendWhenHidden = FALSE)
    
    observeEvent(input$runTest, {
      req(values$setupValid)
      req(!values$processingActive)
      
      values$processingActive <- TRUE
      
      testData <- prepareDataForMethod(
        method = input$processingMethod, selectedLemma = input$lemmaSelect,
        senseData = values$senseData, examplesData = values$examplesData,
        translationsData = values$translationsData, trainingData = values$trainingData,
        headwordCol = input$headwordColumn, definitionCol = input$definitionColumn,
        scope = input$processingScope,
        sampleSize = ifelse(input$processingScope == "TEST", input$sampleSize, NULL),
        confidenceThreshold = input$confidenceThreshold, HeadwordVar = HeadwordVar)
      
      if (is.null(testData) || nrow(testData) == 0) {
        values$processingActive <- FALSE
        output$statusMessage <- renderUI({ div(class = "alert alert-warning", "No test data prepared") })
        return()
      }
      
      tryCatch({
        results <- processSemanticTagging(testData, input$processingMethod, values$senseData,
                                          input$headwordColumn, input$definitionColumn, input$aiProvider,
                                          apiKey = if(input$aiProvider == "claude") input$claudeApiKey else input$geminiApiKey,
                                          modelName = if(input$aiProvider == "claude") input$claudeModel else input$geminiModel,
                                          input$temperature, input$maxTokens, min(input$batchSize, nrow(testData)), HeadwordVar)
        
        values$results <- results
        values$testComplete <- TRUE
        values$processingActive <- FALSE
        
        if (!is.null(results) && nrow(results) > 0) {
          provider <- if(input$aiProvider == "claude") "Claude" else "Gemini"
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          encoded_lemma <- MakeSafeForFilename(input$lemmaSelect)
          
          if (input$processingMethod == "with_translations") {
            filename <- paste0("./data/SemanticTagging/with_translation/", encoded_lemma, "Tagged_WithTranslations_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            refreshTrigger(refreshTrigger() + 1)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "Completed! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          } else if (input$processingMethod == "with_training") {
            filename <- paste0("./data/SemanticTagging/with_trainingdata/", encoded_lemma, "Tagged_WithTraining_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "Completed! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          } else {
            filename <- paste0("./data/SemanticTagging/zero_shot/", encoded_lemma, "Tagged_ZeroShot_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "Completed! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          }
        }
      }, error = function(e) {
        values$processingActive <- FALSE
        output$statusMessage <- renderUI({ div(class = "alert alert-danger", "Error: ", e$message) })
      })
    })
    
    observeEvent(input$processAll, {
      req(values$testComplete, values$setupValid)
      values$processingActive <- TRUE
      
      allData <- prepareDataForMethod(
        method = input$processingMethod, selectedLemma = input$lemmaSelect,
        senseData = values$senseData, examplesData = values$examplesData,
        translationsData = values$translationsData, trainingData = values$trainingData,
        headwordCol = input$headwordColumn, definitionCol = input$definitionColumn,
        scope = "ALL",
        sampleSize = NULL,
        confidenceThreshold = input$confidenceThreshold, HeadwordVar = HeadwordVar)
      
      if (is.null(allData) || nrow(allData) == 0) {
        values$processingActive <- FALSE
        output$statusMessage <- renderUI({ div(class = "alert alert-warning", "No data to process") })
        return()
      }
      
      tryCatch({
        results <- processSemanticTagging(allData, input$processingMethod, values$senseData,
                                          input$headwordColumn, input$definitionColumn, input$aiProvider,
                                          apiKey = if(input$aiProvider == "claude") input$claudeApiKey else input$geminiApiKey,
                                          modelName = if(input$aiProvider == "claude") input$claudeModel else input$geminiModel,
                                          input$temperature, input$maxTokens, input$batchSize, HeadwordVar)
        
        values$results <- results
        values$processingActive <- FALSE
        
        if (!is.null(results) && nrow(results) > 0) {
          provider <- if(input$aiProvider == "claude") "Claude" else "Gemini"
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          encoded_lemma <- MakeSafeForFilename(input$lemmaSelect)
          
          if (input$processingMethod == "with_translations") {
            filename <- paste0("./data/SemanticTagging/with_translation/", encoded_lemma, "Tagged_WithTranslations_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            refreshTrigger(refreshTrigger() + 1)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "ALL DATA COMPLETE! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          } else if (input$processingMethod == "with_training") {
            filename <- paste0("./data/SemanticTagging/with_trainingdata/", encoded_lemma, "Tagged_WithTraining_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "ALL DATA COMPLETE! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          } else {
            filename <- paste0("./data/SemanticTagging/zero_shot/", encoded_lemma, "Tagged_ZeroShot_", provider, "_", timestamp, ".csv")
            write.csv(results, filename, row.names = FALSE)
            output$statusMessage <- renderUI({
              div(class = "alert alert-success", "ALL DATA COMPLETE! ", nrow(results), " sentences. Saved: ", basename(filename))
            })
          }
        }
      }, error = function(e) {
        values$processingActive <- FALSE
        output$statusMessage <- renderUI({ div(class = "alert alert-danger", "Error processing all data: ", e$message) })
      })
    })
    
    output$resultsPreview <- DT::renderDataTable({
      req(values$results)
      DT::datatable(values$results, options = list(pageLength = 10, scrollX = TRUE), escape = FALSE, rownames = FALSE)
    })
    
    output$downloadResults <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        provider <- if(input$aiProvider == "claude") "Claude" else "Gemini"
        encoded_lemma <- MakeSafeForFilename(input$lemmaSelect)
        method_name <- if(input$processingMethod == "with_translations") {
          "WithTranslations"
        } else if(input$processingMethod == "with_training") {
          "WithTraining"
        } else {
          "ZeroShot"
        }
        paste0(encoded_lemma, "Tagged_", method_name, "_", provider, "_", timestamp, ".csv")
      },
      content = function(file) {
        write.csv(values$results, file, row.names = FALSE)
      }
    )
  })
}

# Helper Functions

prepareDataForMethod <- function(method, selectedLemma, senseData, examplesData, translationsData, trainingData,
                                 headwordCol, definitionCol, scope, sampleSize, confidenceThreshold, HeadwordVar) {
  
  if (method == "with_translations") {
    if (is.null(translationsData)) return(NULL)
    translationSubset <- translationsData
    
    if (scope == "TEST" && !is.null(sampleSize) && nrow(translationSubset) > sampleSize) {
      translationSubset <- translationSubset[1:sampleSize, ]
    }
    
    translationSubset$headword <- selectedLemma
    translationSubset$available_senses <- paste(
      senseData[senseData[[headwordCol]] == selectedLemma, ][[definitionCol]], collapse = " | ")
    
    if (!"sentence" %in% colnames(translationSubset)) {
      translationSubset$sentence <- translationSubset$ID
    }
    if (!"translation" %in% colnames(translationSubset)) return(NULL)
    
    return(translationSubset)
  }
  
  if (is.null(examplesData)) return(NULL)
  
  if (HeadwordVar %in% colnames(examplesData)) {
    examplesSubset <- examplesData[examplesData[[HeadwordVar]] == selectedLemma, ]
  } else {
    examplesSubset <- examplesData[examplesData[[1]] == selectedLemma, ]
  }
  
  if (nrow(examplesSubset) == 0) return(NULL)
  
  if (scope == "TEST" && !is.null(sampleSize) && nrow(examplesSubset) > sampleSize) {
    examplesSubset <- examplesSubset[1:sampleSize, ]
  }
  
  examplesSubset$available_senses <- paste(
    senseData[senseData[[headwordCol]] == selectedLemma, ][[definitionCol]], collapse = " | ")
  
  if (method == "with_training") {
    examplesSubset <- addTrainingData(examplesSubset, trainingData, confidenceThreshold)
  }
  
  if (!"sentence" %in% colnames(examplesSubset)) {
    if ("Sent" %in% colnames(examplesSubset)) {
      examplesSubset$sentence <- examplesSubset$Sent
    } else {
      examplesSubset$sentence <- examplesSubset$ID
    }
  }
  
  return(examplesSubset)
}

addTrainingData <- function(examplesData, trainingData, confidenceThreshold) {
  if (is.null(trainingData)) return(examplesData)
  
  if (is.data.frame(trainingData)) {
    all_training <- trainingData
  } else {
    all_training <- do.call(rbind, trainingData)
  }
  
  high_conf_training <- all_training[all_training$confidence >= confidenceThreshold, ]
  
  if (nrow(high_conf_training) > 0) {
    training_examples_list <- paste("ID:", high_conf_training$sentence_id, 
                                    "| Sentence:", high_conf_training$sentence, "| Sense:", high_conf_training$predicted_sense,
                                    "| Confidence:", high_conf_training$confidence)
    training_text <- paste(training_examples_list, collapse = "\n")
    examplesData$training_examples <- training_text
  } else {
    examplesData$training_examples <- ""
  }
  
  return(examplesData)
}

callClaudeAPI <- function(prompt, apiKey, model, temperature, maxTokens) {
  # NAMESPACE-SAFE: Use httr:: and jsonlite:: explicitly to avoid conflicts
  
  request_body <- list(
    model = model, 
    max_tokens = as.integer(maxTokens), 
    temperature = temperature,
    messages = list(list(role = "user", content = prompt))
  )
  
  tryCatch({
    response <- httr::POST(
      url = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "Content-Type" = "application/json",
        "x-api-key" = apiKey,
        "anthropic-version" = "2023-06-01"
      ),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )
    
    # Check for HTTP errors
    if (httr::http_error(response)) {
      error_msg <- paste("HTTP", httr::status_code(response), "error")
      error_content <- httr::content(response, as = "text", encoding = "UTF-8")
      return(list(error = paste(error_msg, "-", error_content)))
    }
    
    content <- httr::content(response, as = "parsed", type = "application/json")
    
    if (!is.null(content$content) && length(content$content) > 0) {
      return(list(text = content$content[[1]]$text))
    } else {
      return(list(error = "No content in API response"))
    }
  }, error = function(e) {
    return(list(error = paste("API call failed:", e$message)))
  })
}

callGeminiAPI <- function(prompt, apiKey, model, temperature, maxTokens) {
  # NAMESPACE-SAFE: Use httr:: and jsonlite:: explicitly to avoid conflicts
  
  request_body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(temperature = temperature, maxOutputTokens = as.integer(maxTokens))
  )
  
  tryCatch({
    response <- httr::POST(
      url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent"),
      query = list(key = apiKey),
      httr::add_headers("Content-Type" = "application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
      encode = "raw"
    )
    
    # Check for HTTP errors
    if (httr::http_error(response)) {
      error_msg <- paste("HTTP", httr::status_code(response), "error")
      error_content <- httr::content(response, as = "text", encoding = "UTF-8")
      return(list(error = paste(error_msg, "-", error_content)))
    }
    
    content <- httr::content(response, as = "parsed", type = "application/json")
    
    if (!is.null(content$candidates) && length(content$candidates) > 0 &&
        !is.null(content$candidates[[1]]$content$parts) && length(content$candidates[[1]]$content$parts) > 0) {
      return(list(text = content$candidates[[1]]$content$parts[[1]]$text))
    } else {
      return(list(error = "No content in API response"))
    }
  }, error = function(e) {
    return(list(error = paste("API call failed:", e$message)))
  })
}

createPromptForMethod <- function(method, data, senseData, headwordCol, definitionCol, HeadwordVar) {
  if ("headword" %in% colnames(data)) {
    headword <- data$headword[1]
  } else if (HeadwordVar %in% colnames(data)) {
    headword <- data[[HeadwordVar]][1]
  } else {
    headword <- "unknown"
  }
  
  available_senses <- data$available_senses[1]
  
  if (method == "zero_shot") {
    return(createZeroShotPrompt(headword, available_senses))
  } else if (method == "with_translations") {
    return(createTranslationPrompt(headword, available_senses))
  } else if (method == "with_training") {
    return(createTrainingPrompt(headword, available_senses, data))
  }
}

createZeroShotPrompt <- function(headword, available_senses) {
  paste0("You will be given sentences containing the word ", headword, ".\n",
         "For each sentence, choose from the following list the sense that best describes the meaning of the word ", headword, " in context:\n\n",
         available_senses, "\n\n",
         "In your answer, please include the correct sense and corresponding ID value of each sentence, ",
         "as well as a value between 0 and 1 indicating your confidence that the tag you chose is correct ",
         "(0 = you are not sure at all, 1 = you are certain it is correct) in this format:\n",
         "ID,sense,confidence\n\n",
         "Process ALL sentences.\n",
         "Do NOT include explanations.")
}

createTranslationPrompt <- function(headword, available_senses) {
  paste0("You will be given sentences containing the word ", headword, ", along with their English translations.\n",
         "You are a semantic tagger.\n\n",
         "For each sentence:\n",
         "First, determine which word or phrase in the English translation corresponds to the word ", headword, "; let us call this the translation-equivalent.\n\n",
         "Second, on the basis of both the translation-equivalent and the original sentence context, determine which of the following senses is closest to the meaning of ", headword, ":\n\n",
         available_senses, "\n\n",
         "In your answer, please include the correct sense, the translation-equivalent, and corresponding ID value of each sentence, ",
         "as well as a value between 0 and 1 indicating your confidence that the tag you chose is correct ",
         "(0 = you are not sure at all, 1 = you are certain it is correct) in this format:\n",
         "ID,sense,translation-equivalent,confidence\n\n",
         "Process ALL sentences.\n",
         "Do NOT include explanations.")
}

createTrainingPrompt <- function(headword, available_senses, data) {
  training_examples <- if("training_examples" %in% colnames(data) && nchar(data$training_examples[1]) > 0) {
    paste0("\n\nHere are training examples with tagged sentences to help you understand which context elements predict each sense:\n", 
           data$training_examples[1], "\n")
  } else {
    ""
  }
  
  paste0("You will be given sentences containing the word ", headword, ".\n",
         "You are a semantic tagger. Your task is to determine the correct sense for each sentence.\n\n",
         "First, look at the training examples below and identify which elements in the immediate context of the word ", headword, " predict each assigned sense.\n\n",
         "Then, for each new sentence, determine which of the following senses is closest to the meaning of ", headword, ":\n\n",
         available_senses,
         training_examples, "\n",
         "In your answer, please include the correct sense and corresponding ID value of each sentence, ",
         "as well as a value between 0 and 1 indicating your confidence that the tag you chose is correct ",
         "(0 = you are not sure at all, 1 = you are certain it is correct) in this format:\n",
         "ID,sense,confidence\n\n",
         "Include only the new sentences (not the training examples) in your answer.\n",
         "Aim to tag all sentences provided.\n",
         "If this exceeds your maximum answer length, tag as many as you can and let me know that some sentences remain unprocessed.\n",
         "Do NOT include explanations.")
}

processSemanticTagging <- function(testData, method, senseData, headwordCol, definitionCol,
                                   aiProvider, apiKey, modelName, temperature, maxTokens, batchSize, HeadwordVar) {
  
  cat("=== ENTERING processSemanticTagging ===\n")
  cat("Test data rows:", nrow(testData), "\n")
  cat("AI Provider:", aiProvider, "\n")
  cat("Batch size:", batchSize, "\n")
  
  if (is.null(testData) || nrow(testData) == 0) return(data.frame())
  
  results <- data.frame()
  prompt <- createPromptForMethod(method, testData, senseData, headwordCol, definitionCol, HeadwordVar)
  
  cat("Prompt created, length:", nchar(prompt), "\n")
  
  num_batches <- ceiling(nrow(testData) / batchSize)
  cat("Number of batches:", num_batches, "\n")
  
  for (i in seq(1, nrow(testData), batchSize)) {
    batch_num <- ceiling(i / batchSize)
    cat("\n--- Processing batch", batch_num, "of", num_batches, "---\n")
    
    endIdx <- min(i + batchSize - 1, nrow(testData))
    batch <- testData[i:endIdx, ]
    cat("Batch rows:", nrow(batch), "\n")
    
    batch_prompt <- paste0(prompt, "\n\nSENTENCES TO TAG:\n", createBatchData(batch, method))
    cat("Batch prompt length:", nchar(batch_prompt), "\n")
    
    cat("Calling", aiProvider, "API...\n")
    
    if (aiProvider == "claude") {
      response <- callClaudeAPI(batch_prompt, apiKey, modelName, temperature, maxTokens)
    } else {
      response <- callGeminiAPI(batch_prompt, apiKey, modelName, temperature, maxTokens)
    }
    
    if ("error" %in% names(response)) {
      cat("API ERROR:", response$error, "\n")
      batch_results <- createErrorResults(batch, response$error, aiProvider, modelName, HeadwordVar)
    } else {
      cat("API SUCCESS - Response length:", nchar(response$text), "\n")
      cat("First 200 chars of response:", substr(response$text, 1, 200), "\n")
      batch_results <- parseAPIResponse(response$text, batch, method, aiProvider, modelName, HeadwordVar)
      cat("Parsed results:", nrow(batch_results), "rows\n")
    }
    
    results <- safeBind(results, batch_results)
    cat("Total results so far:", nrow(results), "rows\n")
    
    # Rate limiting
    sleep_time <- if (aiProvider == "gemini") 4 else 0.5
    cat("Sleeping for", sleep_time, "seconds...\n")
    Sys.sleep(sleep_time)
  }
  
  cat("=== processSemanticTagging COMPLETE ===\n")
  cat("Final results:", nrow(results), "rows\n\n")
  
  return(results)
}

createBatchData <- function(batch, method) {
  data_strings <- c()
  for (i in 1:nrow(batch)) {
    row <- batch[i, ]
    if (method == "with_translations" && "translation" %in% colnames(row)) {
      data_strings <- c(data_strings, paste0("ID: ", row$ID, " | Sentence: ", row$sentence, " | Translation: ", row$translation))
    } else {
      data_strings <- c(data_strings, paste0("ID: ", row$ID, " | Sentence: ", row$sentence))
    }
  }
  return(paste(data_strings, collapse = "\n"))
}

# FIXED parseAPIResponse - handles "ID: " prefix parsing issue
parseAPIResponse <- function(responseText, batch, method, aiProvider, modelName, HeadwordVar) {
  results <- data.frame()
  
  cat("=== PARSING API RESPONSE ===\n")
  cat("Response length:", nchar(responseText), "chars\n")
  
  # Split into lines
  lines <- strsplit(responseText, "\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  
  cat("Parsing", length(lines), "lines from API response\n")
  cat("Batch IDs available:", paste(head(batch$ID, 10), collapse = ", "), "\n")
  
  for (line_num in seq_along(lines)) {
    line <- trimws(lines[line_num])
    
    tryCatch({
      if (!grepl(",", line)) {
        next
      }
      
      # CRITICAL FIX: Remove "ID: " or "ID:" prefix if present (with or without space)
      line <- gsub("^ID:\\s*", "", line)
      
      # Split by comma
      parts <- strsplit(line, ",")[[1]]
      parts <- trimws(gsub('"', '', parts))
      
      if (length(parts) < 3) {
        next
      }
      
      # Extract ID, sense, confidence based on method
      if (method == "with_translations" && length(parts) >= 4) {
        id <- parts[1]
        sense <- parts[2]
        translation_equiv <- parts[3]
        confidence <- as.numeric(parts[4])
        
        batch_row <- batch[batch$ID == id, ]
        
        if (nrow(batch_row) > 0) {
          headword_value <- if("headword" %in% colnames(batch_row)) {
            batch_row$headword[1]
          } else if(HeadwordVar %in% colnames(batch_row)) {
            batch_row[[HeadwordVar]][1]
          } else {
            "unknown"
          }
          
          result <- data.frame(
            sentence_id = id, headword = headword_value, sentence = batch_row$sentence[1],
            predicted_sense = sense, translation_equivalent = translation_equiv, 
            translation = batch_row$translation[1], confidence = confidence,
            ai_provider = aiProvider, model_used = modelName, method = method, 
            stringsAsFactors = FALSE
          )
          results <- safeBind(results, result)
        }
        
      } else if (length(parts) >= 3) {
        id <- parts[1]
        sense <- parts[2]
        confidence <- as.numeric(parts[3])
        
        batch_row <- batch[batch$ID == id, ]
        
        if (nrow(batch_row) > 0) {
          headword_value <- if("headword" %in% colnames(batch_row)) {
            batch_row$headword[1]
          } else if(HeadwordVar %in% colnames(batch_row)) {
            batch_row[[HeadwordVar]][1]
          } else {
            "unknown"
          }
          
          # Handle with_translations method fallback (no translation-equivalent)
          if (method == "with_translations" && "translation" %in% colnames(batch_row)) {
            result <- data.frame(
              sentence_id = id, headword = headword_value, sentence = batch_row$sentence[1],
              predicted_sense = sense, translation = batch_row$translation[1], 
              confidence = confidence, ai_provider = aiProvider, model_used = modelName, 
              method = method, stringsAsFactors = FALSE
            )
          } else {
            result <- data.frame(
              sentence_id = id, headword = headword_value, sentence = batch_row$sentence[1],
              predicted_sense = sense, confidence = confidence, ai_provider = aiProvider,
              model_used = modelName, method = method, stringsAsFactors = FALSE
            )
          }
          results <- safeBind(results, result)
        }
      }
    }, error = function(e) { 
      cat("Error parsing line", line_num, ":", e$message, "\n")
    })
  }
  
  cat("Parsed results:", nrow(results), "rows\n")
  cat("=== PARSING COMPLETE ===\n\n")
  
  return(results)
}

createErrorResults <- function(batch, error_message, aiProvider, modelName, HeadwordVar) {
  results <- data.frame()
  for (i in 1:nrow(batch)) {
    row <- batch[i, ]
    headword_value <- if("headword" %in% colnames(row)) {
      row$headword
    } else if(HeadwordVar %in% colnames(row)) {
      row[[HeadwordVar]]
    } else {
      "unknown"
    }
    
    result <- data.frame(sentence_id = row$ID, headword = headword_value, sentence = row$sentence,
                         predicted_sense = "ERROR", confidence = 0, ai_provider = aiProvider,
                         model_used = modelName, method = "error", error_message = error_message, stringsAsFactors = FALSE)
    results <- safeBind(results, result)
  }
  return(results)
}

safeBind <- function(df1, df2) {
  if (nrow(df1) == 0) return(df2)
  if (nrow(df2) == 0) return(df1)
  all_cols <- unique(c(colnames(df1), colnames(df2)))
  for (col in all_cols) {
    if (!col %in% colnames(df1)) df1[[col]] <- NA
    if (!col %in% colnames(df2)) df2[[col]] <- NA
  }
  df1 <- df1[all_cols]
  df2 <- df2[all_cols]
  return(rbind(df1, df2))
}

ensureDirectoryStructure <- function() {
  required_dirs <- c("./data/SemanticTagging", "./data/SemanticTagging/with_translation",
                     "./data/SemanticTagging/with_trainingdata", "./data/SemanticTagging/zero_shot",
                     "./data/Other", "./data/Other/Translations")
  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

getAvailableLemmasForTranslation <- function(all_lemmas) {
  if (!dir.exists("./data/Other/Translations")) return(character(0))
  translation_files <- dir("./data/Other/Translations", pattern = "_Translation\\.csv$")
  if (length(translation_files) == 0) return(character(0))
  
  available_lemmas <- character()
  for (file in translation_files) {
    tryCatch({
      lemma <- DecodeSafeFilename(file)
      if (lemma %in% all_lemmas) available_lemmas <- c(available_lemmas, lemma)
    }, error = function(e) { })
  }
  return(unique(available_lemmas))
}

getAvailableLemmasForExamples <- function(all_lemmas) {
  if (!dir.exists("./data/Outputs/Examples")) return(character(0))
  example_files <- dir("./data/Outputs/Examples", pattern = "_ExampleSents\\.csv$")
  if (length(example_files) == 0) return(character(0))
  
  available_lemmas <- character()
  for (file in example_files) {
    tryCatch({
      lemma <- DecodeSafeFilename(file)
      if (lemma %in% all_lemmas) available_lemmas <- c(available_lemmas, lemma)
    }, error = function(e) { })
  }
  return(unique(available_lemmas))
}

getAvailableLemmasForTraining <- function(all_lemmas) {
  lemmas_with_examples <- getAvailableLemmasForExamples(all_lemmas)
  if (!dir.exists("./data/SemanticTagging/with_translation")) return(character(0))
  
  training_files <- dir("./data/SemanticTagging/with_translation", pattern = "^_xx.*xx_Tagged.*\\.csv$")
  if (length(training_files) == 0) return(character(0))
  
  lemmas_with_training <- character()
  for (file in training_files) {
    tryCatch({
      lemma <- DecodeSafeFilename(file)
      if (lemma %in% all_lemmas) lemmas_with_training <- c(lemmas_with_training, lemma)
    }, error = function(e) { })
  }
  return(intersect(lemmas_with_examples, lemmas_with_training))
}

getTrainingFilesForLemma <- function(lemma) {
  if (!dir.exists("./data/SemanticTagging/with_translation")) return(character(0))
  
  all_training_files <- dir("./data/SemanticTagging/with_translation", pattern = "^_xx.*xx_Tagged.*\\.csv$")
  
  encoded_lemma <- MakeSafeForFilename(lemma)
  matching_files <- character()
  
  for (file in all_training_files) {
    if (startsWith(file, encoded_lemma)) {
      matching_files <- c(matching_files, file)
    }
  }
  
  return(matching_files)
}

getExamplesFileForLemma <- function(lemma) {
  if (!dir.exists("./data/Outputs/Examples")) return(NULL)
  encoded_lemma <- MakeSafeForFilename(lemma)
  example_file <- paste0(encoded_lemma, "_ExampleSents.csv")
  full_path <- paste0("./data/Outputs/Examples/", example_file)
  if (file.exists(full_path)) {
    return(full_path)
  } else {
    return(NULL)
  }
}

getTranslationFileForLemma <- function(lemma) {
  if (!dir.exists("./data/Other/Translations")) return(NULL)
  encoded_lemma <- MakeSafeForFilename(lemma)
  translation_file <- paste0(encoded_lemma, "_Translation.csv")
  full_path <- paste0("./data/Other/Translations/", translation_file)
  if (file.exists(full_path)) {
    return(full_path)
  } else {
    return(NULL)
  }
}

ensureDirectoryStructure()
