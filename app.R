library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(dplyr)
library(edibble)
library(digest)

# Load pairs_summary data
load_pairs_summary <- function() {
  if (file.exists("all_pairs_ratios/pairs_summary.csv")) {
    return(read.csv("all_pairs_ratios/pairs_summary.csv", stringsAsFactors = FALSE))
  } else {
    stop("pairs_summary.csv not found")
  }
}

# Calculate true value
calculate_true_value <- function(instruction_text, image_info, pairs_summary) {
  pair_number <- image_info$pair_number
  judgment_within_image <- image_info$judgment_within_image
  pair_index_mapping <- c(38, 32, 18, 44, 1)
  actual_index <- pair_index_mapping[pair_number]
  pair_row <- pairs_summary[actual_index, ]
  
  if (judgment_within_image == 1) {
    return(pair_row$State1_Ratio)
  } else if (judgment_within_image == 2) {
    return(pair_row$State2_Ratio)
  } else if (judgment_within_image == 3) {
    return(pair_row$Smaller_to_Bigger_Ratio)
  }
  
  stop(paste("Invalid judgment_within_image:", judgment_within_image))
}

# NEW: Use edibble to create experimental design structure
create_experimental_design <- function() {
  # Define the experimental structure using edibble
  exp_design <- design("Visualization Ratio Assessment") %>%
    # Define units: participant and image presentations
    set_units(
      participant = 1,
      image_presentation = 9
    ) %>%
    # Define treatment structure
    set_trts(
      pair = 5, # 5 possible pairs
      vis_type = 3
    ) %>% # 3 visualization types
    # Allot treatments: randomly select 3 pairs, cross with all vis types
    allot_trts(
      pair ~ image_presentation,
      vis_type ~ image_presentation
    )
  
  return(exp_design)
}

# NEW: Generate design with sequential constraint (no consecutive same pair)
generate_constrained_sequence <- function() {
  # Randomly select 3 out of 5 pairs
  selected_pairs <- sample(1:5, 3, replace = FALSE)
  
  # Create all combinations (3 pairs × 3 viz types = 9 images)
  all_combinations <- expand.grid(
    pair_number = selected_pairs,
    vis_type = c("polar", "overlaid", "dodged")
  )
  
  # Apply sequential constraint using round-robin interleaving
  constrained_sequence <- interleave_by_pair(all_combinations)
  
  return(constrained_sequence)
}

# Interleave to guarantee no consecutive same pairs
interleave_by_pair <- function(combinations_df) {
  # Split by pair
  by_pair <- split(combinations_df, combinations_df$pair_number)
  
  # Randomize within each pair
  by_pair <- lapply(by_pair, function(df) df[sample(nrow(df)), ])
  
  # Randomly order pairs for interleaving
  pair_order <- sample(names(by_pair))
  
  # Round-robin interleave
  result <- data.frame()
  pair_indices <- rep(1, length(by_pair))
  
  while (nrow(result) < nrow(combinations_df)) {
    for (i in seq_along(pair_order)) {
      pair_name <- pair_order[i]
      idx <- pair_indices[i]
      
      if (idx <= nrow(by_pair[[pair_name]])) {
        result <- rbind(result, by_pair[[pair_name]][idx, ])
        pair_indices[i] <- idx + 1
      }
    }
  }
  
  return(result)
}

# Create images metadata structure
create_all_possible_images <- function() {
  specific_pairs <- list(
    list(
      pair_number = 1, state_a_category = "Mentally Ill",
      state_b_category = "Developmentally Disabled"
    ),
    list(
      pair_number = 2, state_a_category = "Mentally Ill",
      state_b_category = "Mentally Ill"
    ),
    list(
      pair_number = 3, state_a_category = "Developmentally Disabled",
      state_b_category = "Developmentally Disabled"
    ),
    list(
      pair_number = 4, state_a_category = "Developmentally Disabled",
      state_b_category = "Mentally Ill"
    ),
    list(
      pair_number = 5, state_a_category = "Developmentally Disabled",
      state_b_category = "Mentally Ill"
    )
  )
  
  all_images <- list()
  image_id <- 1
  
  for (pair_info in specific_pairs) {
    for (vis_type in c("polar", "overlaid", "dodged")) {
      image_info <- list(
        image_id = image_id,
        pair_number = pair_info$pair_number,
        state_a_category = pair_info$state_a_category,
        state_b_category = pair_info$state_b_category,
        vis_type = vis_type,
        filename = paste0("Pair_", pair_info$pair_number, "_", vis_type, ".png"),
        display_name = paste0("Visualization ", image_id)
      )
      all_images[[image_id]] <- image_info
      image_id <- image_id + 1
    }
  }
  
  return(all_images)
}

# Select and order images with constraint
select_random_images_with_constraints <- function(all_images) {
  # Generate constrained sequence
  sequence_df <- generate_constrained_sequence()
  
  cat("Selected pairs:", unique(sequence_df$pair_number), "\n")
  cat("Sequence order:\n")
  print(sequence_df)
  
  # Match to actual image metadata
  selected_images <- list()
  for (i in 1:nrow(sequence_df)) {
    pair_num <- sequence_df$pair_number[i]
    vis <- sequence_df$vis_type[i]
    
    # Find matching image
    for (img in all_images) {
      if (img$pair_number == pair_num && img$vis_type == vis) {
        selected_images[[i]] <- img
        break
      }
    }
  }
  
  # Verify constraint
  for (i in 1:(length(selected_images) - 1)) {
    if (selected_images[[i]]$pair_number == selected_images[[i + 1]]$pair_number) {
      warning("Constraint violation at position ", i)
    }
  }
  
  cat("Successfully created ordering with guaranteed constraint\n")
  return(selected_images)
}

# Generate judgments
generate_judgments_for_images <- function(selected_images, pairs_summary) {
  all_judgments <- list()
  judgment_id <- 1
  pair_index_mapping <- c(38, 32, 18, 44, 1)
  
  for (i in seq_along(selected_images)) {
    img <- selected_images[[i]]
    pair_number <- img$pair_number
    
    actual_index <- pair_index_mapping[pair_number]
    pair_row <- pairs_summary[actual_index, ]
    
    state1_1870 <- pair_row$State1_Count_1870
    state2_1870 <- pair_row$State2_Count_1870
    
    if (state1_1870 < state2_1870) {
      smaller_state <- "State A"
      larger_state <- "State B"
    } else {
      smaller_state <- "State B"
      larger_state <- "State A"
    }
    
    state_b_instruction <- if (pair_number == 1) {
      "Find the ratio of people in State B from 1870 to 1860"
    } else {
      "Find the ratio of people in State B from 1860 to 1870"
    }
    
    judgments <- c(
      "Find the ratio of people in State A from 1860 to 1870",
      state_b_instruction,
      paste0("Find the ratio of ", smaller_state, " to ", larger_state, " in 1870")
    )
    
    for (j in 1:3) {
      img_with_judgment <- img
      img_with_judgment$judgment_within_image <- j
      
      true_value <- calculate_true_value(judgments[j], img_with_judgment, pairs_summary)
      
      all_judgments[[judgment_id]] <- list(
        judgment_id = judgment_id,
        image_index = i,
        judgment_within_image = j,
        instruction_text = judgments[j],
        true_value = true_value,
        pair_number = img$pair_number,
        image_info = img
      )
      
      cat("Judgment", judgment_id, ": Pair", pair_number, "Task", j, "True value:", true_value, "\n")
      judgment_id <- judgment_id + 1
    }
  }
  
  return(all_judgments)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .instruction-box {
        background-color: #f8f9fa;
        border-left: 5px solid #007bff;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        font-size: 16px;
        font-weight: bold;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }
      .example-instruction-box {
        background-color: #e8f5e8;
        border-left: 5px solid #28a745;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        font-size: 16px;
        font-weight: bold;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }
      .completion-box {
        background-color: #d4edda;
        border: 2px solid #28a745;
        padding: 30px;
        margin: 50px auto;
        border-radius: 10px;
        text-align: center;
        max-width: 600px;
      }
      .order-switch-warning {
        background-color: #fff3cd;
        border: 1px solid #ffeeba;
        color: #856404;
        padding: 10px;
        margin-top: 10px;
        border-radius: 5px;
        font-weight: normal;
      }
      .order-standard {
        background-color: #d1ecf1;
        border: 1px solid #bee5eb;
        color: #0c5460;
        padding: 10px;
        margin-top: 10px;
        border-radius: 5px;
        font-weight: normal;
      }
      .proportion-note {
        background-color: #e2e3e5;
        border-left: 5px solid #6c757d;
        padding: 10px;
        margin-bottom: 15px;
        border-radius: 5px;
        font-size: 14px;
      }
      .slider-label {
        margin-top: 15px;
        font-weight: normal;
        font-size: 14px;
        color: #495057;
      }
      @media (max-width: 768px) {
        h2 { font-size: 24px; }
        h3 { font-size: 20px; }
        p, li, .instruction-box, .example-instruction-box { font-size: 14px; }
      }
    "))
  ),
  div(
    id = "login_page",
    style = "width: 300px; max-width: 100%; margin: 0 auto; padding-top: 50px;",
    wellPanel(
      h2("Prolific ID", class = "text-center"),
      helpText("Enter your prolific ID in the box below. If you did not come to this study from Prolific, please instead enter a short description of how you got to this page, such as 'email', 'reddit', 'presentation'."),
      textInput("prolificID", label = "Prolific ID"),
      br(),
      actionButton("login_btn", "Submit and Start Study", class = "btn-primary btn-block")
    )
  ),
  hidden(
    div(
      id = "consent_page",
      style = "width: 800px; max-width: 100%; margin: 0 auto; padding-top: 50px;",
      fluidRow(
        shiny::column(
          width = 12,
          includeHTML("informed_consent_fragment.html")
        ),
        shiny::column(
          width = 2, offset = 3,
          actionButton("consent", "I Consent", class = "btn-success", icon = icon("square-check", lib = "font-awesome"))
        ),
        shiny::column(
          width = 2, offset = 6,
          actionButton("noConsent", label = "I do NOT Consent", class = "btn-info", icon = icon("circle-xmark", lib = "font-awesome"))
        )
      ) # end informed consent fluid row
    )
  ),
  hidden(
    div(
      id = "explanation_page",
      style = "width: 800px; max-width: 100%; margin: 0 auto; padding-top: 20px;",
      wellPanel(
        h2("Experiment Instructions", class = "text-center"),
        
        # Moved proportion note to main intro screen
        div(
          class = "proportion-note",
          icon("info-circle"),
          strong("Important: "),
          "All ratios are proportions between 0 and 1. All judgments will use this scale."
        ),
        br(),
        h3("Understanding Ratio Judgments"),
        p(
          "In this experiment, you'll analyze", strong("9 visualizations"),
          "showing anonymous disability data between pairs of states from 1860-1870."
        ),
        p(
          "For each visualization, you'll make", strong("3 different judgment tasks"),
          "for a total of", strong("27 assessments.")
        ),
        h4("Important Notes:"),
        tags$ul(
          tags$li(strong("Order Matters:"), "Some judgments use 1860→1870 ratios, while others use 1870→1860 ratios. A colored notice will appear when the order switches."),
          tags$li(strong("All proportions are between 0 and 1"), "so use the slider accordingly."),
          tags$li("You will see", strong("9 visualizations"), "with", strong("3 tasks each"), "for a total of 27 judgments.")
        ),
        h3("Practice Example"),
        p("For the chart below, please follow this instruction:"),
        div(
          class = "example-instruction-box",
          "Find the ratio of people in State A from 1860 to 1870. Use Slider to estimate."
        ),
        imageOutput("example_chart", height = "400px"),
        sliderInput("example_ratio", NULL,
                    min = 0, max = 1, value = 0.5, step = 0.01
        ),
        p(
          style = "font-size: 12px; color: #6c757d; margin-top: -5px; margin-bottom: 15px;",
          "Values must be between 0 and 1"
        ),
        actionButton("submit_example", "Submit Example", class = "btn-primary"),
        br(), br(),
        hidden(
          div(
            id = "feedback_section",
            h3("Feedback"),
            textOutput("true_ratio_feedback"),
            br(),
            actionButton("proceed_btn", "Proceed to Experiment", class = "btn-success")
          )
        )
      )
    )
  ),
  hidden(
    div(
      id = "main_app",
      titlePanel("State Population Ratio Assessment"),
      sidebarLayout(
        sidebarPanel(
          h3(textOutput("welcome_message")),
          div(
            class = "instruction-box",
            textOutput("current_instruction")
          ),
          uiOutput("order_switch_indicator"),
          uiOutput("ratio_slider_ui"),
          actionButton("save_btn", "Submit", class = "btn-primary"),
          br(), br(),
          h4("Progress"),
          textOutput("progress_text"),
          textOutput("judgment_progress")
        ),
        mainPanel(
          imageOutput("display_image", height = "500px")
        )
      )
    )
  ),
  hidden(
    div(
      id = "completion_page",
      style = "width: 600px; max-width: 100%; margin: 0 auto; padding-top: 50px;",
      div(
        class = "completion-box",
        h2("Assessment Complete!", style = "color: #28a745;"),
        h3("Thank you for your participation"),
        p("You have successfully completed all 27 judgment tasks."),
        br(),
        uiOutput("prolific_completion_url"),
        p(strong("You may now close this browser window.")),
        br()
      )
    )
  )
)

server <- function(input, output, session) {
  # Connect to database
  con <- dbConnect(RSQLite::SQLite(), "visualization_study_final.db")
  
  pairs_summary <- load_pairs_summary()
  cat("Loaded pairs_summary with", nrow(pairs_summary), "pairs\n")
  
  all_possible_images <- create_all_possible_images()
  cat("Created", length(all_possible_images), "possible images\n")
  
  output$example_chart <- renderImage(
    {
      list(
        src = file.path("specific_pairs_anonymous", "Pair_1_dodged.png"),
        contentType = "image/png",
        width = "100%",
        alt = "Example dodged bar chart"
      )
    },
    deleteFile = FALSE
  )
  
  # Safely create/update database tables
  # First, ensure users table exists with basic structure
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Add columns if they don't exist (safely)
  tryCatch({
    dbExecute(con, "ALTER TABLE users ADD COLUMN prolific_id TEXT")
    cat("Added prolific_id column to users table\n")
  }, error = function(e) {
    cat("prolific_id column may already exist:", e$message, "\n")
  })
  
  tryCatch({
    dbExecute(con, "ALTER TABLE users ADD COLUMN hashed_user_id TEXT")
    cat("Added hashed_user_id column to users table\n")
  }, error = function(e) {
    cat("hashed_user_id column may already exist:", e$message, "\n")
  })
  
  tryCatch({
    dbExecute(con, "ALTER TABLE users ADD COLUMN is_prolific BOOLEAN DEFAULT 0")
    cat("Added is_prolific column to users table\n")
  }, error = function(e) {
    cat("is_prolific column may already exist:", e$message, "\n")
  })
  
  # Create sessions table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sessions (
      session_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER,
      session_start DATETIME DEFAULT CURRENT_TIMESTAMP,
      selected_pair_numbers TEXT,
      completed BOOLEAN DEFAULT FALSE,
      FOREIGN KEY(user_id) REFERENCES users(user_id)
    )
  ")
  
  # Create selected_images table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS selected_images (
      selection_id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id INTEGER,
      image_id INTEGER,
      pair_number INTEGER,
      state_a_category TEXT,
      state_b_category TEXT,
      vis_type TEXT,
      filename TEXT,
      display_name TEXT,
      image_order INTEGER,
      FOREIGN KEY(session_id) REFERENCES sessions(session_id)
    )
  ")
  
  # Create judgments table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS judgments (
      judgment_id INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id INTEGER,
      selection_id INTEGER,
      judgment_order INTEGER,
      image_order INTEGER,
      judgment_within_image INTEGER,
      instruction_text TEXT,
      user_ratio REAL,
      true_value REAL,
      pair_number INTEGER,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(session_id) REFERENCES sessions(session_id),
      FOREIGN KEY(selection_id) REFERENCES selected_images(selection_id)
    )
  ")
  
  # Reactive values
  user <- reactiveValues(
    prolific_id = NULL, 
    hashed_id = NULL,
    is_prolific = FALSE, 
    consent = FALSE, 
    authenticated = FALSE, 
    session_id = NULL,
    user_id = NULL
  )
  selected_images <- reactiveVal(NULL)
  all_judgments <- reactiveVal(NULL)
  current_judgment_index <- reactiveVal(1)
  
  current_judgment <- reactive({
    req(all_judgments())
    all_judgments()[[current_judgment_index()]]
  })
  
  current_image <- reactive({
    req(current_judgment())
    current_judgment()$image_info
  })
  
  output$current_instruction <- renderText({
    req(user$authenticated, current_judgment())
    current_judgment()$instruction_text
  })
  
  output$order_switch_indicator <- renderUI({
    req(current_judgment())
    
    instruction <- current_judgment()$instruction_text
    
    if (grepl("1870 to 1860", instruction)) {
      div(
        class = "order-switch-warning",
        icon("exchange-alt"),
        strong("Order Notice: "),
        "This judgment uses 1870 → 1860 ratio (different direction than standard!)"
      )
    } else if (grepl("1860 to 1870", instruction)) {
      div(
        class = "order-standard",
        icon("arrow-right"),
        strong("Standard order: "),
        "1860 → 1870 ratio"
      )
    } else {
      NULL
    }
  })
  
  output$ratio_slider_ui <- renderUI({
    req(user$authenticated, current_judgment())
    tagList(
      div(class = "slider-label", "Use the slider below to estimate the ratio requested:"),
      sliderInput("ratio_slider", NULL,
                  min = 0, max = 1, value = 0.5, step = 0.01
      ),
      p(
        style = "font-size: 12px; color: #6c757d; margin-top: -10px; margin-bottom: 15px;",
        "Values must be between 0 and 1"
      )
    )
  })
  
  output$welcome_message <- renderText({
    req(user$authenticated)
    paste("Welcome!")
  })
  
  output$progress_text <- renderText({
    req(user$authenticated, all_judgments())
    paste("Overall: Assessment", current_judgment_index(), "of", length(all_judgments()))
  })
  
  output$judgment_progress <- renderText({
    req(current_judgment())
    judgment <- current_judgment()
    paste("Visualization", judgment$image_index, "- Task", judgment$judgment_within_image, "of 3")
  })
  
  output$true_ratio_feedback <- renderText({
    req(input$example_ratio)
    true_example_value <- calculate_true_value(
      "Find the ratio of people in State A from 1860 to 1870",
      list(pair_number = 1, judgment_within_image = 1),
      pairs_summary
    )
    difference <- abs(input$example_ratio - true_example_value)
    
    paste0(
      "Your estimate: ", round(input$example_ratio, 2),
      " | True value: ", round(true_example_value, 2),
      " | Difference: ", round(difference, 2)
    )
  })
  
  output$display_image <- renderImage(
    {
      req(user$authenticated, current_image())
      
      img <- current_image()
      img_path <- file.path("specific_pairs_anonymous", img$filename)
      
      if (!file.exists(img_path)) {
        img_path <- file.path("specific_pairs_anonymous", "placeholder.png")
      }
      
      list(
        src = img_path,
        contentType = "image/png",
        width = "100%",
        alt = paste(img$display_name, "visualization")
      )
    },
    deleteFile = FALSE
  )
  
  output$prolific_completion_url <- renderUI({
    if (user$is_prolific) {
      tags$p(
        tags$a(
          href = "https://app.prolific.com/submissions/complete?cc=C1H0WMUM",
          "Click here to return to Prolific",
          target = "_blank",
          class = "btn btn-primary"
        )
      )
    }
  })
  
  # Login handler
  observeEvent(input$login_btn, {
    req(input$prolificID)
    
    user$prolific_id <- input$prolificID
    
    # Check if this is a Prolific ID (24-character alphanumeric)
    user$is_prolific <- grepl("^[A-Za-z0-9]{24}$", input$prolificID)
    
    # Generate a random hashed ID for anonymous tracking
    user$hashed_id <- digest::digest(paste(input$prolificID, Sys.time(), runif(1)), algo = "md5")
    
    # Check if this user has already completed the assessment
    user_completed <- FALSE
    tryCatch(
      {
        # First check if user exists
        existing_user <- dbGetQuery(con,
                                    "SELECT user_id FROM users WHERE prolific_id = ?",
                                    params = list(input$prolificID)
        )
        
        if (nrow(existing_user) > 0) {
          user_id <- existing_user$user_id[1]
          
          # Check if they have a completed session
          completed_session <- dbGetQuery(con, "
            SELECT session_id FROM sessions 
            WHERE user_id = ? AND completed = 1
            LIMIT 1
          ", params = list(user_id))
          
          if (nrow(completed_session) > 0) {
            user_completed <- TRUE
            user$user_id <- user_id
          }
        }
      },
      error = function(e) {
        cat("Error checking completion status:", e$message, "\n")
        user_completed <- FALSE
      }
    )
    
    if (user_completed) {
      showNotification("You have already completed this assessment. Thank you for your participation!",
                       type = "message", duration = 10
      )
      shinyjs::hide("login_page")
      shinyjs::show("completion_page")
      return()
    }
    
    # Store or update user information
    existing_user <- dbGetQuery(con,
                                "SELECT user_id FROM users WHERE prolific_id = ?",
                                params = list(input$prolificID)
    )
    
    if (nrow(existing_user) == 0) {
      # Insert new user - set hashed_user_id to NULL/blank
      dbExecute(con, 
                "INSERT INTO users (prolific_id, hashed_user_id, is_prolific) VALUES (?, NULL, ?)",
                params = list(input$prolificID, as.integer(user$is_prolific))
      )
      user_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
    } else {
      user_id <- existing_user$user_id[1]
      # Don't update hashed ID - leave it as NULL/blank
      # Remove or comment out the UPDATE statement
    }
    
    user$user_id <- user_id
    
    # Generate experimental sequence with constraints
    session_images <- select_random_images_with_constraints(all_possible_images)
    selected_images(session_images)
    
    selected_pair_numbers <- unique(sapply(session_images, function(x) x$pair_number))
    selected_pairs_text <- paste(selected_pair_numbers, collapse = ",")
    
    # Create new session
    dbExecute(con, "INSERT INTO sessions (user_id, selected_pair_numbers) VALUES (?, ?)",
              params = list(user_id, selected_pairs_text)
    )
    session_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
    user$session_id <- session_id
    
    cat("Session", session_id, "- User ID:", user_id, "- Prolific ID:", input$prolificID, "\n")
    cat("Hashed ID:", user$hashed_id, "(for anonymous tracking)\n")
    cat("Is Prolific:", user$is_prolific, "\n")
    cat("Selected pairs:", selected_pairs_text, "\n")
    
    # Generate judgments
    session_judgments <- generate_judgments_for_images(session_images, pairs_summary)
    all_judgments(session_judgments)
    
    cat("Generated", length(session_judgments), "judgments\n")
    
    # Store selected images
    for (i in seq_along(session_images)) {
      img <- session_images[[i]]
      dbExecute(con,
                "INSERT INTO selected_images
                (session_id, image_id, pair_number, state_a_category,
                 state_b_category, vis_type, filename, display_name, image_order)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                  session_id, img$image_id, img$pair_number,
                  img$state_a_category, img$state_b_category, img$vis_type,
                  img$filename, img$display_name, i
                )
      )
    }
    
    shinyjs::hide("login_page")
    shinyjs::show("consent_page")
  })
  
  observeEvent(input$consent, {
    user$consent <- TRUE
    shinyjs::hide("consent_page")
    shinyjs::show("explanation_page")
  })
  
  observeEvent(input$noConsent, {
    user$consent <- FALSE
    if (input$noConsent > 0) {
      shinyjs::runjs('window.location.href = "https://app.prolific.com/submissions/complete?cc=C342T7TO";')
    }
  })
  
  observeEvent(input$submit_example, {
    shinyjs::show("feedback_section")
  })
  
  observeEvent(input$proceed_btn, {
    user$authenticated <- TRUE
    shinyjs::hide("explanation_page")
    shinyjs::show("main_app")
    current_judgment_index(1)
  })
  
  # Save handler
  observeEvent(input$save_btn, {
    req(user$authenticated, current_judgment(), all_judgments())
    
    judgment <- current_judgment()
    
    selection_result <- dbGetQuery(con,
                                   "SELECT selection_id FROM selected_images
                                  WHERE session_id = ? AND image_order = ?",
                                   params = list(user$session_id, judgment$image_index)
    )
    
    if (nrow(selection_result) == 0) {
      showNotification("Error: Could not find image selection", type = "error")
      return()
    }
    
    selection_id <- selection_result$selection_id[1]
    
    dbExecute(con,
              "INSERT INTO judgments
              (session_id, selection_id, judgment_order, image_order,
               judgment_within_image, instruction_text, user_ratio, true_value,
               pair_number)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(
                user$session_id,
                selection_id,
                current_judgment_index(),
                judgment$image_index,
                judgment$judgment_within_image,
                judgment$instruction_text,
                input$ratio_slider,
                judgment$true_value,
                judgment$pair_number
              )
    )
    
    if (current_judgment_index() < length(all_judgments())) {
      current_judgment_index(current_judgment_index() + 1)
      updateSliderInput(session, "ratio_slider", value = 0.5)
    } else {
      # Mark session as completed
      dbExecute(con, "UPDATE sessions SET completed = 1 WHERE session_id = ?",
                params = list(user$session_id) 
      )
      
      showNotification("All 27 assessments completed! Thank you.", type = "message")
      shinyjs::disable("save_btn")
      
      shinyjs::hide("main_app")
      shinyjs::show("completion_page")
    }
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)