#' Run a multi–block study/test memory experiment in the browser
#'
#' Launches a Shiny app that presents one or more *blocks* of
#' study items followed by a recognition test.  Each block is
#' defined by rows in `stimuli_df`:
#' \enumerate{
#'   \item `type == "study"` – words flashed for a fixed duration,
#'         separated by a blank inter–stimulus interval (ISI).
#'   \item `type == "test"`  – probe words judged *Old* / *New*
#'         via response buttons.  Probes remain on–screen until
#'         a response is made.
#' }
#' When all blocks are finished the app closes itself and the
#' function returns a tidy data frame of responses.
#'
#' @param stimuli_df A data frame (or tibble) with **exactly**
#'   three columns:
#'   \describe{
#'     \item{`block`}{Integer or factor identifying the block
#'       (e.g. 1, 2, 3).  Blocks are presented in ascending order.}
#'     \item{`type`}{Character: `"study"` or `"test"` – case
#'       sensitive.}
#'     \item{`item`}{Character stimulus to display.}
#'   }
#' @param display_duration Numeric. Time each study word remains
#'   visible, in **milliseconds**.  Default = 1000 ms.
#' @param ISI Numeric. Blank inter–stimulus interval between
#'   study words, in **milliseconds**.  Default = 1000 ms.
#'
#' @return A data frame with one row per test probe, in the order
#'   they were presented, containing:
#'   \describe{
#'     \item{`block`}{Block identifier (copied from input).}
#'     \item{`item`}{The probe word.}
#'     \item{`response`}{Participant button press: `"old"` or
#'       `"new"`.}
#'     \item{`correct`}{Logical flag indicating whether the
#'       response matched the study list for that block.}
#'   }
#'
#' @details
#' * **Start → Study phase** Words flash for
#'   `display_duration` ms, with blanks of length `ISI` ms.
#' * **Start Test → Test phase** Each probe waits for an Old/New
#'   response.  Responses are logged in real time.
#' * After the last block a “Thank you!” screen appears and the
#'   app terminates via `shiny::stopApp()`, returning the results
#'   data frame to the calling session.
#'
#' @examples
#' \dontrun{
#' library(tibble)
#'
#' # Example 2–block stimulus set
#' stimuli_df <- tribble(
#'   ~block, ~type, ~item,
#'   1, "study", "apple",
#'   1, "study", "banana",
#'   1, "test", "apple",
#'   1, "test", "orange",
#'   2, "study", "cat",
#'   2, "study", "dog",
#'   2, "test", "dog",
#'   2, "test", "sheep"
#' )
#'
#' results <- run_experiment(stimuli_df,
#'   display_duration = 800,
#'   ISI              = 800
#' )
#' print(results)
#' }
#'
#' @author Your Name
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
#' @import later
#' @export
run_experiment <- function(stimuli_df, display_duration = 1000, ISI = 1000) {
  ##  ── basic sanity checks & dependency loading  ──
  ##  (unchanged, no additional comments)
  req_cols <- c("block", "type", "item")
  if (!all(req_cols %in% names(stimuli_df))) {
    stop("stimuli_df must contain columns: block, type, item")
  }
  if (!all(stimuli_df$type %in% c("study", "test"))) {
    stop('column "type" must only contain "study" or "test"')
  }
  stopifnot(is.numeric(display_duration), is.numeric(ISI))
  blocks <- unique(stimuli_df$block)

  for (pkg in c("shiny", "shinyjs", "later")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Please install package: ", pkg)
    }
  }
  library(shiny)
  library(shinyjs)
  library(later)

  # ──────────────────────────────────────────────────────────────
  # UI  ── what the participant sees in the browser
  # ──────────────────────────────────────────────────────────────
  ui <- fluidPage(
    useShinyjs(), # injects JS helpers so hide()/show() work

    # Inline CSS → centres everything & sets font sizes
    tags$head(tags$style(HTML("
      #mainContainer {display:flex; flex-direction:column;
                      justify-content:center; align-items:center; height:80vh;}
      #word_display  {font-size:48px; text-align:center; height:80px;}
      #response_buttons {margin-top:25px; display:flex; gap:40px;}
    "))),

    # One flexbox container holds every widget we need
    div(
      id = "mainContainer",

      # —— Study‑phase launch button ——
      actionButton("start", "Start", class = "btn-primary btn-lg"),

      # —— Test‑phase launch button (hidden until needed) ——
      shinyjs::hidden(
        actionButton("start_test", "Start Test", class = "btn-success btn-lg")
      ),

      # —— Stimulus / feedback word output ——
      textOutput("word_display"),

      # —— Old/New response buttons (hidden at first) ——
      shinyjs::hidden(
        div(
          id = "response_buttons",
          actionButton("resp_old", "Old", class = "btn-secondary btn-lg"),
          actionButton("resp_new", "New", class = "btn-secondary btn-lg")
        )
      )
    )
  )

  # ──────────────────────────────────────────────────────────────
  # SERVER  ── all logic & reactivity lives here
  # ──────────────────────────────────────────────────────────────
  server <- function(input, output, session) {
    ## Helper wrappers: safely hide/show UI even from later() timers
    hide_el <- function(id) withReactiveDomain(session, shinyjs::hide(id))
    show_el <- function(id) withReactiveDomain(session, shinyjs::show(id))

    ## Reactive state “blackboard” for our tiny state‑machine
    rv <- reactiveValues(
      block_idx = 1, # which block are we in?
      phase = "await_start", # "train" → "ready_test" → "test" / loop
      word = "", # text currently visible
      study = character(), # study list for current block
      probe = character(), # test list for current block
      train_i = 1, # index into study list
      test_i = 1, # index into test list
      results = data.frame( # responses so far
        block = integer(), item = character(),
        response = character(), correct = logical(),
        stringsAsFactors = FALSE
      )
    )

    ## Push rv$word to the browser whenever it changes
    output$word_display <- renderText({
      rv$word
    })

    # ── Load a block’s study & probe vectors into rv ─────────────
    load_block <- function(idx) {
      blk <- blocks[idx]
      rv$study <- stimuli_df$item[stimuli_df$block == blk & stimuli_df$type == "study"]
      rv$probe <- stimuli_df$item[stimuli_df$block == blk & stimuli_df$type == "test"]
      rv$train_i <- rv$test_i <- 1
      # Update the Start button label, e.g. “Start Block 2”
      updateActionButton(session, "start", label = paste("Start Block", blk))
    }
    load_block(1) # pre‑load first block at app startup

    # ────────────────────────────────────────────────────────────
    # STUDY‑PHASE PRESENTER
    # Shows each study word, waits `display_duration`,
    # blanks for `ISI`, then recurses to next word
    # ────────────────────────────────────────────────────────────
    present_train <- function(i = 1) {
      withReactiveDomain(session, {
        study_vec <- isolate(rv$study) # copy once; avoids reactive read later

        # —— If we’ve exhausted this block’s study list … ——
        if (i > length(study_vec)) {
          rv$word <- ""
          rv$phase <- "ready_test"
          show_el("start_test") # reveal the Start Test button
          return(invisible())
        }

        # —— Show current study word immediately ——
        rv$word <- study_vec[i]

        # —— After `display_duration` ms … ——
        later(function() {
          withReactiveDomain(session, {
            rv$word <- "" # blank the screen

            # —— After ISI ms, recurse to next word ——
            later(function() present_train(i + 1), ISI / 1000)
          })
        }, display_duration / 1000)
      })
    }

    # ────────────────────────────────────────────────────────────
    # TEST‑PHASE helpers
    # ────────────────────────────────────────────────────────────
    ## Advance through probes or finish block / experiment
    show_next_probe <- function() {
      if (rv$test_i > length(rv$probe)) { # —— end of block ——

        blk <- blocks[rv$block_idx]
        rv$phase <- "done_block"
        hide_el("response_buttons")
        show_el("start") # show Start for NEXT block

        rv$block_idx <- rv$block_idx + 1
        if (rv$block_idx > length(blocks)) {
          rv$word <- "Thank you!"
          stopApp(rv$results) # exit app & return data
        } else {
          load_block(rv$block_idx) # prime next block
          rv$word <- paste("End Block", blk)
        }
      } else { # —— still probes left ——
        rv$word <- rv$probe[rv$test_i]
      }
    }

    ## Store a participant response and move on
    record_response <- function(resp) {
      blk <- blocks[rv$block_idx]
      item <- rv$probe[rv$test_i]
      correct <- (item %in% rv$study && resp == "old") ||
        (!(item %in% rv$study) && resp == "new")

      rv$results <- rbind(
        rv$results,
        data.frame(
          block = blk, item = item,
          response = resp, correct = correct
        )
      )

      rv$test_i <- rv$test_i + 1
      show_next_probe()
    }

    # ────────────────────────────────────────────────────────────
    # OBSERVERS: glue UI events to behaviour
    # ────────────────────────────────────────────────────────────
    ## Start button → begin study phase
    observeEvent(input$start,
      {
        hide_el("start")
        rv$phase <- "train"
        present_train(1)
      },
      ignoreInit = TRUE
    )

    ## Start Test button → begin test phase
    observeEvent(input$start_test,
      {
        hide_el("start_test")
        show_el("response_buttons")
        rv$phase <- "test"
        show_next_probe()
      },
      ignoreInit = TRUE
    )

    ## Old / New response buttons
    observeEvent(input$resp_old,
      {
        record_response("old")
      },
      ignoreInit = TRUE
    )
    observeEvent(input$resp_new,
      {
        record_response("new")
      },
      ignoreInit = TRUE
    )
  }

  # ──────────────────────────────────────────────────────────────
  # Launch the app; when stopApp() is called inside server
  # runApp() returns rv$results, which becomes the function output
  # ──────────────────────────────────────────────────────────────
  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
}
