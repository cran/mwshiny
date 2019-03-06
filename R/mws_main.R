#' Runs shiny app in multiple specified windows.
#'
#' @param win_titles vector of strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector'.
#' @param ui_win list of shiny ui pages. Must be same length as win_titles, and ui page must be same index as corresponding title in win_titles.
#' @param serv_calc a named list of functions that calculate variables derived from user input, to be used in rendering output. Each function is of the form function(input, calc), where input is the traditional shiny input, and calc is a named list of reactive values. Variables in must be accessed in the form input()$x. All calculated variables that are needed to render output should be added, named, to the calc list. When using reactive functions such as observeEvent(), each should be contained in a separate function. Note that these functions follow all shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @param serv_out a named list of functions that render output. Each function is of the form function(input, calc), where input is the traditional shiny input, and calc is a named list of reactive values that have calculated values derived from input. It returns the results of a shiny render function. The name of each function corresponds to its output label. Note that these functions follow all shiny conventions (reactive values must be accessed in a reactive context, etc.).
#' @return shiny app object (i.e., it runs the app)
#' @export
#' @import shiny
#' @examples
#' if(interactive()){
#' # Run a simple 2-window app, initially bringing up the window selector window:
#' ui_win <- list()
#' ui_win[[1]] <- fluidPage(numericInput(inputId = "click", label = "a", value = 1))
#' ui_win[[2]] <- fluidPage(plotOutput("clickplot"))
#' serv_out <- list()
#' serv_out[["clickplot"]] <- function(input, calc){
#'   renderPlot({
#'       plot(1:input$click,1:input$click)
#'   })
#' }
#' mwsApp(c("clickinput","clickoutput"),
#'     ui_win,
#'     list(),
#'     serv_out)
#' }
mwsApp <- function(win_titles=c(), ui_win=list(), serv_calc=list(), serv_out=list()){
  # safeguards for improper arguments
  if ("WindowSelector" %in% win_titles){
    stop("Argument win_titles contains the reserved window name 'WindowSelector'")
  }

  if (typeof(ui_win)!="list"){
    stop("Argument ui_win is not a list")
  }

  if (typeof(serv_calc)!="list"){
    stop("Argument serv_calc is not a list")
  }

  if (typeof(serv_out)!="list"){
    stop("Argument serv_out is not a list")
  }

  if (length(win_titles)!=length(ui_win)){
    stop("Arguments win_titles and ui_win have different lengths: ",
         length(win_titles), " and ", length(ui_win))
  }

  if (is.null(names(serv_out)) & length(serv_out) > 0){
    stop("Argument serv_out is unnamed")
  }

  # compute ui
  ui <- mwsUI(win_titles, ui_win)

  # preallocate serverValues
  serverValues <- shiny::reactiveValues()

  # create server, getting output
  mws_server <- shiny::shinyServer(function(input,output){

    # run each of the server calculation functions
    if (length(serv_calc) > 0){
      for (s in 1:length(serv_calc)){

        # check for errors related to numbers of inputs
        tryCatch({
          serv_calc[[s]](shiny::reactive(input), serverValues)
        }, error = function(e){
          if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
              length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
            e$message <- paste("Argument", s, "of serv_calc's functions does not have 2 arguments")
          }
          stop(e)
        })
      }
    }

    # then allocate each of these to output
    serverFunct(input, serverValues, output, serv_out)

  })

  # run the app!
  shiny::shinyApp(ui, mws_server)
}
