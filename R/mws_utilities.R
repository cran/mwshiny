#' Render server output.
#'
#' @param input traditional shiny input (reactive values)
#' @param serverValues user-created values (reactive values), to be used in rendering output
#' @param output traditional shiny output
#' @param serv_out_list a named list of functions that render output. Each function is of the form function(input, calc), where input is the traditional shiny input, and calc is a named list of reactive values that have calculated values derived from input. It returns the results of a shiny render function. The name of each function corresponds to its output label.
#' @return traditional shiny output argument
serverFunct <- function(input, serverValues, output, serv_out_list){
  # go through each output rendering and render
  if (length(serv_out_list) > 0){
    for (v in 1:length(serv_out_list)){
      # check for errors related to numbers of inputs
      tryCatch({
        # renderoutput and assign to output
        output[[names(serv_out_list)[v]]] <- serv_out_list[[v]](input, serverValues)
      }, error = function(e){
        if (length(grep("unused argument ", as.character(e)[1], fixed = T))>0 |
            length(grep("is missing, with no default", as.character(e)[1], fixed = T))>0){
          e$message <- paste("Argument", v, "of serv_out's functions does not have 2 arguments")
        }
        stop(e)
      })
    }
  }
  return(output)
}

#' Renders user interface for all mwshiny windows.
#'
#' @param win_titles vector of strings, corresponding to window titles. Must be same length as ui_win, and titles must be same index as corresponding ui page in ui_win. No windows can be named 'WindowSelector'.
#' @param ui_list list of shiny ui pages. Must be same length as win_titles, and ui page must be same index as corresponding title in win_titles.
#' @return ui: user interfaces for all windows
mwsUI <- function(win_titles, ui_list) {
  win_select <- ""
  for (w in win_titles){
    win_select <- paste0(win_select,
                         '<h2><a href="?',w,'">',w,'</a></h2>')
  }

  other_win <- ""
  if (length(ui_list) > 0){
    for (u in 1:length(ui_list)){
      other_win <- paste0(other_win,
                          '<div class="',win_titles[u],' Window">',ui_list[[u]],'</div>')
    }
  }

  ui <- shiny::shinyUI(shiny::bootstrapPage(
    shiny::HTML('<script type="text/javascript">
         $(function() {
         $("div.Window").hide();
         var tokens = window.location.href.split("?");
         if (tokens.length > 1) {
         var shown_window = tokens[1];
         $("div."+shown_window).show();
         } else {
         $("div.WindowSelector").show();
         }
         });
         </script>'),
    shiny::div(class="WindowSelector Window",
        shiny::HTML(win_select),
        style='position: absolute;
        top: 50%; left: 50%;
        margin-right: -50%;
        transform: translate(-50%, -50%)'
    ),
    shiny::HTML(other_win)
    ))

  return(ui)
}
