# Internal helpers shared across MatSamp Shiny apps.
# None of these are exported.

na_if_empty <- function(x) if (is.character(x) && !nzchar(x)) NA_character_ else x

chr_or_empty <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) "" else as.character(x)

format_lobster_id <- function(trip_id, string_no, lobster_no) {
  paste0(trip_id, sprintf("%02d%02d", as.integer(string_no), as.integer(lobster_no)))
}

with_topmost_tk <- function(f) {
  tt <- tcltk::tktoplevel()
  on.exit(tcltk::tkdestroy(tt))
  tcltk::tkwm.geometry(tt, "1x1+0+0")
  tcltk::tcl("wm", "attributes", tt, "-topmost", TRUE)
  tcltk::tkfocus(tt)
  f()
}

is_valid_ddmm <- function(x, type = c("lat", "long")) {
  type <- match.arg(type)
  if (!grepl("^\\d{4}\\.\\d{2}$", x)) return(FALSE)
  deg <- as.integer(substr(x, 1, 2))
  min <- as.integer(substr(x, 3, 4))
  if (min > 59) return(FALSE)
  if (type == "lat" && deg > 89) return(FALSE)
  TRUE
}

# Common JS tags used by all three apps.
mat_js_tags <- function() {
  shiny::tagList(
    shiny::tags$script(shiny::HTML("
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
          var focusable = Array.prototype.filter.call(
            document.querySelectorAll('input, select, textarea, button, [tabindex]'),
            function(el) {
              return el.tabIndex >= 0 && !el.disabled && el.offsetParent !== null;
            }
          );
          var index = focusable.indexOf(document.activeElement);
          if (index > -1 && index + 1 < focusable.length) {
            var next = focusable[index + 1];
            next.focus();
            if (next.tagName === 'INPUT' || next.tagName === 'TEXTAREA') next.select();
          }
        }
      });
    ")),
    shiny::tags$script(shiny::HTML("
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          e.preventDefault();
          document.execCommand('undo');
        }
      });
    ")),
    shiny::tags$script(shiny::HTML("
      $(document).ready(function() {
        function disableNumericScroll(target) {
          $(target).on('wheel', function(e) { e.preventDefault(); });
        }
        function preventBelowMin(target) {
          $(target).on('input', function() {
            var val = parseFloat(this.value);
            var min = parseFloat(this.min);
            if (!isNaN(val) && !isNaN(min) && val < min) {
              this.value = min;
              $(this).trigger('change');
            }
          });
        }
        $('input[type=\"number\"]').each(function() {
          disableNumericScroll(this);
          preventBelowMin(this);
        });
        const observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
              if (!(node instanceof Element)) return;
              if ($(node).is('input[type=\"number\"]')) {
                disableNumericScroll(node);
                preventBelowMin(node);
              }
              $(node).find('input[type=\"number\"]').each(function() {
                disableNumericScroll(this);
                preventBelowMin(this);
              });
            });
          });
        });
        observer.observe(document.body, { childList: true, subtree: true });
      });
    "))
  )
}

# Common export panel UI used by all three apps.
mat_export_ui <- function() {
  shiny::tagList(
    shiny::tags$hr(),
    shiny::h3("DATABASE EXPORT"),
    shiny::fluidRow(
      shiny::column(width = 12, shiny::textInput("db_folder", "Local folder path", value = ""))
    ),
    shiny::fluidRow(
      shiny::column(width = 4, shiny::actionButton("choose_db_folder", "Choose Save Directory", class = "btn-primary")),
      shiny::column(width = 4, shiny::uiOutput("save_db_ui"))
    ),
    shiny::verbatimTextOutput("db_status")
  )
}
