# This function showcases how one might write a function to be used as an
# RStudio project template. This function will be called when the user invokes
# the New Project wizard using the project template defined in the template file
# at:
#
#   inst/rstudio/templates/project/hello_world.dcf
#
# The function itself just echos its inputs and outputs to a file called INDEX,
# which is then opened by RStudio when the new project is opened.
#' @import stringr
#' @import here
project <- function(path, ...) {

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dots <- list(...)

  # Create README.md file
  name <- unlist(stringr::str_split(path, "/"))
  name <- name[length(name)]
  # generate header
  header <- c(
    paste("#", name),
    paste0("__", dots[["author"]], "__"))

  credits <- c(
    "## Acknowledgement",
    "Inspired from https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html.",
    "__The following inputs were received__",
    ""
  )

  # collect inputs
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val)
  })

  # collect into single text string
  contents <- paste(
    paste(header, collapse = "\n"),
    paste(credits, collapse = "\n"),
    paste(text, collapse = "\n"),
    sep = "\n"
  )

  # write to index file
  writeLines(contents, con = file.path(path, "README.md"))

  # Create the files and directory
  library(here)
  # default <- "~/Documents/Coding/Templates/github/"
  RmdTemplate <- paste0(
    system.file("rmarkdown", "templates",  "my_template", package = "DailyHRB"),
    "/skeleton/skeleton.Rmd")

  dir.create(paste0(path, "/Cache"), showWarnings = FALSE)
  message <- c("# Cached data", "",
               "Here will go all intermediate data saved to gain time")
  file.create(paste0(path, "/Cache/README.md"))
  writeLines(message, con = paste0(path, "/Cache/README.md"))

  dir.create(paste0(path, "/Data"), showWarnings = FALSE)

  dir.create(paste0(path, "/Figures"), showWarnings = FALSE)
  message <- c("# Figures", "",
               "Here will go all the main figures of the analysis")
  file.create(paste0(path, "/Figures/README.md"))
  writeLines(message, con = paste0(path, "/Figures/README.md"))

  dir.create(paste0(path, "/Sandbox"), showWarnings = FALSE)
  message <- c("# Sandbox", "",
    "Here will go all the scripts that are used as exploration but do not make it into the final analysis")
  file.create(paste0(path, "/Sandbox/README.md"))
  writeLines(message, con = paste0(path, "/Sandbox/README.md"))

  dir.create(paste0(path, "/Reports"), showWarnings = FALSE)

  file.copy(from = RmdTemplate,
            to = paste0(path, "/Reports/01-FirstAnalysis.Rmd"))

  file.create(paste0(path, "/.here"))

  # .gitignore file
  content <- c(".Rproj.user", ".Rhistory", "*.RData", "*.Rproj", "Data/*",
                "sandbox/*.RData")
  content <- paste(content, collapse = "\n")
  writeLines(content, con = file.path(path, "/.gitignore"))
}
