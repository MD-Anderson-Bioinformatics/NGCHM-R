#' Check for External Utilities used by NGCHM
#'
#' This function checks for the presence of certain external programs that are suggested for the package to
#' function properly. It checks for 'git', 'scp', 'ssh', and 'tar'. If any of these programs are not found,
#' it prints a warning.
#'
#' @param None.
#'
#' @return None. This function is called for its side effects (printing warnings if necessary programs are not found).
#'
#' @noRd
#' @keywords internal
checkForExternalUtilities <- function() {
  suggestedUtilities <- list(
    "git" = paste0("CRITICAL WARNING: Unable to verify 'git' installation. Git is required to create NG-CHMs. ",
                   "Install git in order to create NG-CHMs."),
    "scp" = "Missing suggested external program 'scp'. Some server functionality unavailable.",
    "ssh" = "Missing suggested external program 'ssh'. Some server functionality unavailable.",
    "tar" = "Missing suggested external program 'tar'. Some server functionality unavailable."
  )
  for (name in names(suggestedUtilities)) {
    checkResponse <- NULL
    suppressWarnings(try({
      checkResponse <- system2(name, "--version", stdout = TRUE, stderr = TRUE)
    }, silent = TRUE))
    if (is.null(checkResponse)) {
      warning(suggestedUtilities[[name]])
    }
  }
}

#' Check Java Version
#'
#' This function checks if the Java is installed and if the installed version meets the required version.
#' It extracts the major version number from the Java version string and compares it with the required version.
#'
#' @param requiredJavaVersion The required Java version. Default is 11 because ShaidyMapGen.jar is compiled for Java 11.
#'
#' @return Boolean value indicating if the installed Java version meets the required version.
#'
#' @noRd
#' @keywords internal
checkForJavaVersion <- function(javaExecutable = "java", requiredJavaVersion = 11) {
  requiredJavaVersion <- as.integer(requiredJavaVersion)
  message <- paste0("CRITICAL WARNING: Unable to verify Java installation. ",
                    "Java ", requiredJavaVersion, " is required to create .ngchm and .html files.\n\n",
                    "\tInstall Java ", requiredJavaVersion, " or higher for to create .ngchm and .html files.\n")
  checkResponse <- NULL
  suppressWarnings(try({
    checkResponse <- system2(javaExecutable, "--version", stdout = TRUE, stderr = TRUE)
  }, silent = TRUE))
  if (is.null(checkResponse)) { # java not installed
    warning(message)
    return(FALSE)
  }
  haveVersion <- FALSE
  suppressWarnings(try({
    versionNumber <- strsplit(checkResponse, split = "\\.")[[1]][1] # from first line , rm everything after first '.'
    versionNumber <- as.numeric(gsub("\\D", "", versionNumber)) # rm non-digits, should be left with major version number
    if (versionNumber >= requiredJavaVersion) {
      haveVersion <- TRUE
    }
  }, silent = TRUE))
  if (!haveVersion) { # java installed, but version is less than required (or version number could not be extracted)
    warning(message)
    return(FALSE)
  }
  return(TRUE) # java installed and version is at least required version
}

#' Check for NGCHMSupportFiles package
#'
#' If the environment variables SHAIDYMAPGEN and NGCHMWIDGETPATH are set, we assume the user has what they need.
#' If the environment variables are not set, we check if the NGCHMSupportFiles package is installed and provide
#' user feedback.
#'
#' @param None.
#'
#' @return Boolean value. TRUE if SHAIDYMAPGEN and NGCHMWIDGETPATH are set, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
checkForNGCHMSupportFiles <- function() {
  SHAIDYMAPGEN <- Sys.getenv("SHAIDYMAPGEN")
  NGCHMWIDGETPATH <- Sys.getenv("NGCHMWIDGETPATH")
  if (nzchar(SHAIDYMAPGEN) > 0 && nzchar(NGCHMWIDGETPATH) > 0) { # SHAIDYMAPGEN and NGCHMWIDGETPATH are set
    return(TRUE)
  }
  mary <- suppressWarnings(find.package("NGCHMSupportFiles", quiet = TRUE))
  if (length(suppressWarnings(find.package("NGCHMSupportFiles", quiet = TRUE))) > 0) { # NGCHMSupportFiles is installed
    warning(paste0("WARNING: NGCHMSupportFiles package is installed but not loaded. ",
                   "This package is required to create .ngchm and .html files. ",
                   "Please load the package with command:\n\n",
                   "\tlibrary(NGCHMSupportFiles)\n"))
  } else {
    warning(paste0("CRITICAL WARNING: Package NGCHMSupportFiles is not installed. ",
                   "This package is required to create .ngchm and .html files. ",
                   "Please install and load NGCHMSupportFiles with commands: \n\n",
                   "\tinstall.packages('NGCHMSupportFiles', repos = c('https://md-anderson-bioinformatics.r-universe.dev', 'https://cloud.r-project.org'))\n\n",
                   "\tlibrary(NGCHMSupportFiles)\n"))
  }
  return(FALSE)
}

#' Check for NGCHMDemoData package
#'
#' @param None.
#'
#' @return Boolean. TRUE if NGCHMDemoData is loaded, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
checkForNGCHMDemoData <- function() {
  if ("package:NGCHMDemoData" %in% search()) { # NGCHMDemoData is loaded
    return(TRUE)
  }
  if (length(suppressWarnings(find.package("NGCHMDemoData", quiet = TRUE))) > 0) { # NGCHMDemoData is installed
    warning(paste0("WARNING: NGCHMDemoData package is installed but not loaded. ",
                   "To run examples and website vignettes, please load the package with command:\n\n",
                   "\tlibrary(NGCHMDemoData)\n"))
  } else {
    warning(paste0("NOTE: Package NGCHMDemoData is not installed. ",
                   "This package is used by the examples and website vignettes. Please install and load NGCHMDemoData with command: \n\n",
                   "\tinstall.packages('NGCHMDemoData', repos = c('https://md-anderson-bioinformatics.r-universe.dev', 'https://cloud.r-project.org'))\n\n",
                   "\tlibrary(NGCHMDemoData)\n"))
  }
  return(FALSE)
}

##' Validate and Process Colors
#'
#' This function checks if the provided colors are valid color names or hexadecimal color codes.
#' If a color is a valid 8-digit hexadecimal color code with an alpha channel, the alpha channel
#' is removed, and a warning is issued. (Alpha channel is not supported by ShaidyMapGen.jar)
#'
#' @param color A character vector of color names or hexadecimal color codes.
#' @return A character vector of valid color names or 6-digit hexadecimal color codes.
#' @noRd
#' @keywords internal
#' @examples
#' validateColor(("yellow")
#' validateColor(("not-a-color")
#' validateColor(c("red", "#FF5733", "#FF573300", "#fff"))
validateColor <- function(color) {
  for (i in 1:length(color)) {
    # check if color is a valid color name or hexadecimal color
    tryCatch(
      grDevices::col2rgb(color[i]),
      error = function(e) {
        stop(paste0("Color '", color[i], "' must be a valid color name or hexadecimal color."))
      }
    )
    # If color is a valid hex color with alpha channel, remove alpha channel
    if (!is.null(color[i]) && grepl("^#[0-9A-Fa-f]{8}$", color[i])) {
      warning(paste0("Alpha channel is not supported and will be removed from '", color[i], "'."))
      color[i] <- substr(color[i], 1, 7)
      next
    }
  }
  return(color)
}
