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
    "git" = paste0("IMPORTANT: Unable to verify 'git' installation. Git is required to create NG-CHMs. ",
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
      packageStartupMessage(suggestedUtilities[[name]])
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
  message <- paste0("IMPORTANT: Unable to verify Java installation. ",
                    "Java ", requiredJavaVersion, " is required to create .ngchm and .html files.\n\n",
                    "\tInstall Java ", requiredJavaVersion, " or higher for to create .ngchm and .html files.\n")
  checkResponse <- NULL
  suppressWarnings(try({
    checkResponse <- system2(javaExecutable, "--version", stdout = TRUE, stderr = TRUE)
  }, silent = TRUE))
  if (is.null(checkResponse)) { # java not installed
    packageStartupMessage(message)
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
    packageStartupMessage(message)
    return(FALSE)
  }
  return(TRUE) # java installed and version is at least required version
}

#' Check if NGCHMSupportFiles environment variables are set
#'
#' NGCHMSupportFiles R package contains ShaidyMapGen.jar and ngchmWidget-min.js. Only purpose of NGCHMSupportFiles
#' is to:
#'
#' \itemize{
#'   \item provide the files ShaidyMapGen.jar and ngchmWidget-min.js
#'   \item Define environment variable SHAIDYMAPGEN with the path to ShaidyMapGen.jar
#'   \item Define environment variable NGCHMWIDGETPATH with the path to ngchmWidget-min.js
#' }
#'
#' @param None.
#'
#' @return Boolean value. TRUE if SHAIDYMAPGEN and NGCHMWIDGETPATH are set, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
checkForNGCHMSupportFilesENVs <- function() {
  SHAIDYMAPGEN <- Sys.getenv("SHAIDYMAPGEN")
  NGCHMWIDGETPATH <- Sys.getenv("NGCHMWIDGETPATH")
  if (nzchar(SHAIDYMAPGEN) > 0 && nzchar(NGCHMWIDGETPATH) > 0) { # SHAIDYMAPGEN and NGCHMWIDGETPATH are set
    return(TRUE)
  }
  return(FALSE)
}

#' Load NGCHMSupportFiles package
#'
#' Loads NGCHMSupportFiles package if it is installed.
#' If NGCHMSupportFiles is not installed, prints a message with instructions to install NGCHMSupportFiles.
#'
#' @param None.
#'
#' @return Boolean. TRUE if NGCHMSupportFiles is loaded, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
#' @seealso [checkForNGCHMSupportFilesENVs()]
loadNGCHMSupportFiles <- function() {
  if (length(suppressWarnings(find.package("NGCHMSupportFiles", quiet = TRUE))) > 0) { # NGCHMSupportFiles is installed
    requireNamespace("NGCHMSupportFiles", quietly = TRUE)
    packageStartupMessage("Loaded NGCHMSupportFiles package. NGCHMSupportFiles is used to create .ngchm, .html, and .pdf files.")
    return(TRUE)
  }
  packageStartupMessage(paste0("IMPORTANT: Package NGCHMSupportFiles is not installed. ",
                 "This package is required to create .ngchm, .html, and .pdf files. Installation: \n\n",
                 "  install.packages('NGCHMSupportFiles', repos = 'https://md-anderson-bioinformatics.r-universe.dev')\n"))
  return(FALSE)
}

#' Checks if NGCHMDemoData is loaded.
#'
#' @param None.
#'
#' @return Boolean. TRUE if NGCHMDemoData is loaded, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
checkForNGCHMDemoData <- function() {
  if ("NGCHMDemoData" %in% loadedNamespaces()) { # NGCHMDemoData is loaded
    return(TRUE)
  }
  return(FALSE)
}

#' Load NGCHMDemoData package
#'
#' Loads NGCHMDemoData package if it is installed.
#' If NGCHMDemoData is not installed, prints a message with instructions to install NGCHMDemoData.
#'
#' @param None.
#'
#' @return Boolean. TRUE if NGCHMDemoData is loaded, FALSE otherwise.
#'
#' @noRd
#' @keywords internal
#' @seealso [checkForNGCHMDemoData()]
loadNGCHMDemoData <- function() {
  if (length(suppressWarnings(find.package("NGCHMDemoData", quiet = TRUE))) > 0) { # NGCHMDemoData is installed
    requireNamespace("NGCHMDemoData", quietly = TRUE)
    packageStartupMessage("Loaded NGCHMDemoData package.")
    return(TRUE)
  }
  packageStartupMessage(paste0("NOTE: Package NGCHMDemoData is not installed. ",
                 "This package is used by the examples and website vignettes. Installation: \n\n",
                 "  install.packages('NGCHMDemoData', repos = 'https://md-anderson-bioinformatics.r-universe.dev')\n"))
  return(FALSE)
}

#' Validate and Process Colors
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

#' Enforce Range
#'
#' This function checks if a numeric value is within a specified range. If the value is outside the range,
#' it is clamped to the nearest boundary of the range. A warning message is issued if the value is clamped.
#'
#' @param x A numeric value to be checked.
#' @param min The minimum value of the range.
#' @param max The maximum value of the range.
#' @param warning_message A custom warning message to be displayed if the value is clamped.
#' @return The clamped value if it was outside the range, otherwise the original value.
#' @noRd
#' @keywords internal
enforceRange <- function(x, min, max, warning_message = "Value out of range. Clipping to range.") {
  if (!is.numeric(x) || length(x) != 1) {
    stop("x must be a single numeric value.")
  }
  if (!is.numeric(min) || length(min) != 1) {
    stop("min must be a single numeric value.")
  }
  if (!is.numeric(max) || length(max) != 1) {
    stop("max must be a single numeric value.")
  }
  if (x < min) {
    warning(warning_message)
    x <- min
  } else if (x > max) {
    warning(warning_message)
    x <- max
  }
  return(x)
}
