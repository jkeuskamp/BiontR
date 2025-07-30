#' Retrieve Arguments from Function Call
#'
#' This function retrieves the arguments from a function call to the current function.
#'
#' @description Retrieve arguments from a function call to the current function.
#' @return A list with the arguments of the function call.
#' @param ... Additional arguments passed to the function.
#' @export
get_args <- function(...) {
  as.list(match.call(
    definition = sys.function(-1),
    call = sys.call(-1)
  ))[-1]
}

#' Concatenate Strings with an Underscore
#'
#' This function concatenates character strings with an underscore.
#'
#' @description Concatenates strings with an underscore.
#' @param ... Character strings to be concatenated.
#' @return A character string with the concatenated strings separated by an underscore.
#' @export
paste_ <- function(...) {
  paste(..., sep = "_")
}

#' Calculate the Standard Error of the Mean
#'
#' This function calculates the standard error of the mean for a numeric vector.
#'
#' @description Calculates the standard error of the mean.
#' @param x A numeric vector.
#' @param na.rm Logical, if TRUE, NA values are removed before calculation.
#' @param round Logical, if TRUE, the result is rounded to 3 significant digits.
#' @return A numeric value representing the standard error of the mean.
#' @export
se <- function(x, na.rm = TRUE, round = FALSE) {
  divident <- sqrt(var(x, na.rm = na.rm))
  divisor <- sqrt(length(na.omit(x)))

  se <- divident / divisor
  if (inherits(x, "units")) se <- units::as_units(se, units::deparse_unit(x))
  return(se)
}

#' Drop Units from a Data Frame or a Vector
#'
#' This function drops units from a data frame or a vector.
#'
#' @description Drops units from a data frame or a vector.
#' @param x A data frame or a vector with units.
#' @return A data frame or a vector without units.
#' @details If x is a data frame, all columns with units are dropped.
#' @export
safe_drop_units <- function(x) {
  if (inherits(x, "data.frame") | inherits(x, "units")) x <- units::drop_units(x)
  return(x)
}

#' Transfer Units from One Object to Another
#'
#' This function transfers units from one object to another.
#'
#' @description Transfers units from one object to another.
#' @param recipient An object to which the units will be transferred.
#' @param donor An object from which the units will be taken.
#' @param silent Logical, if TRUE, no error is thrown if the recipient already has units.
#' @return An object with the units of the donor.
#' @export
transfer_units <- function(recipient, donor, silent = TRUE) {
  if (!inherits(recipient, "units") & inherits(donor, "units")) {
    recipient <- units::as_units(recipient, units::deparse_unit(donor))
    return(recipient)
  } else {
    if (silent == TRUE) {
      return(recipient)
    } else {
      stop("This function only works if recipient has no unit, and the donor does")
    }
  }
}

#' Calculate the Standard Error of the Median
#'
#' This function calculates the standard error of the median for a numeric vector.
#'
#' @description Calculates the standard error of the median.
#' @param x A numeric vector.
#' @return A numeric value representing the standard error of the median.
#' @export
se_median <- function(x) {
  sqrt(pi / 2) * se(x)
}

#' Compute the Variance of a Weighted Mean
#'
#' This function computes the variance of a weighted mean following Cochran 1977 definition.
#'
#' @description Computes the variance of a weighted mean following Cochran 1977 definition.
#' @references \insertRef{Gatz1995AE}{BiontR}
#' @importFrom Rdpack reprompt
#' @param x A numeric vector.
#' @param w A numeric vector of weights.
#' @param na.rm Logical, if TRUE, NA values are removed before calculation.
#' @return A numeric value representing the standard error of the weighted mean.
#' @export
weighted.var.se <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  n <- length(w)
  xWbar <- weighted.mean(x, w, na.rm = na.rm)
  wbar <- mean(w)
  out <- n / ((n - 1) * sum(w)^2) * (sum((w * x - wbar * xWbar)^2) - 2 * xWbar * sum((w - wbar) * (w * x - wbar * xWbar)) + xWbar^2 * sum((w - wbar)^2))
  return(out)
}

#' Install a Unit if Not Already Installed
#'
#' This function installs a unit if it is not already installed.
#'
#' @description Installs a unit if it is not already installed.
#' @param symbol A character string representing the unit symbol.
#' @param def A character string representing the unit definition.
#' @param name A character string representing the unit name.
#' @details If the unit is already installed, it is removed first.
#' @return NULL
#' @export
safe_unit_install <- function(symbol = character(0), def = character(0), name = character(0)) {
  if (identical(name, character(0))) {
    name <- symbol
  }
  units::remove_unit(symbol, name)
  try(units::install_unit(symbol, def, name), silent = TRUE)
}

#' Row Sums for Data Frames with Units
#'
#' This function calculates row sums for data frames with units.
#'
#' @description Row sums for data frames with units.
#' @param data A data frame with units.
#' @param ... Additional arguments passed to the `rowSums` function.
#' @return A numeric vector with the row sums of the data frame.
#' @export
rowSums.unit <- function(data, ...) {
  X <- rowSums(data, ...)
  if (inherits(data[[1, 1]], "units")) {
    if (length(unique(sapply(data, units::deparse_unit))) == 1) {
      X <- units::as_units(X, units::deparse_unit(data[[1, 1]]))
    } else {
      warning("Units are not all the same across the row. Cannot calculate result!")
      return(NULL)
    }
  }
  return(X)
}

#' Row Means for Data Frames with Units
#'
#' This function calculates row means for data frames with units.
#'
#' @description Row means for data frames with units.
#' @param data A data frame with units.
#' @param ... Additional arguments passed to the `rowMeans` function.
#' @return A numeric vector with the row means of the data frame.
#' @export
rowMeans.unit <- function(data, ...) {
  X <- rowMeans(data, ...)
  if (inherits(data[[1, 1]], "units")) {
    if (length(unique(sapply(data, units::deparse_unit))) == 1) {
      X <- units::as_units(X, units::deparse_unit(data[[1, 1]]))
    } else {
      warning("Units are not all the same across the row. Cannot calculate result!")
      return(NULL)
    }
  }
  return(X)
}

#' Row Mins for Data Frames with Units
#'
#' This function calculates row minimums for data frames with units.
#'
#' @description Row minimums for data frames with units.
#' @param data A data frame with units.
#' @param ... Additional arguments passed to the `pmin` function.
#' @return A numeric vector with the row minimums of the data frame.
#' @export
rowMins.unit <- function(data, ...) {
  X <- do.call(pmin, c(data, ...))
  return(X)
}

#' If-else for Data Frames with Units
#'
#' This function is a modified version of ifelse for data frames with units. __DEPRECATED
#'
#' @description If-else for data frames with units.
#' @param cond A logical vector or a condition.
#' @param yes A value to return if the condition is TRUE.
#' @param no A value to return if the condition is FALSE.
#' @return A vector with the same class as yes, with units if yes has units.
#' @export
ifelse.unit <- function(cond, yes, no) {
  X <- ifelse(cond, yes, no)
  attributes(X) <- attributes(yes)
  return(X)
}

#' Set Names and Units for a Data Frame
#' This function sets names and units for a data frame based on a specified format.
#' @description Sets names and units for a data frame.
#' @param df A data frame with column names containing labels and units.
#' @param split A regular expression to split the column names into labels and units (default: "[\\[\\]]").
#' @param comment A regular expression to remove comments from the column names (default: "\\s*\\([^\\)]+\\)").
#' @param custom_units A character vector of custom units to install (default: NULL).
#' @return A data frame with updated column names and units.
#' @details The function removes comments from the column names, splits the names into labels and units, and sets the units for numeric columns. If a unit is not specified, it defaults to "1". It also installs any custom units specified in the `custom_units` argument.
#' @export
set_names_and_units <- function(df, split = "[\\[\\]]", comment = "\\s*\\([^\\)]+\\)", custom_units = NULL) {
  # install required units to process file TODO: provide interactive option
  for (custom_unit in custom_units) try(safe_unit_install(custom_unit))
  # remove comments (default: text between brackets)
  names <- names(df) |>
    trimws() |>
    gsub(comment, "", x=_)
  # split label using first part as column name and second part as unit
  split_label <- strsplit(names, split = split, perl = TRUE)
  new_label <- sapply(split_label, "[", 1) |>
    trimws() |>
    make.names(unique = TRUE)
  new_units <- sapply(split_label, "[", 2)
  names(df) <- new_label
  new_units[which(new_units == "")] <- "1"
  for (i in 1:dim(df)[2]) {
    if (class(df[[i]])[[1]] %in% c("numeric", "logical", "integer")) {
      if (!is.na(new_units[[i]])) {
        try(df[[i]] <- do.call(units::set_units, list(df[[i]], new_units[[i]], mode = "standard")), silent = FALSE)
      }
    }
  }
  return(df)
}

#' Save a ggplot object
#' This function saves a ggplot object
#' @description Saves a ggplot object
#' @param plot The plot to save.
#' @param filename The name of the file to save the plot to. If not specified, the plot name is used.
#' @param format The format to save the plot in (default: "pdf"). Options are "pdf", "emf", "png", "jpeg", "tiff", and "svg".
#' @param width The width of the plot in inches (default: 8).
#' @param height The height of the plot in inches (default: 6).
#' @param directory The directory to save the plot in. If not specified, the current working directory is used.
#' @param dpi The resolution in dots per inch (default: 300).
#' @return A message indicating that the plot has been saved.
#' @export
figsave <- function(plot, filename, format = c("pdf", "emf", "png", "jpeg", "tiff", "svg")[1], width = 4, height = 3, directory = NULL, dpi = 300) {
  on.exit(expr = try(grDevices::dev.off, silent = TRUE))

  if (!inherits(plot, "ggplot") && !inherits(plot, "gtable")) {
    stop("Please specify a ggplot object to save.")
  }

  if (is.null(directory)) {
    directory <- "."
  }

  filename <- file.path(directory, paste(filename, format, sep = "."))

  if (!is.list(plot)) {
    plot <- list(plot)
  }

  if (format == "pdf") {
    grDevices::pdf(file = filename, width = width, height = height)
    for (p in plot) {
      if (is.null(p)) next
      if (inherits(p, "ggplot")) suppressWarnings(print(p))
      if (inherits(p, "gtable")) grid::grid.draw(p)
    }
    grDevices::dev.off()
  } else if (format == "emf") {
    for (i in seq_along(plot)) {
      p <- plot[[i]]
      if (is.null(p)) next
      devEMF::emf(
        file = sub("\\.emf$", paste0("_", i, ".emf"), filename),
        width = width,
        height = height,
        emfPlusFont = TRUE
      )
      if (inherits(p, "ggplot")) print(p)
      if (inherits(p, "gtable")) grid::grid.draw(p)
      grDevices::dev.off()
    }
  } else if (format %in% c("png", "jpeg", "tiff")) {
    grDevices::png(file = filename, width = width * dpi, height = height * dpi)
    for (p in plot) {
      if (is.null(p)) next
      if (inherits(p, "ggplot")) print(p)
      if (inherits(p, "gtable")) grid::grid.draw(p)
    }
    grDevices::dev.off()
  } else if (format == "svg") {
    grDevices::svg(file = filename, width = width, height = height)
    for (p in plot) {
      if (is.null(p)) next
      if (inherits(p, "ggplot")) print(p)
      if (inherits(p, "gtable")) grid::grid.draw(p)
    }
    grDevices::dev.off()
  }

  return(invisible(NULL))
}

#' Save the figure currently displayed in RStudio
#' This function saves the figure displayed in RStudio in the specified format.
#' @description Saves a figure in RStudio.
#' @param filename The name of the file to save the figure to. If not specified, the figure name is used. do not use a file extension, it will be added automatically.
#' @param format The format to save the figure in (default: "pdf"). Options are "pdf", "png", "jpeg", "tiff", and "svg".
#' @param width The width of the figure in inches (default: 8).
#' @param height The height of the figure in inches (default: 6).
#' @param directory The directory to save the figure in. If not specified, the current working directory is used.
#' @param dpi The resolution in dots per inch (default: 300).
#' @return A message indicating that the figure has been saved.
#' @export
RSfigsave <- function(filename=NULL, format = c("png", "jpeg", "tiff", "svg")[1], width = 4, height = 3, directory = NULL, dpi = 300) {
  on.exit(expr = try(grDevices::dev.off, silent = TRUE))

  if (is.null(directory)) {
    directory <- getwd()
  }

  if (!rstudioapi::isAvailable()) {
    stop("RStudio API is not available.")
  }

  if (is.null(filename)) {
    filename <- "RSTUDIO_PLOT"
  }

  filename <- file.path(directory, paste(filename, format, sep = "."))

  rstudioapi::savePlotAsImage(
    file = filename,
    format = format,
    width = width * dpi,
    height = height * dpi
  )

  message(paste("Saved current plot as", filename))
}

#' Get Elements from a Nested List
#' This function retrieves all elements with a specific name from a nested list.
#' @description Retrieves elements from a nested list.
#' note that this code is retrieved from https://stackoverflow.com/questions/64578972/pull-all-elements-with-specific-name-from-a-nested-list
#' @param x A nested list.
#' @param element A character string representing the name of the element to retrieve.
#' @return A list containing all elements with the specified name.
#' @export
get_elements <- function(x, element) {
  if (is.list(x)) {
    if (element %in% names(x)) {
      x[[element]]
    } else {
      lapply(x, get_elements, element = element)
    }
  }
}

# list_dependencies<-function(file.path,...) {
#  #try to get open file in rstudio if no file path is specified
#  if(missing(file.path)) file_path<- rstudioapi::getSourceEditorContext()$path
#    spot_funs(file_path,...) }

#' Create a Label for Gases with the Right Formatting
#' This function creates a label for gases with the right formatting, using uppercase letters for the gas name and subscripts for the numbers.
#' @description Creates a label for gases with the right formatting.
#' @param gas A character string representing the gas name.
#' @return A character string representing the formatted gas label.
#' @export
make_gas_label <- function(gas) {
  gas_normalfont <- gas |>
    stringr::str_match_all("[A-Z]+") |>
    unlist()
  gas_subscript <- gas |>
    stringr::str_match_all("[0-9]+") |>
    unlist()
  gaslabel <- paste0(gas_normalfont, "[", gas_subscript, "]")
  return(gaslabel)
}

#' Calculate a Rolling Mean with a Minimum Number of Observations
#' This function calculates a rolling mean for a numeric vector with a specified width and a minimum number of observations required to compute the mean.
#' @description Calculates a rolling mean with a minimum number of observations.
#' @param x A numeric vector.
#' @param minobs An integer specifying the minimum number of observations required to compute the mean (default: width/2).
#' @param width An integer specifying the width of the rolling mean window.
#' @param align A character string specifying the alignment of the rolling mean ("left" or "right", default: "left").
#' @return A numeric vector representing the rolling mean with the same length as the input vector, with NA values where the minimum number of observations is not met.
#' @export
rollmean <- function(x, minobs = width / 2, width, align = c("left", "right")[1]) {
  if (!align %in% c("left", "right")) {
    stop("align must be left or right")
  } else if (align == "right") x <- rev(x)
  output <- sapply(seq_along(x), function(u) {
    data <- NULL
    data <- x[max(u - (width / 2 - 1), 0):min((u + width / 2), length(x))]
    if (sum(!is.na(data)) >= minobs) {
      mean(data, na.rm = TRUE)
    } else {
      (NA)
    }
  })
  if (align == "right") output <- rev(output)
  transfer_units(output, x)
}
