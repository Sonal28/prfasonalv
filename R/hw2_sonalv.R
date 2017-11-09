## Code to accept any of the offence_descriptions found in Offence Level and
## will accept a 2-element vector of postcodes.

#' Postcode correlation ggplot
#' \code{crime_file} : Gets crime data of year 2012 to 2017, filter it and generates ggplot between 2 postcode
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of "offence_level_1"
#' @param postcodes A two-element character vector. Each element is an SA postcode.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input postcodes.
#' @examples
#' crime_file(data1,"ACTS INTENDED TO CAUSE INJURY", c(5000, 5006))

library(readxl)
library(data.table)
library(dplyr)

crime_file <- function(crime_data, offence_description, postcodes) {
  require(data.table)
  require(ggplot2)


  # Error catching: Test to ensure the input `postcodes` vector has two elements
  if (length(postcodes) != 2) {
    stop("Please enter two postcodes")
  }

  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")

  #Get names of actual table
  actual_colnames <- names(crime_data)

  #Test to see if the input table has the right column names
  if (!all.equal(expected_colnames, actual_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input postcode and offence description exist in crime_data
  if (any(!postcodes %in% crime_data$postcode) |
      !offence_description %in% crime_data$offence_level_1) {
    stop("Input postcode and offence description does not exist in crime_data")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "postcode", "total_offence_count"
  plot_data <- crime_data[postcode %in% c(postcodes[1], postcodes[2]) & offence_level_1 %in% c(offence_description),
                          list(total_offence_count = sum(crime_data$offence_count),postcode),
                          by = date]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, postcode := plyr::mapvalues(postcode, postcodes, c("x", "y"))]

  plot_data <- dcast(plot_data, date ~ postcode, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot
  ggplot(plot_data, aes(x , y , group = month(date))) +
    geom_count() +
    labs(x = postcodes[1],
         y = postcodes[2])
}

