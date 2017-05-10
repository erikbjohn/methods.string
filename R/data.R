#' abbrev
#'
#' A dataset containing address abbreviations
#'
#' @docType data
#'
#' @usage abbrev
#'
#' @format An object of class \code{"data.table"}.
#'
#' @keywords datasets
#'
#' @references Postal service abbreviations created in hashtables.R
#' (\href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R})
#'
#' @source \href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R}
#'
#' @examples
#' data(abbrev)
#' table(abbrev$class)
"abbrev"

#' study.cities
#'
#' A list of front range cities in the study area
#'
#' @docType data
#'
#' @usage study.cities
#'
#' @format An object of class \code{"character"}.
#'
#' @keywords datasets
#'
#' @references study.cities
#' (\href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R})
#'
#' @source \href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R}
#'
#' @examples
#' study.cities
#' cat(paste(study.cities, sep=','))
"study.cities"

#' regex.colorado.cities
#'
#' A regex string containing the same data in study.cities
#'
#' @docType data
#'
#' @usage regex.colorado.cities
#'
#' @format An object of class \code{"character"}.
#'
#' @keywords datasets
#'
#' @references regex.colorado.cities
#' (\href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R})
#'
#' @source \href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R}
#'
#' @examples
#' regex.colorado.cities
#' cat(regex.colorado.cities)
"regex.colorado.cities"

#' study.zips
#'
#' List of all zip codes in the study area
#'
#' @docType data
#'
#' @usage study.zips
#'
#' @format An object of class \code{"character"} with 205 observations.
#'
#' @keywords datasets
#'
#' @references study.zips
#' (\href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R})
#'
#' @source \href{https://www.dropbox.com/s/ix2fdubcqulb0rj/hash.tables.R?dl=0}{hashtables.R}
#'
#' @examples
#' study.zips
#' cat(min(study.zips), max(study.zips))
"study.zips"

