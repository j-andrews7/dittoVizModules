#' Parse a string indicating a set of vectors to a list of vectors.
#' 
#' Used to parse text inputs into a list of vectors.
#' 
#' @param x A string indicating a set of vectors, e.g. "[a, b, c], [d, e]".
#'   Should not contain internal quotes around elements.
#' 
#' @return A list like `list(c("a", "b", "c"), c("d", "e"))`.
#'   If the input is "", just returns "". If the input is `NULL`, returns `NULL`.
#' 
#' @author Jared Andrews
#' @rdname INTERNAL_string_to_list_of_vectors
.string_to_list_of_vectors <- function(x) {
    if (!is.null(x)) {
        if (x != "") {
            # Remove all whitespace
            x <- gsub("\\s", "", x)

            # Split the string into a list of vectors
            x <- strsplit(x, "\\],\\[")

            # Remove the brackets from the first and last elements
            x[[1]][1] <- gsub("\\[", "", x[[1]][1])
            x[[1]][length(x[[1]])] <- gsub("\\]", "", x[[1]][length(x[[1]])])

            # Split each element into a vector
            x <- lapply(x[[1]], function(y) {
                strsplit(y, ",")[[1]]
            })
        }
    }

    x
}

#' Parse a string delimited by commas, whitespace, or new lines to a vector.
#' 
#' Used to parse text inputs into a vector.
#' 
#' @param x A string of elements delimited by comma, whitespace, or new lines,
#'   e.g. "a, b c,d, e".
#' 
#' @return A vector of strings like `c("a", "b", "c", "d", "e")`.
#'   If the input is "", just returns "". If the input is `NULL`, returns `NULL`.
#' 
#' @author Jared Andrews
#' @rdname INTERNAL_string_to_vector
.string_to_vector <- function(x) {
    if (!is.null(x)) {
        
        if (x != "") {
            # Split string to vector based on commas, whitespace, or new lines
            x <- strsplit(x, ",|\\s|,\\s")[[1]]
        }
    }

    x
}