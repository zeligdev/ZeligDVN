#' Create a Valid Inline XML Tag
#' 
#' This method creates a valid Inline XML tag based on the function's call object.
#'
#' @param ... an arbirtaty number of parameters. The first specifies the actual
#' name of the tag, while all other specify its attributes. Key-value pair
#' style attribute, while simlpe character strings (without a parameter
#' specification) stand-alone within the tag.
#' @return a block of valid XML code as a character-string
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
inline.tag <- function (...) {

  # Create the call object
  CALL <- match.call()

  # The first parameter is always the tag's name
  tag.name <- as.character(CALL[[2]])

  # Parse the attributes (everything beside the function call's name and 
  # first parameter)
  attribute.list <- as.list(CALL[-(1:2)])
  

  NAMES <- names(attribute.list)
  results <- NULL

  if (!length(attribute.list))
    return(sprintf("<%s />", tag.name))


  # Iterate through all the attributes and store the results in the variable
  # - appropriately - named 'results'
  for (k in 1:length(attribute.list)) {
    key <- NAMES[[k]]
    val <- as.character(attribute.list[[k]])

    # Catch invalid attribute names
    if (grepl("[^a-zA-Z0-9\\.]", key)) {
      warning("Skipping `", key, "' because it is invalidly formatted")
      next
    }

    # Ignore crazy overtly destructive attribtues quietly
    else if(!is.character(val) || length(val) > 1)
      next

    # This handles the case where we want a key-value pair:
    #   <tag attribute="value" />
    if (is.character(key) && nchar(key)) {
      val <- gsub("([^\\\\])\"", '\\1\\\\"', val)
      results <- c(results, paste(key, '="', val, '"', sep=""))
    }

    # This handles the case where we want an attribute sans value:
    #   <tag value />
    else if (!grepl("[^a-zA-Z0-9\\.]", val))
      results <- c(results, val)
  }

  # Join results
  attribs <- paste(results, collapse=" ")

  # Add a space if the attribute list actually has content
  # This prevents the tag's name and attribute list from touching
  if (!is.null(attribs) && nchar(attribs))
    attribs <- paste(" ", attribs, sep="")
  
  # Return surrounded by single angled quotes
  sprintf("<%s%s />", tag.name, attribs)
}
