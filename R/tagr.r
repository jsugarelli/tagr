#' Add tags to an R object
#'
#' This function adds tags to an existing R object. The tags are stored as attributes of the object
#' and can be later used to identify and manipulate the object. If the specified object does not exist
#' in the specified environment, an error is thrown.
#'
#' @param x The name of the object to tag (unquoted).
#' @param ... The tags to add to the object.
#' @param envir The environment in which the object exists (defaults to the parent frame).
#'
#' @return No return value.
#'
#' @examples
#' # create a vector and add some tags
#' x <- c(1, 2, 3)
#' add_tags(x, "foo", "bar")
#'
#' @export
add_tags <- function(x, ..., envir = parent.frame()) {
  # check if the object exists in the specified environment
  obj_name <- deparse(substitute(x))
  if (!exists(obj_name, envir = envir)) {
    stop("Object not found")
  }

  # get the current tags (if any)
  current_tags <- attributes(get(obj_name, envir = envir))$tags

  # add new tags
  new_tags <- c(current_tags, ...)

  # remove duplimessagees
  new_tags <- unique(new_tags)

  # create a local copy of the object in the specified environment
  obj_copy <- get(obj_name, envir = envir)

  # attach the new tags to the object copy
  attributes(obj_copy)$tags <- new_tags

  # update the object in the specified environment with the modified copy
  assign(obj_name, obj_copy, envir = envir)
}



#' Check if an R object has specified tags
#'
#' This function checks if an existing R object has specified tags. The tags are stored as attributes of
#' the object. If the specified object does not exist in the specified environment, an error is thrown.
#'
#' @param x The name of the object to check (unquoted).
#' @param ... The tags to check for.
#' @param envir The environment in which the object exists (defaults to the parent frame).
#'
#' @return A logical value indimessageing whether the object has all of the specified tags.
#'
#' @examples
#' # create a vector and add some tags
#' x <- c(1, 2, 3)
#' add_tags(x, "foo", "bar")
#'
#' # check if the vector has the specified tags
#' has_tag(x, "foo", "bar")
#'
#' @export
has_tag <- function(x, ..., envir = parent.frame()) {
  # check if the object exists in the specified environment
  obj_name <- deparse(substitute(x))
  if (!exists(obj_name, envir = envir)) {
    stop("Object not found")
  }

  # get the current tags (if any)
  current_tags <- attributes(get(obj_name, envir = envir))$tags

  # check if all tags are present
  all_present <- all(c(...) %in% current_tags)

  return(all_present)
}



#' List objects in an environment by tag
#'
#' This function lists all objects in a specified environment that have all of the specified tags.
#' The tags are stored as attributes of the objects. If the only.tags argument is set to TRUE, only
#' objects that have at least one tag are listed. If no objects with the specified tags are found,
#' a message is printed to the console. If the specified environment does not exist, an error is thrown.
#'
#' @param ... The tags to filter by.
#' @param only.tags Logical value indimessageing whether to include only objects that have at least one tag.
#' @param envir The environment to search for tagged objects (defaults to the parent frame).
#'
#' @return No return value. A table with information about each tagged object, including its name, type, and tags is printed to the console.
#'
#' @examples
#' # create some objects and add tags
#' x <- c(1, 2, 3)
#' y <- matrix(1:9, nrow = 3)
#' z <- "hello world"
#' add_tags(x, "foo")
#' add_tags(y, "bar")
#' add_tags(z, "baz")
#'
#' # list objects with specified tags
#' ls_bytag("foo", "bar")
#'
#' @export
ls_bytag <- function(..., only.tags = TRUE, envir = parent.frame()) {
  # get all objects in the specified environment
  all_objects <- ls(envir = envir)

  # filter the objects that have all of the specified tags
  tag_filter <- function(obj) {
    all(c(...) %in% attributes(get(obj, envir = envir))$tags)
  }
  tagged_objects <- Filter(tag_filter, all_objects)

  # print a message if no objects with the specified tags were found
  if (length(tagged_objects) == 0) {
    message("No objects with the specified tags were found.", appendLF=FALSE)
  } else {
    if (only.tags) {
      tagged_objects <- Filter(function(obj) !is.null(attributes(get(obj, envir = envir))$tags), all_objects)
    }

    # get information about each tagged object
    info_list <- lapply(tagged_objects, function(obj_name) {
      obj <- get(obj_name, envir = envir)
      obj_type <- class(obj)[1]
      obj_tags <- paste0(attributes(obj)$tags, collapse = ", ")
      list(name = obj_name, type = obj_type, tags = obj_tags)
    })

    # order the list alphabetically by object name
    info_list <- info_list[order(sapply(info_list, function(x) x$name))]

    # print the information in a tabular format
    message(sprintf("%-20s%-20s%-20s\n", "Name", "Type", "Tags"), appendLF=FALSE)
    message(sprintf("%-20s%-20s%-20s\n", "----", "----", "----"), appendLF=FALSE)
    for (info in info_list) {
      message(sprintf("%-20s%-20s%-20s\n", info$name, info$type, info$tags), appendLF=FALSE)
    }
  }
}



#' Remove tags from an object
#'
#' This function removes one or more tags from an object. The tags to be removed
#' can be specified as separate arguments or using the ellipsis (`...`)
#' syntax. Alternatively, setting `all` to TRUE will remove all tags from the
#' object. If the specified object does not have the specified tag(s), the
#' function will throw an error. If `all` is set to TRUE and the object has no tags,
#' the function will do nothing.
#'
#' @param obj an object in the current environment
#' @param ... one or more character strings specifying the tags to remove
#' @param all a logical value indimessageing whether to remove all tags from the object
#' @param envir the environment in which the object is defined
#' @return No return value.
#' @keywords tag
#' @export
#' @examples
#' x <- 1:10
#' add_tags(x, "numbers", "positive")
#' add_tags(x, "even")
#' tags(x)
#' # "even", "numbers", "positive"
#'
#' # Remove the "positive" tag from x
#' untag(x, "positive")
#' tags(x)
#' # "even", "numbers"
#'
#' # Remove all tags from x
#' untag(x, all = TRUE)
#' tags(x)
#' # "NULL"
untag <- function(obj, ..., all = FALSE, envir = parent.frame()) {
  obj_name <- deparse(substitute(obj))

  # check if the object exists in the environment
  if (!exists(obj_name, envir = envir)) {
    stop(paste0("Object '", obj_name, "' not found in environment."))
  }

  # check if any tags are provided
  if (length(list(...)) == 0 & !all)  {
    stop("Please provide at least one tag to remove or set 'all' to TRUE.")
  }

  # get the list of tags to remove
  if (all) {
    tags <- attributes(obj)$tags
  } else {
    tags <- list(...)
  }

  # check if the object has any of the tags provided as arguments
  obj_tags <- attributes(obj)$tags
  common_tags <- intersect(obj_tags, tags)
  if (length(common_tags) == 0) {
    stop(paste0("Object '", obj_name, "' does not have any of the specified tags."))
  } else if (length(common_tags) < length(tags)) {
    warning(paste0("Object '", obj_name, "' only has tags '", paste(common_tags, collapse = "', '"), "'."))
  }

  # remove the tags from the object
  for (tag in tags) {
    attributes(obj)$tags <- setdiff(attributes(obj)$tags, tag)
  }

  # assign the modified object back to its original name in the environment
  assign(obj_name, obj, envir = envir)
}



#' Retrieve tags associated with an object
#'
#' This function retrieves the tags associated with a specified object.
#'
#' @param obj The object to retrieve tags from.
#' @param envir The environment in which the object exists. Defaults to the parent environment.
#'
#' @return Returns a sorted vector of tags associated with the object. If the object has no tags, the function prints a message and returns NULL.
#'
#' @examples
#' # create a variable
#' x <- 5
#'
#' # add tags to the variable
#' add_tags(x, "important", "numeric")
#'
#' # retrieve the tags
#' tags(x)
#'
#' @export
tags <- function(obj, envir = parent.frame()) {
  if (!is.null(attributes(obj)$tags)) {
    sorted_tags <- sort(attributes(obj)$tags)
    message(paste0(sorted_tags, collapse =", "), appendLF=FALSE)
    invisible(sorted_tags)
  } else {
    message("No tags found for object.", appendLF=FALSE)
    invisible(NULL)
  }
}



#' Remove the 'tags' attribute from an object
#'
#' This function removes the 'tags' attribute from an object, effectively
#' removing all tags attached to the object. If the object does not have a
#' 'tags' attribute, the function simply returns without modifying the object.
#'
#' @param obj The object to remove tags from
#' @param envir The environment in which to look for the object
#'
#' @return No return value.
#'
#' @examples
#' # create an object and add some tags
#' my_vec <- c(1, 2, 3)
#' add_tags(my_vec, "important", "numeric")
#'
#' # remove the tags from the object
#' clean_tags(my_vec)
#'
#' @export
clean_tags <- function(obj, envir = parent.frame()) {
  obj_name <- deparse(substitute(obj))

  # check if the object has a "tags" attribute
  if (!"tags" %in% names(attributes(obj))) {
    message("The object does not have a 'tags' attribute.", appendLF=FALSE)
    return(invisible())
  }

  # remove the tags attribute from the object
  attributes(obj)$tags <- NULL

  # assign the modified object back to its original name in the environment
  assign(obj_name, obj, envir = envir)

}



#' Remove Objects by Tag
#'
#' Remove all objects in the current environment that have a specified tag.
#'
#' @param tag The tag to search for.
#' @param envir The environment to search in. Defaults to the current environment.
#' @param confirm If \code{TRUE}, a confirmation prompt will be displayed before objects are deleted.
#'
#' @return No return value.
#' @export
#'
#' @examples
#' # create some objects with tags
#' x <- 1:10
#' y <- matrix(rnorm(16), 4, 4)
#' add_tags(x, "numbers")
#' add_tags(y, "matrix")
#'
#' # remove all objects with the "numbers" tag
#' rm_bytag("numbers")
#'
#' # remove all objects with the "matrix" tag without confirmation prompt
#' set_confirm(FALSE)
#' rm_bytag("matrix")
#'
#' # confirm that objects have been removed
#' ls()
#'
#' # clean up
#' rm(set_confirm, x, y)
#'
#' @seealso \code{\link{add_tags}}, \code{\link{has_tag}}, \code{\link{ls_bytag}}
#' @export
rm_bytag <- function(tag, envir = parent.frame(), confirm = getOption("tagsr.confirm", FALSE)) {
  # find objects with the specified tag
  objs <- ls(envir)
  tag_objs <- objs[sapply(objs, function(x) { exists("tags", envir = envir) && tag %in% attributes(get(x, envir = envir))$tags })]

  if (length(tag_objs) == 0) {
    message(paste0("No object with tag '", tag, "' found in environment '", deparse(substitute(envir)), "'.\n", sep = ""), appendLF=FALSE)
  }
  else {
    # confirmation prompt
    if (confirm) {
      message("The following objects will be deleted:\n", appendLF=FALSE)
      message(paste(tag_objs, collapse = ", "), appendLF=FALSE)
      message("\n\n")

      delete <- readline(prompt = "Are you sure you want to delete these objects? (y/n) ")
      if (tolower(delete) != "y") {
        message("Deletion cancelled.\n", appendLF=FALSE)
      }
    }

    # remove objects with the specified tag
    for (obj in tag_objs) {
      rm(list = obj, envir = envir)
    }

    # output names of deleted objects
    message("The following objects were deleted:\n", appendLF=FALSE)
    message(paste(tag_objs, collapse = ", "), appendLF=FALSE)
    message("\n", appendLF=FALSE)
  }
}



#' Set confirmation prompt for deleting objects
#'
#' This function sets the confirmation prompt for the \code{\link{rm_bytag}} function
#' when deleting objects with a specified tag.
#'
#' @param confirm A logical value indimessageing whether to prompt for confirmation before deleting objects. If \code{TRUE}, a confirmation prompt will be displayed. If \code{FALSE}, no confirmation prompt will be displayed. If \code{NULL}, the current setting will be returned without changing it.
#'
#' @return If \code{confirm} is \code{NULL}, the current setting for the confirmation prompt is returned. Otherwise, the confirmation prompt setting is updated.
#'
#' @examples
#' # turn on confirmation prompt
#' set_confirm(TRUE)
#'
#' # turn off confirmation prompt
#' set_confirm(FALSE)
#'
#' # get current confirmation prompt setting
#' set_confirm()
#'
#' @export
set_confirm <- function(confirm = NULL) {
  if (!is.null(confirm)) {
    options(tagsr.confirm = confirm)
  } else {
    getOption("tagsr.confirm")
  }
}
