% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tagr.r
\name{untag}
\alias{untag}
\title{Remove tags from an object}
\usage{
untag(obj, ..., all = FALSE, envir = parent.frame())
}
\arguments{
\item{obj}{an object in the current environment}

\item{...}{one or more character strings specifying the tags to remove}

\item{all}{a logical value indimessageing whether to remove all tags from the object}

\item{envir}{the environment in which the object is defined}
}
\value{
No return value.
}
\description{
This function removes one or more tags from an object. The tags to be removed
can be specified as separate arguments or using the ellipsis (`...`)
syntax. Alternatively, setting `all` to TRUE will remove all tags from the
object. If the specified object does not have the specified tag(s), the
function will throw an error. If `all` is set to TRUE and the object has no tags,
the function will do nothing.
}
\examples{
x <- 1:10
add_tags(x, "numbers", "positive")
add_tags(x, "even")
tags(x)
# "even", "numbers", "positive"

# Remove the "positive" tag from x
untag(x, "positive")
tags(x)
# "even", "numbers"

# Remove all tags from x
untag(x, all = TRUE)
tags(x)
# "NULL"
}
\keyword{tag}
