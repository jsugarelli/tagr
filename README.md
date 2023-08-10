# tagsr: R Package for Object Tagging

`tagsr` provides functions to manage tags associated with R objects. You can add, remove, check, list, and clean tags on any R object in the environment.

## 1. Installation

```R
# install.packages("tagr", dependencies = TRUE)
```


## 2. Overview

The `tagsr` package allows users to:

- Add tags to objects.
- Check if an object has specific tags.
- List all objects in an environment by their tags.
- Remove tags from objects.
- Retrieve tags associated with an object.
- Clean the 'tags' attribute from an object.
- Remove objects from an environment based on their tags.

## 3. Getting Started

Below are a few quick examples to get you started:

### Adding Tags

```R
x <- c(1, 2, 3)
add_tags(x, "foo", "bar")
```

### Checking Tags

```R
x <- c(1, 2, 3)
add_tags(x, "foo", "bar")
has_tag(x, "foo") # Returns TRUE
```

### Listing Objects by Tag

```R
x <- c(1, 2, 3)
y <- matrix(1:9, nrow = 3)
z <- "hello world"
add_tags(x, "foo")
add_tags(y, "bar")
add_tags(z, "baz")
ls_bytag("foo")
```

### Removing Tags

```R
x <- 1:10
add_tags(x, "numbers", "positive")
untag(x, "positive") # Removes the "positive" tag
```

### Retrieving Tags

```R
x <- 5
add_tags(x, "important", "numeric")
tags(x) # Returns "important, numeric"
```

For more detailed documentation and examples, refer to the individual function documentation using the `help()` or `?` function in R.
