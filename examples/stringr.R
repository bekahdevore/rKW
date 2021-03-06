library(stringr)
#https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html

## Using base R to replace multiple strings with one
vec = c('blue','red','flower','bee')
sub("blue|red", "colour", vec)

fruits <- c("one apple", "two pears", "three bananas")

str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")

str_replace(fruits, "([aeiou])", "")
str_replace(fruits, "([aeiou])", "\\1\\1")
str_replace(fruits, "[aeiou]", c("1", "2", "3"))
str_replace(fruits, c("a", "e", "i"), "-")

fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")

str_replace_all(fruits, "([aeiou])", "")
str_replace_all(fruits, "([aeiou])", "\\1\\1")
str_replace_all(fruits, "[aeiou]", c("1", "2", "3"))
str_replace_all(fruits, c("a", "e", "i"), "-")

# If you want to apply multiple patterns and replacements to the same
# string, pass a named version to pattern.
str_replace_all(str_c(fruits, collapse = "---"),
                c("one" = "1", "two" = "2", "three" = "3"))
