require(rmarkdown)
render(file.path(getwd(), "10KCourse.Rmd"), output_format="html_document")
file.rename(file.path(getwd(), "10KCourse.md"), file.path(getwd(), "README.md"))
