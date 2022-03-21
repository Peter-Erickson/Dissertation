# allow duplicate chunk labels
options(knitr.duplicate.label = "allow")
options(kableExtra.latex.load_packages = FALSE)
options(knitr.table.format="latex")


# knit book
bookdown::render_book(output_format = "bookdown::pdf_book",
                      output_dir = "book",
                      output_file = "Erickson_dissertation.pdf")

