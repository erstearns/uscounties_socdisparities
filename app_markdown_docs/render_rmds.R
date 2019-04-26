library(rmarkdown)

#render rmarkdown files to html into app folder
output_path <- "app/"

#about
render("app_markdown_docs/about.Rmd", output_dir = output_path,
       output_format = "html_document")


