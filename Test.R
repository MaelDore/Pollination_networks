install.packages('backports')

devtools::install_github("benmarwick/rrtools", force = T)

rrtools::use_readme_rmd(pkg = ".")

?use_readme_rmd()
