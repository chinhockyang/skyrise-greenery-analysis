# Testing
flag <- is.element("sf", installed.packages())
flag

if(flag==FALSE)
  install.packages("sf", dep = TRUE)