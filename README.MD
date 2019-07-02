# data_cleaning_ex1


docker run -d -p 8787:8787 --name=rstudio --restart=always -v /${PWD}:/home/rstudio -e PASSWORD=1324 -e ROOT=TRUE rocker/rstudio