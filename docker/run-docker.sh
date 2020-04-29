pushd ..
docker run --name lightgbm -v ${PWD}:/home/rstudio --rm -it -e PASSWORD=lightgbm -p 8787:8787 lightgbm-r
popd
