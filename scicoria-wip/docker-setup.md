

## Setup a local image with the following:

```
mkdir lightgbm-docker
cd lightgbm-docker
wget https://raw.githubusercontent.com/Microsoft/LightGBM/master/docker/dockerfile-r
docker build -t lightgbm-r -f dockerfile-r .
```



## Now run the local docker container

> NOTE: user is rstudio
```
docker run --name lightgbm -v ${PWD}:/home/rstudio --rm -it -e PASSWORD=lightgbm -p 8787:8787 lightgbm-r
```

Open your browser to [http://localhost:8787](http://localhost:8787)

