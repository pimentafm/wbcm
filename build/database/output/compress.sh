#!/bin/bash

for i in $(ls *.nc); do
  gdal_translate -co COMPRESS=DEFLATE -co PREDICTOR=1  $i "compress/"$i".tif"

  echo $i
done
