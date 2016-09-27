#!/usr/bin/env bash
cp -R -u src ./build
cp -u build.sbt ./build
cd ./build
sbt package
cd ../spark-1.3.0-bin-hadoop2.4/bin
./spark-submit --master local[2] --driver-memory 4G ../../build/target/scala-2.10/tdsp_2.10-1.0.jar ../../datasets/road_1000 20 4 0.01