#! /usr/bin/env bash

./k1-cv-convnet.R

sleep 60; 

./k1-cv-convnet-dupe1.R

sleep 60; 

./k1-cv-convnet-dupe2.R

sleep 60; 

./k1-cv-dnn.R

# sleep 60; ./k1-cv-rf.R

# sleep 60; ./k1-cv-rnn.R

# sleep 60; ./k1-cv-lm.R
