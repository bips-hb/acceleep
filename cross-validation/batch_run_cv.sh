#! /usr/bin/env bash

./k1-cv-lm.R

sleep 60

./k1-cv-convnet.R

sleep 60

./k1-cv-dnn.R

sleep 60

./k1-cv-rnn.R

