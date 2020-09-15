#! /usr/bin/env bash

echo "####################"
echo "## Starting CNN (cb) ##"
echo "####################"

./k1-cv-convnet-currentbest.R

echo "####################"
echo "## Starting CNN 1 ##"
echo "####################"

./k1-cv-convnet-1.R

echo "####################"
echo "## Starting CNN 2 ##"
echo "####################"

./k1-cv-convnet-2.R

echo "####################"
echo "## Starting CNN 3 ##"
echo "####################"

./k1-cv-convnet-3.R

echo "####################"
echo "## Starting CNN 4 ##"
echo "####################"

./k1-cv-convnet-4.R

echo "####################"
echo "## Starting CNN 5 ##"
echo "####################"

./k1-cv-convnet-5.R

echo "####################"
echo "## Starting DNN 1 ##"
echo "####################"

./k1-cv-dnn.R

# echo "####################"
# echo "## Starting DNN (cb) ##"
# echo "####################"
#
# ./k1-cv-dnn-currentbest.R
#
# echo "####################"
# echo "## Starting RNN 1Hz ##"
# echo "####################"
#
# ./k1-cv-rnn-1hz.R
#
# echo "####################"
# echo "## Starting RNN 10Hz ##"
# echo "####################"
#
# ./k1-cv-rnn-10hz.R

# sleep 60; ./k1-cv-lm.R
# sleep 60; ./k1-cv-rf.R
