#! /usr/bin/env bash

# echo "#######################"
# echo "## Starting CNN (cb) ##"
# echo "#######################"
#
# ./k1-cv-convnet-currentbest.R

echo "####################"
echo "## Starting CNN 1 ##"
echo "####################"

./k1-cv-convnet-1.R

sleep 120

echo "####################"
echo "## Starting CNN 2 ##"
echo "####################"

./k1-cv-convnet-2.R

sleep 120

echo "####################"
echo "## Starting CNN 3 ##"
echo "####################"

./k1-cv-convnet-3.R

sleep 120

echo "####################"
echo "## Starting CNN 4 ##"
echo "####################"

./k1-cv-convnet-4.R

sleep 120

echo "####################"
echo "## Starting CNN 5 ##"
echo "####################"

./k1-cv-convnet-5.R

sleep 120

echo "####################"
echo "## Starting CNN 6 ##"
echo "####################"

./k1-cv-convnet-6.R

sleep 120

echo "####################"
echo "## Starting CNN 7 ##"
echo "####################"

./k1-cv-convnet-7.R

sleep 120

echo "####################"
echo "## Starting CNN 8 ##"
echo "####################"

./k1-cv-convnet-8.R

sleep 120

echo "####################"
echo "## Starting CNN 9 ##"
echo "####################"

./k1-cv-convnet-9.R

sleep 120

echo "#####################"
echo "## Starting CNN 10 ##"
echo "#####################"

./k1-cv-convnet-10.R

sleep 120

echo "####################"
echo "## Starting DNN 1 ##"
echo "####################"

./k1-cv-dnn-1.R

sleep 120

echo "####################"
echo "## Starting DNN 2 ##"
echo "####################"

./k1-cv-dnn-2.R

sleep 120

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

echo "#######################"
echo "## Starting RNN 10Hz ##"
echo "#######################"

./k1-cv-rnn-10hz.R

# sleep 60; ./k1-cv-lm.R
# sleep 60; ./k1-cv-rf.R

echo "\n\n\n"
echo "BEHOLD! The deed is done!\n"
echo "*takes bow*"
