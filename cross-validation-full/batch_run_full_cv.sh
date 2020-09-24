#! /usr/bin/env bash

echo "####################################"
echo "## Starting full cross validation ##"
echo "####################################"

cd ~/projects/acceleep/cross-validation-full

echo "#######################"
echo "## Starting DNN      ##"
echo "#######################"

./full-k1-cv-dnn.R

sleep 120

echo "#######################"
echo "## Starting CNN      ##"
echo "#######################"

./full-k1-cv-convnet.R

sleep 120

echo "#######################"
echo "## Starting RNN 1    ##"
echo "#######################"

./full-k1-cv-rnn-1hz.R

sleep 120

echo "#######################"
echo "## Starting RNN 10   ##"
echo "#######################"

./full-k1-cv-rnn-10hz.R

sleep 120

echo "#######################"
echo "## Starting RNN orig ##"
echo "#######################"

./full-k1-cv-rnn-origres.R

echo ""
echo "BEHOLD! The deed is done!\n"
echo "*takes bow*"
