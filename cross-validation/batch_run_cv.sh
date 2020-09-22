#! /usr/bin/env bash

# echo "#######################"
# echo "## Starting CNN (cb) ##"
# echo "#######################"
#
# ./k1-cv-convnet-currentbest.R

# echo "####################"
# echo "## Starting CNN 1 ##"
# echo "####################"
#
# ./k1-cv-convnet-1.R
#
# sleep 120

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
echo "## Starting DNN (cb) ##"
echo "####################"

./k1-cv-dnn-currentbest.R

sleep 120

echo "####################"
echo "## Starting DNN 1 ##"
echo "####################"

./k1-cv-dnn-1.R

echo "####################"
echo "## Starting DNN 2 ##"
echo "####################"

./k1-cv-dnn-2.R

sleep 120


# echo "####################"
# echo "## Starting RNN 1Hz ##"
# echo "####################"
#
# ./k1-cv-rnn-1hz.R

# echo "#######################"
# echo "## Starting RNN 10Hz ##"
# echo "#######################"
#
# ./k1-cv-rnn-10hz.R

# echo "#######################"
# echo "## Starting RNN orig ##"
# echo "#######################"
#
# ./k1-cv-rnn-origres.R

# sleep 60; ./k1-cv-lm.R
# sleep 60; ./k1-cv-rf.R

echo ""
echo "BEHOLD! The deed is done!\n"
echo "*takes bow*"
