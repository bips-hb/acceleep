#! /usr/bin/env bash

cd ~/projects/acceleep/holdout-validation


echo "######################################"
echo "## Starting holdout validation: CNN ##"
echo "######################################"

./holdout-eval-convnet.R

sleep 60

echo "#############################################"
echo "## Starting holdout validation: RNN (orig) ##"
echo "#############################################"

./holdout-eval-rnn-origres.R

# Done already
# ./holdout-eval-lm.R
# ./holdout-eval-rf.R
# ./holdout-eval-dnn.R
# ./holdout-eval-rnn-1hz.R
# ./holdout-eval-rnn-10hz.R

echo "###########"
echo "## Done! ##"
echo "###########"
