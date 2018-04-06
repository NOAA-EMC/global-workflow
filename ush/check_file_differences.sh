#!/bin/bash

baseline_dir=$1
com_dir=$2

diff --brief -Nr --exclude "*.log*" --exclude $baseline_dir $com_dir
