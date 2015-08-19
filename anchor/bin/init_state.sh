#!/bin/sh

INSTANCES=$1
OUTDIR=$2
TOPICS=${3:-30}

mkdir -p $OUTDIR

INITIAL_STATE=$OUTDIR/initial_state-$TOPICS.gz
PRUNED_INSTANCES=$OUTDIR/instances.pruned
ANCHORS=$OUTDIR/anchors-$TOPICS.txt
TOPIC_WORDS=$OUTDIR/topic_words-$TOPICS.txt
OUTPUT_STATE=$OUTDIR/anchor_state-$TOPICS.gz

bin/mallet train-topics --input $INSTANCES --output-state $INITIAL_STATE --num-iterations 0 --num-topics $TOPICS

bin/mallet prune --input $INSTANCES --output $PRUNED_INSTANCES --prune-count 10

bin/mallet run cc.mallet.classify.tui.Vectors2Info --input $PRUNED_INSTANCES --print-features > $OUTDIR/vocab.txt

bin/anchor train-anchor --input $PRUNED_INSTANCES --anchors-file $ANCHORS --num-topics $TOPICS --topics-file $TOPIC_WORDS --min-docs 100 --num-random-projections 2000 --projection-density 0.1

gzcat $INITIAL_STATE | perl -w bin/modify-state.pl $OUTDIR/vocab.txt $TOPIC_WORDS | gzip > $OUTDIR/anchor_state-$TOPICS.gz
