#!/bin/sh

INSTANCES=$1
OUTDIR=$2
TOPICS=${3:-30}
OUTLABEL=${4:-}

mkdir -p $OUTDIR

INITIAL_STATE=$OUTDIR/initial_state-$OUTLABEL$TOPICS.gz
PRUNED_INSTANCES=$OUTDIR/instances.pruned$OUTLABEL
ANCHORS=$OUTDIR/anchors$OUTLABEL-$TOPICS.txt
TOPIC_WORDS=$OUTDIR/topic_words$OUTLABEL-$TOPICS.txt
OUTPUT_STATE=$OUTDIR/anchor_state$OUTLABEL-$TOPICS.gz

bin/mallet train-topics --input $INSTANCES --output-state $INITIAL_STATE --num-iterations 0 --num-topics $TOPICS

bin/mallet prune --input $INSTANCES --output $PRUNED_INSTANCES --prune-count 10

bin/mallet run cc.mallet.classify.tui.Vectors2Info --input $PRUNED_INSTANCES --print-features > $OUTDIR/vocab$OUTLABEL.txt

bin/anchor train-anchor --input $PRUNED_INSTANCES --anchors-file $ANCHORS --num-topics $TOPICS --topics-file $TOPIC_WORDS --min-docs 100 --num-random-projections 10000 --projection-density 0.2

gzcat $INITIAL_STATE | perl -w bin/modify-state.pl $OUTDIR/vocab$OUTLABEL.txt $TOPIC_WORDS | gzip > $OUTPUT_STATE
