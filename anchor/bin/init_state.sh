#!/bin/sh

INSTANCES=$1
OUTDIR=$2

mkdir -p $OUTDIR

bin/mallet train-topics --input $INSTANCES --output-state $OUTDIR/initial_state.gz --num-iterations 0

bin/mallet prune --input $INSTANCES --output $OUTDIR/instances.pruned --prune-count 10

bin/mallet run cc.mallet.classify.tui.Vectors2Info --input $OUTDIR/instances.pruned --print-features > $OUTDIR/vocab.txt

bin/anchor train-anchor --input $OUTDIR/instances.pruned --anchors-file $OUTDIR/anchors.txt --num-topics 30 --topics-file $OUTDIR/topic_words.txt --min-docs 100 --num-random-projections 2000 --projection-density 0.1

gzcat $OUTDIR/initial_state.gz | perl -w bin/modify-state.pl $OUTDIR/vocab.txt $OUTDIR/topic_words.txt | gzip > $OUTDIR/anchor_state.gz
