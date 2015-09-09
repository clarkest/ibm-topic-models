# How to run this script that generates the static HTML topic browser
# topics.html is the root file for the topic browser
# To create the topic browser, call the reformatHTML.pl script
# script parameters are:
# 	1) topic_tf_idf_file: file location for word list for topic terms per topic
# 	2) yearly_tf_idf_file: file location for yearly topic term lists for yearly subcorpora
#	3) topic_proportions_file: file location for document topic proportions
# 	4) rawtext: Raw corpus data
#	5) output_folder: folder location to deposit all static html files 
#	6) topic_term_assignments: file location for all document term topic assignments
#	7) static
# For file formats, please examine the files in the Dropbox folder as mentioned in the exmaple script
# usage found below
#example usage:
perl reformatHTML.pl topic_12_ctm.csv ytr_12_ctm.csv topic_props_12_ctm.csv rawtext.txt ./topic_browser/ wdt_12_ctm.dat "STATIC_TEXT_HERE"


perl reformatHTML.pl ibm/topic_word_score.csv ibm/sub_corpus_vocab.txt ibm/doc_topic_props.csv ibm/text.txt /users/clarkbernier/sandbox/ibm_topic_browser ibm/doc_token_topic.csv "Anchor Model 1"

export RUNID=9
export DOCSDIR="/Users/clarkbernier/sandbox/ibm_docs/ibm_$RUNID"
mkdir /users/clarkbernier/sandbox/ibm_topic_browser_$RUNID
perl reformatHTML.pl $DOCSDIR/topic_word_score.csv $DOCSDIR/sub_corpus_vocab.txt $DOCSDIR/doc_topic_props.csv $DOCSDIR/text.txt /users/clarkbernier/sandbox/ibm_topic_browser_$RUNID $DOCSDIR/doc_token_topic.csv "Anchor Model $RUNID"