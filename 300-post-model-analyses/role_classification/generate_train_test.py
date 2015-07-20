import os, random
os.chdir("/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/300-post-model-analyses/role_classification")


with open('biz_units_jobs.tsv','r') as tsv, \
	open('biz_units_jobs.train','w') as train, \
	open('biz_units_jobs.test','w') as test:
    for line in tsv:
    	# replace some of the crap titles
    	line = line.replace("\"","")
    	line = line.replace("*","")
    	line = line.replace(".","")
    	# but only include the item if there is actually a title left
    	if str.split(line, "\t")[1] != "\n":
    		if random.random() >= 0.1:
    			train.write(line)
    		else:
    			test.write(line)
    	