# load the world reloaded data from tsv using tsv import
# load the post-prcessed world data to see which ended up being kept

SELECT commentid, title, TEXT, COUNT(*)
	FROM world_reloaded  
	GROUP BY commentid, title, TEXT 
	HAVING COUNT(*) >1;

# move the offending rows into a new table
CREATE TABLE temp_id_fix 
	SELECT * FROM world_reloaded 
	WHERE commentid IN  
		("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>")
; 
# then, update the jacked commentids to parse out by comment and text
UPDATE temp_id_fix SET commentid = CONCAT(SUBSTR(commentid,2,20), SUBSTR(title,1,20)); 

# delete the off rows by hand
# rule 1: keep only one of each unique title-id-text (16 -> 8 rows)
# rule 2: it is obvious which text goes with each title, delete the off ones (8 -> 4 rows)

# remove the original 16 rows from the main table
DELETE FROM world_reloaded WHERE commentid IN  
		("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>");
		
# move the four updated rows into the world_updated table
INSERT INTO world_reloaded SELECT * FROM temp_id_fix;

# update the parent ids that reference these comments
UPDATE world_reloaded 
	SET parent_comment_id = CONCAT(SUBSTR(parent_comment_id,2,20), SUBSTR(title,1,20))
	WHERE parent_comment_id IN ("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>");		
# we need the new_ids for the deleted ones
SELECT wo.new_id
	FROM world_orig wo
	LEFT JOIN temp_id_fix tif
	USING (new_id)
	WHERE wo.commentid IN ("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>")
		AND tif.new_id IS NULL;
		
		


SELECT new_id FROM temp_id_fix;


SELECT COUNT(DISTINCT title), COUNT(*) FROM world_reloaded;

# which parent ids in the processed data do not have post-processed parents?
ALTER TABLE world_ngrams ADD INDEX commentidx (commentid);
ALTER TABLE world_ngrams ADD INDEX parentidx (parent_comment_id);
SELECT wn1.parent_comment_id, CHAR_LENGTH(wr.text), CHAR_LENGTH(wr.title), 
	CHAR_LENGTH(wr.text)-CHAR_LENGTH(wr.title) AS text_len, wr.text, wn1.text
	FROM world_ngrams wn1 
	LEFT JOIN world_ngrams wn2
		ON (wn1.parent_comment_id = wn2.commentid)
	LEFT JOIN world_reloaded wr
		ON (wn1.parent_comment_id = wr.commentid)
	WHERE wn2.commentid IS NULL AND wn1.parent_comment_id <> "null"
	ORDER BY text_len;

SELECT * FROM world_reloaded WHERE parent_comment_id = "<ffd612e85c.f002a818.WORLDJAM@d25was503.mkm.can.ibm.com>";

<ffd6bd1fa2.b12dfe2b.WORLDJAM@d25was503.mkm.can.ibm.com>;
SELECT commentid, parent_comment_id, TEXT, CHAR_LENGTH(TEXT) FROM world_reloaded WHERE TEXT LIKE "%develop a better way to follow up on a project%";

SELECT parent_comment_id, `text` FROM world_reloaded WHERE commentid="<ffd6a88d39.1aacc6ff.WORLDJAM@d25was503.mkm.can.ibm.com>";




# VALUES JAM
#   load the post-process values jam data
# load the pre-process values data
ALTER TABLE values_ngrams ADD INDEX commentidx (id);
ALTER TABLE values_ngrams ADD INDEX parentidx (Parent);
ALTER TABLE values_clean ADD INDEX commentidx (id);
SELECT wn1.Parent, CHAR_LENGTH(wr.text), CHAR_LENGTH(wr.Name), 
	CHAR_LENGTH(wr.text)-CHAR_LENGTH(wr.Name) AS text_len, wr.text, wn1.text
	FROM values_ngrams wn1 
	LEFT JOIN values_ngrams wn2
		ON (wn1.Parent = wn2.id)
	LEFT JOIN values_clean wr
		ON (wn1.Parent = wr.id)
	WHERE wn2.id IS NULL AND wn1.Parent <> "null" AND wn1.Parent <> ""
	ORDER BY text_len;

SELECT COUNT(DISTINCT Parent), SUM(Parent <> "null" AND Parent <> ""), COUNT(*) FROM values_clean;
SELECT COUNT(DISTINCT parent_comment_id), SUM(`parent_comment_id` <> "null" AND parent_comment_id <> ""), COUNT(*) FROM world_reloaded;


# THREADS
# let's get some thread summary statistics from the raw data
DROP TABLE IF EXISTS combined_ids;
CREATE TABLE combined_ids
	SELECT commentid AS id, parent_comment_id AS parent_id, title AS title FROM world_reloaded
	UNION
	SELECT id AS id, Parent AS parent_id, `Name` AS title FROM values_clean;

ALTER TABLE combined_ids ADD INDEX commentidx (id);
ALTER TABLE combined_ids ADD INDEX parentidx (parent_id);

UPDATE combined_ids SET parent_id = "null" WHERE parent_id = "";

SELECT id
	FROM combined_ids  
	GROUP BY id 
	HAVING COUNT(*) >1;


DROP TABLE IF EXISTS ancestry;
CREATE TABLE ancestry
	SELECT gen1.id AS id,
		title, 
		gen1.parent_id AS parent_id,
		gen1.parent_id AS ancestry,
		IF(parent_id="null", "complete", parent_id) AS oldest,
		1 AS generation
	FROM combined_ids gen1 ;
	
ALTER TABLE ancestry ADD INDEX oldestidx (oldest);

# run this until no rows are updated
UPDATE ancestry a, combined_ids cid 
	SET a.ancestry=IF(a.oldest="complete" OR cid.parent_id = "null", a.ancestry, CONCAT(a.ancestry,",",cid.parent_id)),
		a.oldest=IF(a.oldest="complete" OR cid.parent_id = "null", "complete", cid.parent_id),
		a.generation = a.generation+1
	WHERE a.oldest = cid.id
; 

# the ones that are not "complete" had a generation where an ancestor id didn't exist in the data.  
#    34 chains end this way
SELECT COUNT(*) FROM ancestry WHERE oldest<>"complete";

SELECT MAX(generation) FROM ancestry;


# we need to get updated titles for hte new ids to map properly
# first, dump the full set of old titles:
SELECT DISTINCT title FROM ancestry WHERE id LIKE "%WORLDJAM%" 
INTO OUTFILE "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model/place_docs_here/old_titles.tsv"
FIELDS TERMINATED BY '\t'
ENCLOSED BY ''
LINES TERMINATED BY '\n';

ALTER TABLE ancestry DROP COLUMN new_id;
ALTER TABLE ancestry ADD COLUMN new_id VARCHAR(60);
UPDATE ancestry 
	SET new_id =  IF(id LIKE "%WORLDJAM%", CONCAT(SUBSTRING(id,2,20), SUBSTRING(title, 1, 20)), id); 

SELECT new_id FROM ancestry WHERE id LIKE "%ALUESJAM%" LIMIT 10;





SELECT 'id','title','parent_id','ancestry','generation'
UNION ALL
SELECT new_id,title,parent_id,ancestry,generation FROM ancestry 
INTO OUTFILE "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model/place_docs_here/thread_ancestry.csv"
FIELDS TERMINATED BY '\t'
ENCLOSED BY ''
LINES TERMINATED BY '\n';


#####################################
# Titles
#####################################
SELECT title, COUNT(*) AS number 
	FROM (SELECT JobResp AS title 
			FROM values_clean 
			GROUP BY `AuthorEmail`
		UNION ALL
		SELECT jobresp AS title 
			FROM world_reloaded
			GROUP BY author_email
		) AS a 
	GROUP BY title 
	ORDER BY number DESC
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/data/metadata/combined_titles.tsv'
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
;

SELECT title, COUNT(*) AS number 
	FROM (SELECT JobResp AS title 
			FROM values_clean 
			GROUP BY `AuthorEmail`
		UNION ALL
		SELECT jobresp AS title 
			FROM world_reloaded
			GROUP BY author_email
		) AS a 
	GROUP BY title 
	ORDER BY number DESC
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/data/metadata/combined_titles.tsv'
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
;

SELECT title, MAX(business_unit), COUNT(*) AS number 
	FROM (SELECT JobResp AS title, "" AS business_unit 
			FROM values_clean 
			GROUP BY `AuthorEmail`
		UNION ALL
		SELECT jobresp AS title, business_unit 
			FROM world_reloaded
			GROUP BY author_email
		) AS a 
	GROUP BY title, business_unit 
	ORDER BY number DESC
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/data/metadata/combined_titles_bu.tsv'
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
;

