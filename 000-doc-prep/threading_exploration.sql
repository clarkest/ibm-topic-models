# load the world reloaded data from tsv using tsv import
# load the post-prcessed world data to see which ended up being kept

select commentid, title, text, count(*)
	from world_reloaded  
	group by commentid, title, text 
	having count(*) >1;

# move the offending rows into a new table
create table temp_id_fix 
	select * from world_reloaded 
	where commentid in  
		("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>")
; 
# then, update the jacked commentids to parse out by comment and text
update temp_id_fix set commentid = concat(substr(commentid,2,20), substr(title,1,20)); 

# delete the off rows by hand
# rule 1: keep only one of each unique title-id-text (16 -> 8 rows)
# rule 2: it is obvious which text goes with each title, delete the off ones (8 -> 4 rows)

# remove the original 16 rows from the main table
delete from world_reloaded where commentid in  
		("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>");
		
# move the four updated rows into the world_updated table
insert into world_reloaded select * from temp_id_fix;

# update the parent ids that reference these comments
update world_reloaded 
	set parent_comment_id = concat(substr(parent_comment_id,2,20), substr(title,1,20))
	where parent_comment_id in ("<ffd67d2eec.4af25522.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffe043f7ee.3100b90a.WORLDJAM@d25was504.mkm.can.ibm.com>");		
	


select count(distinct title), count(*) from world_reloaded;

# which parent ids in the processed data do not have post-processed parents?
alter table world_ngrams add index commentidx (commentid);
alter table world_ngrams add index parentidx (parent_comment_id);
select wn1.parent_comment_id, char_length(wr.text), char_length(wr.title), 
	char_length(wr.text)-char_length(wr.title) as text_len, wr.text, wn1.text
	from world_ngrams wn1 
	left join world_ngrams wn2
		on (wn1.parent_comment_id = wn2.commentid)
	left join world_reloaded wr
		on (wn1.parent_comment_id = wr.commentid)
	where wn2.commentid is null and wn1.parent_comment_id <> "null"
	order by text_len;

select * from world_reloaded where parent_comment_id = "<ffd612e85c.f002a818.WORLDJAM@d25was503.mkm.can.ibm.com>";

<ffd6bd1fa2.b12dfe2b.WORLDJAM@d25was503.mkm.can.ibm.com>;
select commentid, parent_comment_id, text, char_length(text) from world_reloaded where text like "%develop a better way to follow up on a project%";

select parent_comment_id, `text` from world_reloaded where commentid="<ffd6a88d39.1aacc6ff.WORLDJAM@d25was503.mkm.can.ibm.com>";




# VALUES JAM
#   load the post-process values jam data
# load the pre-process values data
alter table values_ngrams add index commentidx (id);
alter table values_ngrams add index parentidx (Parent);
alter table values_clean add index commentidx (id);
select wn1.Parent, char_length(wr.text), char_length(wr.Name), 
	char_length(wr.text)-char_length(wr.Name) as text_len, wr.text, wn1.text
	from values_ngrams wn1 
	left join values_ngrams wn2
		on (wn1.Parent = wn2.id)
	left join values_clean wr
		on (wn1.Parent = wr.id)
	where wn2.id is null and wn1.Parent <> "null" and wn1.Parent <> ""
	order by text_len;

select count(distinct Parent), sum(Parent <> "null" and Parent <> ""), count(*) from values_clean;
select count(distinct parent_comment_id), sum(`parent_comment_id` <> "null" and parent_comment_id <> ""), count(*) from world_reloaded;


# THREADS
# let's get some thread summary statistics from the raw data
drop table if exists combined_ids;
create table combined_ids
	select commentid as id, parent_comment_id as parent_id, title as title from world_reloaded
	union
	select id as id, Parent as parent_id, `Name` as title from values_clean;

alter table combined_ids add index commentidx (id);
alter table combined_ids add index parentidx (parent_id);

update combined_ids set parent_id = "null" where parent_id = "";

select id
	from combined_ids  
	group by id 
	having count(*) >1;


drop table if exists ancestry;
create table ancestry
	select gen1.id as id,
		title, 
		gen1.parent_id as parent_id,
		gen1.parent_id as ancestry,
		if(parent_id="null", "complete", parent_id) as oldest,
		1 as generation
	from combined_ids gen1 ;
	
alter table ancestry add index oldestidx (oldest);

# run this until no rows are updated
update ancestry a, combined_ids cid 
	set a.ancestry=if(a.oldest="complete" or cid.parent_id = "null", a.ancestry, concat(a.ancestry,",",cid.parent_id)),
		a.oldest=if(a.oldest="complete" or cid.parent_id = "null", "complete", cid.parent_id),
		a.generation = a.generation+1
	where a.oldest = cid.id
; 

# the ones that are not "complete" had a generation where an ancestor id didn't exist in the data.  
#    34 chains end this way
select count(*) from ancestry where oldest<>"complete";

select max(generation) from ancestry;


