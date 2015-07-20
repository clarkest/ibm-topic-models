drop table combined_titles;
create table world_titles 
	select AuthorEmail as uname, JobResp as job, business_unit as business_unit
	from world_jam
	group by AuthorEmail;
	
SELECT DISTINCT job1 from combined_titles where ucase(job1) like "%DEV%";

# some business units are listed more than one way
drop table if exists temp_bu;
create table temp_bu 
	select AuthorEmail, 
		if(char_length(business_unit)>3, business_unit, 
			if(char_length(business_unit_2)>3, business_unit_2, 
				if(business_unit = business_unit_2, business_unit, "")
			)
		) as bu,
		concat(business_unit,"|",business_unit_2) as old_bu 
	from world_jam
	group by AuthorEmail;
 

create table temp_bu_lookup
	select bu, count(*) as cnt 
	from temp_bu
	group by bu 
	having count(*) > 25
	order by cnt desc
	;

select business_unit, job from 
	temp_bu_lookup bu join world_titles wt
	on wt.business_unit = bu.bu
	where business_unit !="" and job!=""
	INTO OUTFILE ‘/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/300-post-model-analyses/role_classification/biz_units_jobs.tsv’ 
	FIELDS TERMINATED BY '\t' OPTIONALLY ENCLOSED BY '"' 
	LINES TERMINATED BY '\n';
	

select bu1.business_unit, bu1.business_unit_2, bu2.business_unit, bu2.business_unit_2, char_length(bu2.business_unit_2) from 	
(select business_unit, business_unit_2 from world_jam group by business_unit) bu1
join (select business_unit, business_unit_2 from world_jam group by business_unit_2, business_unit) bu2
on bu1.business_unit = bu2.business_unit_2
where bu1.business_unit != bu1.business_unit_2;