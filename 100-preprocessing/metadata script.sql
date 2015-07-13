select dom, count(*) as number 
	from (select substring_index(AuthorEmail,'@',-1) as dom 
		from value_jam 
		where AuthorEmail<>'') as a 
	group by dom 
	order by number desc
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/ibm-code/metadata/values_domains.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
;

select dom, count(*) as number 
	from (select substring_index(AuthorEmail,'@',-1) as dom 
		from world_jam 
		where AuthorEmail<>'') as a 
	group by dom 
	order by number desc
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/ibm-code/metadata/world_domains.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
;


