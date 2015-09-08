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

select `Div`, count(*) as number 
	from world_jam 
	group by `Div` 
	order by number desc
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/ibm-code/metadata/world_Div.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
;
select `Div`, count(*) as number 
	from value_jam 
	group by `Div` 
	order by number desc
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/ibm-code/metadata/value_Div.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
;


select JobResp, count(*) as cnt from combined group by JobResp having cnt=1;


select 'Field' from (show columns from value_jam) as a;
SELECT column_name
FROM information_schema.columns
WHERE table_name = 'value_jam';

show columns from value_jam;


select parent, count(*) as cnt from combined group by parent order by cnt desc limit 1000;

create table user_names select `From` as full_name from value_jam;
insert into user_names select `From` as full_name from world_jam;


alter table user_names add column first_name varchar(30);
update user_names set first_name =  substring_index(full_name,' ',1); 

select distinct(first_name) from user_names;
alter table user_names drop column gender;
create index nam on gender (first_name);
create table gendered_names select full_name, un.first_name, gender 
	from user_names un left join gender on ucase(un.first_name)=gender.first_name;
	
	
select distinct(ucase(full_name)) from gendered_names where gender is Null;
select distinct(full_name) from gendered_names where length(first_name)<3 and gender is null;

select * from gendered_names where gender='unknown';


Update gendered_names set gender = "male" where full_name in ("M. Kent Burel", "C. Ed Brown", "L. Maurice Carter","J. Vincent Sandoval","J. Kevin Lowe","G. Jamieson Bryan","R. Martin Pascual","C. Gilbert Godshall","P. Douglas Hermann","F. Joseph AuBuchon","J. Randall MacDonald","T. Keith Wright","J. Scott Sims","J. L. Williams II","K. Mohammed Bashirudeen","R. David Jay","C. Ryan Coulter","C. Briant Wolfe","T. Richard Erickson","A. Steven Krantz","H. Peter Hofstee","K. Paul Muller","B. William Irlbeck","J. Richard Behun","C. Spencer Brown","R. Colby Mims","L. TRENTON MARSH","W. Lee Brink","E. Scott Von Kamecke","C. Kenneth Ewell","S. Jay Nalli","J. Bruce Harreld","J. Patrick Brennan","Dj Fagg","R. Keith Dahlgren","G. Franco Bertoletti","G. Paolo Avesani","C. Steven Lingafelt","H. Wesley Poteet","R. Breck Barker","J. Keaton Hearn");

Update gendered_names set gender = "female" where full_name in ("M. Elsbeth Sweeney","A. Silvia Bernotti","C. Diane Jetmund-Perigny","M. Linda Beaufort","M. Eileen Flynn","J. Candice D'Orsay","H. Nancy Breed","R. Shannon Kurtz","C. Diane Jetmund","M. Gabriella Russo","M. Gloria G. Lopez Menchon","M. Catia Mellica");

Update gendered_names set gender = "unknown" where full_name in ("O. J. O Connell","J. Graaff De","JR Mosca","SC C. Powers","NL Noble","V. J. Van Eeden","JP Farrell","A. P. Dorp Van","J. L_F Gloudemans","T. L. H. Rooy van","J. G. Voort Van De","F. Von Dielingen","HM Mehale","F. Van Der Merwe","A. F. R. Heijden Van Der","M. Wonderen Van","E. F. van Kersen","H. J. Dorenmalen Van","R. A_G Moon","JD Mcmaster","WP Lindemann","CR Greyling","G. J. M. Kempen Van","LC Sithole","PC Potgieter","CF Botha","SJ Bonney","AM Van Rensburg","CJ Bennett","LN Thebehali","RD Ward","CM Lehmkuhl","CB Butler","CM Worwood","I- Taylor","JM Ruthven","MS Brookes","DR Hoffman","AM Cilliers","JM Copeland","JG Pienaar","HM Hill","A. Van Den Heever","N. Fraser Ker","CW Kelway","SJ Frasco","GJ De Klerk","M. A. Put Van De","CL De Nobrega","MS Dawood","RG Wale","R. P. Wit De","AJ Patel","CA Schnehage","Ds Hutchings","DM Ntlhokwe","N. Smidt de","CD Dimmick","JP Frenza","R. P. Engelshoven Van","E. A. J. Burg Van Der","T. J. Vries De","F. P. A. Kooten Van","JM Mao-Cheia","A. Das Gupta","L. Out-Van Staveren","L. Van Der Merwe","GA Whitehead","J. De Kock","A. El Gabry","HJ Court","M. D. Meer Van Der","AA Lottering","TM Mahlaule","J. Blank De","MS Brahmadu","J. R. Werkhoven Van","SM Jeggo","VM Moshe","Di Podmore","OI Bello","MS Shashikumar","Bv Vaidyanath","CM Mahlakwane","R. Vos Van Der","H. Run Van","E. J. Overbeek Van","EA Mcclymont","D. Du Toit",
"JR Foulds","A. B. Hagens - Bakker","TR Muralidhara","AM Ramaphakela","L. Le Roux","P. A. Spbm Clarke","H. Dam Van","DC Hulley","V. Noble Le","E. C. T. Puckle Hobbs","AJ Grimbeck");


select distinct(ucase(first_name)) from ce_names where gender is Null ;
select distinct(ucase(first_name)) from gendered_names where gender = "NOLOOKUP";
select full_name from gendered_names where gender = "NOLOOKUP";


# do all of our world names have matches in the gendered names?
create index nme on world_reloaded(user_name);
create index nme on gendered_names(full_name);

select wr.user_name, count(*) as cnt 
	from world_reloaded wr
		left join gendered_names gn
		on wr.user_name=gn.full_name
	where gn.`full_name` is null
	group by wr.user_name
	order by cnt desc;
	
# and values?	
create index nme on value_jam(`from`);
select vj.`from`, count(*) as cnt 
	from value_jam vj
		left join gendered_names gn
		on vj.`from`=gn.full_name
	where gn.`full_name` is null
	group by vj.`from`;
	order by cnt desc;

# are there any names in the world/values data that have been found in the CE lookups so far?
alter table gendered_names add column uc_first_name varchar(50);
update gendered_names set uc_first_name = ucase(first_name);
create index ucfname on gendered_names(uc_first_name);
create index fname on `extra_gender_lookups`(first_name);
select gn.first_name, count(*) as cnt, min(egl.gender), max(egl.gender) 
	from gendered_names gn
		join `extra_gender_lookups` egl
		on gn.uc_first_name=egl.`first_name`
	where gn.gender in ("NOLOOKUP","unknown") and egl.gender not in  ("NOLOOKUP", "UNKNOWN")
	group by gn.first_name
	order by cnt desc;


create table gen_names_bkup select * from gendered_names;


update gendered_names set gender = "male" where uc_first_name="ALEX";
update gendered_names set gender = "female" where uc_first_name="KIM";


# dump the gendered names to disk in order to fold them into the documents set
#    make sure that we've got all the names
select count(*) from gendered_names;
select count(distinct full_name) 
	from (select `From` as full_name from world_jam 
	union all 
		select `From` as full_name from value_jam) a ;
		
# now, we need to make sure to index genders by email.  that's what's in the documents 
select email, full_name, gender  
	from gendered_names gn
	join (select AuthorEmail as email, `From` as full_name from world_jam 
			union all 
			select AuthorEmail as email, `From` as full_name from value_jam) a
	using (full_name)
	group by email 	
INTO OUTFILE '/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/place_docs_here/gendered_names.csv'
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
;

select * from values_clean where AuthorEmail like "%abscott%";
