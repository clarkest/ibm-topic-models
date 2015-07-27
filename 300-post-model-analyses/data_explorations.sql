# number of users per jam, and how much overlap there is
ALTER TABLE values_clean ADD INDEX us (AuthorEmail);
ALTER TABLE world_reloaded ADD INDEX us (author_email);
SELECT SUM(IF(ISNULL(v_user_name),1,0)) AS world_only, 
	SUM(IF(ISNULL(w_user_name),1,0)) AS val_only, 
	SUM(IF(ISNULL(w_user_name) OR ISNULL(v_user_name), 0 ,1)) AS overlap
	FROM 
(	
SELECT AuthorEmail AS v_user_name, author_email AS w_user_name FROM values_clean v LEFT JOIN world_reloaded w ON (v.AuthorEmail=w.author_email) GROUP BY AuthorEmail
UNION
SELECT AuthorEmail AS v_user_name, author_email AS w_user_name FROM values_clean v RIGHT JOIN world_reloaded w ON (v.AuthorEmail=w.author_email) GROUP BY author_email	
) a
;
	
SELECT COUNT(DISTINCT author_email) FROM world_reloaded;
SELECT COUNT(DISTINCT AuthorEmail) FROM values_clean;

