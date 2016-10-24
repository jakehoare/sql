
-- Assignment 2, COMP9311 Semester 2 2014.
-- Submission by Harold Jacob Hoare.
-- Student Id : z5021639.


-- Copy InProceeding and RelationPersonInProceeding as Paper and Author to avoid confusion.
CREATE OR REPLACE VIEW Paper(PaperId, Title, Pages, Url, ProceedingId) as (
	SELECT InProceedingId, Title, Pages, Url, ProceedingId
	FROM InProceeding
);
CREATE OR REPLACE VIEW Author(AuthorId, PaperId) as (
  	SELECT PersonId, InProceedingId
  	FROM RelationPersonInProceeding 
);
-- Create list of distinct Editors.
CREATE OR REPLACE VIEW Editors(EditorId, Name) as (
	SELECT DISTINCT c.EditorId, p.Name
	FROM Proceeding c, Person p
	WHERE c.EditorId = p.PersonId
);
-- Create table of tuples linking Authors, Papers and Proceedings.
CREATE OR REPLACE VIEW ProceedingPaperAuthor(EditorId, PublisherId, ProceedingId, PaperId, AuthorId) as (
	SELECT p.EditorId, p.PublisherId, p.ProceedingId, r.PaperId, a.AuthorId
	FROM Proceeding p, Paper r, Author a
	WHERE p.ProceedingId = r.ProceedingId
	AND r.PaperId = a.PaperId
	GROUP BY p.ProceedingId, r.PaperId, a.AuthorId
);


-- Q1. List all the authors, distinct PersonId not distinct Name.
CREATE OR REPLACE VIEW DistinctAuthors(AuthorId) as (
	SELECT DISTINCT AuthorId
	FROM Author
);
CREATE OR REPLACE VIEW Q1(Name) as (
	SELECT Name
	FROM Person p, DistinctAuthors da
	WHERE p.PersonId = da.AuthorId
);


-- Q2. Authors that have never been editors.
-- Take the set difference between lists of distinct Authors and Editors.
CREATE OR REPLACE VIEW Q2(Name) as (
	SELECT Name
	FROM Person
	WHERE PersonId IN
	(SELECT AuthorId FROM DistinctAuthors
	EXCEPT
	SELECT EditorId FROM Editors)
);


-- Q4. For all editors that have authored a paper in a proceeding that they have edited, 
-- list their names and the number of such papers in total.
CREATE OR REPLACE VIEW EditorSelfAuthor(EditorId, Total) as (
	SELECT EditorId, count(PaperId)
	FROM ProceedingPaperAuthor
	WHERE EditorId = AuthorId
	GROUP BY EditorId
);
CREATE OR REPLACE VIEW Q4(Name, Total) as (
	SELECT p.Name, esa.Total
	FROM EditorSelfAuthor esa, Person p
	WHERE esa.EditorId = p.PersonId
	ORDER BY Total DESC, p.Name ASC
);


-- Q3. Editors that have never authored a paper in a proceeding they edited.
-- Including editors that have never authored a paper
-- Take the set difference between all Editors and Q4.
CREATE OR REPLACE VIEW Q3(Name) as (
	SELECT Name 
	FROM Person
	WHERE PersonId IN
	(SELECT EditorId FROM Editors
	EXCEPT
	SELECT EditorId FROM EditorSelfAuthor)	
);


-- Q5. Find the title of all papers with each author having no experience as an editor prior to the year
-- of publication of that paper. Do not include papers with an unknown year of publication. 
-- Also, do not consider editor experience for those proceedings with unknown year. 
-- 1. List the first year that an editor edited (includes null Editor).
-- 2. Collect the year that a paper was published if known and its authors.
-- 3. List the papers where an author has been an editor prior to the year of publication.
-- 4. Take the set difference with the list of papers with a known year of publication.
CREATE OR REPLACE VIEW FirstEdit(EditorId, Year) as (
	SELECT EditorId, min(Year)
	FROM Proceeding
	GROUP BY EditorId
);
CREATE OR REPLACE VIEW YearPaper(YearPublish, AuthorId, PaperId) as (
	SELECT p.Year, ppa.AuthorId, ppa.PaperId
	FROM ProceedingPaperAuthor ppa, Proceeding p
	WHERE ppa.ProceedingId = p.ProceedingId
);
CREATE OR REPLACE VIEW AuthorPriorEditor(PaperId, YearPublish, YearFirstEdit) as (
	SELECT y.PaperId, y.YearPublish, f.Year
	FROM YearPaper y, FirstEdit f
	WHERE (f.Year < y.YearPublish)
	AND y.AuthorId = f.EditorId
);
CREATE OR REPLACE VIEW Q5(Title) as (
	SELECT Title 
	FROM Paper 
	WHERE PaperId in (
	SELECT PaperId from YearPaper
	EXCEPT
	SELECT PaperId from AuthorPriorEditor)
);


-- Q6. List the total number of papers published in each year, ordered by year in ascending order.
-- Do not include papers with an unknown year of publication. Also do not include years with no publication. 
CREATE OR REPLACE VIEW Q6(Year, Total) as (
	SELECT p.Year, count(r.PaperId)
	FROM Proceeding p, Paper r
	WHERE p.ProceedingId = r.ProceedingId
	GROUP BY Year
	ORDER BY Year ASC
);


-- Q7. Find the most common publisher(s) (the name). 
-- (i.e., the publisher that has published the maximum total number of papers in the database).
-- 1. List papers with their by publishers.
-- 2. Aggregate papers by publisher.
-- 3. Find the published with the maximum number of papers.
CREATE OR REPLACE VIEW PaperWithPublisher(PaperId, PublisherId) as (
	SELECT p.PaperId, pu.PublisherId
	FROM Paper p, Proceeding pr, Publisher pu
	WHERE p.ProceedingId = pr.ProceedingId
	AND pu.PublisherId = pr.PublisherId
);
CREATE OR REPLACE VIEW PaperByPublisher(Name, Count) as (
	SELECT p.Name, count(pwp.PaperId)
	FROM Publisher p, PaperWithPublisher pwp
	WHERE p.PublisherId = pwp.PublisherId
	GROUP BY p.Name
);
CREATE OR REPLACE VIEW Q7(Name) as (
	SELECT Name
	FROM PaperByPublisher
	WHERE Count = (SELECT max(count) FROM PaperByPublisher)
);


-- Q8. Find the author(s) that co-authors the most papers (output the name). 
-- If there is more than one author with the same maximum number of co-authorships, output all of them. 
-- 1. Find the number of authors on each paper.
-- 2. List all authors of co-authored papers.
-- 3. Count the co-authored papers by author.
-- 4. Take the maximum.
CREATE OR REPLACE VIEW AuthorsByPaper(PaperId, Count) as (
	SELECT PaperId, count(AuthorId)
	FROM Author
	GROUP BY PaperId
);
CREATE OR REPLACE VIEW CoAuthorPapers(PaperId, AuthorId) as (
	SELECT abp.PaperId, a.AuthorId
	FROM AuthorsByPaper abp, Author a
	WHERE abp.Count >= 2
	AND a.PaperId = abp.PaperId
);
CREATE OR REPLACE VIEW NumCoAuth(AuthorId, Count) as (
	SELECT AuthorId, count(PaperId)
	FROM CoAuthorPapers
	GROUP BY AuthorId	
);
CREATE OR REPLACE VIEW Q8(Name) as (
	SELECT p.Name
	FROM Person p, NumCoAuth n
	WHERE p.PersonId = n.AuthorId
	AND n.Count = (SELECT max(Count) FROM NumCoAuth)
);



-- Q9. Find all the authors that never co-author (i.e., always published as sole author). 
-- 1. Find authors of papers with only one author.
-- 2. For each such paper count the number of coauthors.
-- 3. List the maximum nuber of coauthors by author, and take those with only 1.
CREATE OR REPLACE VIEW PapersOneAuthor(PaperId, AuthorId) as (
	SELECT abp.PaperId, a.AuthorId
	FROM AuthorsByPaper abp, Author a
	WHERE abp.PaperId = a.PaperId
	AND abp.Count = 1
);
CREATE OR REPLACE VIEW SoleAuthorPapers(PaperId, AuthorId, NumAuth) as (
	SELECT DISTINCT a.PaperId, poa.AuthorId, abp.Count
	FROM PapersOneAuthor poa, Author a, AuthorsByPaper abp
	WHERE poa.AuthorId = a.AuthorId
	AND a.PaperId = abp.PaperId
);
CREATE OR REPLACE VIEW MaxCoauthors(AuthorId, MaxCo) as (
	SELECT AuthorId, max(NumAuth)
	FROM SoleAuthorPapers
	GROUP BY AuthorId
);
CREATE OR REPLACE VIEW Q9(Name) as (
	SELECT p.Name
	FROM Person p, MaxCoauthors mc
	WHERE p.PersonId = mc.AuthorId
	AND mc.MaxCo = 1
);


-- Q10. For each author, list their total number of co-authors. Authors that never co-author, total is 0.
-- 1. Link view of Authors to itself by paper, to list the coauthors by author on each paper.
-- 2. Count the distinct coauthors from 1, subtracting 1 for the original author.
CREATE OR REPLACE VIEW CoauthorByAuthor(CoId, AuthorId) as (
	SELECT DISTINCT aa.AuthorId, a.AuthorId 
	FROM Author a, Author aa
	WHERE a.PaperId = aa.PaperId
);
CREATE OR REPLACE VIEW CoauthorCountbyAuthor(AuthorId, Total) as (
	SELECT AuthorId, count(CoId) - 1
	FROM CoauthorByAuthor
	GROUP BY AuthorId
);
CREATE OR REPLACE VIEW Q10(Name, Total) as (
	SELECT p.Name, cca.Total
	FROM CoauthorCountbyAuthor cca, Person p
	WHERE cca.AuthorId = p.PersonId
	ORDER BY Total DESC, Name ASC 
);



-- Q11. Find all the author names that have never co-authored with any co-author of Richard.
-- (i.e. Richard is the author's first name), nor co-authored with Richard himself. 
-- 1. List of all authors with Richard as a first name (begins with 'richard' case insensitive).
-- 2. List of all papers where Richard is an author.
-- 3. List of all authors for all papers that have Richard as an author.
-- 4. Remove the set Richards, to leave just the coauthors.
-- 5. List of all the papers authored by coauthors of Richard.
-- 6. List all the authors of papers authored by a coauthor of Richard.
-- 7. TAke the set difference with the list of all authors.
CREATE OR REPLACE VIEW Richards(AuthorId) as (
	SELECT DISTINCT a.AuthorId
	FROM Person p, Author a
	WHERE p.PersonId = a.AuthorId
	AND p.Name ILIKE 'richard%'
);
CREATE OR REPLACE VIEW RichardPapers(PaperId) as (
	SELECT DISTINCT a.PaperId
	FROM Author a, Richards r
	WHERE a.AuthorId = r.AuthorId
);
CREATE OR REPLACE VIEW RichardCoIncRich(AuthorId) as (
	SELECT DISTINCT a.AuthorId
	FROM Author a, RichardPapers r
	WHERE a.PaperId = r.PaperId
);
CREATE OR REPLACE VIEW RichardCoauthors(AuthorId) as (
	SELECT AuthorId FROM RichardCoIncRich
	EXCEPT
	SELECT AuthorId FROM Richards
);
CREATE OR REPLACE VIEW RichardCoauthorPapers(PaperId) as (
	SELECT DISTINCT a.PaperId
	FROM Author a, RichardCoauthors r
	WHERE a.AuthorId = r.AuthorId
);
CREATE OR REPLACE VIEW RichardCoCo(AuthorId) as (
	SELECT DISTINCT a.AuthorId
	FROM Author a, RichardCoauthorPapers r
	WHERE a.PaperId = r.PaperId
);
CREATE OR REPLACE VIEW Q11(Name) as (
	SELECT Name
	FROM Person 
	WHERE PersonId IN (
	SELECT AuthorId FROM Author
	EXCEPT
	SELECT AuthorId FROM RichardCoco)
);


-- Q12. Output all the authors that have co-authored with or are indirectly linked to Richard.
-- (i.e. Richard is the author's first name). a is indirectly linked to b if there exists 
-- a C p1, p1 C p2,..., pn C b, where x C y means x is co-authored with y. 
-- 1. Create tuples of Richard's co-coauthors and their papers.
-- 2. Recursively add any paper authored by those authors or any author of those papers.
CREATE OR REPLACE VIEW Q12Start(AuthorId, PaperId) as (
	SELECT a.AuthorId, a.PaperId
	FROM Author a, RichardCoCo r
	WHERE a.AuthorId = r.AuthorId
);
CREATE OR REPLACE VIEW Indirects AS
	WITH RECURSIVE RichardIndirect AS
	(
		SELECT AuthorId, PaperId
   		FROM Q12Start -- starting point for recursion
   		
		UNION
		
		SELECT a.AuthorId, a.PaperId
		FROM Author a, RichardIndirect r
		WHERE (a.AuthorId = r.AuthorId OR a.PaperId = r.PaperId)
		
	)
SELECT DISTINCT AuthorId FROM RichardIndirect
;
CREATE OR REPLACE VIEW Q12(Name) as (
	SELECT p.Name
	FROM Person p, Indirects i
	WHERE p.PersonId = i.AuthorId
);


-- Q13. Output the authors name, their total number of publications, first and last year they published.
-- 1. Link each paper to its year of publish using outer join to retain nulls.
-- 2. Link the paper and year information to its author(s).
-- 3. Take the maximuim and minimum year by author, using coalesce to replace nulls.
CREATE OR REPLACE VIEW PaperYear(PaperId, ProceedingId, Year) as (
	SELECT p.PaperId, pr.ProceedingId, pr.Year
	FROM Proceeding pr RIGHT OUTER JOIN Paper p on (p.ProceedingId = pr.ProceedingId)
);
CREATE OR REPLACE VIEW AuthorPaperYear(AuthorId, PaperId, Year) as (
	SELECT a.AuthorId, py.PaperId, py.Year
	FROM Author a, PaperYear py
	WHERE a.PaperId = py.PaperId
);
CREATE OR REPLACE VIEW AuthorSummary(AuthorId, Total, FirstYear, LastYear) as (
	SELECT AuthorId, count(PaperId), min(Year), max(Year)
	FROM AuthorPaperYear
	GROUP BY AuthorId
);
CREATE OR REPLACE VIEW Q13(Author, Total, FirstYear, LastYear) as (
	SELECT p.Name, a.Total, COALESCE(a.FirstYear, 'unknown') FirstYear, COALESCE(a.LastYear, 'unknown') LastYear
	FROM Person p, AuthorSummary a
	WHERE p.PersonId = a.AuthorId
	ORDER BY Total DESC, Name AsC
);


-- Q14. Find the number of authors that are in the database research area.
-- All papers that are in the database research area either contain the word or substring "data" (case insensitive) 
-- in their title or in a proceeding that contains the word or substring "data".
-- 1. List papers from proceedings containing the word 'data' (anywhere in string case insensitive).
-- 2. List of papers containing the word 'data' in the title.
-- 3. List of authors of 1.
-- 4. List of authors of 2.
-- 5. Count set union of 3 and 4 (necessarily unique).
CREATE OR REPLACE VIEW DataProceedings(ProceedingId, Title, PaperId) as (
	SELECT pr.ProceedingId, pr.Title, p.PaperId
	FROM Proceeding pr, Paper p
	WHERE pr.Title ILIKE '%data%'
	AND pr.ProceedingId = p.ProceedingId
);
CREATE OR REPLACE VIEW DataPapers(PaperId, Title) as (
	SELECT p.PaperId, p.Title
	FROM Paper p
	WHERE p.Title ILIKE '%data%'
);
CREATE OR REPLACE VIEW DataProceedingAuthors(AuthorId) as (
	SELECT DISTINCT a.AuthorId
	FROM Author a, DataProceedings dp
	WHERE a.PaperId = dp.PaperId
);
CREATE OR REPLACE VIEW DataPaperAuthors(AuthorId) as (
	SELECT DISTINCT a.AuthorId
	FROM Author a, DataPapers dp
	WHERE a.PaperId = dp.PaperId
);
CREATE OR REPLACE VIEW DataAuthors(AuthorId) as (
	(SELECT AuthorId from DataProceedingAuthors)
	UNION
	(SELECT AuthorId from DataPaperAuthors)
);
CREATE OR REPLACE VIEW Q14(Total) as (
	SELECT count(AuthorId)
	FROM DataAuthors
);


-- Q15. For all proceedings: editor name, title, publisher name, year, total number of papers.
-- 1. Count all papers by proceeding.
-- 2. Find the publisher and editor names for each proceeding using outer joins to preserve null entries.
CREATE OR REPLACE VIEW PapersByProceeding(ProceedingId, Count) as (
	SELECT pr.ProceedingId, count(p.PaperId)
	FROM Proceeding pr LEFT OUTER JOIN Paper p ON (pr.ProceedingId = p.ProceedingId)
	GROUP BY pr.ProceedingId
);
CREATE OR REPLACE VIEW ProceedingPublisher(ProceedingId, PublisherId, PubName) as ( 
	SELECT pr.ProceedingId, pu.PublisherId, pu.Name
	FROM Proceeding pr LEFT OUTER JOIN Publisher pu ON (pr.PublisherId = pu.PublisherId)
);
CREATE OR REPLACE VIEW ProceedingEditor(EditorName, ProceedingId) as (
	SELECT p.Name, pr.ProceedingId
	FROM Proceeding pr LEFT OUTER JOIN Person p ON (pr.EditorId = p.PersonId)
);
CREATE OR REPLACE VIEW Q15(EditorName, Title, PublisherName, Year, Total) as (
	SELECT pe.EditorName, pr.Title, pp.PubName, pr.Year, pbp.Count
	FROM ProceedingEditor pe, ProceedingPublisher pp, Proceeding pr, PapersByProceeding pbp
	WHERE pe.ProceedingId = pr.ProceedingId
	AND pp.ProceedingId = pr.ProceedingId
	AND pbp.ProceedingId = pr.ProceedingId
	ORDER BY pbp.Count DESC, pr.Year ASC, pr.Title Asc
);


-- Q16. Output the author names that have always co-authored (i.e sole author) but never edited a proceeding.
-- 1. Take the set difference between all authors and authors who have coauthored.
-- 2. Take the set difference of 1 and the list of all editors.
CREATE OR REPLACE VIEW NeverCoAuthors(AuthorId) as (
	SELECT AuthorId from Author
	EXCEPT
	SELECT AuthorId from NumCoAuth
);
CREATE OR REPLACE VIEW Q16(Name) as (
	SELECT Name
	FROM Person
	WHERE PersonId IN 
	(SELECT AuthorId from NeverCoAuthors
	EXCEPT
	SELECT EditorId from Editors)
);


-- Q17. Output the author name, and the total number of proceedings in which the author has at least one paper published.
-- 1. Find the author/paper tuples where the proceeding is not null.
-- 2. Group the proceedings by author together.
CREATE OR REPLACE VIEW AuthorsInProceeding(AuthorId, ProceedingId) as (
	SELECT a.AuthorId, p.ProceedingId
	FROM Author a, Paper p
	WHERE a.PaperId = p.PaperId
	AND p.ProceedingId IS NOT NULL
	GROUP BY a.AuthorId, p.ProceedingId
);
CREATE OR REPLACE VIEW Q17(Name, Total) as (
	SELECT p.Name, count(aip.ProceedingId)
	FROM AuthorsInProceeding aip, Person p
	WHERE aip.AuthorId = p.PersonId
	GROUP BY p.PersonId
	ORDER BY count(aip.ProceedingId) DESC, p.Name ASC
);


-- Q18. Count the number of publications per author and output the minimum, average and maximum count per author.
-- 1. For each author count their papers that are in a proceeding.
-- 2. Calculate the maximum, minimum and average. Round the average to an integer.
CREATE OR REPLACE VIEW PapersPerAuthor(AuthorId, Count) as (
	SELECT a.AuthorId, count(a.PaperId)
	FROM Author a, Paper p
	WHERE a.PaperId = p.PaperId
	AND p.ProceedingId IS NOT NULL
	GROUP BY a.AuthorId
);
CREATE OR REPLACE VIEW Q18(MinPub, AvgPub, MaxPub) as (
	SELECT min(Count), (avg(Count)::integer), max(Count)
	FROM PapersPerAuthor
);


-- Q19. Count the number of publications (in his edited proceedings) per editor per year and output the min, avg and max
-- for each editor. Also output the number of years that he/she is being an editor. 
-- Do not count proceedings with an unknown year of publication, and do not count years with no publication. 
-- For example, if Mike Jordan has edited the proceedings with total 50 papers in 2000, 20 in 2002 and 20 in 2004. 
-- Then ('Mike Jordan', 3, 20, 50, 30) should be in the output. 
CREATE OR REPLACE VIEW PapersEditorYear(EditorId, Year, Count) as (
	SELECT pr.EditorId, pr.Year, count(pa.PaperId)
	FROM Proceeding pr, Paper pa
	WHERE pr.ProceedingId = pa.ProceedingId
	GROUP BY pr.EditorId, pr.Year
);
CREATE OR REPLACE VIEW Q19(Name, Years, MinPub, MaxPub, AvgPub) as (
	SELECT p.Name, count(pey.Count), min(pey.Count), max(pey.Count), (avg(pey.Count)::integer)
	FROM Person p, PapersEditorYear pey
	WHERE p.PersonId = pey.EditorId
	GROUP BY p.PersonId
	ORDER BY Name ASC
);


-- Q20. Create a trigger on RelationPersonInProceeding, to check and disallow any insert or update 
-- of a paper in the RelationPersonInProceeding table from an author that is also the editor of 
-- the proceeding in which the paper has published. 
CREATE OR REPLACE FUNCTION checkEditor() RETURNS TRIGGER AS $$
begin
   	PERFORM * 
	FROM InProceeding ip, Proceeding p
	WHERE p.EditorId = new.PersonId
	AND ip.InProceedingId = new.InProceedingId
	AND p.ProceedingId = ip.ProceedingId;

   	IF (FOUND) THEN
      		RAISE EXCEPTION 'Author cannot be editor of proceeding in which paper is published.';
   	END IF;

   	RETURN NEW;
end;
$$ LANGUAGE plpgsql;

CREATE TRIGGER authorNotEditor BEFORE INSERT OR UPDATE
ON RelationPersonInProceeding FOR EACH ROW EXECUTE PROCEDURE checkEditor();


-- Q21. Create a trigger on Proceeding to check and disallow any insert or update of a proceeding in the Proceeding table 
-- with an editor that has authored fewer than 3 papers up to (and including) the year of the proceeding being published. 
-- Do not count any papers that do not have a publication year. However, allow any insert or update of a proceeding if 
-- the year of proceeding is not recorded yet.
CREATE OR REPLACE FUNCTION checkPapers() RETURNS TRIGGER AS $$
begin
	IF (new.Year IS NULL) THEN
		RETURN NEW;
	END IF;

	IF NOT EXISTS (SELECT yp.AuthorId, Count(yp.PaperId)
		FROM YearPaper yp
		WHERE yp.AuthorId = new.EditorId
		AND yp.YearPublish <= new.Year
		GROUP BY yp.AuthorId
		HAVING COUNT(yp.PaperId) >= 3)
 		THEN
      			RAISE EXCEPTION 'Editor must author at least 3 papers up to and including year of proceeding being published.';	
 	END IF;

	RETURN NEW;
end;
$$ LANGUAGE plpgsql;

CREATE TRIGGER editorTooFewPapers BEFORE INSERT OR UPDATE
ON Proceeding FOR EACH ROW EXECUTE PROCEDURE checkPapers();


-- Q22. Create a trigger on InProceeding to check and disallow any insert or update of a proceeding in the InProceeding table 
-- with an editor of the proceeding that is also the author of at least two papers in the proceeding. 

CREATE OR REPLACE FUNCTION CheckInProc() RETURNS TRIGGER AS $$
begin
	IF EXISTS (SELECT a.AuthorId, Count(p.PaperId)
		FROM Paper p, Author a, Proceeding pr
		WHERE p.ProceedingId = new.ProceedingId
		AND p.PaperId = a.PaperId
		AND a.AuthorId = pr.EditorId
		AND pr.ProceedingId = p.ProceedingId
		GROUP BY a.AuthorId
		HAVING COUNT(p.PaperId) >= 2)
   		THEN
      			RAISE EXCEPTION 'Editor of proceeding is also the author of at least two papers in the proceeding.';	
	END IF;

	RETURN NEW;
end;
$$ LANGUAGE plpgsql;

CREATE TRIGGER editorPapersInProc BEFORE INSERT OR UPDATE
ON InProceeding FOR EACH ROW EXECUTE PROCEDURE checkInProc();
