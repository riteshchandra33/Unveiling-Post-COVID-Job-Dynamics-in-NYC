SELECT * FROM Job.job_nyc_postings;


SELECT `Posting Type`, COUNT(*) AS TypeCount
FROM job_nyc_postings
GROUP BY `Posting Type`;

SELECT `Work Location`, COUNT(*) AS LocationCount
FROM job_nyc_postings
GROUP BY `Work Location`
ORDER BY LocationCount DESC;

SELECT Agency, COUNT(*) AS PostingsCount
FROM job_nyc_postings
GROUP BY Agency
ORDER BY PostingsCount DESC;