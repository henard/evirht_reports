SELECT 
o.name AS Organisation,
o.organisationId AS Organisation_ID,
o.accountId AS AccountID,
ip.childId AS Child_ID,
cs.status AS Status,
c2.dob AS Child_DOB,
ip.schoolYear AS School_Year,
ip.age AS Age,
g.name AS Gender,
ip.individualProfileId AS Profile_ID,
c.childId AS Profiled_ID,
p.completedDate AS Completed_Date,
"Baseline Skills" AS Profile_Type,
p.developmentalStageId AS Dev_Stage,
CONCAT('[',GROUP_CONCAT(CONCAT('[',ps.needId,',',ps.score,',',ps.percentage,']')),']') AS profileScores,
p.overallPercentage AS Overall_Score
FROM individualProfile AS ip
JOIN profile AS p ON ip.profileId = p.profileId
JOIN child AS c ON ip.childId = c.childId
LEFT JOIN child AS c2 ON c2.childId = c.childId
INNER JOIN childOrganisation AS co ON co.childId = c.childId AND co.organisationId = p.organisationId
INNER JOIN organisation AS o ON o.organisationId = p.organisationId
LEFT JOIN gender AS g ON g.genderId = c.genderId
INNER JOIN childStatus AS cs ON cs.childStatusId = co.childStatusId
LEFT JOIN profileScore AS ps ON ps.profileId = p.profileId
WHERE p.completed = '1'
GROUP BY p.profileId
ORDER BY cs.childStatusId ASC , c.childId ASC , p.completedDate ASC
;
