SELECT
n.needId AS Need_ID,
n.name AS Need,
n.developmentalStageId AS Developmental_Stage_ID
FROM need AS n
INNER JOIN profileScore AS ps ON ps.needId = n.needId
INNER JOIN profile AS p ON p.profileId = ps.profileId
WHERE p.completed = '1'
GROUP BY n.needId
;
