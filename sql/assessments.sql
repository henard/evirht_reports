SELECT 
a.assessmentId AS Assessment_ID,
a.assessmentTypeId AS Assessment_Type_ID,
at.type AS Assessment_Type,
ad.developmentalStageId AS Developmental_Stage_ID,
ds.name AS Dev_Stage,
GROUP_CONCAT(n.needId) AS needs
FROM assessment AS a
LEFT JOIN assessmentType AS at ON a.assessmentTypeId = at.assessmentTypeId
LEFT JOIN assessmentDevelopmentalStage AS ad ON ad.assessmentId = a.assessmentId
LEFT JOIN developmentalStage AS ds ON ds.developmentalStageId = ad.developmentalStageId
LEFT JOIN need AS n ON n.developmentalStageId = ad.developmentalStageId
WHERE a.assessmentTypeId = '2'
GROUP BY a.assessmentId
;
