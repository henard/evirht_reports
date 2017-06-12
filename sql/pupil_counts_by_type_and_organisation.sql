SELECT
licenseOrganisation.organisationId AS Organisation_ID,
organisation.name AS Organisation,
account.accountId AccountID,
account.name AS Account,
licenseOrganisation.licenseId AS License_ID,
license.startDate,
license.endDate,
licenseOrganisation.allocatedCapacity AS N_Allocated,
licenseOrganisation.paidForCapacity,
organisation.usedCapacity,
active.activeChild_n AS N_Active,
profiledChild_n AS N_Profiled,
indProfiledChild_n AS N_indProfiled,
groupProfiledChild_n AS N_groupProfiled
FROM organisation
LEFT JOIN account ON organisation.accountId = account.accountId
LEFT JOIN licenseOrganisation ON organisation.organisationId=licenseOrganisation.organisationId
LEFT JOIN license ON licenseOrganisation.licenseId=license.licenseId
INNER JOIN (
SELECT
licenseOrganisation.organisationId,
max(license.licenseId) AS licenseId_latest
FROM thrive_online.licenseOrganisation
LEFT JOIN thrive_online.license ON licenseOrganisation.licenseId=license.licenseId
WHERE (DATE(NOW()) BETWEEN license.startDate AND license.endDate)
GROUP BY organisationId) AS licenseId_latest ON licenseOrganisation.organisationId=licenseId_latest.organisationId AND license.licenseId=licenseId_latest.licenseId_latest
LEFT JOIN (
SELECT
childOrganisation.organisationId,
count(DISTINCT(childOrganisation.childId)) AS activeChild_n
FROM childOrganisation
LEFT JOIN child ON childOrganisation.childId=child.childId
WHERE childOrganisation.childStatusId=1 AND childOrganisation.deleted=0 AND child.deleted=0
GROUP BY childOrganisation.organisationId) AS active ON licenseOrganisation.organisationId=active.organisationId
LEFT JOIN (
SELECT
profile.organisationId,
count(DISTINCT(profileChild.childId)) AS profiledChild_n
FROM profile
LEFT JOIN profileChild ON profile.profileId=profileChild.profileId
LEFT JOIN child ON profileChild.childId=child.childId
LEFT JOIN childOrganisation ON child.childId=childOrganisation.childId
WHERE childOrganisation.childStatusId=1 AND childOrganisation.deleted=0 AND child.deleted=0
GROUP BY profile.organisationId) AS profiled ON licenseOrganisation.organisationId=profiled.organisationId
LEFT JOIN (
SELECT
profile.organisationId,
count(DISTINCT(profileChild.childId)) AS IndProfiledChild_n
FROM individualProfile
LEFT JOIN profile ON individualProfile.profileId = profile.profileId
LEFT JOIN profileChild ON profile.profileId=profileChild.profileId
LEFT JOIN child ON profileChild.childId=child.childId
LEFT JOIN childOrganisation ON child.childId=childOrganisation.childId
WHERE childOrganisation.childStatusId=1 AND childOrganisation.deleted=0 AND child.deleted=0
GROUP BY profile.organisationId) AS indprofiled ON licenseOrganisation.organisationId=indprofiled.organisationId
LEFT JOIN (
SELECT
profile.organisationId,
count(DISTINCT(profileChild.childId)) AS groupProfiledChild_n
FROM groupProfile
LEFT JOIN profile ON groupProfile.profileId = profile.profileId
LEFT JOIN profileChild ON profile.profileId=profileChild.profileId
LEFT JOIN child ON profileChild.childId=child.childId
LEFT JOIN childOrganisation ON child.childId=childOrganisation.childId
WHERE childOrganisation.childStatusId=1 AND childOrganisation.deleted=0 AND child.deleted=0
GROUP BY profile.organisationId) AS groupprofiled ON licenseOrganisation.organisationId=groupprofiled.organisationId
;
