SELECT
organisation.organisationId AS Organisation_ID,
organisation.name AS Organisation,
organisation.accountId AS AccountID,
account.name AS Account,
licenseOrganisation.licenseId AS License_ID,
1.1*max(licenseOrganisation.allocatedCapacity) AS Allocated_Capacity,
max(licenseOrganisation.paidForCapacity) AS N_Allocated,
active.activeChild_n AS N_Active,
profiledChild_n AS N_Profiled
FROM thrive_online.organisation
LEFT JOIN thrive_online.account ON organisation.accountId = account.accountId
LEFT JOIN thrive_online.licenseOrganisation ON organisation.organisationId=licenseOrganisation.organisationId
LEFT JOIN (
SELECT childOrganisation.organisationId, count(DISTINCT(childOrganisation.childId)) AS activeChild_n
FROM childOrganisation
GROUP BY childOrganisation.organisationId
) AS active ON organisation.organisationId=active.organisationId
LEFT JOIN (
SELECT profile.organisationId, count(DISTINCT(profileChild.childId)) AS profiledChild_n
FROM profile
LEFT JOIN profileChild ON profile.profileId=profileChild.profileId
GROUP BY profile.organisationId
) AS profiled ON organisation.organisationId=profiled.organisationId
GROUP BY organisation.organisationId;
