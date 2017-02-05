SELECT
tol_id AS Organisation_ID,
address_region_id AS Locality
FROM tabs.thr_user_organisation
WHERE tol_id >0 AND tol_id IS NOT NULL
ORDER BY tol_id;
