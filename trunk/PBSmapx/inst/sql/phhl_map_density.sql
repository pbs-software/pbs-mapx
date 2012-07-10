-- PacHarvHL query for fisherlog catch (KG) of a target species, POP, and ORF (other rockfish)
-- fisheryid: 2=Halibut, 3=Sablefish, 4=Schdeule II, 5=ZN (takes long time to execute), 
--            6=Sablefish+ZN, 7=Sablefish+Halibut, 8=Dogfish, 9=Lingcod

DECLARE @logtype VARCHAR(10)
SET @logtype   = @logtypeval

SET NOCOUNT ON -- prevents timeout errors

-- Event info on FID 
SELECT 
  FE.OBFL_HAIL_IN_NO,
  FE.OBFL_LOG_TYPE_CDE,
  FE.OBFL_SET_NO,
  IsNull(FE.OBFL_FISHERY_ID,0) AS fid,
  COALESCE(-FE.OBFL_START_LONGITUDE,-FE.OBFL_END_LONGITUDE,0) AS X,
  COALESCE(FE.OBFL_START_LATITUDE,FE.OBFL_END_LATITUDE,0) AS Y,
  COALESCE(-FE.OBFL_END_LONGITUDE,-FE.OBFL_START_LONGITUDE,0) AS X2,
  COALESCE(FE.OBFL_END_LATITUDE,FE.OBFL_START_LATITUDE,0) AS Y2,
  IsNull(FE.Fishing_Depth,0) AS fdep,
  CAST(CONVERT(char(10),COALESCE(FE.Start_FE,FE.OBFL_START_DT,FE.End_FE,FE.OBFL_END_DT,T.OBFL_DEPARTURE_DT,T.OBFL_OFFLOAD_DT),20) AS smalldatetime) AS [date],
  IsNull(T.OBFL_VSL_CFV_NO,0) AS cfv,
  IsNull(FE.Duration,0) AS eff
INTO #Events
FROM
  B2_Trips T INNER JOIN
  B3_Fishing_Events FE ON
    T.OBFL_HAIL_IN_NO = FE.OBFL_HAIL_IN_NO AND 
    T.OBFL_LOG_TYPE_CDE = FE.OBFL_LOG_TYPE_CDE
WHERE
  FE.OBFL_LOG_TYPE_CDE IN (@logtype)

-- Compile the catch stats for the target species
SELECT 
  FC.OBFL_HAIL_IN_NO,
  FC.OBFL_LOG_TYPE_CDE,
  FC.OBFL_SET_NO,
  'landed' = Sum( CASE 
    WHEN FC.OBFL_SPECIES_CDE IN (@sppcode) AND FC.OBFL_CATCH_UTILIZATION_CDE NOT IN (4,6,9,22,23,24,27,28)
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END ),
  'discard' = Sum( CASE 
    WHEN FC.OBFL_SPECIES_CDE IN (@sppcode) AND FC.OBFL_CATCH_UTILIZATION_CDE IN (4,6,9,22,23,24,27,28) -- discard codes
    THEN IsNull(FC.OBFL_EST_WEIGHT,0)
    ELSE 0 END )
INTO #Catch
FROM 
  B4_Catches FC  -- B4 Catch is in kg (Lisa Lacko)
WHERE
  FC.OBFL_LOG_TYPE_CDE IN (@logtype)
GROUP BY
  FC.OBFL_HAIL_IN_NO,
  FC.OBFL_LOG_TYPE_CDE,
  FC.OBFL_SET_NO

-- Collect all events and associate some catch, even if non reported
SELECT 
  E.fid, E.X, E.Y, E.X2, E.Y2, E.fdep, E.date, E.cfv, E.eff,
  IsNull(C.landed + C.discard,0) AS @sppcode
FROM 
  #Events E LEFT OUTER JOIN
  #Catch C ON 
    E.OBFL_HAIL_IN_NO = C.OBFL_HAIL_IN_NO AND 
    E.OBFL_LOG_TYPE_CDE = C.OBFL_LOG_TYPE_CDE AND 
    E.OBFL_SET_NO = C.OBFL_SET_NO
WHERE 
  IsNull(E.eff,0) > 0 AND
  IsNull(E.X,0) + IsNull(E.X2,0) < 0 AND
  IsNull(E.Y,0) + IsNull(E.Y2,0) > 0

-- SELECT * FROM #Catch
-- getData("phhl_map_density.sql","PacHarvHL",strSpp="394",logtype="FISHERLOG")


