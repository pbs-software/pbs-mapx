-- Mean species weight calculated using `gfb_mean_weight.sql', which emulates PJS algorithm for GFBIO data
-- Last modified: 2016-06-26 (RH)
DECLARE @MEAN_WEIGHT TABLE (SPECIES_CODE VARCHAR(5), MNWT REAL)
INSERT INTO @MEAN_WEIGHT VALUES
  ('222', 2.190844),  -- ttype=c(1,4), gear=1, major=3:9 (queried 160726)
  ('228', 0.507153),  -- ttype=c(1,4), gear=6, major=1   (queried 160726)
  ('394', 1.529992),  -- ttype=c(1,4), gear=1, major=3:9 (queried 160307)
  -- ('396', 0.856856),  -- ttype=c(1,4), gear=1, major=3:9 (queried 160726)
  ('396', 0.851637),  -- ttype=c(1,4), gear=1, major=5:7 (queried 160726)
  ('401', 1.724491),  -- ttype=c(1,4), gear=1, major=3:9
  ('405', 1.916324),  -- ttype=c(1,4), gear=1
  ('410', 1.071379),  -- ttype=c(1,4,5), gear=1, major=3:9 (queried 180515)
  ('417', 1.392816),  -- ttype=c(1,4), gear=1, major=3:9 (queried 151123)
  ('418', 1.45),      -- Paul Starr conversion for 2014 YTR assessment
  ('437', 1.922279),  -- ttype=c(1,4,5), gear=1, major=3:9 (queried 180622, gear 6 = 1.004036)
  ('439', 0.4726085), -- ttype=c(1,4), gear=1, major=3:9 (queried 171128; 3C=0.4449038, 3D5AB=0.4357049, 5CD=0.4705325, 5E=0.630529, 3CD5ABC=0.4341745, 5DE=0.630529)
  ('440', 1.462206),  -- ttype=c(1,4), gear=1, major=3:9 (queried 160307)
  ('442', 3.575088),  -- ttype=c(1,4), gear=5, major=3:9 (queried 150409)
  ('450', 0.3947),    -- not enough info in GFBioSQL to perform PJS regression; use mean weight from SST 2015 assessment
  ('451', 0.3947),    -- not enough info in GFBioSQL to perform PJS regression; use mean weight from SST 2015 assessment
  ('453', 0.2000),    -- not enough info in GFBioSQL to perform PJS regression; use guesstimate mean weight
  ('602', 1.124977),  -- ttype=c(1,4), gear=1, major=3:9
  ('621', 0.534608)   -- ttype=c(1,4), gear=1

