-- Mean species weight calculated using `gfb_mean_weight.sql', which emulates PJS algorithm for GFBIO data
-- Now has defaults for all species in GFBio (from GFB_SPECIES_MEAN_WEIGHT).
-- Added a field 'FISH' to identify fish species and to speed up catch summaries.
-- Last modified: 2019-08-07 (RH)
DECLARE @MEAN_WEIGHT TABLE (SPECIES_CODE VARCHAR(5), MNWT REAL, FISH INTEGER)
INSERT INTO @MEAN_WEIGHT VALUES
  ('222', 2.190844, 1),  -- PAC: ttype=c(1,4), gear=1, major=3:9 (queried 160726)
  ('228', 0.507153, 1),  -- WAP: ttype=c(1,4), gear=6, major=1   (queried 160726)
  ('394', 1.529992, 1),  -- RER: ttype=c(1,4), gear=1, major=3:9 (queried 160307)
--('396', 0.856856, 1),  -- POP: ttype=c(1,4), gear=1, major=3:9 (queried 160726)
  ('396', 0.851637, 1),  -- POP: ttype=c(1,4), gear=1, major=5:7 (queried 160726)
  ('401', 1.724491, 1),  -- RBR: ttype=c(1,4), gear=1, major=3:9
  ('405', 1.916324, 1),  -- SGR: ttype=c(1,4), gear=1
  ('410', 1.071379, 1),  -- DBR: ttype=c(1,4,5), gear=1, major=3:9 (queried 180515)
--('417', 1.392816, 1),  -- WWR: ttype=c(1,4), gear=1, major=3:9 (queried 151123)
--('417', 0.772676, 1),  -- WWR: ttype=c(1,4,5), gear=1, major=3:9 (queried 180803) [gear 6 = 1.262358]
  ('417', 0.933798, 1),  -- WWR: ttype=c(1,4,5), gear=1, major=c(5,9) (queried 180803) [gear 6 = 1.3536784] [major=3, gear=6, mw=1.253677] [major=4, gear=6, mw=1.350136] -- no gear 1 in 3:4
  ('418', 1.45, 1),      -- YTR: Paul Starr conversion for 2014 YTR assessment
  ('425', 1.529992, 1),  -- BSR: Use same as RER for now
  ('435', 3.901019, 1),  -- BOR: ttype=c(1,4,5), gear=6, major=3:9 (queried 180912, 190705)
  ('437', 1.922279, 1),  -- CAR: ttype=c(1,4,5), gear=1, major=3:9 (queried 180622, gear 6 = 1.004036)
--('439', 0.4726085, 1), -- RSR: ttype=c(1,4), gear=1, major=3:9 (queried 171128; 3C=0.4449038, 3D5AB=0.4357049, 5CD=0.4705325, 5E=0.630529, 3CD5ABC=0.4341745, 5DE=0.630529)
  ('439', 0.4766806, 1), -- RSR: ttype=c(1,4), gear=1, major=3:9 (queried 180925)
--('440', 1.462206, 1),  -- YMR: ttype=c(1,4), gear=1, major=3:9 (queried 160307)
  ('440', 1.2531026, 1), -- YMR: ttype=c(1,4), gear=1, major=3:9 (queried 180925) [gear 6 = 0.9904563]
  ('442', 3.575088, 1),  -- YYR: ttype=c(1,4), gear=5, major=3:9 (queried 150409)
  ('450', 0.3947, 1),    -- SCR: not enough info in GFBioSQL to perform PJS regression; use mean weight from SST 2015 assessment
  ('451', 0.3947, 1),    -- LST: not enough info in GFBioSQL to perform PJS regression; use mean weight from SST 2015 assessment
  ('453', 0.2000, 1),    -- SST: not enough info in GFBioSQL to perform PJS regression; use guesstimate mean weight
  ('602', 1.124977, 1),  -- ARF: ttype=c(1,4), gear=1, major=3:9
  ('621', 0.534608, 1),  -- ROL: ttype=c(1,4), gear=1
-- Following are mean weight defaults from GFB_SPECIES_MEAN_WEIGHT
-- If value already given above, comment out value below.
  ('001', 0.227, 0),   -- Fish eggs
  ('002', 0.01, 0),    -- Unidentified larvae
  ('004', 54.745, 0),  -- Inanimate object(s)
  ('013', 27.017, 1),  -- Unidentified shark
  ('015', 0.422, 1),   -- Unknown fish
  ('016', 0.192, 1),   -- Hagfishes
  ('017', 0.242, 1),   -- Black hagfish
  ('018', 0.309, 1),   -- Pacific hagfish
  ('019', 0.181, 1),   -- Lampreys
  ('01B', 0.04, 0),    -- Aplacophora
  ('020', 0.056, 1),   -- Pacific lamprey
  ('022', 0.033, 1),   -- River lamprey
  ('027', 36.74, 1),   -- Sixgill shark
  ('036', 14.11, 1),   -- Salmon shark
  ('037', 0.68, 1),    -- Cat sharks
  ('038', 0.383, 1),   -- Brown cat shark
  ('039', 25.098, 1),  -- Requiem sharks
  ('040', 22.45, 1),   -- Soupfin shark
  ('041', 7.12, 1),    -- Blue shark
  ('042', 1.635, 1),   -- Dogfish sharks
  ('043', 39.743, 1),  -- Pacific sleeper shark
  ('044', 1.036, 1),   -- Spiny dogfish
  ('050', 3.915, 1),   -- Pacific electric ray
  ('051', 0.195, 1),   -- Skates
  ('052', 6.117, 1),   -- Aleutian skate
  ('054', 3.502, 1),   -- Abyssal skate
  ('056', 6.573, 1),   -- Big skate
  ('057', 2.254, 1),   -- Roughtail skate
  ('058', 1.385, 1),   -- Sandpaper skate
  ('059', 5.404, 1),   -- Longnose skate
  ('061', 2.478, 1),   -- Alaska skate
  ('065', 0.75, 1),    -- Ratfishes
  ('066', 0.384, 1),   -- Spotted ratfish
  ('076', 1.41, 1),    -- Whitebrow skate
  ('081', 20., 1),     -- Sturgeons
  ('082', 41.075, 1),  -- Green sturgeon
  ('084', 0.045, 1),   -- Conger eels
  ('087', 0.203, 1),   -- Snipe eels
  ('090', 0.1, 1),     -- Slender snipe eel
  ('094', 0.656, 1),   -- Herrings
  ('095', 0.576, 1),   -- American shad
  ('096', 0.073, 1),   -- Pacific herring
  ('0A0', 0.455, 0),   -- Invertebrates
  ('0AA', 0.01, 0),    -- Segmented worms
  ('0AB', 0.22, 0),    -- Polychaete worms
  ('0AD', 0.01, 0),    -- Aphroditidae
  ('0AE', 0.11, 0),    -- Sea mouse
  ('0FA', 0.299, 0),   -- Tube worms
  ('100', 0.155, 1),   -- Pacific sardine
  ('103', 0.05, 1),    -- Anchovies
  ('104', 0.024, 1),   -- Northern anchovy
  ('106', 3.147, 1),   -- Salmonids
  ('107', 0.387, 1),   -- Pacific salmon and native trout
  ('108', 0.821, 1),   -- Pink salmon
  ('10A', 0.11, 0),    -- Gastropods
  ('112', 3.595, 1),   -- Chum salmon
  ('115', 1.478, 1),   -- Coho salmon
  ('118', 2.007, 1),   -- Sockeye salmon
  ('124', 2.304, 1),   -- Chinook salmon
  ('128', 4.006, 1),   -- Rainbow trout (aka steelhead)
  ('131', 1.996, 1),   -- Atlantic salmon
  ('136', 0.047, 1),   -- Smelts
  ('138', 0.019, 1),   -- Whitebait smelt
  ('139', 10., 1),     -- Surf smelt
  ('140', 0.064, 1),   -- Lampanyctus
  ('142', 0.008, 1),   -- Stenobrachius
  ('146', 0.042, 1),   -- Longfin smelt
  ('147', 0.01, 1),    -- Diaphanous hatchetfish
  ('148', 0.25, 1),    -- Eulachon
  ('14H', 0.1, 0),     -- Diodora
  ('150', 0.057, 1),   -- Argentines
  ('151', 0.035, 1),   -- Bluethroat argentine
  ('152', 0.052, 1),   -- Deepsea smelts
  ('153', 0.057, 1),   -- Stout blacksmelt
  ('154', 0.008, 1),   -- Popeye blacksmelt
  ('155', 0.05, 1),    -- Pacific blacksmelt
  ('156', 0.008, 1),   -- Northern smoothtongue
  ('160', 0.001, 1),   -- Lightfishes
  ('162', 0.01, 1),    -- Marine hatcahetfishes
  ('163', 0.007, 1),   -- Lowcrest hatchetfish
  ('164', 0.022, 1),   -- Scaleless black dragonfishes
  ('165', 0.081, 1),   -- Highfin dragonfish
  ('167', 0.048, 1),   -- Longfin dragonfish
  ('169', 0.043, 1),   -- Shining loosejaw
  ('16C', 0.008, 0),   -- Topshells
  ('170', 0.055, 1),   -- Viperfishes
  ('171', 0.015, 1),   -- Pacific viperfish
  ('172', 0.055, 1),   -- Tubeshoulders
  ('173', 0.031, 1),   -- Shining tubeshoulder
  ('174', 3.1, 1),     -- Lancetfishes
  ('175', 2., 1),      -- Longnose lancetfish
  ('179', 0.023, 1),   -- Northern pearleye
  ('180', 0.337, 1),   -- Barracudinas
  ('181', 0.024, 1),   -- Slender barracudina
  ('182', 0.036, 1),   -- White barracudina
  ('185', 0.01, 1),    -- Lanternfishes
  ('189', 0.01, 1),    -- California headlightfish
  ('191', 0.065, 1),   -- Pinpoint lampfish
  ('196', 0.001, 1),   -- Bigeye flashlightfish
  ('197', 0.013, 1),   -- Lanternfish
  ('198', 0.044, 1),   -- Northern lampfish
  ('1FA', 0.23, 0),    -- Peanutworms
  ('200', 0.007, 1),   -- Bigfin lanternfish
  ('202', 0.004, 1),   -- Blue lanternfish
  ('207', 0.097, 1),   -- Plainfin midshipman
  ('213', 0.2, 1),     -- Dreamers
  ('216', 0.1, 1),     -- Spiny dreamer
  ('220', 0.64, 1),    -- Pacific flatnose
  --('222', 1.365, 1), -- Pacific cod
  ('223', 0.59, 1),    -- Slender codling
  ('225', 0.502, 1),   -- Pacific hake
  ('226', 0.071, 1),   -- Pacific tomcod
  --('228', 0.612, 1), -- Walleye pollock
  ('231', 0.269, 1),   -- Eelpouts
  ('233', 0.25, 1),    -- Bigfin eelpout
  ('234', 0.01, 1),    -- Eelpout
  ('235', 0.643, 1),   -- Twoline eelpout
  ('236', 5.1, 1),     -- Soft eelpout
  ('237', 1., 1),      -- Longsnout eelpout
  ('239', 0.007, 1),   -- Blackmouth eelpout
  ('241', 0.025, 1),   -- Pallid eelpout
  ('242', 0.098, 1),   -- Shortfin eelpout
  ('243', 0.097, 1),   -- Black eelpout
  ('244', 0.151, 1),   -- Wattled eelpout
  ('245', 0.054, 1),   -- Blackbelly eelpout
  ('249', 1.436, 1),   -- Grenadiers
  ('250', 0.283, 1),   -- Popeye
  ('251', 0.412, 1),   -- Pacific grenadier
  ('253', 0.188, 1),   -- Ghostly grenadier
  ('254', 0.788, 1),   -- Threadfin grenadier
  ('255', 0.455, 1),   -- Bearded rattail
  ('256', 2.164, 1),   -- Giant grenadier
  ('263', 0.004, 1),   -- Highsnout bigscale
  ('264', 0.036, 1),   -- Crested bigscale
  ('271', 5.071, 1),   -- King-of-the-salmon
  ('273', 0.007, 1),   -- Tube snouts
  ('276', 0.005, 1),   -- Threespine stickleback
  ('27F', 0.115, 0),   -- Lewismoon snail
  ('287', 1.189, 1),   -- Jack mackerel
  ('288', 1.52, 1),    -- Yellowtail
  ('289', 0.04, 1),    -- Pomfrets
  ('28G', 0.06, 0),    -- Neptunea amianta
  ('28H', 1.93, 0),    -- Cymatiidae
  ('28I', 0.078, 0),   -- Oregontriton
  ('290', 0.824, 1),   -- Pacific pomfret
  ('294', 0.152, 1),   -- White croaker
  ('298', 0.55, 1),    -- Surfperches
  ('2A0', 0.789, 0),   -- Sponges
  ('2I0', 0.678, 0),   -- Glass sponges
  ('2Q0', 0.327, 0),   -- Bath sponges
  ('2QA', 0.008, 0),   -- Lampshells
  ('304', 0.034, 1),   -- Shiner perch
  ('306', 0.183, 1),   -- Striped seaperch
  ('311', 0.053, 1),   -- Pink seaperch
  ('312', 0.374, 1),   -- Pile perch
  ('315', 0.15, 1),    -- Sandfishes
  ('316', 0.189, 1),   -- Pacific sandfish
  ('318', 1., 1),      -- Blue-eyed searcher
  ('319', 0.03, 1),    -- Northern ronquil
  ('31E', 0.043, 0),   -- Rock snails
  ('324', 0.018, 1),   -- Pricklebacks
  ('331', 0.097, 1),   -- Pearly prickleback
  ('332', 0.083, 1),   -- Decorated warbonnet
  ('336', 0.013, 1),   -- Daubed shanny
  ('337', 0.004, 1),   -- Snake prickleback
  ('33G', 0.015, 0),   -- Whelks
  ('340', 0.029, 1),   -- Whitebarred prickleback
  ('34F', 0.103, 0),   -- Neptuneidae
  ('351', 4.178, 1),   -- Wolf eel
  ('355', 5.123, 1),   -- Giant wrymouth
  ('356', 0.1, 1),     -- Dwarf wrymouth
  ('359', 0.712, 1),   -- Prowfish
  ('360', 0.023, 1),   -- Sand lances
  ('361', 0.023, 1),   -- Pacific sand lance
  ('374', 0.519, 1),   -- Chub mackerel
  ('376', 6.48, 1),    -- Albacore
  ('379', 3.333, 1),   -- Bluefin tuna
  ('37I', 0.02, 0),    -- Dogwhelks
  ('381', 0.02, 1),    -- Medusafish
  ('383', 1.13, 1),    -- Pacific pompano
  ('385', 18.833, 1),  -- Ragfishes
  ('386', 14.756, 1),  -- Ragfish
  ('388', 0.756, 1),   -- Scorpionfishes
  ('389', 0.612, 1),   -- Rockfishes
  --('394', 1.611, 1), -- Rougheye rockfish
  --('396', 0.865, 1), -- Pacific ocean perch
  ('398', 0.398, 1),   -- Brown rockfish
  ('39A', 0.713, 0),   -- Mitridae
  ('39G', 0.001, 0),   -- Oliveshells
  ('3A1', 0.725, 0),   -- Coeclenterates
  ('3A2', 0.517, 0),   -- Hydroid
  ('3AA', 0.224, 0),   -- Bryozoa
  ('3G0', 0.901, 0),   -- Jellyfish
  ('3J0', 0.49, 0),    -- Anthozoa
  ('3J1', 0.05, 0),    -- Zoantharia
  ('3J2', 0.694, 0),   -- Stony corals
  ('3L0', 0.389, 0),   -- Anemone
  ('3M8', 0.933, 0),   -- Metridium
  ('3N2', 0.05, 0),    -- Stomphia
  ('3N6', 0.873, 0),   -- Tealia
  ('3R0', 0.91, 0),    -- Soft corals
  ('3S0', 1.106, 0),   -- Gorgonian corals
  ('3S6', 0.888, 0),   -- Bubble gum coral
  ('3S7', 0.343, 0),   -- Paragorgia pacifica
  ('3T0', 1.032, 0),   -- Primnoa
  ('3U0', 0.146, 0),   -- Sea pens
  ('3U2', 0.09, 0),    -- Sea whip
  ('3U5', 0.178, 0),   -- Sea pen
  ('3V0', 0.112, 0),   -- Virgularia
  ('3X0', 0.015, 0),   -- Ctenophora
  ('400', 0.495, 1),   -- Aurora rockfish
  --('401', 1.363, 1), -- Redbanded rockfish
  ('402', 1.322, 1),   -- Rockfish
  ('403', 4.777, 1),   -- Shortraker rockfish
  --('405', 1.93, 1),  -- Silvergray rockfish
  ('407', 0.679, 1),   -- Copper rockfish
  ('408', 0.54, 1),    -- Greenspotted rockfish
  ('409', 1.639, 1),   -- Dusky rockfish
  --('410', 0.794, 1), -- Darkblotched rockfish
  ('412', 0.366, 1),   -- Splitnose rockfish
  ('414', 0.342, 1),   -- Greenstriped rockfish
  ('415', 0.025, 1),   -- Puget sound rockfish
  --('417', 1.456, 1), -- Widow rockfish
  --('418', 1.345, 1), -- Yellowtail rockfish
  ('41S', 0.23, 0),    -- Triopha
  ('420', 0.448, 1),   -- Chilipepper
  ('421', 0.271, 1),   -- Rosethorn rockfish
  ('422', 0.14, 1),    -- Cowcod
  ('423', 0.084, 1),   -- Shortbelly rockfish
  ('424', 0.798, 1),   -- Quillback rockfish
  --('425', 1.38, 1),  -- Blackspotted rockfish
  ('426', 1.861, 1),   -- Black rockfish
  ('427', 0.898, 1),   -- Blackgill rockfish
  ('428', 1.713, 1),   -- Vermilion rockfish
  ('429', 1.092, 1),   -- Blue rockfish
  ('431', 0.71, 1),    -- China rockfish
  ('433', 1.128, 1),   -- Tiger rockfish
  --('435', 3.812, 1), -- Bocaccio
  --('437', 1.881, 1), -- Canary rockfish
  --('439', 0.473, 1), -- Redstripe rockfish
  ('43I', 0.025, 0),   -- Tritonia
  --('440', 1.343, 1), -- Yellowmouth rockfish
  --('442', 2.864, 1), -- Yelloweye rockfish
  ('444', 0.182, 1),   -- Stripetail rockfish
  ('445', 0.075, 1),   -- Halfbanded rockfish
  ('446', 0.169, 1),   -- Harlequin rockfish
  ('448', 0.058, 1),   -- Pygmy rockfish
  ('44N', 0.7, 0),     -- Flabellina
  --('450', 0.338, 1), -- Sharpchin rockfish
  --('451', 0.42, 1),  -- Shortspine thornyhead
  ('452', 0.855, 1),   -- Thornyheads
  --('453', 0.123, 1), -- Longspine thornyhead
  ('455', 2.699, 1),   -- Sablefish
  ('458', 10.219, 1),  -- Skilfish
  ('459', 1.103, 1),   -- Greenlings
  ('461', 0.624, 1),   -- Kelp greenling
  ('462', 5.897, 1),   -- Painted greenling
  ('466', 0.062, 1),   -- Whitespotted greenling
  ('467', 2.859, 1),   -- Lingcod
  ('471', 0.048, 1),   -- Longspine combfish
  ('472', 0.809, 1),   -- Sculpins
  ('473', 0.05, 1),    -- Artedius-type larvae
  ('478', 0.025, 1),   -- Padded sculpin
  ('48E', 0.12, 0),    -- Seahares
  ('491', 0.027, 1),   -- Roughback sculpin
  ('497', 0.625, 1),   -- Spinyhead sculpin
  ('499', 0.297, 1),   -- Buffalo sculpin
  ('4A0', 0.01, 0),    -- Flatworms
  ('4AA', 0.516, 0),   -- Echinoderms
  ('4AB', 0.036, 0),   -- Sea lilies and feather stars
  ('4GA', 0.66, 0),    -- Starfish
  ('4GD', 0.244, 0),   -- Sand star
  ('4HC', 0.072, 0),   -- Mud star
  ('4HF', 0.058, 0),   -- Cheiraster dawsoni
  ('4HI', 0.166, 0),   -- Nearchaster variabilis
  ('4IA', 0.04, 0),    -- Astropectinidae
  ('4IB', 0.177, 0),   -- Dipsacaster borealis
  ('4IJ', 0.1, 0),     -- Leptychaster
  ('4IK', 0.088, 0),   -- Nearchaster
  ('4IT', 0.026, 0),   -- Mediaster tenellus
  ('4JA', 0.02, 0),    -- Goniasteridae
  ('4JB', 0.071, 0),   -- Pseudarchaster alascensis
  ('4JD', 0.074, 0),   -- Vermillion starfish
  ('4JE', 0.02, 0),    -- Ceramaster arcticus
  ('4JF', 0.229, 0),   -- Spiny red sea star
  ('4JG', 0.08, 0),    -- Cookie star
  ('4JI', 0.128, 0),   -- Hippasteria californica
  ('4JL', 0.06, 0),    -- Ceramaster japonicus
  ('4JM', 0.338, 0),   -- Hippasteria
  ('4NB', 0.065, 0),   -- Gephyreaster swifti
  ('4OB', 0.18, 0),    -- Poraniidae
  ('4OC', 0.433, 0),   -- Leather star
  ('4PD', 0.05, 0),    -- Bat star
  ('4QB', 0.018, 0),   -- Henricia aspera
  ('4QC', 0.145, 0),   -- Poraniopsis inflata
  ('4QD', 0.132, 0),   -- Henricia asthenactis
  ('4QG', 0.038, 0),   -- Henricia longispina
  ('4QH', 0.03, 0),    -- Henricia sanguinolenta
  ('4QI', 0.404, 0),   -- Henricia
  ('4RA', 0.01, 0),    -- Blood star
  ('4TA', 0.171, 0),   -- Solasteridae
  ('4TB', 0.171, 0),   -- Morning sun starfish
  ('4TC', 0.4, 0),     -- Striped sun starfish
  ('4TD', 0.096, 0),   -- Northern sun star
  ('4TF', 0.14, 0),    -- Solaster paxillatus
  ('4TG', 0.118, 0),   -- Rose starfish
  ('4TI', 0.02, 0),    -- Lophaster furcilliger
  ('4TJ', 0.074, 0),   -- Heterozonias alternatus
  ('4UE', 0.27, 0),    -- Pterasteridae
  ('4UF', 0.307, 0),   -- Diplopteraster multipes
  ('4UG', 0.07, 0),    -- Winged sea star
  ('4UH', 0.205, 0),   -- Cushion star
  ('4UK', 0.013, 0),   -- Pteraster jordani
  ('4UL', 0.425, 0),   -- Pteraster marsippus
  ('4UN', 0.74, 0),    -- Hymenaster
  ('4WB', 0.07, 0),    -- Asteriidae
  ('4XE', 0.868, 0),   -- Sunflower starfish
  ('4XF', 0.202, 0),   -- Fish-eating star
  ('4XG', 0.358, 0),   -- Rathbunaster californicus
  ('4XT', 0.15, 0),    -- Myxasteridae
  ('4YB', 0.244, 0),   -- Long-armed sea star
  ('4YE', 0.656, 0),   -- Mottled star
  ('4YW', 0.117, 0),   -- Zoroasteridae
  ('4YX', 0.03, 0),    -- Myxoderma sacculatum
  ('4YY', 0.03, 0),    -- Zoroaster evermanni
  ('4ZA', 0.29, 0),    -- Purple starfish
  ('4ZB', 0.26, 0),    -- Giant star
  ('4ZC', 0.947, 0),   -- Pink short-spined star
  ('501', 0.007, 1),   -- Soft sculpin
  ('502', 0.275, 1),   -- Red irish lord
  ('504', 0.505, 1),   -- Brown irish lord
  ('505', 4.58, 1),    -- Bigmouth sculpin
  ('507', 0.01, 1),    -- Northern sculpin
  ('508', 0.1, 1),     -- Dusky sculpin
  ('510', 0.155, 1),   -- Threadfin sculpin
  ('512', 0.05, 1),    -- Fringed sculpin
  ('513', 0.067, 1),   -- Spotfin sculpin
  ('515', 0.017, 1),   -- Thorny sculpin
  ('518', 0.246, 1),   -- Pacific staghorn sculpin
  ('519', 0.102, 1),   -- Blackfin sculpin
  ('51A', 0.25, 0),    -- Seaslugs
  ('51F', 0.031, 0),   -- Dorididae
  ('521', 0.488, 1),   -- Great sculpin
  ('522', 0.16, 1),    -- Sailfin sculpin
  ('531', 0.3, 1),     -- Alaska snailfish
  ('533', 0.01, 1),    -- Tadpole sculpin
  ('534', 5.513, 1),   -- Giant blobsculpin
  ('535', 0.059, 1),   -- Slim sculpin
  ('539', 0.564, 1),   -- Emarginate snailfish
  ('540', 2.961, 1),   -- Cabezon
  ('543', 0.03, 1),    -- Roughspine sculpin
  ('545', 0.035, 1),   -- Ribbed sculpin
  ('546', 0.08, 1),    -- Poachers
  ('549', 0.12, 1),    -- Northern spearnose poacher
  ('54B', 0.02, 0),    -- Arminidae
  ('54E', 0.05, 0),    -- Dendronotidae
  ('54T', 0.266, 0),   -- Tritoniidae
  ('54U', 0.147, 0),   -- Rosy tritonia
  ('550', 0.052, 1),   -- Sturgeon poacher
  ('551', 0.03, 1),    -- Falcate snailfish
  ('553', 0.075, 1),   -- Gray starsnout
  ('555', 0.051, 1),   -- Smootheye poacher
  ('556', 0.06, 1),    -- Bigeye poacher
  ('557', 0.053, 1),   -- Blackfin poacher
  ('562', 0.1, 1),     -- Warty poacher
  ('564', 0.105, 1),   -- Pygmy poacher
  ('566', 0.043, 1),   -- Blacktip poacher
  ('567', 0.075, 1),   -- Bluespotted poacher
  ('568', 0.374, 1),   -- Lumpfishes and snailfishes
  ('569', 0.5, 1),     -- Lumpfishes
  ('570', 1., 1),      -- Pink snailfish
  ('573', 0.015, 1),   -- Smalldisk snailfish
  ('574', 0.429, 1),   -- Blacktail snailfish
  ('578', 0.535, 1),   -- Snailfishes
  ('591', 0.001, 1),   -- Tadpole snailfish
  ('595', 0.278, 1),   -- Lefteye flounders
  ('596', 0.161, 1),   -- Pacific sanddab
  ('597', 0.097, 1),   -- Flatfishes
  ('598', 0.079, 1),   -- Speckled sanddab
  ('599', 1., 1),      -- Righteye flounders
  ('5AA', 0.342, 0),   -- Ophiuroidea
  ('5AB', 0.021, 0),   -- Phrynophiurida
  ('5AC', 0.3, 0),     -- Basket stars
  ('5AD', 0.123, 0),   -- Gorgonocephalus
  ('5AE', 0.146, 0),   -- Basket star
  ('5BC', 0.008, 0),   -- Asteronyx loveni
  ('5GA', 0.005, 0),   -- Ophiuridae
  ('5GD', 0.01, 0),    -- Amphiophiura ponderosa
  ('5QA', 0.16, 0),    -- Basket stars
  ('600', 0.447, 1),   -- Snailfishes
  --('602', 0.97, 1),  -- Arrowtooth flounder
  ('604', 2.371, 1),   -- Roughscale sole
  ('605', 0.531, 1),   -- Deepsea sole
  ('607', 0.771, 1),   -- Petrale sole
  ('60A', 0.095, 0),   -- Bivalve molluscs
  ('610', 0.208, 1),   -- Rex sole
  ('612', 0.266, 1),   -- Flathead sole
  ('614', 5.496, 1),   -- Pacific halibut
  ('619', 0.261, 1),   -- Butter sole
  --('621', 0.501, 1), -- Southern rock sole
  ('622', 0.585, 1),   -- Northern rock sole
  ('623', 0.053, 1),   -- Yellowfin sole
  ('625', 0.085, 1),   -- Slender sole
  ('626', 0.596, 1),   -- Dover sole
  ('628', 0.306, 1),   -- English sole
  ('631', 2.734, 1),   -- Starry flounder
  ('633', 0.37, 1),    -- C-o sole
  ('635', 0.375, 1),   -- Curlfin sole
  ('636', 0.37, 1),    -- Sand sole
  ('638', 2., 1),      -- Greenland halibut
  ('640', 9.5, 1),     -- Ocean sunfish
  ('642', 0.323, 1),   -- Slickheads
  ('643', 0.13, 1),    -- Threadfin slickhead
  ('65G', 0.033, 0),   -- Mytilus
  ('665', 0.046, 1),   -- Cuskeels
  ('677', 0.083, 1),   -- Snakehead eelpout
  ('67B', 0.132, 0),   -- Scallop
  ('67C', 0.015, 0),   -- Spiny scallop
  ('67E', 0.024, 0),   -- Pink scallop, (aka reddish scallop)
  ('68A', 0.32, 0),    -- Weathervane scallop
  ('68H', 0.275, 0),   -- Green false-jingle
  ('6AA', 0.063, 0),   -- Echinoidea
  ('6AB', 0.086, 0),   -- Sea urchins
  ('6AF', 0.126, 0),   -- Fragile urchin
  ('6BA', 0.056, 0),   -- Pallid urchin
  ('6BB', 0.106, 0),   -- Green urchin
  ('6BC', 1.036, 0),   -- Red urchin
  ('6BD', 0.5, 0),     -- Purple sea urchins
  ('6KA', 0.056, 0),   -- Heart urchins
  ('6N0', 0.01, 0),    -- Proboscis worm
  ('6NA', 0.334, 0),   -- Sea cucumbers
  ('6OB', 0.506, 0),   -- Giant red sea cucumber
  ('6OC', 0.192, 0),   -- Whitespotted sea cucumber
  ('6PA', 0.04, 0),    -- Synallactidae
  ('6PB', 0.077, 0),   -- Soft sea cucumber
  ('6PC', 0.11, 0),    -- Papillose sea cucumber
  ('6QA', 0.024, 0),   -- Psolidae
  ('6QD', 0.023, 0),   -- Scaly sea cucumber
  ('6RA', 0.077, 0),   -- Cucumariidae
  ('6RB', 0.74, 0),    -- Black sea cucumber
  ('6RC', 0.55, 0),    -- Orange sea cucumber
  ('6RD', 0.022, 0),   -- Pale sea cucumber
  ('6SB', 0.01, 0),    -- Smoothplated pentamera
  ('6TC', 0.203, 0),   -- White sea cucumber
  ('6UA', 0.123, 0),   -- Molpadiidae
  ('6UB', 0.133, 0),   -- Sweet potato sea cucumber
  ('6XO', 0.19, 0),    -- Pannychia moseleyi
  ('77C', 0.003, 0),   -- Macoma
  ('799', 0.1, 1),     -- Slimy sculpin
  ('82E', 0.183, 0),   -- Butter clam
  ('830', 0.01, 1),    -- Pacific hatchetfish
  ('834', 0.5, 1),     -- California slickhead
  ('835', 0.023, 1),   -- Rough abyssal grenadier
  ('84C', 1.62, 0),    -- Pacific geoduck
  ('853', 453.6, 0),   -- Steller sea lion
  ('872', 100., 0),    -- Dall porpoise
  ('8A9', 0.01, 0),    -- Tunicata
  ('8AB', 0.125, 0),   -- Ascidians and tunicates
  ('8DG', 0.053, 0),   -- Ascidia
  ('8DU', 0.006, 0),   -- Chelyosoma productum
  ('8EZ', 0.077, 0),   -- Halocynthia hilgendorfi
  ('901', 5., 0),      -- Albatrosses
  ('903', 3.29, 0),    -- Black-footed albatross
  ('91A', 0.199, 0),   -- Cephalopods
  ('91G', 0.043, 0),   -- Pacific bobtail squid
  ('92A', 2.368, 0),   -- Squids
  ('92C', 0.39, 0),    -- Loliginidae
  ('92D', 0.098, 0),   -- Opalescent inshore squid
  ('92H', 0.186, 0),   -- Histioteuthidae
  ('92J', 0.099, 0),   -- Histioteuthis heteropsis
  ('92R', 11.61, 0),   -- Giant squid
  ('939', 4., 0),      -- Ring-billed gull
  ('93B', 0.043, 0),   -- Cranchiidae
  ('93F', 0.005, 0),   -- Taonius pavo
  ('93J', 0.007, 0),   -- Abraliopsis felis
  ('93Q', 0.01, 0),    -- Chiroteuthis calyx
  ('94D', 0.135, 0),   -- Octopoteuthis deletron
  ('94H', 1.224, 0),   -- Neon flying squid
  ('94I', 7.643, 0),   -- Humboldt squid
  ('95A', 0.413, 0),   -- Gonate squids
  ('95B', 0.019, 0),   -- Gonatus
  ('95E', 0.739, 0),   -- Schoolmaster gonate squid
  ('95G', 0.389, 0),   -- Boreopacific gonate squid
  ('95N', 0.006, 0),   -- Clawed armhook squid
  ('96A', 0.234, 0),   -- Boreal clubhook squid
  ('96C', 4.506, 0),   -- Robust clubhook squid
  ('96E', 1.566, 0),   -- Vampire squid
  ('96F', 0.01, 0),    -- Vampyroteuthidae
  ('96G', 0.2, 0),     -- Vampire squid
  ('97A', 4.643, 0),   -- Octopus
  ('97D', 1.096, 0),   -- Flapjack devilfish
  ('98C', 0.75, 0),    -- Octopodidae
  ('98D', 3.311, 0),   -- Octopus
  ('98E', 6.312, 0),   -- Giant pacific octopus
  ('98F', 0.07, 0),    -- Smoothskin octopus
  ('98G', 0.098, 0),   -- Pacific red octopus
  ('999', 0.45, 0),    -- Missing sample
  ('99A', 0.533, 0),   -- Benthoctopus
  ('99F', 0.467, 0),   -- Benthoctopus sp b
  ('99G', 0.05, 0),    -- Benthoctopus sp c
  ('AAA', 0.05, 0),    -- Crustaceans
  ('HOC', 0.407, 0),   -- Acorn barnacle
  ('HOE', 0.29, 0),    -- Giant barnacle
  ('IJM', 0.01, 0),    -- Gnathophausia ingens
  ('KAA', 0.008, 0),   -- Isopods
  ('SAA', 0.755, 0),   -- Decapods
  ('SAB', 0.03, 0),    -- Shrimp
  ('SBA', 0.058, 0),   -- Pandalid shrimp
  ('SCD', 0.006, 0),   -- Pink shrimp
  ('SCF', 0.007, 0),   -- Coonstripe shrimp
  ('SDB', 0.02, 0),    -- Pink shrimp (smooth)
  ('SDD', 0.006, 0),   -- Yellowleg shrimp
  ('SDF', 0.094, 0),   -- Prawn
  ('SEE', 0.026, 0),   -- Sidestripe shrimp
  ('SIA', 0.033, 0),   -- Crangons
  ('SIC', 0.01, 0),    -- Northern crangon
  ('SQA', 0.025, 0),   -- Eualus
  ('SQH', 0.001, 0),   -- Large eyed eualid
  ('TAA', 0.003, 0),   -- Pasiphaeidae
  ('TAC', 0.005, 0),   -- Glass shrimp
  ('TAE', 0.075, 0),   -- Crimson pasiphaeid
  ('TPC', 0.003, 0),   -- Sergestes
  ('UAA', 0.429, 0),   -- Reptantia
  ('URB', 0.044, 0),   -- Ghost shrimp
  ('VAB', 0.036, 0),   -- Paguroidea
  ('VAC', 0.11, 0),    -- Right-handed hermits
  ('VAG', 0.35, 0),    -- Furry hermit
  ('VBB', 0.109, 0),   -- Pagurus
  ('VIA', 0.2, 0),     -- Lithodio crabs
  ('VLA', 0.245, 0),   -- Lithodinae
  ('VLC', 0.1, 0),     -- Bristly crab
  ('VMB', 0.736, 0),   -- Lithodes
  ('VMC', 0.768, 0),   -- Golden king crab
  ('VMD', 0.493, 0),   -- Lithodes couesi
  ('VMH', 0.441, 0),   -- Box crabs
  ('VMI', 0.444, 0),   -- Brown box crab
  ('VNF', 0.932, 0),   -- Alaskan king crabs
  ('VNG', 0.691, 0),   -- Brown king crab
  ('VNH', 11.189, 0),  -- Red king crab
  ('VOG', 0.453, 0),   -- Paralomis multispina
  ('VOH', 0.554, 0),   -- Paralomis verrilli
  ('VSA', 0.013, 0),   -- Squat lobster
  ('WAA', 0.311, 0),   -- True crabs
  ('XKA', 0.076, 0),   -- Cancer crabs
  ('XKC', 0.056, 0),   -- Furrowed rock crab
  ('XKE', 0.254, 0),   -- Graceful rock crab
  ('XKG', 0.689, 0),   -- Dungeness crab
  ('XLA', 0.207, 0),   -- Red rock crab
  ('ZAA', 0.645, 0),   -- Spider crabs
  ('ZAB', 0.01, 0),    -- Spider crabs
  ('ZAD', 0.498, 0),   -- Tanner crabs
  ('ZAE', 0.54, 0),    -- Triangle tanner crab
  ('ZAF', 0.397, 0),   -- Inshore tanner crab
  ('ZAG', 0.538, 0),   -- Grooved tanner crab
  ('ZCA', 0.091, 0),   -- Graceful decorator crab
  ('ZDF', 0.155, 0),   -- Northern kelp crab
  ('ZGC', 0.012, 0),   -- Sharp-nosed crab
  ('ZGE', 0.067, 0),   -- Redclaw crab
  ('ZKA', 0.086, 0),   -- Salps
  ('ZLA', 0.057, 0),   -- Salps
  ('ZLB', 0.044, 0),   -- Salp
  ('ZLI', 1., 0)       -- Salp
