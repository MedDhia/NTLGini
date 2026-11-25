#' Internal fast Gini index calculation
#' @keywords internal
.fast_gini <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2L) return(0.0)
  s <- sum(x)
  if (s <= 0) return(0.0)
  x_sort <- sort(x)
  i <- seq_len(n)
  unname((2.0 * sum(i * x_sort)) / (n * s) - (n + 1.0) / n)
}

.country_bbox_table <- list(
  AFG = c(xmin = 60.4857, ymin = 29.3919, xmax = 74.8913, ymax = 38.4564),
  AGO = c(xmin = 11.7431, ymin = -18.0197, xmax = 24.0467, ymax = -4.4289),
  AIA = c(xmin = -63.16, ymin = 18.1714, xmax = -62.9796, ymax = 18.2697),
  ALA = c(xmin = 19.519, ymin = 60.0117, xmax = 20.6113, ymax = 60.4058),
  ALB = c(xmin = 19.2807, ymin = 39.6535, xmax = 21.0311, ymax = 42.6479),
  AND = c(xmin = 1.4148, ymin = 42.4345, xmax = 1.7402, ymax = 42.6427),
  ARE = c(xmin = 51.5684, ymin = 22.6215, xmax = 56.388, ymax = 26.0682),
  ARG = c(xmin = -73.5763, ymin = -55.0321, xmax = -53.6686, ymax = -21.8025),
  ARM = c(xmin = 43.4395, ymin = 38.869, xmax = 46.5848, ymax = 41.291),
  ASM = c(xmin = -170.8205, ymin = -14.3598, xmax = -170.5681, ymax = -14.2574),
  ATG = c(xmin = -61.8871, ymin = 16.9972, xmax = -61.686, ymax = 17.7141),
  AUS = c(xmin = 112.9082, ymin = -54.7492, xmax = 158.9589, ymax = -10.0518),
  AUT = c(xmin = 9.524, ymin = 46.3997, xmax = 17.1474, ymax = 49.0011),
  AZE = c(xmin = 44.7683, ymin = 38.3987, xmax = 50.3659, ymax = 41.891),
  BDI = c(xmin = 29.0142, ymin = -4.4559, xmax = 30.8114, ymax = -2.313),
  BEL = c(xmin = 2.5249, ymin = 49.5109, xmax = 6.3645, ymax = 51.4911),
  BEN = c(xmin = 0.7634, ymin = 6.2168, xmax = 3.8345, ymax = 12.3838),
  BES = c(xmin = -68.5, ymin = 12.0, xmax = -68.1, ymax = 12.4),
  BFA = c(xmin = -5.5235, ymin = 9.4247, xmax = 2.3892, ymax = 15.0779),
  BGD = c(xmin = 88.0234, ymin = 20.7904, xmax = 92.6316, ymax = 26.5715),
  BGR = c(xmin = 22.344, ymin = 41.2436, xmax = 28.5854, ymax = 44.2378),
  BHR = c(xmin = 50.4524, ymin = 25.8068, xmax = 50.6175, ymax = 26.2464),
  BHS = c(xmin = -78.9856, ymin = 20.9374, xmax = -72.7473, ymax = 26.9401),
  BIH = c(xmin = 15.7366, ymin = 42.5597, xmax = 19.5838, ymax = 45.2766),
  BLM = c(xmin = -62.8754, ymin = 17.8752, xmax = -62.7997, ymax = 17.9223),
  BLR = c(xmin = 23.1751, ymin = 51.265, xmax = 32.7103, ymax = 56.1458),
  BLZ = c(xmin = -89.2375, ymin = 15.8887, xmax = -87.7886, ymax = 18.4823),
  BMU = c(xmin = -64.8628, ymin = 32.2596, xmax = -64.6683, ymax = 32.3869),
  BOL = c(xmin = -69.6457, ymin = -22.8917, xmax = -57.4957, ymax = -9.7104),
  BRA = c(xmin = -74.0021, ymin = -33.7422, xmax = -34.8055, ymax = 5.258),
  BRB = c(xmin = -59.6467, ymin = 13.0622, xmax = -59.4276, ymax = 13.3177),
  BRN = c(xmin = 114.0639, ymin = 4.024, xmax = 115.3268, ymax = 5.0224),
  BTN = c(xmin = 88.7388, ymin = 26.7016, xmax = 92.0834, ymax = 28.3112),
  BWA = c(xmin = 19.9773, ymin = -26.8542, xmax = 29.3648, ymax = -17.7876),
  CAF = c(xmin = 14.4312, ymin = 2.2701, xmax = 27.4033, ymax = 10.9962),
  CAN = c(xmin = -141.0021, ymin = 41.6749, xmax = -52.6537, ymax = 83.1161),
  CHE = c(xmin = 5.97, ymin = 45.83, xmax = 10.4546, ymax = 47.7756),
  CHL = c(xmin = -109.4341, ymin = -55.8917, xmax = -66.4358, ymax = -17.5061),
  CHN = c(xmin = 73.6073, ymin = 18.2183, xmax = 134.7523, ymax = 53.5556),
  CIV = c(xmin = -8.6036, ymin = 4.3513, xmax = -2.5059, ymax = 10.7241),
  CMR = c(xmin = 8.5328, ymin = 1.6762, xmax = 16.1834, ymax = 13.0785),
  COD = c(xmin = 12.2137, ymin = -13.4538, xmax = 31.274, ymax = 5.3121),
  COG = c(xmin = 11.1302, ymin = -5.0043, xmax = 18.6222, ymax = 3.6873),
  COK = c(xmin = -159.8425, ymin = -21.2495, xmax = -159.7369, ymax = -21.1864),
  COL = c(xmin = -79.0254, ymin = -4.2359, xmax = -66.876, ymax = 12.4344),
  COM = c(xmin = 43.2267, ymin = -12.3683, xmax = 44.5268, ymax = -11.3685),
  CPV = c(xmin = -25.3416, ymin = 14.8182, xmax = -22.6819, ymax = 17.1937),
  CRI = c(xmin = -85.908, ymin = 8.0707, xmax = -82.5636, ymax = 11.1895),
  CUB = c(xmin = -84.8872, ymin = 19.8555, xmax = -74.1368, ymax = 23.1904),
  CYM = c(xmin = -81.4191, ymin = 19.2719, xmax = -79.7423, ymax = 19.7657),
  CYP = c(xmin = 32.301, ymin = 34.5696, xmax = 34.0502, ymax = 35.1827),
  CZE = c(xmin = 12.0897, ymin = 48.5762, xmax = 18.8322, ymax = 51.0378),
  DEU = c(xmin = 5.8575, ymin = 47.2788, xmax = 15.0166, ymax = 55.0587),
  DJI = c(xmin = 41.7646, ymin = 10.941, xmax = 43.4098, ymax = 12.7086),
  DMA = c(xmin = -61.4812, ymin = 15.2273, xmax = -61.2511, ymax = 15.6331),
  DNK = c(xmin = 8.1215, ymin = 54.6289, xmax = 15.1371, ymax = 57.7369),
  DOM = c(xmin = -72.0004, ymin = 17.6356, xmax = -68.3392, ymax = 19.914),
  DZA = c(xmin = -8.6833, ymin = 18.9866, xmax = 11.9679, ymax = 37.0924),
  ECU = c(xmin = -91.6542, ymin = -4.9906, xmax = -75.2496, ymax = 1.4554),
  EGY = c(xmin = 24.7032, ymin = 21.9949, xmax = 36.8714, ymax = 31.655),
  ERI = c(xmin = 36.4268, ymin = 12.3766, xmax = 43.1167, ymax = 18.0051),
  ESH = c(xmin = -17.0988, ymin = 20.8062, xmax = -8.6821, ymax = 27.6564),
  ESP = c(xmin = -18.1605, ymin = 27.6464, xmax = 4.3221, ymax = 43.7646),
  EST = c(xmin = 21.8545, ymin = 57.5255, xmax = 28.1511, ymax = 59.639),
  ETH = c(xmin = 32.9989, ymin = 3.4561, xmax = 47.9782, ymax = 14.8523),
  FIN = c(xmin = 20.6222, ymin = 59.816, xmax = 31.5365, ymax = 70.0648),
  FJI = c(xmin = -180.0, ymin = -21.7059, xmax = 180.0, ymax = -12.477),
  FRA = c(xmin = -61.7941, ymin = -21.369, xmax = 55.8391, ymax = 51.0971),
  FRO = c(xmin = -7.4226, ymin = 61.4143, xmax = -6.4061, ymax = 62.3557),
  FSM = c(xmin = 138.0619, ymin = 5.2772, xmax = 162.9935, ymax = 9.5933),
  GAB = c(xmin = 8.7031, ymin = -3.9163, xmax = 14.4806, ymax = 2.3022),
  GBR = c(xmin = -8.1448, ymin = 50.0214, xmax = 1.7466, ymax = 60.8319),
  GEO = c(xmin = 39.9783, ymin = 41.0702, xmax = 46.6726, ymax = 43.5698),
  GGY = c(xmin = -2.6461, ymin = 49.4287, xmax = -2.5123, ymax = 49.5066),
  GHA = c(xmin = -3.2439, ymin = 4.7625, xmax = 1.1872, ymax = 11.1669),
  GIN = c(xmin = -15.0512, ymin = 7.2159, xmax = -7.6812, ymax = 12.6739),
  GLP = c(xmin = -61.8, ymin = 15.8, xmax = -61.0, ymax = 16.6),
  GMB = c(xmin = -16.8248, ymin = 13.0642, xmax = -13.8267, ymax = 13.8121),
  GNB = c(xmin = -16.7118, ymin = 10.9401, xmax = -13.6735, ymax = 12.6799),
  GNQ = c(xmin = 8.4343, ymin = 0.9601, xmax = 11.3354, ymax = 3.7583),
  GRC = c(xmin = 19.6465, ymin = 34.9345, xmax = 28.2318, ymax = 41.7438),
  GRD = c(xmin = -61.7822, ymin = 12.0084, xmax = -61.607, ymax = 12.237),
  GRL = c(xmin = -72.8181, ymin = 59.8155, xmax = -11.4255, ymax = 83.5996),
  GTM = c(xmin = -92.2352, ymin = 13.7365, xmax = -88.2283, ymax = 17.8164),
  GUF = c(xmin = -54.6, ymin = 2.1, xmax = -51.6, ymax = 5.8),
  GUM = c(xmin = 144.6493, ymin = 13.2575, xmax = 144.9408, ymax = 13.6224),
  GUY = c(xmin = -61.3908, ymin = 1.2012, xmax = -56.4828, ymax = 8.5493),
  HND = c(xmin = -89.3626, ymin = 12.9792, xmax = -83.1575, ymax = 16.514),
  HRV = c(xmin = 13.5172, ymin = 42.4329, xmax = 19.401, ymax = 46.5346),
  HTI = c(xmin = -74.4781, ymin = 18.0392, xmax = -71.6453, ymax = 20.0937),
  HUN = c(xmin = 16.0931, ymin = 45.753, xmax = 22.8767, ymax = 48.5535),
  IDN = c(xmin = 95.2066, ymin = -10.9097, xmax = 140.9762, ymax = 5.907),
  IMN = c(xmin = -4.7854, ymin = 54.0587, xmax = -4.338, ymax = 54.4072),
  IND = c(xmin = 68.165, ymin = 6.7487, xmax = 97.3436, ymax = 35.4959),
  IRL = c(xmin = -10.3902, ymin = 51.4737, xmax = -6.0274, ymax = 55.3658),
  IRN = c(xmin = 44.0232, ymin = 25.1021, xmax = 63.3052, ymax = 39.7686),
  IRQ = c(xmin = 38.7735, ymin = 29.0637, xmax = 48.5465, ymax = 37.3719),
  ISL = c(xmin = -24.4757, ymin = 63.4067, xmax = -13.5561, ymax = 66.5261),
  ISR = c(xmin = 34.2453, ymin = 29.4773, xmax = 35.888, ymax = 33.4157),
  ITA = c(xmin = 6.6277, ymin = 36.6878, xmax = 18.4858, ymax = 47.0821),
  JAM = c(xmin = -78.3395, ymin = 17.7149, xmax = -76.2108, ymax = 18.5222),
  JEY = c(xmin = -2.2358, ymin = 49.1698, xmax = -2.0099, ymax = 49.2664),
  JOR = c(xmin = 34.9508, ymin = 29.1905, xmax = 39.2928, ymax = 33.3722),
  JPN = c(xmin = 123.6798, ymin = 24.2661, xmax = 145.833, ymax = 45.5095),
  KAZ = c(xmin = 46.6092, ymin = 40.6086, xmax = 87.3229, ymax = 55.3896),
  KEN = c(xmin = 33.9, ymin = -4.6924, xmax = 41.884, ymax = 5.4923),
  KGZ = c(xmin = 69.2291, ymin = 39.2075, xmax = 80.2462, ymax = 43.2404),
  KHM = c(xmin = 102.3197, ymin = 10.4112, xmax = 107.6055, ymax = 14.7051),
  KNA = c(xmin = -62.8405, ymin = 17.1006, xmax = -62.5322, ymax = 17.4026),
  KOR = c(xmin = 126.0075, ymin = 33.2015, xmax = 130.9343, ymax = 38.6234),
  KWT = c(xmin = 46.5314, ymin = 28.5332, xmax = 48.4425, ymax = 30.0973),
  LAO = c(xmin = 100.1149, ymin = 13.9212, xmax = 107.6531, ymax = 22.4953),
  LBN = c(xmin = 35.1086, ymin = 33.0757, xmax = 36.585, ymax = 34.6787),
  LBR = c(xmin = -11.5075, ymin = 4.3513, xmax = -7.3999, ymax = 8.5377),
  LBY = c(xmin = 9.3103, ymin = 19.4966, xmax = 25.1505, ymax = 33.1819),
  LCA = c(xmin = -61.0731, ymin = 13.7176, xmax = -60.8868, ymax = 14.0934),
  LIE = c(xmin = 9.4795, ymin = 47.0574, xmax = 9.6105, ymax = 47.2708),
  LKA = c(xmin = 79.7078, ymin = 5.9494, xmax = 81.877, ymax = 9.8127),
  LSO = c(xmin = 27.0518, ymin = -30.6423, xmax = 29.3907, ymax = -28.5817),
  LTU = c(xmin = 20.8998, ymin = 53.893, xmax = 26.7757, ymax = 56.4112),
  LUX = c(xmin = 5.725, ymin = 49.4455, xmax = 6.4938, ymax = 50.1672),
  LVA = c(xmin = 21.0149, ymin = 55.6675, xmax = 28.2021, ymax = 58.0634),
  MAR = c(xmin = -17.0031, ymin = 21.4207, xmax = -1.0655, ymax = 35.9299),
  MDA = c(xmin = 26.6189, ymin = 45.4504, xmax = 30.1311, ymax = 48.4777),
  MDG = c(xmin = 43.2571, ymin = -25.5705, xmax = 50.4827, ymax = -12.0796),
  MEX = c(xmin = -118.4014, ymin = 14.5454, xmax = -86.6963, ymax = 32.7153),
  MHL = c(xmin = 166.8447, ymin = 5.7998, xmax = 171.7568, ymax = 11.1687),
  MKD = c(xmin = 20.4486, ymin = 40.8499, xmax = 23.0057, ymax = 42.3582),
  MLI = c(xmin = -12.2806, ymin = 10.1433, xmax = 4.2347, ymax = 24.9956),
  MLT = c(xmin = 14.1804, ymin = 35.8202, xmax = 14.5662, ymax = 36.0758),
  MMR = c(xmin = 92.1796, ymin = 9.8754, xmax = 101.1473, ymax = 28.517),
  MNE = c(xmin = 18.4363, ymin = 41.8691, xmax = 20.3477, ymax = 43.5423),
  MNG = c(xmin = 87.7432, ymin = 41.5955, xmax = 119.8979, ymax = 52.1173),
  MNP = c(xmin = 145.1521, ymin = 14.1113, xmax = 145.8354, ymax = 18.8068),
  MOZ = c(xmin = 30.2218, ymin = -26.8616, xmax = 40.8445, ymax = -10.4644),
  MRT = c(xmin = -17.064, ymin = 14.7454, xmax = -4.8226, ymax = 27.2859),
  MSR = c(xmin = -62.223, ymin = 16.6812, xmax = -62.1484, ymax = 16.8096),
  MTQ = c(xmin = -61.3, ymin = 14.3, xmax = -60.8, ymax = 14.9),
  MUS = c(xmin = 57.3177, ymin = -20.5132, xmax = 57.792, ymax = -19.9899),
  MWI = c(xmin = 32.6704, ymin = -17.1311, xmax = 35.8928, ymax = -9.395),
  MYS = c(xmin = 99.6463, ymin = 0.862, xmax = 119.2663, ymax = 7.3517),
  MYT = c(xmin = 45.0, ymin = -13.0, xmax = 45.3, ymax = -12.6),
  NAM = c(xmin = 11.7217, ymin = -28.9388, xmax = 25.2588, ymax = -16.9677),
  NCL = c(xmin = 159.9282, ymin = -22.6611, xmax = 168.1391, ymax = -19.1146),
  NER = c(xmin = 0.1639, ymin = 11.6963, xmax = 15.9632, ymax = 23.5179),
  NGA = c(xmin = 2.686, ymin = 4.2774, xmax = 14.6271, ymax = 13.8729),
  NIC = c(xmin = -87.6702, ymin = 10.7354, xmax = -83.1575, ymax = 15.0081),
  NLD = c(xmin = -68.3711, ymin = 12.0321, xmax = 7.1973, ymax = 53.6255),
  NOR = c(xmin = -9.0989, ymin = 58.0209, xmax = 33.6293, ymax = 80.4778),
  NPL = c(xmin = 80.0517, ymin = 26.3603, xmax = 88.1615, ymax = 30.3875),
  NRU = c(xmin = 166.907, ymin = -0.5508, xmax = 166.9584, ymax = -0.4894),
  NZL = c(xmin = -176.8477, ymin = -52.5703, xmax = 178.5362, ymax = -8.5465),
  OMN = c(xmin = 51.9776, ymin = 16.6484, xmax = 59.8375, ymax = 26.3563),
  PAK = c(xmin = 60.8434, ymin = 23.7534, xmax = 77.0486, ymax = 37.0367),
  PAN = c(xmin = -83.0273, ymin = 7.2201, xmax = -77.196, ymax = 9.5979),
  PER = c(xmin = -81.3366, ymin = -18.3456, xmax = -68.6853, ymax = -0.0417),
  PHL = c(xmin = 116.9695, ymin = 5.0602, xmax = 126.5934, ymax = 20.8413),
  PLW = c(xmin = 131.135, ymin = 3.0219, xmax = 134.6596, ymax = 7.7121),
  PNG = c(xmin = 140.8623, ymin = -11.6306, xmax = 155.9576, ymax = -1.3532),
  POL = c(xmin = 14.1286, ymin = 49.0208, xmax = 24.1058, ymax = 54.8382),
  PRI = c(xmin = -67.9371, ymin = 17.9473, xmax = -65.2949, ymax = 18.5222),
  PRK = c(xmin = 124.3486, ymin = 37.719, xmax = 130.6873, ymax = 42.9981),
  PRT = c(xmin = -31.283, ymin = 32.6483, xmax = -6.2125, ymax = 42.1374),
  PRY = c(xmin = -62.651, ymin = -27.5538, xmax = -54.2418, ymax = -19.2862),
  PSE = c(xmin = 34.1981, ymin = 31.2083, xmax = 35.5721, ymax = 32.5344),
  PYF = c(xmin = -151.5124, ymin = -20.8759, xmax = -136.2939, ymax = -8.7815),
  QAT = c(xmin = 50.7546, ymin = 24.5646, xmax = 51.6089, ymax = 26.1533),
  REU = c(xmin = 55.2, ymin = -21.4, xmax = 55.9, ymax = -20.8),
  ROU = c(xmin = 20.2418, ymin = 43.6708, xmax = 29.7059, ymax = 48.2635),
  RUS = c(xmin = -180.0, ymin = 41.1993, xmax = 180.0, ymax = 81.8542),
  RWA = c(xmin = 28.8576, ymin = -2.8086, xmax = 30.8766, ymax = -1.0631),
  SAU = c(xmin = 34.6162, ymin = 16.3718, xmax = 55.641, ymax = 32.1245),
  SDN = c(xmin = 21.8253, ymin = 8.6656, xmax = 38.6095, ymax = 22.2024),
  SEN = c(xmin = -17.5356, ymin = 12.328, xmax = -11.3824, ymax = 16.6789),
  SGP = c(xmin = 103.6502, ymin = 1.2654, xmax = 103.9964, ymax = 1.4471),
  SHN = c(xmin = -14.4149, ymin = -16.004, xmax = -5.6597, ymax = -7.8826),
  SLB = c(xmin = 155.6775, ymin = -11.8322, xmax = 166.9292, ymax = -6.6089),
  SLE = c(xmin = -13.2927, ymin = 6.9065, xmax = -10.2832, ymax = 9.9965),
  SLV = c(xmin = -90.1059, ymin = 13.164, xmax = -87.7153, ymax = 14.4311),
  SMR = c(xmin = 12.3969, ymin = 43.8941, xmax = 12.5146, ymax = 43.9897),
  SOM = c(xmin = 40.9645, ymin = -1.6953, xmax = 51.3902, ymax = 11.9837),
  SPM = c(xmin = -56.3869, ymin = 46.7528, xmax = -56.1374, ymax = 47.099),
  SRB = c(xmin = 18.8391, ymin = 42.2421, xmax = 22.9769, ymax = 46.1692),
  SSD = c(xmin = 24.1474, ymin = 3.4907, xmax = 35.2684, ymax = 12.2231),
  STP = c(xmin = 6.4682, ymin = 0.0474, xmax = 7.4523, ymax = 1.6991),
  SUR = c(xmin = -58.0545, ymin = 1.8422, xmax = -53.9905, ymax = 5.9935),
  SVK = c(xmin = 16.8627, ymin = 47.7634, xmax = 22.5387, ymax = 49.5977),
  SVN = c(xmin = 13.3782, ymin = 45.4284, xmax = 16.5162, ymax = 46.8633),
  SWE = c(xmin = 11.1472, ymin = 55.3464, xmax = 24.1555, ymax = 69.0369),
  SWZ = c(xmin = 30.7875, ymin = -27.31, xmax = 32.1129, ymax = -25.743),
  SYC = c(xmin = 55.3834, ymin = -4.7855, xmax = 55.543, ymax = -4.5588),
  SYR = c(xmin = 35.7645, ymin = 32.3173, xmax = 42.3591, ymax = 37.2973),
  TCA = c(xmin = -72.3424, ymin = 21.7517, xmax = -71.6369, ymax = 21.9519),
  TCD = c(xmin = 13.4482, ymin = 7.4753, xmax = 23.9834, ymax = 23.4452),
  TGO = c(xmin = -0.0902, ymin = 6.0894, xmax = 1.7779, ymax = 11.1156),
  THA = c(xmin = 97.3739, ymin = 5.6368, xmax = 105.641, ymax = 20.4244),
  TJK = c(xmin = 67.3496, ymin = 36.684, xmax = 75.1188, ymax = 41.0351),
  TKL = c(xmin = -172.6, ymin = -9.5, xmax = -171.1, ymax = -8.5),
  TKM = c(xmin = 52.4938, ymin = 35.1708, xmax = 66.6293, ymax = 42.7785),
  TLS = c(xmin = 124.0363, ymin = -9.5119, xmax = 127.2961, ymax = -8.1399),
  TON = c(xmin = -175.3624, ymin = -21.4506, xmax = -173.9219, ymax = -18.5653),
  TTO = c(xmin = -61.9061, ymin = 10.0646, xmax = -60.5255, ymax = 11.3254),
  TUN = c(xmin = 7.4956, ymin = 30.2294, xmax = 11.5359, ymax = 37.3404),
  TUR = c(xmin = 25.6689, ymin = 35.8314, xmax = 44.8172, ymax = 42.0933),
  TUV = c(xmin = 179.1957, ymin = -8.535, xmax = 179.2166, ymax = -8.4663),
  TWN = c(xmin = 118.2873, ymin = 21.925, xmax = 121.929, ymax = 25.2769),
  TZA = c(xmin = 29.3234, ymin = -11.7162, xmax = 40.4636, ymax = -0.9949),
  UGA = c(xmin = 29.5619, ymin = -1.4699, xmax = 34.9782, ymax = 4.2202),
  UKR = c(xmin = 22.1318, ymin = 45.2341, xmax = 40.1283, ymax = 52.3536),
  UMI = c(xmin = -180.0, ymin = -1.0, xmax = 180.0, ymax = 29.0),
  URY = c(xmin = -58.4381, ymin = -34.9328, xmax = -53.1256, ymax = -30.1011),
  USA = c(xmin = -178.1945, ymin = 18.9639, xmax = 179.78, ymax = 71.4077),
  UZB = c(xmin = 55.9757, ymin = 37.1722, xmax = 73.1369, ymax = 45.5554),
  VCT = c(xmin = -61.3535, ymin = 12.6947, xmax = -61.124, ymax = 13.3587),
  VEN = c(xmin = -73.3662, ymin = 0.688, xmax = -59.8289, ymax = 12.1779),
  VGB = c(xmin = -64.6951, ymin = 18.3991, xmax = -64.2736, ymax = 18.7527),
  VIR = c(xmin = -65.0236, ymin = 17.7017, xmax = -64.5805, ymax = 18.3852),
  VNM = c(xmin = 102.1274, ymin = 8.5833, xmax = 109.4449, ymax = 23.3452),
  VUT = c(xmin = 166.5261, ymin = -20.2418, xmax = 169.8963, ymax = -13.7095),
  WLF = c(xmin = -178.1944, ymin = -14.3249, xmax = -176.1281, ymax = -13.2217),
  WSM = c(xmin = -172.7785, ymin = -14.0473, xmax = -171.4496, ymax = -13.4652),
  XAD = c(xmin = 32.7, ymin = 34.5, xmax = 34.0, ymax = 35.1),
  XKO = c(xmin = 20.0295, ymin = 41.8538, xmax = 21.7529, ymax = 43.2611),
  YEM = c(xmin = 42.549, ymin = 12.319, xmax = 54.5111, ymax = 18.9961),
  ZAF = c(xmin = 16.4476, ymin = -46.9629, xmax = 37.8877, ymax = -22.1463),
  ZMB = c(xmin = 21.9789, ymin = -18.0415, xmax = 33.6615, ymax = -8.1937),
  ZNC = c(xmin = 32.2, ymin = 35.1, xmax = 34.6, ymax = 35.7),
  ZWE = c(xmin = 25.224, ymin = -22.4021, xmax = 33.0067, ymax = -15.6431)
)

#' Fetch and crop Harmonized Nighttime-Light rasters (Cheng et al. 2026)
#'
#' Retrieves and crops 500 m VIIRS-like nighttime light rasters for a custom region
#' of interest (`roi`), bounding box, or country code for any year (1992-2024)
#' or month (1-12). Uses GDAL cloud-optimized HTTP streaming (/vsizip/ and /vsicurl/)
#' to crop directly from the remote repository without downloading global archives.
#'
#' @param country Optional character; country ISO3 code or country name (e.g. `"TUN"` or `"Tunisia"`).
#' @param roi Optional spatial region of interest: an `sf` or `sfc` object, `SpatVector`,
#'   or numeric bounding box `c(xmin, ymin, xmax, ymax)`.
#' @param year Integer year between 1992 and 2024. Default `2024`.
#' @param month Optional integer month between 1 and 12. If `NULL` (default),
#'   fetches the annual composite.
#' @param output_dir Directory where cropped rasters are cached.
#' @param overwrite Logical; whether to re-download if file already exists in cache.
#' @param return_raster Logical; if `TRUE` and `terra` is installed, returns a
#'   `SpatRaster`. If `FALSE`, returns the local file path.
#' @param quiet Logical; whether to suppress download/streaming messages.
#'   Default `!interactive()`.
#' @return A `terra::SpatRaster` object or character file path to the cropped GeoTIFF.
#' @seealso [ntl_extract()], [ntl_gini_monthly()]
#' @examples
#' \dontrun{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r_tun <- ntl_raster("TUN", year = 2024, month = 1)
#'   terra::plot(r_tun)
#' }
#' }
#' @export
ntl_raster <- function(country = NULL,
                       roi = NULL,
                       year = 2024L,
                       month = NULL,
                       output_dir = tools::R_user_dir("ntlgini", which = "cache"),
                       overwrite = FALSE,
                       return_raster = TRUE,
                       quiet = !interactive()) {
  year <- as.integer(year)
  if (length(year) != 1L || is.na(year) || year < 1992L || year > 2024L) {
    stop("year must be an integer between 1992 and 2024.", call. = FALSE)
  }

  if (!is.null(month)) {
    month <- as.integer(month)
    if (length(month) != 1L || is.na(month) || month < 1L || month > 12L) {
      stop("month must be an integer between 1 and 12, or NULL.", call. = FALSE)
    }
  }

  # Resolve bounding box
  ext_target <- NULL
  country_tag <- NULL

  if (!is.null(roi)) {
    if (is.numeric(roi) && length(roi) == 4L) {
      ext_target <- unname(roi)
      names(ext_target) <- c("xmin", "ymin", "xmax", "ymax")
    } else if (inherits(roi, c("sf", "sfc"))) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        stop("Package 'sf' is required to extract bbox from sf objects.", call. = FALSE)
      }
      bb <- sf::st_bbox(roi)
      ext_target <- c(xmin = as.numeric(bb["xmin"]), ymin = as.numeric(bb["ymin"]), xmax = as.numeric(bb["xmax"]), ymax = as.numeric(bb["ymax"]))
    } else if (inherits(roi, "SpatVector")) {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("Package 'terra' is required for SpatVector.", call. = FALSE)
      }
      e <- terra::ext(roi)
      ext_target <- c(xmin = as.numeric(e$xmin), ymin = as.numeric(e$ymin), xmax = as.numeric(e$xmax), ymax = as.numeric(e$ymax))
    } else {
      stop("Unsupported roi class: ", paste(class(roi), collapse = ", "), call. = FALSE)
    }
    # Deterministic cache tag from rounded coordinates
    country_tag <- sprintf("roi_%s_%s_%s_%s",
                           gsub("-", "m", sprintf("%.2f", ext_target["xmin"])),
                           gsub("-", "m", sprintf("%.2f", ext_target["ymin"])),
                           gsub("-", "m", sprintf("%.2f", ext_target["xmax"])),
                           gsub("-", "m", sprintf("%.2f", ext_target["ymax"])))
    country_tag <- gsub("\\.", "p", country_tag)
  } else if (!is.null(country)) {
    iso3_c <- .to_iso3(country)
    if (!iso3_c %in% names(.country_bbox_table)) {
      stop("Unknown country or ISO3 code: ", country, call. = FALSE)
    }
    ext_target <- .country_bbox_table[[iso3_c]]
    country_tag <- iso3_c
  } else {
    stop("Either 'country' or 'roi' must be specified.", call. = FALSE)
  }

  m_str <- if (!is.null(month)) sprintf("_%02d", month) else ""
  out_name <- sprintf("ntl_%s_%d%s.tif", country_tag, year, m_str)
  out_path <- file.path(output_dir, out_name)

  if (file.exists(out_path) && !overwrite) {
    if (return_raster && requireNamespace("terra", quietly = TRUE)) {
      return(terra::rast(out_path))
    }
    return(out_path)
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required to crop and stream remote nighttime light rasters. ",
      "Install with install.packages('terra').",
      call. = FALSE
    )
  }

  # Build Cheng et al. (2026) remote URL
  base_url <- "https://huggingface.co/datasets/faviolc/hku-viirs-nighttime-lights-global/resolve/main"
  if (is.null(month)) {
    fname <- if (year >= 2012L) sprintf("SVDNB_npp_%d.tif", year) else sprintf("SVDNB_npp_simulated_%d.tif", year)
    remote_vsi <- sprintf("/vsicurl/%s/yearly/%d/%s", base_url, year, fname)
  } else {
    is_sim <- (year < 2012L) || (year == 2012L && month < 4L)
    fname <- if (!is_sim) sprintf("SVDNB_npp_%d%02d.tif", year, month) else sprintf("SVDNB_npp_simulated_%d%02d.tif", year, month)
    remote_vsi <- sprintf("/vsizip//vsicurl/%s/raw/%d/%d.zip/%d/%s", base_url, year, year, year, fname)
  }

  # Configure GDAL for HTTP streaming, multiplexing, large chunk prefetching, and multi-threaded decompression
  terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif,.zip")
  terra::setGDALconfig("VSI_CACHE", "TRUE")
  terra::setGDALconfig("GDAL_HTTP_MERGE_CONSECUTIVE_RANGES", "YES")
  terra::setGDALconfig("GDAL_HTTP_MULTIPLEX", "YES")
  terra::setGDALconfig("VSI_CACHE_SIZE", "268435456")
  terra::setGDALconfig("CPL_VSIL_CURL_CHUNK_SIZE", "1048576")
  terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")

  if (!quiet) {
    message(sprintf("Streaming and cropping %s %d%s from Cheng et al. archive...", country_tag, year, m_str))
  }

  r_remote <- tryCatch(
    terra::rast(remote_vsi),
    error = function(e) {
      stop("Failed to open remote raster at ", remote_vsi, ": ", e$message, call. = FALSE)
    }
  )

  e_crop <- terra::ext(ext_target[["xmin"]], ext_target[["xmax"]], ext_target[["ymin"]], ext_target[["ymax"]])
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  r_cropped <- terra::crop(r_remote, e_crop, filename = out_path, overwrite = TRUE)

  if (return_raster) {
    r_cropped
  } else {
    out_path
  }
}

#' Extract zonal nighttime light metrics and Gini inequality for polygons
#'
#' Mimics `blackmarblepy::bm_extract` by extracting pixel distributions and
#' computing zonal statistics (sum of lights, mean radiance, lit pixel proportion,
#' and Gini inequality) across polygon features in `roi` from Cheng et al. (2026) rasters.
#'
#' @param roi An `sf` polygon object (e.g. administrative boundaries, districts, buffers).
#' @param year Integer vector of years (1992-2024). Default `2024`.
#' @param month Optional integer vector of months (1-12) or `NULL` for annual composites.
#' @param stat Character vector of statistics to compute: `"sum"`, `"mean"`,
#'   `"gini"`, `"lit_share"`, `"pixels"`.
#' @param output_dir Directory where intermediate cropped rasters are cached.
#' @param drop_geom Logical; if `TRUE` (default), returns a standard `tibble` without geometry.
#' @param cores Number of CPU cores for parallel raster fetching across periods (Unix/macOS only).
#'   Default `1L`.
#' @param quiet Logical; whether to suppress progress messages. Default `!interactive()`.
#' @return A `tibble` or `sf` data frame with extracted metrics joined to the input polygons.
#' @seealso [ntl_raster()], [ntl_gini_monthly()]
#' @examples
#' \dontrun{
#' if (requireNamespace("sf", quietly = TRUE) && requireNamespace("terra", quietly = TRUE)) {
#'   # Extract monthly NTL metrics for custom study areas
#' }
#' }
#' @export
ntl_extract <- function(roi,
                        year = 2024L,
                        month = NULL,
                        stat = c("sum", "mean", "gini", "lit_share", "pixels"),
                        output_dir = tools::R_user_dir("ntlgini", which = "cache"),
                        drop_geom = TRUE,
                        cores = 1L,
                        quiet = !interactive()) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for ntl_extract.", call. = FALSE)
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required for ntl_extract.", call. = FALSE)
  }
  if (!inherits(roi, "sf")) {
    stop("roi must be an 'sf' object with polygon geometries.", call. = FALSE)
  }

  stat <- match.arg(stat, several.ok = TRUE)
  years <- as.integer(year)
  months <- if (is.null(month)) list(NULL) else as.list(as.integer(month))

  # Prepare base attributes
  roi_base <- roi
  if (drop_geom) {
    roi_base <- sf::st_drop_geometry(roi_base)
  }
  n_units <- nrow(roi_base)

  v_roi <- terra::vect(roi)

  # Collect all requested (year, month) combinations
  periods <- list()
  for (yr in years) {
    for (mo in months) {
      periods[[length(periods) + 1L]] <- list(year = yr, month = mo)
    }
  }

  n_periods <- length(periods)
  if (n_periods == 0L) {
    return(if (drop_geom) tibble::as_tibble(roi_base) else roi_base)
  }

  if (!quiet && n_periods > 1L) {
    message(sprintf("Processing %d raster period(s) for zonal extraction across %d features...", n_periods, n_units))
  }

  # Fetch or crop each layer (with optional multi-core parallelism across periods)
  r_list <- if (cores > 1L && .Platform$OS.type == "unix" && n_periods > 1L) {
    n_c <- min(as.integer(cores), n_periods)
    parallel::mclapply(seq_len(n_periods), function(k) {
      p <- periods[[k]]
      ntl_raster(
        roi = roi,
        year = p$year,
        month = p$month,
        output_dir = output_dir,
        return_raster = TRUE,
        quiet = TRUE
      )
    }, mc.cores = n_c)
  } else {
    lapply(seq_len(n_periods), function(k) {
      p <- periods[[k]]
      ntl_raster(
        roi = roi,
        year = p$year,
        month = p$month,
        output_dir = output_dir,
        return_raster = TRUE,
        quiet = quiet
      )
    })
  }

  # Multi-layer raster stacking:
  # Intersects polygons with raster geometry ONCE instead of n_periods times
  r_stack <- if (n_periods == 1L) r_list[[1L]] else terra::rast(r_list)

  ext_df <- terra::extract(r_stack, v_roi, ID = TRUE, raw = TRUE, touches = TRUE)

  n_extracted <- nrow(ext_df)
  layer_mat <- if (n_extracted > 0L) ext_df[, -1L, drop = FALSE] else matrix(numeric(0), nrow = 0, ncol = n_periods)

  # Ultra-fast contiguous range grouping: terra::extract guarantees ID is ordered 1..n_units
  unit_ranges <- vector("list", n_units)
  if (n_extracted > 0L) {
    id_col <- as.integer(ext_df[, 1L])
    rle_id <- rle(id_col)
    end_indices <- cumsum(rle_id$lengths)
    start_indices <- c(1L, end_indices[-length(end_indices)] + 1L)
    for (idx in seq_along(rle_id$values)) {
      u_id <- rle_id$values[idx]
      if (u_id >= 1L && u_id <= n_units) {
        unit_ranges[[u_id]] <- start_indices[idx]:end_indices[idx]
      }
    }
  }

  out_rows <- vector("list", n_periods)

  for (k in seq_len(n_periods)) {
    p <- periods[[k]]
    yr <- p$year
    mo <- p$month
    vals_k <- if (n_extracted > 0L) layer_mat[, k] else numeric(0)

    sum_vals <- numeric(n_units)
    mean_vals <- numeric(n_units)
    px_vals <- integer(n_units)
    share_vals <- numeric(n_units)
    gini_vals <- numeric(n_units)

    for (i in seq_len(n_units)) {
      rows_i <- unit_ranges[[i]]
      if (!is.null(rows_i)) {
        vals <- vals_k[rows_i]
        vals_ok <- vals[!is.na(vals)]
        n_p <- length(vals_ok)
        px_vals[i] <- n_p
        if (n_p > 0L) {
          lit_idx <- vals_ok > 0
          n_l <- sum(lit_idx)
          s_l <- sum(vals_ok[lit_idx])
          sum_vals[i] <- s_l
          mean_vals[i] <- s_l / n_p
          share_vals[i] <- n_l / n_p
          if ("gini" %in% stat) {
            gini_vals[i] <- .fast_gini(vals_ok)
          }
        } else {
          sum_vals[i] <- NA_real_
          mean_vals[i] <- NA_real_
          share_vals[i] <- NA_real_
          gini_vals[i] <- NA_real_
        }
      } else {
        sum_vals[i] <- NA_real_
        mean_vals[i] <- NA_real_
        share_vals[i] <- NA_real_
        gini_vals[i] <- NA_real_
      }
    }

    period_df <- roi_base
    period_df$year <- yr
    period_df$month <- if (is.null(mo)) NA_integer_ else mo
    period_df$date <- if (is.null(mo)) sprintf("%d", yr) else sprintf("%d-%02d-01", yr, mo)

    if ("sum" %in% stat) period_df$sum_of_lights <- sum_vals
    if ("mean" %in% stat) period_df$mean_radiance <- mean_vals
    if ("pixels" %in% stat) period_df$pixels <- px_vals
    if ("lit_share" %in% stat) period_df$lit_share <- share_vals
    if ("gini" %in% stat) period_df$gini <- gini_vals

    out_rows[[k]] <- period_df
  }

  res <- do.call(rbind, out_rows)
  if (drop_geom) {
    tibble::as_tibble(res)
  } else {
    res
  }
}
