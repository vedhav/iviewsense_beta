-- --------------------------------------------------------
-- Host:                         127.0.0.1
-- Server version:               5.7.23 - MySQL Community Server (GPL)
-- Server OS:                    Win64
-- HeidiSQL Version:             10.1.0.5464
-- --------------------------------------------------------

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET NAMES utf8 */;
/*!50503 SET NAMES utf8mb4 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;

-- Dumping structure for table rane.defects
CREATE TABLE IF NOT EXISTS `defects` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Machine` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Date_Time` varchar(100) COLLATE utf8_bin NOT NULL,
  `Family` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Cust` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Model` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Stn` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Opr` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `defects_category` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `defects_qty` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

-- Dumping data for table rane.defects: 0 rows
/*!40000 ALTER TABLE `defects` DISABLE KEYS */;
/*!40000 ALTER TABLE `defects` ENABLE KEYS */;

-- Dumping structure for table rane.testresults
CREATE TABLE IF NOT EXISTS `testresults` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `Machine` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Date_Time` varchar(100) COLLATE utf8_bin NOT NULL,
  `Family` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Cust` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Model` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Gear_No` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Stn` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Opr` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Result` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `NPD` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Flow` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Tempr` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Leak_CW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Leak_CCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Leak_Pr_CW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Leak_Pr_CCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Eff_CW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Eff_CCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `TT_CW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `TT_CCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `TT_Diff` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Wr_Slv_Pr` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Blk_Psge_Pr` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `TB_1` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `TB_2` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Hys_CW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Hys_CCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `Direction` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `PCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `PCCW` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_1` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_1` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_2` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_2` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_3` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_3` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_4` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_4` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_5` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_5` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_6` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_6` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_7` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_7` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_8` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_8` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_9` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_9` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CW_10` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  `CCW_10` varchar(45) COLLATE utf8_bin DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin;

-- Dumping data for table rane.testresults: 0 rows
/*!40000 ALTER TABLE `testresults` DISABLE KEYS */;
/*!40000 ALTER TABLE `testresults` ENABLE KEYS */;

/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '') */;
/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
