-- MySQL

--
-- Table structure for table `balances`
--

DROP TABLE IF EXISTS `balances`;
CREATE TABLE `balances` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `date` date DEFAULT NULL,
  `acct_type` tinytext,
  `acct_name` tinytext,
  `amount` decimal(10,2) DEFAULT NULL,
  `reconciled` tinyint(1) DEFAULT NULL,
  `interest` decimal(10,2) DEFAULT NULL,
  `charges` decimal(10,2) DEFAULT NULL,
  `payments` decimal(10,2) DEFAULT NULL,
  `mindue` decimal(10,2) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Table structure for table `ledger`
--

DROP TABLE IF EXISTS `ledger`;
CREATE TABLE `ledger` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `date` date DEFAULT NULL,
  `amount` decimal(7,2) DEFAULT NULL,
  `dr_acct` varchar(50) DEFAULT NULL,
  `cr_acct` varchar(50) DEFAULT NULL,
  `payee` text,
  `description` text,
  `dr_seen` varchar(10) DEFAULT NULL,
  `cr_seen` varchar(10) DEFAULT NULL,
  `dr_deduct` varchar(50) DEFAULT NULL,
  `cr_deduct` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

--
-- Table structure for table `statements`
--

DROP TABLE IF EXISTS `statements`;
CREATE TABLE `statements` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `acct` varchar(50) DEFAULT NULL,
  `date` date DEFAULT NULL,
  `amount` decimal(7,2) DEFAULT NULL,
  `description` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;
