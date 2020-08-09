CREATE TABLE balances (
  id integer NOT NULL,
  date date DEFAULT NULL,
  acct_type tinytext,
  acct_name tinytext,
  amount decimal(10,2) DEFAULT NULL,
  a_flow_in decimal(10,2) DEFAULT NULL,
  a_flow_out decimal(10,2) DEFAULT NULL,
  l_int_paid decimal(10,2) DEFAULT NULL,
  l_charges decimal(10,2) DEFAULT NULL,
  l_payments decimal(10,2) DEFAULT NULL,
  l_mindue decimal(10,2) DEFAULT NULL,
  isstmt tinyint(1) DEFAULT NULL,
  reconciled tinyint(1) DEFAULT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE ledger (
  id integer NOT NULL,
  date date DEFAULT NULL,
  amount decimal(7,2) DEFAULT NULL,
  dr_acct varchar(50) DEFAULT NULL,
  cr_acct varchar(50) DEFAULT NULL,
  payee text,
  description text,
  dr_seen varchar(10) DEFAULT NULL,
  cr_seen varchar(10) DEFAULT NULL,
  dr_deduct varchar(50) DEFAULT NULL,
  cr_deduct varchar(50) DEFAULT NULL,
  PRIMARY KEY (id)
);
