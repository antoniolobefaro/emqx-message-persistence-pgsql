
CREATE TABLE messages (
  id SERIAL primary key,
  ts timestamp,
  t1 character varying(50),
  t2 character varying(50),
  t3 character varying(50),
  t4 character varying(50),
  t5 character varying(50),
  t6 character varying(50),
  t7 character varying(50),
  t8 character varying(50),
  t9 character varying(50),
  payload character varying(200),
  da character varying(200),
  flag character varying(200),
  topic character varying(200),
  qos integer,
  val float
);
