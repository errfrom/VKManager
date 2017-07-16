-- SQLite v3


-----------------------------------------------------------------------------
-- Отношение USERS, представляющее
-- пользователя 'ВКонтакте'.

CREATE TABLE USERS
(
     UID        INTEGER NOT NULL,
     FNAME      TEXT    NOT NULL,
     SNAME      TEXT    NOT NULL,
     GENGER     TEXT            ,
     DOB        INTEGER         ,
     MOB        INTEGER         ,
     YOB        INTEGER         ,
     COUNTRY    TEXT            ,
     CITY       TEXT            ,
     PHONE      TEXT            ,

  PRIMARY KEY (UID)
);
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Отношение, описывающее связь многие ко многим между
-- отношениями USERS и UNIVERSITIES. (Связующая таблица)

CREATE TABLE USERS_UNIVERS
(
     UID   INTEGER NOT NULL, -- ID пользователя
     UN_ID INTEGER NOT NULL, -- ID университета

  FOREIGN KEY (UID)
    REFERENCES USERS
      ON DELETE SET NULL

  FOREIGN KEY (UN_ID)
    REFERENCES UNIVERSITIES
      ON DELETE SET NULL
);

-- Примечание: такое отношение следует использовать
-- для примерного получения места проживания пользователя
-- в случае недоступности получения значения прямым способом.
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Связующее отношение между USERS и SCHOOLS

CREATE TABLE USERS_SCHOOLS
(
    UID    INTEGER NOT NULL,
    SCH_ID INTEGER NOT NULL,

  FOREIGN KEY (UID)
    REFERENCES USERS
      ON DELETE SET NULL

  FOREIGN KEY (SCH_ID)
    REFERENCES SCHOOLS
      ON DELETE SET NULL
);
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------

CREATE TABLE USERS_SNS
(
    UID      INTEGER NOT NULL,
    SN_TITLE INTEGER NOT NULL,
    LINK     TEXT    NOT NULL,

  FOREIGN KEY (UID)
    REFERENCES USERS
      ON DELETE SET NULL
);
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Статическое отношение, содержащее информацию об
-- определенном высшем учебном заведении.

CREATE TABLE UNIVERSITIES
(
    UN_ID   INTEGER NOT NULL,
    TITLE   TEXT    NOT NULL,
  PRIMARY KEY (UN_ID)
);
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- Статическое отношение, содержащее информацию об
-- определенном учреждении среднего образования.

CREATE TABLE SCHOOLS
(
    SCH_ID  INTEGER NOT NULL,
    TITLE   TEXT    NOT NULL,
  PRIMARY KEY (SCH_ID)
);
-----------------------------------------------------------------------------
