000000*****************************************************************
000000*--- 確認用 SELECT
000000*---------------------------------------------------------------*
000000     SELECT * FROM MYDB.DB_ACCOUNT_BALANCE;
000000     SELECT * FROM MYDB.DB_INTEREST_INFO;
000000     SELECT * FROM MYDB.DB_ACCOUNT_SAVINGS;
000000*---------------------------------------------------------------*
000000*--- テスト用 DELETE
000000*---------------------------------------------------------------*
000000     DELETE FROM MYDB.DB_ACCOUNT_BALANCE;
000000     DELETE FROM MYDB.DB_ACCOUNT_SAVINGS;
000000     DELETE FROM MYDB.DB_INTEREST_INFO;
000000*---------------------------------------------------------------*
000000*--- テスト用 DROP
000000*---------------------------------------------------------------*
000000     DROP TABLE MYDB.DB_ACCOUNT_SAVINGS;
000000     DROP TABLE MYDB.DB_INTEREST_INFO;
000000     DROP TABLE MYDB.DB_ACCOUNT_BALANCE;
000000*****************************************************************