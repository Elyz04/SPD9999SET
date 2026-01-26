
000000*****************************************************************
000000*--- テーブル名 ：DB_INTEREST_INFO
000000*--- 説明      ：預金種別ごとの利率マスタ
000000*--- 主キー    ：SAVING_TYPE
000000*---------------------------------------------------------------*
000000     CREATE TABLE MYDB.DB_INTEREST_INFO (
000000         SAVING_TYPE   CHAR(10)      NOT NULL,
000000         INTEREST_RATE DECIMAL(5,4)  NOT NULL,
000000         CONSTRAINT PK_INTEREST_INFO
000000             PRIMARY KEY (SAVING_TYPE)
000000     );
000000*---------------------------------------------------------------*
000000*--- 初期データ登録（利率マスタ）
000000*---------------------------------------------------------------*
000000     INSERT INTO MYDB.DB_INTEREST_INFO (SAVING_TYPE, INTEREST_RATE)
000000     SELECT           'NON-TERM', 0.0080 FROM SYSIBM.SYSDUMMY1
000000     UNION ALL SELECT 'FIXED-03', 0.0450 FROM SYSIBM.SYSDUMMY1
000000     UNION ALL SELECT 'FIXED-06', 0.0550 FROM SYSIBM.SYSDUMMY1
000000     UNION ALL SELECT 'FIXED-12', 0.0600 FROM SYSIBM.SYSDUMMY1;
000000*****************************************************************
