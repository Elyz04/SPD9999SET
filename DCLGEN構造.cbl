000000*****************************************************************
000000*--- DCLGEN TABLE(MYDB.DB_ACCOUNT_BALANCE)                          
000000*---        LIBRARY(XXXXXXXXXX)                                     
000000*---        ACTION(REPLACE)                                         
000000*---        LANGUAGE(COBOL)                                         
000000*---        QUOTE                                               
000000*---        DBCSDELIM(NO)                                           
000000*--- ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   
000000*---------------------------------------------------------------*
000000     EXEC SQL 
000000         DECLARE MYDB.DB_ACCOUNT_BALANCE TABLE               
000000         ( 
000000           ACC_ID                INTEGER         NOT NULL,           
000000           BALANCE               DECIMAL(15, 2)  NOT NULL     
000000         ) 
000000     END-EXEC.                                                  
000000*---------------------------------------------------------------*
000000*--- COBOL DECLARATION FOR TABLE MYDB.DB_ACCOUNT_BALANCE            
000000*--- アカウント残高情報テーブル                                       
000000*--- ACC_ID     : アカウントID (主キー)                         
000000*--- BALANCE    : 現在の残高                                          
000000*---------------------------------------------------------------*
000000 01 DCLDB-ACCOUNT-BALANCE.                                       
000000    03 AB-ACC-ID                 PIC S9(9)       USAGE COMP.             
000000    03 AB-BALANCE                PIC S9(13)V9(2) USAGE COMP-3.    
000000*---------------------------------------------------------------*
000000*--- THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       
000000*---------------------------------------------------------------*
000000*--- DCLGEN TABLE(MYDB.DB_ACCOUNT_SAVINGS)                          
000000*---        LIBRARY(XXXXXXXXXX)                                     
000000*---        ACTION(REPLACE)                                     
000000*---        LANGUAGE(COBOL)                                         
000000*---        QUOTE                                                   
000000*---        DBCSDELIM(NO)                                           
000000*--- ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   
000000*---------------------------------------------------------------*
000000     EXEC SQL 
000000         DECLARE MYDB.DB_ACCOUNT_SAVINGS TABLE               
000000         ( 
000000           ORDER_ID              INTEGER         NOT NULL,           
000000           ACC_ID                INTEGER         NOT NULL,           
000000           SAVING_TYPE           CHAR(10)        NOT NULL,          
000000           START_DATE            CHAR(8)         NOT NULL,           
000000           END_DATE              CHAR(8)         NOT NULL,           
000000           MONEY_ROOT            DECIMAL(15, 2)  NOT NULL,    
000000           INTEREST              DECIMAL(15, 2)  NOT NULL,    
000000           MONEY                 DECIMAL(15, 2)  NOT NULL,    
000000           STATUS                CHAR(1)         NOT NULL            
000000         ) 
000000     END-EXEC.                                                  
000000*---------------------------------------------------------------*
000000*--- COBOL DECLARATION FOR TABLE MYDB.DB_ACCOUNT_SAVINGS            
000000*--- 定期預金情報テーブル                                             
000000*--- ORDER_ID     : 注文ID (主キー)                                  
000000*--- ACC_ID       : アカウントID                                     
000000*--- SAVING_TYPE  : 預金タイプ                                      
000000*--- START_DATE   : 預金開始日                                      
000000*--- END_DATE     : 預金終了日                                       
000000*--- MONEY_ROOT   : 元本                                             
000000*--- INTEREST     : 利息                                            
000000*--- MONEY        : 合計金額 (元本+利息)                              
000000*--- STATUS       : 状態 (例: 'A'=有効, 'C'=解約)                     
000000*---------------------------------------------------------------*
000000 01 DCLDB-ACCOUNT-SAVINGS.                                       
000000    03 AS-ORDER-ID               PIC S9(9)       USAGE COMP.             
000000    03 AS-ACC-ID                 PIC S9(9)       USAGE COMP.      
000000    03 AS-SAVING-TYPE            PIC X(10).                      
000000    03 AS-START-DATE             PIC X(8).                         
000000    03 AS-END-DATE               PIC X(8).                         
000000    03 AS-MONEY-ROOT             PIC S9(13)V9(2) USAGE COMP-3.    
000000    03 AS-INTEREST               PIC S9(13)V9(2) USAGE COMP-3.    
000000    03 AS-MONEY                  PIC S9(13)V9(2) USAGE COMP-3.    
000000    03 AS-STATUS                 PIC X(1).                         
000000*---------------------------------------------------------------*
000000*--- THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       
000000*---------------------------------------------------------------*
000000*--- DCLGEN TABLE(MYDB.DB_INTEREST_INFO)                            
000000*---        LIBRARY(XXXXXXXXXX)                                     
000000*---        ACTION(REPLACE)                                         
000000*---        LANGUAGE(COBOL)                                         
000000*---        QUOTE                                                   
000000*---        DBCSDELIM(NO)                                           
000000*--- ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   
000000*---------------------------------------------------------------*
000000     EXEC SQL 
000000         DECLARE MYDB.DB_INTEREST_INFO TABLE                 
000000         ( 
000000           SAVING_TYPE           CHAR(10)        NOT NULL,          
000000           INTEREST_RATE         DECIMAL(5, 4)   NOT NULL      
000000         ) 
000000     END-EXEC.                                                  
000000*---------------------------------------------------------------*
000000*--- COBOL DECLARATION FOR TABLE MYDB.DB_INTEREST_INFO              
000000*--- 預金利率情報テーブル                                             
000000*--- SAVING_TYPE    : 預金タイプ                                     
000000*--- INTEREST_RATE  : 利率 (例: 0.0250 = 2.5%)                       
000000*---------------------------------------------------------------*
000000 01 DCLDB-INTEREST-INFO.                                         
000000    03 II-SAVING-TYPE            PIC X(10).                        
000000    03 II-INTEREST-RATE          PIC S9(1)V9(4)  USAGE COMP-3.      
000000*---------------------------------------------------------------*
000000*--- THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       
000000*****************************************************************