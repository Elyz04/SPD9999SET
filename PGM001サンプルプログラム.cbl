000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.                            
000000 PROGRAM-ID.                     PGM001.  
000000*/-------------------------------------------------------------/*     
000000*    PROGRAM-ID     :            PGM001                               
000000*    CREATE DATE    :            2026/01/07                              
000000*    AUTHOR         :            Elyz                      
000000*    PURPOSE        :            利息計算および満期決済処理
000000*/-------------------------------------------------------------/*   
000000*    UPDATE         :                                           
000000*        2026/01/08 : FUN_001 : 利息計算（プレビューのみ、DB更新なし）      
000000*        2026/01/09 : SQLエラー時はABEND処理                     
000000*        2026/01/10 : 対象はDB_ACCOUNT_SAVINGSテーブル            
000000*        2026/01/11 : FUN_002 : 満期決済および残高更新         
000000*        2026/01/13 : 処理ロジックの最終調整（INTEREST と MONEY 更新）
000000*        2026/01/15 : プログラムを再構築し、異常終了の詳細を追加し
000000*                     ます。エラーを明確に説明します。
000000*        2026/01/18 : 送信入力ACCIDを更新、1アカウントの決済のみ処理、
000000*                     送信入力検証パラメータを追加
000000*        2026/01/20 : ACC_ID が存在しないことを検証するケースの再構
000000*                     築と修正
000000*/-------------------------------------------------------------/*
000000 ENVIRONMENT                     DIVISION.         
000000 DATA                            DIVISION.                                
000000 WORKING-STORAGE                 SECTION.  
000000*/-------------------------------------------------------------/*         
000000*  ワークエリア                                                           
000000*/-------------------------------------------------------------/*     
000000*--- SQLCA および DCLGEN インクルード部分    
000000     EXEC SQL                                            
000000         INCLUDE                 SQLCA                                   
000000     END-EXEC.
000000*--- DCLGEN参照：MYDB.DB_INTEREST_INFO                                    
000000     EXEC SQL                                            
000000         INCLUDE                 INTINFO                                 
000000     END-EXEC.
000000*--- DCLGEN参照：MYDB.DB_ACCOUNT_SAVINGS                                  
000000     EXEC SQL                                            
000000         INCLUDE                 ACCSAV                                  
000000     END-EXEC.
000000*--- DCLGEN参照：MYDB.DB_ACCOUNT_BALANCE                                  
000000     EXEC SQL                                            
000000         INCLUDE                 ACCBAL                                  
000000     END-EXEC.
000000*       
000000*/-------------------------------------------------------------/*
000000*  ワークエリア                                                   
000000*/-------------------------------------------------------------/*
000000 01 WS-VARIABLES.                                               
000000    03 WS-DAYS-ACTUAL            PIC 9(05).                    
000000    03 WS-DAYS-TERM              PIC 9(05).                    
000000    03 WS-AMOUNT-INTEREST        PIC S9(13)V99    COMP-3.         
000000    03 WS-AMOUNT-TOTAL           PIC S9(13)V99    COMP-3.  
000000    03 WS-RATE-INTEREST          PIC S9(01)V9(04) COMP-3.        
000000    03 WS-RATE-NONTERM           PIC S9(01)V9(04) COMP-3.            
000000    03 WS-PARAM-FUNC             PIC X(01).             
000000    03 WS-PARAM-ACCID-CHAR       PIC X(09).   
000000    03 WS-PARAM-ACCID-DISP       PIC 9(09). 
000000    03 WS-PARAM-ACCID            PIC S9(09)       COMP. 
000000*/-------------------------------------------------------------/*
000000*  ホスト変数                                                    
000000*/-------------------------------------------------------------/*     
000000 01 HV-VARIABLES.                                       
000000    03 HV-DAYS-CURRENT-COMP      PIC S9(09) COMP.               
000000    03 HV-DAYS-START-COMP        PIC S9(09) COMP.               
000000    03 HV-DAYS-END-COMP          PIC S9(09) COMP.                 
000000    03 HV-DATE-START-9           PIC 9(08).                    
000000    03 HV-DATE-END-9             PIC 9(08).                             
000000    03 HV-DATE-CURRENT-9         PIC 9(08). 
000000    03 HV-ACC-STATUS             PIC X(01).
000000    03 HV-ACTIVE-SAVING-CNT      PIC S9(09) COMP.
000000    03 HV-TOTAL-SAVING-CNT       PIC S9(09) COMP.                  
000000*/-------------------------------------------------------------/*
000000*  定数定義                                                      
000000*/-------------------------------------------------------------/*     
000000 01 CST-VARIABLES.
000000    03 CST-START-PGM-MSG         PIC X(12)  VALUE 'START PGM001'.
000000    03 CST-STOP-PGM-MSG          PIC X(11)  VALUE 'STOP PGM001'.
000000    03 CST-DONE-FNC001-MSG       PIC X(13)  VALUE 'DONE FUNC_001'.
000000    03 CST-DONE-FNC002-MSG       PIC X(13)  VALUE 'DONE FUNC_002'.
000000    03 CST-STATUS-1              PIC X(01)  VALUE '1'.          
000000    03 CST-STATUS-9              PIC X(01)  VALUE '9'.          
000000    03 CST-EOF-CRS1              PIC X(01)  VALUE 'N'. 
000000    03 CST-EOF-CRS2              PIC X(01)  VALUE 'N'.  
000000    03 CST-NON-TERM              PIC X(10)  VALUE 'NON-TERM'.
000000    03 CST-FIXED-03              PIC X(10)  VALUE 'FIXED-03'.
000000    03 CST-FIXED-06              PIC X(10)  VALUE 'FIXED-06'.
000000    03 CST-FIXED-12              PIC X(10)  VALUE 'FIXED-12'.        
000000    03 CST-FIXED-VALUE-03        PIC 9(03)  VALUE 90.  
000000    03 CST-FIXED-VALUE-06        PIC 9(03)  VALUE 180.      
000000    03 CST-FIXED-VALUE-12        PIC 9(03)  VALUE 365.
000000    03 CST-COUNT-FUNC001         PIC 9(05)  VALUE 0.
000000    03 CST-COUNT-UPD-BALANCE     PIC 9(05)  VALUE 0.
000000    03 CST-COUNT-UPD-STATUS      PIC 9(05)  VALUE 0.  
000000    03 CST-COUNT-FUNC002         PIC 9(05)  VALUE 0.
000000    03 CST-ACCID-FLAG            PIC X(01)  VALUE 'N'.
000000    03 CST-PARAM-1               PIC X(01)  VALUE '1'.
000000    03 CST-PARAM-2               PIC X(01)  VALUE '2'.
000000    03 CST-PARAM-3               PIC X(01)  VALUE '3'.
000000*--- DEBUG / ABEND 処理  
000000    03 CST-ABEND-BREAKPOINT      PIC X(100) VALUE SPACES.
000000    03 CST-ABEND-DETAIL          PIC X(100) VALUE SPACES.  
000000    03 CST-DEBUG-MODE            PIC X(01)  VALUE 'N'.
000000*/-------------------------------------------------------------/*
000000*  JCL パラメータ受け取りエリア                                                     
000000*/-------------------------------------------------------------/* 
000000 LINKAGE                         SECTION.
000000 01 LNK-PARAM-JCL.
000000    03 LNK-PARAM-LENGHT          PIC S9(04) COMP.
000000    03 LNK-PARAM-DATA            PIC X(11).   
000000*===============================================================*         
000000*====        ＰＲＯＣＥＤＵＲＥ　　 　　ＤＩＶＩＳＩＯＮ        ====*         
000000*===============================================================*       
000000 PROCEDURE                       DIVISION USING LNK-PARAM-JCL.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: メイン処理                       
000000* MAIN                   SECTION |      （MAIN）                           
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000 MAIN.   
000000*
000000     PERFORM                     INIT-VARIABLE.
000000     PERFORM                     HANDLE-JCL-PARAM.
000000     DISPLAY                     CST-START-PGM-MSG.
000000*--- デバッグモードが有効な場合のみ、詳細情報を表示する
000000     IF CST-DEBUG-MODE = 'Y'
000000         PERFORM                 DISPLAY-TOTAL
000000     END-IF.
000000*
000000     PERFORM                     COMMIT-DATA.
000000*
000000     DISPLAY                     CST-STOP-PGM-MSG.
000000*
000000     STOP RUN.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 変数初期化                       
000000* INITIALIZE             SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 INIT-VARIABLE.
000000*                                              
000000     INITIALIZE                  WS-VARIABLES.                           
000000     INITIALIZE                  HV-VARIABLES.                          
000000*
000000     EXIT. 
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: JCLパラメータ処理                       
000000* HANDLE-JCL-PARAM       SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000 HANDLE-JCL-PARAM.
000000*
000000     IF LNK-PARAM-LENGHT = 0
000000     OR LNK-PARAM-LENGHT > 11
000000         DISPLAY 'INVALID JCL PARAM LENGTH'
000000         STOP RUN
000000     END-IF.
000000*
000000     IF CST-DEBUG-MODE = 'Y'
000000         DISPLAY 'LNK-PARAM-LENGHT : ' LNK-PARAM-LENGHT
000000         DISPLAY 'LNK-PARAM-DATA   : ' LNK-PARAM-DATA
000000     END-IF.
000000*--- (X,YYYYYYYYY)
000000     UNSTRING LNK-PARAM-DATA     
000000         DELIMITED BY ','        
000000         INTO WS-PARAM-FUNC      
000000              WS-PARAM-ACCID-CHAR
000000     END-UNSTRING.
000000*
000000     IF WS-PARAM-ACCID-CHAR = SPACES
000000     OR WS-PARAM-ACCID-CHAR = LOW-VALUES                      
000000         DISPLAY 'ACCOUNT ID PARAM IS REQUIRED'
000000         STOP RUN                        
000000     ELSE                                                 
000000         MOVE 'Y'                TO      CST-ACCID-FLAG               
000000         MOVE WS-PARAM-ACCID-CHAR
000000                                 TO 
000000              WS-PARAM-ACCID-DISP  
000000         MOVE WS-PARAM-ACCID-DISP
000000                                 TO 
000000              WS-PARAM-ACCID       
000000     END-IF.                                              
000000*
000000     PERFORM VALIDATE-JCL-PARAM.
000000*---
000000     IF CST-ACCID-FLAG = 'Y'
000000         PERFORM CHECK-ACCID-STATUS
000000     END-IF. 
000000*---                             
000000     EVALUATE WS-PARAM-FUNC
000000         WHEN CST-PARAM-1
000000             DISPLAY 'START FUN_001 : CALCULATE_INTEREST'
000000             PERFORM             FUNCTION-001
000000             DISPLAY CST-DONE-FNC001-MSG
000000         WHEN CST-PARAM-2
000000             DISPLAY 'START FUN_002 : SETTLEMENT'
000000             PERFORM             FUNCTION-002
000000             DISPLAY CST-DONE-FNC002-MSG
000000         WHEN CST-PARAM-3
000000             DISPLAY 'START FUN_001 : CALCULATE_INTEREST'
000000             PERFORM             FUNCTION-001
000000             DISPLAY CST-DONE-FNC001-MSG
000000             DISPLAY 'START FUN_002 : SETTLEMENT'
000000             PERFORM             FUNCTION-002
000000             DISPLAY CST-DONE-FNC002-MSG
000000     END-EVALUATE. 
000000*---
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 呼び出し処理モジュール                    
000000* VALIDATE-JCL-PARAM     SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 VALIDATE-JCL-PARAM.
000000*
000000     PERFORM VALIDATE-FUNC-PARAM
000000     PERFORM VALIDATE-ACCID-PARAM
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: チェック機能値                         
000000* VALIDATE-FUNC-PARAM    SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 VALIDATE-FUNC-PARAM.
000000*--- DEFAULT FUNCTION
000000     IF WS-PARAM-FUNC = SPACES
000000         MOVE CST-PARAM-3        TO      WS-PARAM-FUNC
000000     END-IF.
000000*--- CHECK VALUE
000000     IF  WS-PARAM-FUNC NOT = CST-PARAM-1
000000     AND WS-PARAM-FUNC NOT = CST-PARAM-2
000000     AND WS-PARAM-FUNC NOT = CST-PARAM-3
000000         DISPLAY 'FUNCTION PARAM IS INVALID : '
000000                 WS-PARAM-FUNC
000000         STOP RUN
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: ACC_ID 値を確認してください               
000000* VALIDATE-ACCID-PARAM   SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 VALIDATE-ACCID-PARAM.
000000*--- ACC_ID FORMAT CHECK
000000     IF WS-PARAM-ACCID-CHAR(1:9) IS NOT NUMERIC    
000000         DISPLAY 'ACCOUNT ID PARAM IS NOT NUMERIC : '  
000000                 WS-PARAM-ACCID-CHAR               
000000         STOP RUN                                  
000000     END-IF.                                       
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: ACC_IDを確認してください                  
000000* CHECK-ACCID-STATUS     SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000 CHECK-ACCID-STATUS.
000000*
000000     EXEC SQL
000000         SELECT COUNT(*)
000000         INTO   :HV-TOTAL-SAVING-CNT
000000         FROM   MYDB.DB_ACCOUNT_SAVINGS
000000         WHERE  ACC_ID = :WS-PARAM-ACCID
000000     END-EXEC.
000000     IF SQLCODE NOT = 0
000000         MOVE 'CHECK-ACCID-STATUS'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT ACC_ID EXIST FAILED'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM ABEND-PROGRAM
000000     END-IF.
000000     IF HV-TOTAL-SAVING-CNT = 0
000000         DISPLAY 'ACC_ID '
000000                 WS-PARAM-ACCID
000000                 ' NOT FOUND'
000000         STOP RUN
000000     END-IF.
000000     EXEC SQL
000000         SELECT COUNT(*)
000000         INTO   :HV-ACTIVE-SAVING-CNT
000000         FROM   MYDB.DB_ACCOUNT_SAVINGS
000000         WHERE  ACC_ID = :WS-PARAM-ACCID
000000         AND    STATUS = :CST-STATUS-1
000000     END-EXEC.
000000     IF SQLCODE NOT = 0
000000         MOVE 'CHECK-ACCID-STATUS'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT ACTIVE SAVING FAILED'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM ABEND-PROGRAM
000000     END-IF.
000000     IF HV-ACTIVE-SAVING-CNT = 0
000000         DISPLAY 'ACC_ID '
000000                 WS-PARAM-ACCID
000000                 ' HAS BEEN FULLY SETTLED'
000000         STOP RUN
000000     END-IF.
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息計算                         
000000* FUNCTION-001           SECTION |      （FUN_001)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 FUNCTION-001.                                                          
000000*    
000000     MOVE 'N'                    TO     CST-EOF-CRS1.
000000* 
000000     EXEC SQL                                             
000000         DECLARE CRS1 CURSOR FOR                          
000000         SELECT  ORDER_ID,                                 
000000                 ACC_ID,                                   
000000                 SAVING_TYPE,                              
000000                 START_DATE,                                            
000000                 MONEY_ROOT                             
000000         FROM    MYDB.DB_ACCOUNT_SAVINGS                     
000000         WHERE   STATUS = :CST-STATUS-1
000000         AND     ACC_ID = :WS-PARAM-ACCID
000000     END-EXEC. 
000000*--- OPEN CURSOR1                             
000000     EXEC SQL                                                
000000         OPEN CRS1                                           
000000     END-EXEC.
000000*---                                      
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'FUNCTION-001'     TO     CST-ABEND-BREAKPOINT 
000000         MOVE 'OPEN CSR 1 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL             
000000         PERFORM ABEND-PROGRAM                            
000000     END-IF.                                                 
000000     PERFORM UNTIL CST-EOF-CRS1 = 'Y'                            
000000         PERFORM FETCH-AND-CALCULATE                                  
000000     END-PERFORM.                                            
000000*--- CLOSE CURSOR1                                                        
000000     EXEC SQL                                                
000000         CLOSE CRS1                                          
000000     END-EXEC.
000000*---  
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'FUNCTION-001'     TO     CST-ABEND-BREAKPOINT
000000         MOVE 'CLOSE CSR 1 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL              
000000         PERFORM ABEND-PROGRAM                            
000000     END-IF.     
000000     EXIT.                                                   
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 決済処理                         
000000* FUNCTION-002           SECTION |      （FUN_002)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*         
000000 FUNCTION-002.
000000*                                                            
000000     MOVE 'N'                    TO      CST-EOF-CRS2.
000000*                               
000000     EXEC SQL                                            
000000         DECLARE CRS2 CURSOR FOR
000000         SELECT  ORDER_ID,
000000                 ACC_ID,
000000                 SAVING_TYPE,
000000                 START_DATE,
000000                 END_DATE,
000000                 MONEY_ROOT
000000         FROM    MYDB.DB_ACCOUNT_SAVINGS  
000000         WHERE   STATUS = :CST-STATUS-1
000000         AND     ACC_ID = :WS-PARAM-ACCID              
000000     END-EXEC.                                            
000000*--- OPEN-CURSOR-2                                                
000000     EXEC SQL                                             
000000         OPEN CRS2                                        
000000     END-EXEC.
000000*---                                                               
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE
000000         MOVE 'FUNCTION-002'     TO      CST-ABEND-BREAKPOINT
000000         MOVE 'OPEN CSR 2 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL
000000         PERFORM ABEND-PROGRAM                          
000000     END-IF.                                              
000000*                                                         
000000     PERFORM UNTIL CST-EOF-CRS2 = 'Y'                         
000000         PERFORM FETCH-SAV-SETTLEMENT                               
000000     END-PERFORM.                                         
000000*--- CLOSE CURSOR2                                                    
000000     EXEC SQL                                             
000000         CLOSE CRS2                                       
000000     END-EXEC.                                            
000000*---      
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'FUNCTION-002'     TO     CST-ABEND-BREAKPOINT
000000         MOVE 'CLOSE CSR 2 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL              
000000         PERFORM ABEND-PROGRAM                            
000000     END-IF. 
000000     EXIT. 
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データ取得・計算                 
000000* FETCH-AND-CALCULATE    SECTION |      （FUN_001）
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 FETCH-AND-CALCULATE.
000000*                                                 
000000     EXEC SQL                                                    
000000         FETCH CRS1                                            
000000         INTO  :AS-ORDER-ID,                                    
000000               :AS-ACC-ID,                                      
000000               :AS-SAVING-TYPE,                                 
000000               :AS-START-DATE,                                  
000000               :AS-MONEY-ROOT                      
000000     END-EXEC.                                                 
000000*                                                              
000000     EVALUATE SQLCODE                                          
000000         WHEN 100
000000             MOVE 'Y'            TO            CST-EOF-CRS1             
000000         WHEN 0                                           
000000             PERFORM GET-CURR-DATE-FUN001
000000             PERFORM EXEC-GET-INTEREST-RATE                             
000000             PERFORM CALCULATE-FUN001                     
000000*--- デバッグモードが有効な場合のみ、詳細情報を表示する
000000             IF CST-DEBUG-MODE = 'Y'
000000                 PERFORM DISPLAY-DETAIL-FUN001
000000             END-IF 
000000             ADD 1               TO            CST-COUNT-FUNC001
000000         WHEN OTHER                                              
000000             MOVE 'FETCH-AND-CALCULATE' 
000000                                 TO 
000000                  CST-ABEND-BREAKPOINT
000000             MOVE 'FETCH CSR1 FAILED'     
000000                                 TO     
000000                  CST-ABEND-DETAIL        
000000             PERFORM ABEND-PROGRAM                              
000000     END-EVALUATE.
000000*                                                         
000000     EXIT.                                                     
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 現在日付取得                    
000000* GET-CURR-DATE-FUN001   SECTION |      （FUN_001）                       
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 GET-CURR-DATE-FUN001.
000000*
000000     MOVE FUNCTION CURRENT-DATE(1:8)
000000         TO HV-DATE-CURRENT-9.
000000*--- CONVERT CURRENT DATE TO INTEGER DAYS
000000     COMPUTE HV-DAYS-CURRENT-COMP =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-CURRENT-9).
000000*--- CONVERT START DATE TO INTEGER
000000     COMPUTE HV-DATE-START-9      =
000000         FUNCTION NUMVAL(AS-START-DATE).
000000     COMPUTE HV-DAYS-START-COMP   =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-START-9).
000000*--- CALCULATE ACTUAL DAYS FROM START DATE
000000     COMPUTE WS-DAYS-ACTUAL       =
000000             HV-DAYS-CURRENT-COMP - 
000000             HV-DAYS-START-COMP.
000000*--- PREVENT NEGATIVE DAYS
000000     IF WS-DAYS-ACTUAL < 0
000000         MOVE 0                  TO WS-DAYS-ACTUAL
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 現在日付および期間日数取得                    
000000* GET-CURR-DATE-FUN002   SECTION |      （FUN_002）                        
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 GET-CURR-DATE-FUN002.
000000*
000000     MOVE FUNCTION CURRENT-DATE(1:8)
000000         TO HV-DATE-CURRENT-9.
000000*--- CONVERT CURRENT DATE TO INTEGER DAYS
000000     COMPUTE HV-DAYS-CURRENT-COMP =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-CURRENT-9).
000000*--- CONVERT START DATE TO INTEGER
000000     COMPUTE HV-DATE-START-9      =
000000         FUNCTION NUMVAL(AS-START-DATE).
000000     COMPUTE HV-DAYS-START-COMP   =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-START-9).
000000*--- CONVERT END DATE TO INTEGER
000000     COMPUTE HV-DATE-END-9        =
000000         FUNCTION NUMVAL(AS-END-DATE).
000000     COMPUTE HV-DAYS-END-COMP     =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-END-9).
000000*--- CALCULATE ACTUAL DAYS FROM START DATE
000000     COMPUTE WS-DAYS-ACTUAL       =
000000             HV-DAYS-CURRENT-COMP - 
000000             HV-DAYS-START-COMP.
000000*--- PREVENT NEGATIVE DAYS
000000     IF WS-DAYS-ACTUAL < 0
000000         MOVE 0                  TO WS-DAYS-ACTUAL
000000     END-IF.
000000*
000000     EXIT.                                             
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利率取得                         
000000* EXEC-GET-INTEREST-RATE SECTION |      （COMMON）                         
000000*                                |                                      
000000*/-------------------------------------------------------------/*      
000000 EXEC-GET-INTEREST-RATE.
000000*                                                 
000000     EXEC SQL                                                       
000000         SELECT INTEREST_RATE                                       
000000         INTO   :WS-RATE-INTEREST                                     
000000         FROM   MYDB.DB_INTEREST_INFO                            
000000         WHERE  SAVING_TYPE = :AS-SAVING-TYPE                   
000000     END-EXEC.
000000*                                                      
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'EXEC-GET-INTEREST-RATE' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT INTEREST_RATE INTO :WS-RATE-INTEREST FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL       
000000         PERFORM ABEND-PROGRAM                               
000000     END-IF.
000000*                                                        
000000     EXIT.                                                     
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 非定期利率取得                   
000000* EXEC-GET-NONTERM-RATE  SECTION |       (FUN_002)                      
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 EXEC-GET-NONTERM-RATE.
000000*                                             
000000     EXEC SQL                                                  
000000         SELECT  INTEREST_RATE                                  
000000         INTO    :WS-RATE-NONTERM                                 
000000         FROM    MYDB.DB_INTEREST_INFO                            
000000         WHERE   SAVING_TYPE = :CST-NON-TERM                     
000000     END-EXEC.
000000*                                                        
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'EXEC-GET-NONTERM-RATE' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT INTEREST_RATE INTO :WS-RATE-NON-TERM FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL                                        
000000         PERFORM ABEND-PROGRAM                              
000000     END-IF.
000000*                                                          
000000     EXIT.   
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: 利息計算ロジック
000000* CALCULATE-FUN001       SECTION |      （FUN_001)
000000*                                |
000000*/-------------------------------------------------------------/*
000000 CALCULATE-FUN001.
000000     IF AS-SAVING-TYPE = CST-NON-TERM
000000* 非定期預金の利息計算（実日数ベース）
000000* INTEREST = MONEY_ROOT × INTEREST_RATE × ACTUAL_DAYS / 365
000000         COMPUTE WS-AMOUNT-INTEREST =
000000                 AS-MONEY-ROOT    *
000000                 WS-RATE-INTEREST *
000000                 WS-DAYS-ACTUAL   / 
000000                 CST-FIXED-VALUE-12
000000     ELSE
000000* 定期預金の利息計算（プレビュー）
000000* ※ 満期／中途解約の判定はFUN_002で実施
000000* INTEREST = MONEY_ROOT × INTEREST_RATE × ACTUAL_DAYS / 365
000000         COMPUTE WS-AMOUNT-INTEREST =
000000                 AS-MONEY-ROOT    *
000000                 WS-RATE-INTEREST *
000000                 WS-DAYS-ACTUAL   / 
000000                 CST-FIXED-VALUE-12
000000     END-IF.
000000*--- MONEY = MONEY_ROOT + INTEREST
000000     COMPUTE WS-AMOUNT-TOTAL    =
000000             AS-MONEY-ROOT      + 
000000             WS-AMOUNT-INTEREST.
000000* 
000000     EXIT.                 
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息計算ロジック
000000* CALCULATE-FUN002       SECTION |      （FUN_002)                   
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 CALCULATE-FUN002.
000000*
000000*--- 定期なし口座の処理                                     
000000     IF AS-SAVING-TYPE = CST-NON-TERM 
000000*--- INTEREST = MONEY_ROOT * INTEREST_RATE * ACTUAL_DAYS / 365          
000000         COMPUTE WS-AMOUNT-INTEREST     =                             
000000                 AS-MONEY-ROOT          * 
000000                 WS-RATE-INTEREST       * 
000000                 WS-DAYS-ACTUAL         / 
000000                 CST-FIXED-VALUE-12
000000*--- 定期口座（90日／180日／365日）の処理     
000000     ELSE                                                   
000000         IF HV-DAYS-CURRENT-COMP >= HV-DAYS-END-COMP                    
000000             EVALUATE AS-SAVING-TYPE                        
000000                 WHEN CST-FIXED-03                               
000000                     MOVE CST-FIXED-VALUE-03 
000000                                 TO 
000000                          WS-DAYS-TERM        
000000                 WHEN CST-FIXED-06                             
000000                     MOVE CST-FIXED-VALUE-06 
000000                                 TO 
000000                          WS-DAYS-TERM        
000000                 WHEN CST-FIXED-12                             
000000                     MOVE CST-FIXED-VALUE-12 
000000                                 TO 
000000                          WS-DAYS-TERM        
000000             END-EVALUATE 
000000*--- INTEREST = MONEY_ROOT * INTEREST_RATE * TERM_DAYS / 365          
000000             COMPUTE WS-AMOUNT-INTEREST =                      
000000                     AS-MONEY-ROOT      * 
000000                     WS-RATE-INTEREST   * 
000000                     WS-DAYS-TERM       / 
000000                     CST-FIXED-VALUE-12
000000         ELSE       
000000             PERFORM EXEC-GET-NONTERM-RATE
000000*--- INTEREST = MONEY_ROOT * NONTERM_RATE * ACTUAL_DAYS / 365           
000000             COMPUTE WS-AMOUNT-INTEREST =                      
000000                     AS-MONEY-ROOT      * 
000000                     WS-RATE-NONTERM    * 
000000                     WS-DAYS-ACTUAL     / 
000000                     CST-FIXED-VALUE-12        
000000         END-IF                                             
000000     END-IF.
000000*--- MONEY = MONEY_ROOT + INTEREST                                      
000000     COMPUTE WS-AMOUNT-TOTAL            =                               
000000             AS-MONEY-ROOT              + 
000000             WS-AMOUNT-INTEREST.
000000*             
000000     EXIT.                                                   
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データ取得・決済計算              
000000* FETCH-SAV-SETTLEMENT   SECTION |      （FUN_002)                         
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 FETCH-SAV-SETTLEMENT.                                              
000000*  
000000     EXEC SQL                                             
000000         FETCH CRS2                                       
000000         INTO  :AS-ORDER-ID,                               
000000               :AS-ACC-ID,                                 
000000               :AS-SAVING-TYPE,
000000               :AS-START-DATE,
000000               :AS-END-DATE,
000000               :AS-MONEY-ROOT                                   
000000     END-EXEC.  
000000*                                          
000000     EVALUATE SQLCODE                                     
000000         WHEN 100                                           
000000             MOVE 'Y'            TO      CST-EOF-CRS2                     
000000         WHEN 0
000000             PERFORM GET-CURR-DATE-FUN002
000000             PERFORM EXEC-GET-INTEREST-RATE
000000             PERFORM CALCULATE-FUN002
000000             MOVE AS-ACC-ID      TO      AB-ACC-ID                     
000000             PERFORM UPDATE-ACCOUNT-BALANCE
000000             ADD 1 TO CST-COUNT-UPD-BALANCE                   
000000             PERFORM UPDATE-SAVING-STATUS
000000             ADD 1 TO CST-COUNT-UPD-STATUS
000000*--- デバッグモードが有効な場合のみ、詳細情報を表示する
000000             IF CST-DEBUG-MODE = 'Y'
000000                 PERFORM DISPLAY-DETAIL-FUN002
000000             END-IF 
000000             ADD 1               TO      CST-COUNT-FUNC002
000000         WHEN OTHER                                          
000000             MOVE 'FETCH-SAV-SETTLEMENT' 
000000                                 TO 
000000                  CST-ABEND-BREAKPOINT
000000             MOVE 'FETCH CSR2 FAILED'     
000000                                 TO     
000000                  CST-ABEND-DETAIL     
000000             PERFORM ABEND-PROGRAM                        
000000     END-EVALUATE.
000000*                                                  
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 口座残高更新                 
000000* UPDATE-ACCOUNT-BALANCE SECTION |      （FUN_002)                       
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 UPDATE-ACCOUNT-BALANCE.
000000*                                  
000000     EXEC SQL                                             
000000         UPDATE  MYDB.DB_ACCOUNT_BALANCE                   
000000         SET     BALANCE = BALANCE + :WS-AMOUNT-TOTAL             
000000         WHERE   ACC_ID  = :AB-ACC-ID                        
000000     END-EXEC.
000000*                                              
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE
000000         MOVE 'UPDATE-ACCOUNT-BALANCE' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT 
000000         MOVE 'UPDATE ACCOUNT BALANCE FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL         
000000         PERFORM ABEND-PROGRAM                                  
000000     END-IF. 
000000*                                                        
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 預金ステータス更新              
000000* UPDATE-SAVING-STATUS   SECTION |      （FUN_002)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 UPDATE-SAVING-STATUS.
000000*                                              
000000     EXEC SQL                                                      
000000         UPDATE     MYDB.DB_ACCOUNT_SAVINGS                            
000000         SET        STATUS   = :CST-STATUS-9                         
000000         WHERE      ORDER_ID = :AS-ORDER-ID                      
000000     END-EXEC.                                                     
000000*                
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'UPDATE-SAVING-STATUS' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'UPDATE SAVING STATUS FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL           
000000         PERFORM ABEND-PROGRAM                                   
000000     END-IF.
000000*                                                         
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データのコミット                     
000000* COMMIT                 SECTION |      （COMMON）                      
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 COMMIT-DATA.
000000*--- COMMIT 
000000     EXEC SQL
000000         COMMIT
000000     END-EXEC.
000000*---
000000     IF SQLCODE = 0
000000         CONTINUE          
000000     ELSE
000000         MOVE 'COMMIT-DATA'      TO      CST-ABEND-BREAKPOINT
000000         MOVE 'COMMIT FAILED'    TO      CST-ABEND-DETAIL    
000000         PERFORM ABEND-PROGRAM
000000     END-IF.
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息・決済明細表示                
000000* DISPLAY-DETAIL-FUN001  SECTION |      （FUN_001)                    
000000*                                |                                      
000000*/-------------------------------------------------------------/*  
000000*
000000 DISPLAY-DETAIL-FUN001.
000000*
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'OUTPUT FUNCTION-001 : INTEREST CALCULATION'.
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'ORDER_ID            : ' AS-ORDER-ID
000000     DISPLAY 'ACC_ID              : ' AS-ACC-ID
000000     DISPLAY 'SAVING_TYPE         : ' AS-SAVING-TYPE
000000     DISPLAY 'MONEY_ROOT          : ' AS-MONEY-ROOT
000000     DISPLAY 'INTEREST_RATE       : ' WS-RATE-INTEREST
000000     DISPLAY 'INTEREST            : ' WS-AMOUNT-INTEREST
000000     DISPLAY 'TOTAL               : ' WS-AMOUNT-TOTAL
000000     DISPLAY 'STATUS              : ' CST-STATUS-1
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'START_DATE(DB)      : ' AS-START-DATE
000000     DISPLAY 'CURRENT_DATE        : ' HV-DATE-CURRENT-9
000000     DISPLAY 'START_DATE(INT)     : ' HV-DAYS-START-COMP
000000     DISPLAY 'CURRENT_DATE(INT)   : ' HV-DAYS-CURRENT-COMP
000000     DISPLAY 'ACTUAL_DAYS         : ' WS-DAYS-ACTUAL
000000     DISPLAY '*/----------------------------------------------/*'
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息・決済明細表示                
000000* DISPLAY-DETAIL-FUN002  SECTION |      （FUN_002)                       
000000*                                |                                      
000000*/-------------------------------------------------------------/*  
000000*
000000 DISPLAY-DETAIL-FUN002.
000000*
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'OUTPUT FUNCTION-002 : SETTLEMENT'
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'ORDER_ID            : ' AS-ORDER-ID
000000     DISPLAY 'ACC_ID              : ' AB-ACC-ID
000000     DISPLAY 'SAVING_TYPE         : ' AS-SAVING-TYPE
000000     DISPLAY 'MONEY_ROOT          : ' AS-MONEY-ROOT
000000     DISPLAY 'INTEREST            : ' WS-AMOUNT-INTEREST
000000     DISPLAY 'TOTAL               : ' WS-AMOUNT-TOTAL
000000     DISPLAY 'STATUS              : ' CST-STATUS-9
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'START_DATE(DB)      : ' AS-START-DATE
000000     DISPLAY 'END_DATE(DB)        : ' AS-END-DATE
000000     DISPLAY 'CURRENT_DATE        : ' HV-DATE-CURRENT-9
000000     DISPLAY 'START_DATE(INT)     : ' HV-DAYS-START-COMP
000000     DISPLAY 'END_DATE(INT)       : ' HV-DAYS-END-COMP
000000     DISPLAY 'CURRENT_DATE(INT)   : ' HV-DAYS-CURRENT-COMP
000000     DISPLAY 'ACTUAL_DAYS         : ' WS-DAYS-ACTUAL
000000     DISPLAY 'TERM_DAYS           : ' WS-DAYS-TERM
000000     DISPLAY 'INTEREST_RATE       : ' WS-RATE-INTEREST
000000     DISPLAY 'NONTERM_RATE        : ' WS-RATE-NONTERM
000000     DISPLAY '*/----------------------------------------------/*'
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息・決済処理件数表示                
000000* DISPLAY-TOTAL          SECTION |      （COMMON）                    
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 DISPLAY-TOTAL.
000000*    
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'TOTAL ACCOUNTS PROCESSED IN FUNCTION-001 : ' 
000000             CST-COUNT-FUNC001.
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'TOTAL ACCOUNTS PROCESSED IN FUNCTION-002 : ' 
000000             CST-COUNT-FUNC002.
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'TOTAL ACCOUNTS UPDATED BALANCE           : '
000000             CST-COUNT-UPD-BALANCE.
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'TOTAL ACCOUNTS UPDATED SAVING STATUS     : '
000000             CST-COUNT-UPD-STATUS.
000000     DISPLAY '*/----------------------------------------------/*'
000000*
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 異常終了処理                     
000000* ABEND-PROGRAM          SECTION |      （COMMON）                      
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 ABEND-PROGRAM.
000000*                 
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'ABEND-PROGRAM'.
000000     DISPLAY '*/----------------------------------------------/*'
000000     DISPLAY 'ERROR MODULE : ' CST-ABEND-BREAKPOINT.
000000     DISPLAY 'ERROR DETAIL : ' CST-ABEND-DETAIL.
000000     DISPLAY 'SQLCODE      : ' SQLCODE.
000000     DISPLAY 'SQLSTATE     : ' SQLSTATE.
000000     DISPLAY '*/----------------------------------------------/*'
000000*--- ROLLBACK
000000     EXEC SQL
000000         ROLLBACK
000000     END-EXEC.
000000*--- ROLLBACK 結果確認
000000     IF SQLCODE = 0
000000         DISPLAY '*/------------------------------------------/*'
000000         DISPLAY 'ROLLBACK SUCCESS'
000000         DISPLAY '*/------------------------------------------/*'
000000     ELSE
000000         DISPLAY '*/------------------------------------------/*'
000000         DISPLAY 'ROLLBACK FAILED'
000000         DISPLAY '*/------------------------------------------/*'
000000         DISPLAY 'ROLLBACK SQLCODE  : ' SQLCODE
000000         DISPLAY 'ROLLBACK SQLSTATE : ' SQLSTATE
000000         DISPLAY '*/------------------------------------------/*'
000000     END-IF.
000000*
000000     STOP RUN.  
000000*===============================================================*         
000000*====           ＥＮＤ　 　ＯＦ　 　ＰＲＯＣＥＤＵＲＥ　       ====*         
000000*===============================================================*
000000*****************************************************************        