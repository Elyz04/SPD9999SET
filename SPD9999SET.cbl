000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.                            
000000 PROGRAM-ID.                     SPD9999SET.  
000000*/-------------------------------------------------------------/*     
000000*    PROGRAM-ID     :            SPD9999SET  
000000*    SPX78439216    :            利息計算（プレビュー処理）
000000*                                ※DB更新なし
000000*    SPX95160487    :            満期決済処理 
000000*                                ※残高・ステータス更新あり                          
000000*    CREATE DATE    :            2026/01/07 
000000*    UPDATE DATE    :            2026/01/28                             
000000*    AUTHOR         :            Elyz04                      
000000*    PURPOSE        :            利息計算および満期決済処理
000000*/-------------------------------------------------------------/*   
000000*    UPDATE         :                                           
000000*        2026/01/08 : SPX78439216 : 利息計算（プレビューのみ、DB更新なし）      
000000*        2026/01/09 : SQLエラー時はABEND処理                     
000000*        2026/01/10 : 対象はDB_ACCOUNT_SAVINGSテーブル            
000000*        2026/01/11 : SPX95160487 : 満期決済および残高更新         
000000*        2026/01/13 : 処理ロジックの最終調整（INTEREST と MONEY 更新）
000000*        2026/01/15 : プログラムを再構築し、異常終了の詳細を追加し
000000*                     ます。エラーを明確に説明します。
000000*        2026/01/18 : 送信入力ACC-IDを更新、1アカウントの決済のみ処理、
000000*                     送信入力検証パラメータを追加
000000*        2026/01/20 : ACC_ID が存在しないことを検証するケースの再構
000000*                     築と修正
000000*        2026/01/28 : 実際の業務に基づいてプログラムの再構築
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
000000*/-------------------------------------------------------------/*
000000*  ワークエリア                                                   
000000*/-------------------------------------------------------------/*
000000 01 WS-VARIABLES.                                               
000000    03 WS-DAYS-ACTUAL            PIC 9(05).                    
000000    03 WS-DAYS-TERM              PIC 9(05).                    
000000    03 WS-AMOUNT-INTEREST        PIC S9(13)V99    COMP-3.         
000000    03 WS-AMOUNT-TOTAL           PIC S9(13)V99    COMP-3.
000000    03 WS-NEW-BALANCE            PIC S9(13)V99    COMP-3.  
000000    03 WS-RATE-INTEREST          PIC S9(01)V9(04) COMP-3.        
000000    03 WS-RATE-NONTERM           PIC S9(01)V9(04) COMP-3.            
000000    03 WS-PARAM-MODE             PIC X(01).             
000000    03 WS-PARAM-ACC-ID-CHAR      PIC X(09).   
000000    03 WS-PARAM-ACC-ID-DISP      PIC 9(09). 
000000    03 WS-PARAM-ACC-ID-COMP      PIC S9(09)       COMP. 
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
000000*--- PROGRAM / PROCESS LOG MESSAGE AREA
000000    03 CST-START-PGM-MSG         PIC X(50)  VALUE SPACES.
000000    03 CST-END-PGM-MSG           PIC X(50)  VALUE SPACES.
000000    03 CST-START-PROC1-MSG       PIC X(50)  VALUE SPACES.
000000    03 CST-START-PROC2-MSG       PIC X(50)  VALUE SPACES.
000000    03 CST-END-PROC1-MSG         PIC X(50)  VALUE SPACES.
000000    03 CST-END-PROC2-MSG         PIC X(50)  VALUE SPACES.
000000*--- STATUS / EOF FLAG
000000    03 CST-STATUS-1              PIC X(01)  VALUE '1'.          
000000    03 CST-STATUS-9              PIC X(01)  VALUE '9'.          
000000    03 CST-EOF-CRS1              PIC X(01)  VALUE 'N'. 
000000    03 CST-EOF-CRS2              PIC X(01)  VALUE 'N'.  
000000*--- SAVING TYPE CONSTANT
000000    03 CST-NON-TERM              PIC X(10)  VALUE 'NON-TERM'.
000000    03 CST-FIXED-03              PIC X(10)  VALUE 'FIXED-03'.
000000    03 CST-FIXED-06              PIC X(10)  VALUE 'FIXED-06'.
000000    03 CST-FIXED-12              PIC X(10)  VALUE 'FIXED-12'. 
000000*--- TERM DAY CONSTANT       
000000    03 CST-FIXED-VALUE-03        PIC 9(03)  VALUE 90.  
000000    03 CST-FIXED-VALUE-06        PIC 9(03)  VALUE 180.      
000000    03 CST-FIXED-VALUE-12        PIC 9(03)  VALUE 365.
000000*--- PROCESS COUNTER
000000    03 CST-COUNT-PROC1           PIC 9(05)  VALUE 0.
000000    03 CST-COUNT-PROC2           PIC 9(05)  VALUE 0.
000000    03 CST-COUNT-UPD-BALANCE     PIC 9(05)  VALUE 0.
000000    03 CST-COUNT-UPD-STATUS      PIC 9(05)  VALUE 0.
000000*--- PARAMETER / MODE FLAG  
000000    03 CST-ACC-ID-FLAG           PIC X(01)  VALUE 'N'.
000000    03 CST-MODE-PREVIEW          PIC X(01)  VALUE '1'.
000000    03 CST-MODE-SETTLE           PIC X(01)  VALUE '2'.
000000    03 CST-MODE-BOTH             PIC X(01)  VALUE '3'.
000000*--- COMMIT CONTROL
000000    03 CST-COMMIT-CNT            PIC 9(05)  VALUE 0.
000000    03 CST-COMMIT-LIMIT          PIC 9(05)  VALUE 100.   
000000*--- DEBUG / ABEND  
000000    03 CST-ABEND-BREAKPOINT      PIC X(100) VALUE SPACES.
000000    03 CST-ABEND-DETAIL          PIC X(100) VALUE SPACES.  
000000    03 CST-DEBUG-MODE            PIC X(01)  VALUE 'N'.
000000*/-------------------------------------------------------------/*
000000*  JCL パラメータ受け取りエリア                                                     
000000*/-------------------------------------------------------------/* 
000000 LINKAGE                         SECTION.
000000 01 LNK-PARAM-JCL.
000000    03 LNK-PARAM-LENGTH          PIC S9(04) COMP.
000000    03 LNK-PARAM-DATA            PIC X(11).   
000000*===============================================================*         
000000*====        ＰＲＯＣＥＤＵＲＥ　　 　　ＤＩＶＩＳＩＯＮ        ====*         
000000*===============================================================*       
000000 PROCEDURE                       DIVISION USING LNK-PARAM-JCL.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: メイン処理                       
000000* SPD9999-MAIN           SECTION |      （SPD9999-MAIN）                  
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000 SPD9999-MAIN.
000000     PERFORM                     SPD9999-SET-LOG-MSG.   
000000     PERFORM                     SPD9999-INIT-VAR.
000000     PERFORM                     SPD9999-INIT-DATE.
000000     PERFORM                     SPD9999-HANDLE-PARAM.
000000     DISPLAY                     CST-START-PGM-MSG.
000000     IF CST-DEBUG-MODE = 'Y'
000000         PERFORM                 SPD9999-DISP-TOTAL
000000     END-IF.
000000     DISPLAY                     CST-END-PGM-MSG.
000000     STOP RUN.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 変数初期化                       
000000* SPD9999-INIT-VAR       SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 SPD9999-INIT-VAR.
000000     INITIALIZE                  WS-VARIABLES.                           
000000     INITIALIZE                  HV-VARIABLES.                          
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 計算用ワーク初期化             
000000* SPD9999-INIT-CALC      SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 SPD9999-INIT-CALC.
000000     MOVE           0            TO      WS-DAYS-ACTUAL.
000000     MOVE           0            TO      WS-DAYS-TERM.
000000     MOVE           0            TO      WS-AMOUNT-INTEREST.
000000     MOVE           0            TO      WS-AMOUNT-TOTAL.
000000     MOVE           0            TO      WS-RATE-INTEREST.
000000     MOVE           0            TO      WS-RATE-NONTERM.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 計算用ワーク初期化         
000000* SPD9999-INIT-DATE      SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 SPD9999-INIT-DATE.
000000     MOVE FUNCTION CURRENT-DATE(1:8)
000000                                 TO 
000000                   HV-DATE-CURRENT-9.
000000     COMPUTE HV-DAYS-CURRENT-COMP =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-CURRENT-9).
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: ログメッセージ設定
000000* SPD9999-SET-LOG-MSG    SECTION |      （COMMON）
000000*                                |
000000*/-------------------------------------------------------------/*
000000 SPD9999-SET-LOG-MSG.
000000*--- PROGRAM START / END MESSAGE
000000     MOVE 'START SPD9999SET'
000000                                 TO
000000              CST-START-PGM-MSG.
000000     MOVE 'END   SPD9999SET'
000000                                 TO
000000              CST-END-PGM-MSG.
000000*--- PROCESS 1 (SPX78439216 : PREVIEW)
000000     MOVE 'START SPX78439216'
000000                                 TO
000000              CST-START-PROC1-MSG.
000000     MOVE 'END   SPX78439216'
000000                                 TO
000000              CST-END-PROC1-MSG.
000000*--- PROCESS 2 (SPX95160487 : SETTLE)
000000     MOVE 'START SPX95160487'
000000                                 TO
000000              CST-START-PROC2-MSG.
000000     MOVE 'END   SPX95160487'
000000                                 TO
000000              CST-END-PROC2-MSG.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: JCLパラメータ処理                       
000000* SPD9999-HANDLE-PARAM   SECTION |      （COMMON）                        
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000 SPD9999-HANDLE-PARAM.
000000     IF LNK-PARAM-LENGTH = 0
000000     OR LNK-PARAM-LENGTH > 11
000000         MOVE 'SPD9999-HANDLE-PARAM'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'INVALID JCL PARAM LENGTH'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     IF CST-DEBUG-MODE = 'Y'
000000         DISPLAY 'LNK-PARAM-LENGTH : ' LNK-PARAM-LENGTH
000000         DISPLAY 'LNK-PARAM-DATA   : ' LNK-PARAM-DATA
000000     END-IF.
000000     UNSTRING LNK-PARAM-DATA     
000000         DELIMITED BY ','        
000000         INTO WS-PARAM-MODE      
000000              WS-PARAM-ACC-ID-CHAR
000000     END-UNSTRING.
000000     IF WS-PARAM-ACC-ID-CHAR = SPACES
000000     OR WS-PARAM-ACC-ID-CHAR = LOW-VALUES                   
000000         MOVE 'SPD9999-HANDLE-PARAM'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'ACCOUNT ID PARAM IS REQUIRED'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND                     
000000     ELSE                                                 
000000         MOVE 'Y'                TO      CST-ACC-ID-FLAG               
000000         MOVE WS-PARAM-ACC-ID-CHAR
000000                                 TO 
000000              WS-PARAM-ACC-ID-DISP  
000000         MOVE WS-PARAM-ACC-ID-DISP
000000                                 TO 
000000              WS-PARAM-ACC-ID-COMP      
000000     END-IF.                                              
000000     PERFORM SPD9999-VALIDATE-PARAM.
000000     IF CST-ACC-ID-FLAG = 'Y'
000000         PERFORM SPD9999-CHK-ACC-EXIST
000000         PERFORM SPD9999-CHK-ACC-ACTIVE
000000     END-IF.
000000     EVALUATE WS-PARAM-MODE
000000         WHEN CST-MODE-PREVIEW
000000             DISPLAY CST-START-PROC1-MSG
000000             PERFORM SPD9999-PREVIEW
000000             DISPLAY CST-END-PROC1-MSG
000000         WHEN CST-MODE-SETTLE
000000             DISPLAY CST-START-PROC2-MSG
000000             PERFORM SPD9999-SETTLE
000000             DISPLAY CST-END-PROC2-MSG
000000         WHEN CST-MODE-BOTH
000000             DISPLAY CST-START-PROC1-MSG
000000             PERFORM SPD9999-PREVIEW
000000             DISPLAY CST-END-PROC1-MSG
000000             DISPLAY CST-START-PROC2-MSG
000000             PERFORM SPD9999-SETTLE
000000             DISPLAY CST-END-PROC2-MSG
000000     END-EVALUATE. 
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 呼び出し処理モジュール                    
000000* SPD9999-VALIDATE-PARAM SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 SPD9999-VALIDATE-PARAM.
000000     PERFORM SPD9999-CHK-MODE
000000     PERFORM SPD9999-CHK-ACC-ID
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: チェック機能値                         
000000* SPD9999-CHK-MODE       SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 SPD9999-CHK-MODE.
000000     IF WS-PARAM-MODE = SPACES
000000         MOVE 'SPD9999-CHK-MODE'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'MODE PARAM IS REQUIRED'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     IF  WS-PARAM-MODE NOT = CST-MODE-PREVIEW
000000     AND WS-PARAM-MODE NOT = CST-MODE-SETTLE
000000     AND WS-PARAM-MODE NOT = CST-MODE-BOTH
000000         MOVE 'SPD9999-CHK-MODE'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'MODE PARAM IS INVALID'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: ACC_ID 値を確認してください               
000000* SPD9999-CHK-ACC-ID     SECTION |      （COMMON)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 SPD9999-CHK-ACC-ID.
000000     IF WS-PARAM-ACC-ID-CHAR(1:9) IS NOT NUMERIC  
000000         MOVE 'SPD9999-CHK-ACC-ID'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'ACCOUNT ID PARAM IS NOT NUMERIC'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND                                
000000     END-IF.                                       
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: ACC_ID チェック
000000* SPD9999-CHK-ACC-EXIST  SECTION |      （COMMON）
000000*                                |       DB_ACCOUNT_SAVINGS
000000*/-------------------------------------------------------------/*
000000 SPD9999-CHK-ACC-EXIST.
000000     EXEC SQL
000000         SELECT COUNT(*)
000000         INTO   :HV-TOTAL-SAVING-CNT
000000         FROM   MYDB.DB_ACCOUNT_SAVINGS
000000         WHERE  ACC_ID = :WS-PARAM-ACC-ID
000000     END-EXEC.
000000     IF SQLCODE NOT = 0
000000         MOVE 'SPD9999-CHK-ACC-EXIST'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT ACC_ID EXIST FAILED'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     IF HV-TOTAL-SAVING-CNT = 0
000000         MOVE 'SPD9999-CHK-ACC-EXIST'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'ACCOUNT NOT FOUND'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: 有効な預金チェック
000000* SPD9999-CHK-ACC-ACTIVE SECTION |      （COMMON）
000000*                                |       ACTIVE SAVING 判定
000000*/-------------------------------------------------------------/*
000000 SPD9999-CHK-ACC-ACTIVE.
000000     EXEC SQL
000000         SELECT COUNT(*)
000000         INTO   :HV-ACTIVE-SAVING-CNT
000000         FROM   MYDB.DB_ACCOUNT_SAVINGS
000000         WHERE  ACC_ID = :WS-PARAM-ACC-ID-COMP
000000         AND    STATUS = :CST-STATUS-1
000000     END-EXEC.
000000     IF SQLCODE NOT = 0
000000         MOVE 'SPD9999-CHK-ACC-ACTIVE'
000000                                 TO 
000000               CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT ACTIVE SAVING FAILED'
000000                                 TO 
000000               CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     IF HV-ACTIVE-SAVING-CNT = 0
000000         MOVE 'SPD9999-CHK-ACC-ACTIVE'
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'NO ACTIVE SAVING FOUND'
000000                                 TO 
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息計算                         
000000* SPD9999-PREVIEW        SECTION |      （SPX78439216)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*
000000 SPD9999-PREVIEW.                                                         
000000     MOVE 'N'                    TO     CST-EOF-CRS1.
000000     EXEC SQL                                             
000000         DECLARE CRS1 CURSOR FOR                          
000000         SELECT  ORDER_ID,                                 
000000                 ACC_ID,                                   
000000                 SAVING_TYPE,                              
000000                 START_DATE,                                            
000000                 MONEY_ROOT                             
000000         FROM    MYDB.DB_ACCOUNT_SAVINGS                     
000000         WHERE   STATUS = :CST-STATUS-1
000000         AND     ACC_ID = :WS-PARAM-ACC-ID
000000     END-EXEC. 
000000     EXEC SQL                                                
000000         OPEN CRS1                                           
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-PREVIEW'     
000000                                 TO     
000000              CST-ABEND-BREAKPOINT 
000000         MOVE 'OPEN CSR 1 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL             
000000         PERFORM SPD9999-ABEND                            
000000     END-IF.
000000     PERFORM SPD9999-FETCH-PREV                                        
000000     PERFORM UNTIL CST-EOF-CRS1 = 'Y'
000000         PERFORM SPD9999-PROC-PREV                            
000000         PERFORM SPD9999-FETCH-PREV                                  
000000     END-PERFORM.                                            
000000     EXEC SQL                                                
000000         CLOSE CRS1                                          
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-PREVIEW'     
000000                                 TO     
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'CLOSE CSR 1 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL              
000000         PERFORM SPD9999-ABEND                            
000000     END-IF.     
000000     EXIT.                                                   
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 決済処理                         
000000* SPD9999-SETTLE         SECTION |      （SPX95160487)                   
000000*                                |                                      
000000*/-------------------------------------------------------------/*         
000000 SPD9999-SETTLE.
000000     MOVE 'N'                    TO      CST-EOF-CRS2.
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
000000         AND     ACC_ID = :WS-PARAM-ACC-ID-COMP             
000000     END-EXEC.                                            
000000     EXEC SQL                                             
000000         OPEN CRS2                                        
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE
000000         MOVE 'SPD9999-SETTLE'     
000000                                 TO      
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'OPEN CSR 2 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND                          
000000     END-IF.                                                                                                       
000000     PERFORM SPD9999-FETCH-SET
000000     PERFORM UNTIL CST-EOF-CRS2 = 'Y'
000000         PERFORM SPD9999-PROC-SET
000000         PERFORM SPD9999-FETCH-SET
000000     END-PERFORM                                        
000000     EXEC SQL                                             
000000         CLOSE CRS2                                       
000000     END-EXEC.                                            
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-SETTLE'     
000000                                 TO     
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'CLOSE CSR 2 FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL              
000000         PERFORM SPD9999-ABEND                            
000000     END-IF. 
000000     EXIT. 
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データ取得・計算                 
000000* SPD9999-FETCH-PREV     SECTION |      （SPX78439216）
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 SPD9999-FETCH-PREV.
000000     EXEC SQL
000000         FETCH CRS1
000000         INTO  :AS-ORDER-ID,
000000               :AS-ACC-ID,
000000               :AS-SAVING-TYPE,
000000               :AS-START-DATE,
000000               :AS-MONEY-ROOT
000000     END-EXEC.
000000     EVALUATE SQLCODE
000000         WHEN 0
000000             CONTINUE
000000         WHEN 100
000000             MOVE 'Y'            TO      CST-EOF-CRS1
000000         WHEN OTHER
000000             MOVE 'SPD9999-FETCH-PREV'
000000                                 TO 
000000                  CST-ABEND-BREAKPOINT
000000             MOVE 'FETCH CSR1 FAILED'
000000                                 TO 
000000                  CST-ABEND-DETAIL
000000             PERFORM SPD9999-ABEND
000000     END-EVALUATE.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データ取得・計算                 
000000* SPD9999-PROC-PREV      SECTION |      （SPX78439216）
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-PROC-PREV.
000000     PERFORM SPD9999-INIT-CALC.
000000     PERFORM SPD9999-GET-DATE-PREV.
000000     PERFORM SPD9999-GET-RATE.
000000     PERFORM SPD9999-CALC-PREV.
000000     ADD 1                       TO      CST-COUNT-PROC1.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 現在日付取得                    
000000* SPD9999-GET-DATE-PREV  SECTION |      （SPX78439216）                       
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-GET-DATE-PREV.
000000     COMPUTE HV-DATE-START-9      =
000000         FUNCTION NUMVAL(AS-START-DATE).
000000     COMPUTE HV-DAYS-START-COMP   =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-START-9).
000000     COMPUTE WS-DAYS-ACTUAL       =
000000             HV-DAYS-CURRENT-COMP - 
000000             HV-DAYS-START-COMP.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 現在日付および期間日数取得                    
000000* SPD9999-GET-DATE-SET   SECTION |      （SPX95160487）                        
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-GET-DATE-SET.
000000     COMPUTE HV-DATE-START-9      =
000000         FUNCTION NUMVAL(AS-START-DATE).
000000     COMPUTE HV-DAYS-START-COMP   =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-START-9).
000000     COMPUTE HV-DATE-END-9        =
000000         FUNCTION NUMVAL(AS-END-DATE).
000000     COMPUTE HV-DAYS-END-COMP     =
000000         FUNCTION INTEGER-OF-DATE(HV-DATE-END-9).
000000     COMPUTE WS-DAYS-ACTUAL       =
000000             HV-DAYS-CURRENT-COMP - 
000000             HV-DAYS-START-COMP.
000000     EXIT.                                             
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利率取得                         
000000* SPD9999-GET-RATE       SECTION |      （COMMON）                         
000000*                                |                                      
000000*/-------------------------------------------------------------/*      
000000 SPD9999-GET-RATE.
000000     EXEC SQL                                                       
000000         SELECT INTEREST_RATE                                       
000000         INTO   :WS-RATE-INTEREST                                     
000000         FROM   MYDB.DB_INTEREST_INFO                            
000000         WHERE  SAVING_TYPE = :AS-SAVING-TYPE                   
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-GET-RATE' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT INTEREST_RATE INTO :WS-RATE-INTEREST FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL       
000000         PERFORM SPD9999-ABEND                               
000000     END-IF.
000000     EXIT.                                                     
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 非定期利率取得                   
000000* SPD9999-GET-NONTERM    SECTION |       (SPX95160487)                      
000000*                                |                                       
000000*/-------------------------------------------------------------/*         
000000 SPD9999-GET-NONTERM.
000000     EXEC SQL                                                  
000000         SELECT  INTEREST_RATE                                  
000000         INTO    :WS-RATE-NONTERM                                 
000000         FROM    MYDB.DB_INTEREST_INFO                            
000000         WHERE   SAVING_TYPE = :CST-NON-TERM                     
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-GET-NONTERM' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT INTEREST_RATE INTO :WS-RATE-NON-TERM FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL                                        
000000         PERFORM SPD9999-ABEND                              
000000     END-IF.
000000     EXIT.   
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: 利息計算ロジック
000000* SPD9999-CALC-PREV      SECTION |      （SPX78439216)
000000*                                |
000000*/-------------------------------------------------------------/*
000000 SPD9999-CALC-PREV.
000000     IF AS-SAVING-TYPE = CST-NON-TERM
000000         COMPUTE WS-AMOUNT-INTEREST =
000000                 AS-MONEY-ROOT      *
000000                 WS-RATE-INTEREST   *
000000                 WS-DAYS-ACTUAL     / 
000000                 CST-FIXED-VALUE-12
000000     ELSE
000000         COMPUTE WS-AMOUNT-INTEREST =
000000                 AS-MONEY-ROOT      *
000000                 WS-RATE-INTEREST   *
000000                 WS-DAYS-ACTUAL     / 
000000                 CST-FIXED-VALUE-12
000000     END-IF.
000000     COMPUTE WS-AMOUNT-TOTAL        =
000000             AS-MONEY-ROOT          + 
000000             WS-AMOUNT-INTEREST.
000000     EXIT.                 
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息計算ロジック
000000* SPD9999-CALC-SET       SECTION |      （SPX95160487)                   
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-CALC-SET.
000000     IF AS-SAVING-TYPE = CST-NON-TERM 
000000         COMPUTE WS-AMOUNT-INTEREST     =                             
000000                 AS-MONEY-ROOT          * 
000000                 WS-RATE-INTEREST       * 
000000                 WS-DAYS-ACTUAL         / 
000000                 CST-FIXED-VALUE-12
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
000000             COMPUTE WS-AMOUNT-INTEREST =                      
000000                     AS-MONEY-ROOT      * 
000000                     WS-RATE-INTEREST   * 
000000                     WS-DAYS-TERM       / 
000000                     CST-FIXED-VALUE-12
000000         ELSE       
000000             PERFORM SPD9999-GET-NONTERM
000000             COMPUTE WS-AMOUNT-INTEREST =                      
000000                     AS-MONEY-ROOT      * 
000000                     WS-RATE-NONTERM    * 
000000                     WS-DAYS-ACTUAL     / 
000000                     CST-FIXED-VALUE-12        
000000         END-IF                                             
000000     END-IF.
000000     COMPUTE WS-AMOUNT-TOTAL            =                               
000000             AS-MONEY-ROOT              + 
000000             WS-AMOUNT-INTEREST.            
000000     EXIT.                                                   
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 決済対象データ取得               
000000* SPD9999-FETCH-SET      SECTION |      （SPX95160487）                        
000000*                                |       STATUS = '1' の預金を取得        
000000*/-------------------------------------------------------------/*
000000 SPD9999-FETCH-SET.
000000     EXEC SQL
000000         FETCH CRS2
000000         INTO  :AS-ORDER-ID,
000000               :AS-ACC-ID,
000000               :AS-SAVING-TYPE,
000000               :AS-START-DATE,
000000               :AS-END-DATE,
000000               :AS-MONEY-ROOT
000000     END-EXEC.
000000     EVALUATE SQLCODE
000000         WHEN 0
000000             CONTINUE
000000         WHEN 100
000000             MOVE 'Y'            TO      CST-EOF-CRS2
000000         WHEN OTHER
000000             MOVE 'SPD9999-FETCH-SET'
000000                                 TO 
000000                  CST-ABEND-BREAKPOINT
000000             MOVE 'FETCH CRS2 FAILED'
000000                                 TO 
000000                  CST-ABEND-DETAIL
000000             PERFORM SPD9999-ABEND
000000     END-EVALUATE.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 満期決済レコード処理             
000000* SPD9999-PROC-SET       SECTION |      （SPX95160487）                  
000000*                                |                      
000000*/-------------------------------------------------------------/*
000000 SPD9999-PROC-SET.
000000     PERFORM SPD9999-INIT-CALC.
000000     PERFORM SPD9999-GET-DATE-SET.
000000     PERFORM SPD9999-GET-RATE.
000000     PERFORM SPD9999-CALC-SET.
000000     MOVE AS-ACC-ID              TO      AB-ACC-ID.
000000     PERFORM SPD9999-GET-BAL.
000000     COMPUTE WS-NEW-BALANCE  =
000000             AB-BALANCE      + 
000000             WS-AMOUNT-TOTAL.
000000     PERFORM SPD9999-UPD-BAL.
000000     PERFORM SPD9999-UPD-SAV.
000000     ADD 1                       TO      CST-COUNT-PROC2.
000000     ADD 1                       TO      CST-COMMIT-CNT.
000000     IF CST-COMMIT-CNT >= CST-COMMIT-LIMIT
000000         PERFORM SPD9999-COMMIT
000000         MOVE 0                  TO      CST-COMMIT-CNT
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*
000000*                                | NOTE: 口座残高取得                     
000000* SPD9999-GET-BAL        SECTION |      （SPX95160487）                        
000000*                                |      対象: DB_ACCOUNT_BALANCE          
000000*/-------------------------------------------------------------/*
000000 SPD9999-GET-BAL.
000000     EXEC SQL
000000         SELECT BALANCE
000000         INTO   :AB-BALANCE
000000         FROM   MYDB.DB_ACCOUNT_BALANCE
000000         WHERE  ACC_ID = :AB-ACC-ID
000000         FOR UPDATE
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE
000000     ELSE
000000         MOVE 'SPD9999-GET-BAL'
000000                                 TO
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'SELECT BALANCE FAILED'
000000                                 TO
000000              CST-ABEND-DETAIL
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 口座残高更新                 
000000* SPD9999-UPD-BAL        SECTION |      （SPX95160487)                       
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-UPD-BAL.
000000     EXEC SQL                                             
000000         UPDATE  MYDB.DB_ACCOUNT_BALANCE                   
000000         SET     BALANCE = :WS-NEW-BALANCE             
000000         WHERE   ACC_ID  = :AB-ACC-ID                        
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         ADD 1 TO CST-COUNT-UPD-BALANCE
000000         CONTINUE
000000     ELSE
000000         MOVE 'SPD9999-UPD-BAL' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT 
000000         MOVE 'UPDATE ACCOUNT BALANCE FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL         
000000         PERFORM SPD9999-ABEND                                  
000000     END-IF. 
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 預金ステータス更新              
000000* SPD9999-UPD-SAV        SECTION |      （SPX95160487)                        
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 SPD9999-UPD-SAV.
000000     EXEC SQL                                                      
000000         UPDATE     MYDB.DB_ACCOUNT_SAVINGS                            
000000         SET        STATUS   = :CST-STATUS-9                         
000000         WHERE      ORDER_ID = :AS-ORDER-ID                      
000000     END-EXEC.                                                     
000000     IF SQLCODE = 0
000000         ADD 1 TO CST-COUNT-UPD-STATUS
000000         CONTINUE
000000     ELSE  
000000         MOVE 'SPD9999-UPD-SAV' 
000000                                 TO 
000000              CST-ABEND-BREAKPOINT
000000         MOVE 'UPDATE SAVING STATUS FAILED'     
000000                                 TO     
000000              CST-ABEND-DETAIL           
000000         PERFORM SPD9999-ABEND                                   
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 利息・決済処理件数表示                
000000* SPD9999-DISP-TOTAL     SECTION |      （COMMON）                    
000000*                                |                                      
000000*/-------------------------------------------------------------/* 
000000 SPD9999-DISP-TOTAL.
000000     DISPLAY 'TOTAL ACCOUNTS PROCESSED IN SPD9999-PREVIEW : ' 
000000             CST-COUNT-PROC1.
000000     DISPLAY 'TOTAL ACCOUNTS PROCESSED IN SPD9999-SETTLE : ' 
000000             CST-COUNT-PROC2.
000000     DISPLAY 'TOTAL ACCOUNTS UPDATED BALANCE           : '
000000             CST-COUNT-UPD-BALANCE.
000000     DISPLAY 'TOTAL ACCOUNTS UPDATED SAVING STATUS     : '
000000             CST-COUNT-UPD-STATUS.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: データのコミット                     
000000* SPD9999-COMMIT         SECTION |      （COMMON）                      
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 SPD9999-COMMIT.
000000     EXEC SQL
000000         COMMIT
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         CONTINUE          
000000     ELSE
000000         MOVE 'SPD9999-COMMIT'   TO      CST-ABEND-BREAKPOINT
000000         MOVE 'COMMIT FAILED'    TO      CST-ABEND-DETAIL    
000000         PERFORM SPD9999-ABEND
000000     END-IF.
000000     EXIT.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: 異常終了処理                     
000000* SPD9999-ABEND          SECTION |      （COMMON）                      
000000*                                |                                      
000000*/-------------------------------------------------------------/*     
000000 SPD9999-ABEND.
000000     DISPLAY 'SPD9999-ABEND'.
000000     DISPLAY 'ERROR MODULE : ' CST-ABEND-BREAKPOINT.
000000     DISPLAY 'ERROR DETAIL : ' CST-ABEND-DETAIL.
000000     DISPLAY 'SQLCODE      : ' SQLCODE.
000000     DISPLAY 'SQLSTATE     : ' SQLSTATE.
000000     EXEC SQL
000000         ROLLBACK
000000     END-EXEC.
000000     IF SQLCODE = 0
000000         DISPLAY 'ROLLBACK SUCCESS'
000000     ELSE
000000         DISPLAY 'ROLLBACK FAILED'
000000         DISPLAY 'ROLLBACK SQLCODE  : ' SQLCODE
000000         DISPLAY 'ROLLBACK SQLSTATE : ' SQLSTATE
000000     END-IF.
000000     STOP RUN.  
000000*===============================================================*         
000000*====           ＥＮＤ　 　ＯＦ　 　ＰＲＯＣＥＤＵＲＥ　       ====*         
000000*===============================================================*
000000*****************************************************************        