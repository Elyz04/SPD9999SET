000000*****************************************************************
000000 IDENTIFICATION                  DIVISION.   
000000 PROGRAM-ID.                     SPD9999DRV.
000000*/-------------------------------------------------------------/*     
000000*    PROGRAM-ID     :            SPD9999DRV                               
000000*    CREATE DATE    :            2026/01/21
000000*    UPDATE DATE    :            XXXX/XX/XX                              
000000*    AUTHOR         :            Elyz                      
000000*    PURPOSE        :            SPD9999SET に送信された一連の 
000000*                                PARM の送信と実行
000000*/-------------------------------------------------------------/*   
000000*    UPDATE         :                                           
000000*        2026/01/21 :            プログラムの作成
000000*/-------------------------------------------------------------/*        
000000 ENVIRONMENT                     DIVISION.
000000 INPUT-OUTPUT                    SECTION.
000000 FILE-CONTROL.
000000     SELECT INPUT-FILE ASSIGN    TO  'INPUT'
000000         ORGANIZATION IS SEQUENTIAL
000000         FILE STATUS  IS WS-IN-STATUS.
000000 DATA                            DIVISION.
000000 FILE                            SECTION.
000000 FD  INPUT-FILE
000000     RECORDING MODE IS F
000000     RECORD CONTAINS 11 CHARACTERS.
000000 01 INPUT-REC                    PIC X(11).
000000 WORKING-STORAGE                 SECTION.
000000*/-------------------------------------------------------------/*
000000*  ワークエリア                                                   
000000*/-------------------------------------------------------------/*
000000 01  WS-IN-STATUS                PIC X(02).
000000*/-------------------------------------------------------------/*
000000*  定数定義                                                      
000000*/-------------------------------------------------------------/*  
000000 01 CST-VARIABLES.
000000    03 CST-EOF                   PIC X      VALUE 'N'.
000000    03 CST-REC-COUNT             PIC 9(03)  VALUE 0.
000000    03 CST-START-PGM-MSG         PIC X(10)  VALUE 'START SPD9DRV'.
000000    03 CST-STOP-PGM-MSG          PIC X(09)  VALUE 'STOP SPD9DRV'.
000000*/-------------------------------------------------------------/*
000000*  JCL パラメータ受け取りエリア                                                     
000000*/-------------------------------------------------------------/* 
000000 01 LNK-PARAM-JCL.
000000    03 LNK-PARAM-LENGHT          PIC S9(04) COMP VALUE 11.
000000    03 LNK-PARAM-DATA            PIC X(11).
000000*===============================================================*         
000000*====        ＰＲＯＣＥＤＵＲＥ　　 　　ＤＩＶＩＳＩＯＮ        ====*         
000000*===============================================================*
000000 PROCEDURE                       DIVISION.
000000*/-------------------------------------------------------------/*         
000000*                                | NOTE: メイン処理                       
000000* SPD9999DRV             SECTION |      （MAIN）                           
000000*                                |                                       
000000*/-------------------------------------------------------------/*
000000*
000000     DISPLAY CST-START-PGM-MSG
000000     OPEN INPUT INPUT-FILE.
000000     IF WS-IN-STATUS NOT = '00'
000000         DISPLAY 'ERROR OPEN INPUT FILE, STATUS : ' WS-IN-STATUS
000000         STOP RUN
000000     END-IF.
000000     PERFORM UNTIL CST-EOF = 'Y'
000000         READ INPUT-FILE
000000             AT END
000000                 MOVE 'Y'        TO          CST-EOF
000000             NOT AT END
000000                 ADD 1           TO          CST-REC-COUNT
000000                 MOVE INPUT-REC  TO          LNK-PARAM-DATA
000000                 DISPLAY 'RECORD #'          CST-REC-COUNT
000000                 DISPLAY 'LNK-PARAM-DATA : ' LNK-PARAM-DATA
000000                 CALL    'SPD9999SET' USING  LNK-PARAM-JCL
000000         END-READ
000000     END-PERFORM
000000     CLOSE INPUT-FILE.
000000     IF WS-IN-STATUS NOT = '00'
000000         DISPLAY 'ERROR CLOSE INPUT FILE, STATUS : ' WS-IN-STATUS
000000         STOP RUN
000000     END-IF.
000000     DISPLAY 'TOTAL RECORD : ' CST-REC-COUNT
000000     DISPLAY CST-STOP-PGM-MSG
000000     STOP RUN.
000000*/-------------------------------------------------------------/* 
000000 END PROGRAM SPD9999DRV.
000000*===============================================================*         
000000*====           ＥＮＤ　 　ＯＦ　 　ＰＲＯＣＥＤＵＲＥ　       ====*         
000000*===============================================================*
000000*****************************************************************       