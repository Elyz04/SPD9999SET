//**************************************************************
//* JOB定義                                                           
//* 内容：COBOLプログラムのコンパイル、リンク、BIND、実行を行う          
//* 備考：各ステップはDB2連携のために必要                                
//*------------------------------------------------------------* 
//*    UPDATE         :                                           
//*        2026/01/19 : COBOL ソースに値を渡す      
//*-------------------------------------------------------------*
//PGM001   JOB (ACCT#),'PGM001',CLASS=A,MSGCLASS=X,                     
//              NOTIFY=&SYSUID                                               
// SET MEMB=PGM001                                                       
// SET LOAD=XXX.XXX.LOAD                                                
// SET DBRMLIB=XXX.XXX.DBRMLIB                                          
// SET SRCLIB=XXX.XXX.CBL                                              
// SET WSPC=500                                                         
// SET INCLUDE=XXX.XXX.DCLGEN                     
//*------------------------------------------------------------*
//* PRE-COMPILE（DB2プリコンパイル）                                    
//* 内容：COBOLソース内のEXEC SQL文をプリコンパイルし、DBRMを生成         
//* 備考：DB2バインド用のPLAN作成に必要                                  
//*------------------------------------------------------------*
//PC       EXEC PGM=DSNHPC,PARM='HOST(IBMCOB)'                          
//DBRMLIB  DD   DISP=SHR,DSN=&DBRMLIB(&MEMB)                        
//STEPLIB  DD   DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT                        
//         DD   DISP=SHR,DSN=DSN910.SDSNLOAD                             
//SYSCIN   DD   DSN=&&DSNHOUT,DISP=(MOD,PASS),UNIT=SYSDA,                
//              SPACE=(800,(&WSPC,&WSPC))                                
//SYSIN    DD   DISP=SHR,DSN=&SRCLIB(&MEMB)                              
//SYSLIB   DD   DISP=SHR,DSN=&INCLUDE                                    
//SYSPRINT DD   SYSOUT=*                                                 
//SYSTERM  DD   SYSOUT=*                                                 
//SYSUDUMP DD   SYSOUT=*                                                 
//SYSUT1   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT2   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA   
//*------------------------------------------------------------*
//* COMPILE（COBOLコンパイル）                                                        
//* 内容：プリコンパイル済みCOBOLソースをコンパイルし、                  
//*      SYSUTで中間結果作成                                           
//* 備考：リンクステップでロードモジュール作成に使用                     
//*------------------------------------------------------------*
//COB      EXEC PGM=IGYCRCTL,                                           
//              PARM=(NOSEQUENCE,QUOTE,RENT,'PGMNAME(LONGUPPER)'),       
//              COND=(4,LT,PC)                                           
//SYSPRINT DD   SYSOUT=*                                                 
//SYSTERM  DD   SYSOUT=*                   
//SYSLIN   DD   DSN=&&LOADSET,                                           
//              DISP=(MOD,PASS),UNIT=SYSDA,                              
//              SPACE=(800,(&WSPC,&WSPC))                                
//SYSIN    DD   DSN=&&DSNHOUT,DISP=(OLD,DELETE)                          
//SYSUT1   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT2   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT3   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT4   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT5   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT6   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//SYSUT7   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA             
//*------------------------------------------------------------*
//* PRELINK（リンク編集前処理）                                            
//* 内容：ランタイムライブラリを解決し、リンク編集準備                    
//* 備考：EDCPRLKでロードモジュール作成前処理                            
//*------------------------------------------------------------*
//PLKED    EXEC PGM=EDCPRLK,COND=((4,LT,PC),(4,LT,COB))                  
//STEPLIB  DD   DISP=SHR,DSN=CEE.SCEERUN                                 
//SYSMSGS  DD   DISP=SHR,                                                
//              DSN=CEE.SCEEMSGP(EDCPMSGE)                               
//SYSIN    DD   DSN=&&LOADSET,DISP=(OLD,DELETE)     
//SYSMOD   DD   DSN=&&PLKSET,UNIT=SYSDA,DISP=(MOD,PASS),                 
//              SPACE=(32000,(30,30)),                                   
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)                     
//SYSDEFSD DD   DUMMY                                                    
//SYSOUT   DD   SYSOUT=*                                                 
//SYSPRINT DD   SYSOUT=*                                                 
//SYSTERM  DD   SYSOUT=*   
//*------------------------------------------------------------*
//* LINKEDIT（ロードモジュール作成）                                    
//* 内容：LOADモジュールを生成                                          
//* 備考：IEWLでロードモジュール作成、MAP出力可能                        
//*------------------------------------------------------------*
//LKED     EXEC PGM=IEWL,PARM='MAP',                                    
//              COND=((4,LT,PC),(4,LT,COB),(4,LT,PLKED))                     
//SYSLIB   DD   DISP=SHR,DSN=CEE.SCEELKED                                
//         DD   DISP=SHR,DSN=DSN910.SDSNLOAD                             
//         DD   DISP=SHR,DSN=ISP.SISPLOAD                                
//         DD   DISP=SHR,DSN=GDDM.SADMMOD                                
//SYSLIN   DD   DSN=&&PLKSET,DISP=(OLD,DELETE)                           
//         DD   DDNAME=SYSIN                                             
//SYSLMOD  DD   DSN=&LOAD(&MEMB),SPACE=(32000,(30,30)),   
//              DISP=(MOD,KEEP),                                         
//              DCB=(RECFM=U,LRECL=80,BLKSIZE=3200)                      
//SYSPRINT DD   SYSOUT=*                                                 
//SYSUT1   DD   SPACE=(1024,(50,50)),UNIT=SYSDA,                         
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)                     
//*------------------------------------------------------------*
//* BIND（DB2バインド）                                                
//* 内容：DBRMをPLANにバインド                                         
//* 備考：PLAN(DF01)に登録、置換可能                                    
//*------------------------------------------------------------*
//BIND     EXEC PGM=IKJEFT01                                            
//STEPLIB  DD   DSN=DSN910.SDSNLOAD,DISP=SHR                            
//DBRMLIB  DD   DSN=&DBRMLIB(&MEMB),DISP=SHR                            
//SYSTSPRT DD   SYSOUT=*                                                
//SYSPRINT DD   SYSOUT=*                                                
//SYSOUT   DD   SYSOUT=*                                                
//SYSDUMP  DD   SYSOUT=*                                                
//SYSTSIN  DD   *                                                       
  DSN SYSTEM(DB9G)                                                      
  BIND PLAN(DF01)-                                                      
  MEMBER(PGM001)-                      
  QUALIFIER(DF)-                                                        
  ENCODING(EBCDIC)-                                                     
  ACTION(REPLACE)                                                       
  END                                                                   
/*                  
//*------------------------------------------------------------*                                             
//* RUN（プログラム実行）                                               
//* 内容：作成したロードモジュールをPLAN指定で実行                        
//* 実行結果：SYSOUTにログ出力、RC=0で正常終了                           
//* 更新データ：DB2テーブル(DB_ACCOUNT_BALANCE,                         
//*            DB_ACCOUNT_SAVINGS)更新済み                             
//* SYSOUT参照：SYSPRINT, SYSUDUMPに詳細ログを出力                      
//* 後処理：一時データセット&&LOADSET,                                 
//*        &&PLKSETはDISP=(OLD,DELETE)で自動削除                                            
//*------------------------------------------------------------*
//RUNSTP   EXEC PGM=IKJEFT01,REGION=0M                                  
//STEPLIB  DD   DSN=DSN910.DB9G.SDSNEXIT,DISP=SHR                       
//         DD   DSN=DSN910.SDSNLOAD,DISP=SHR                            
//SYSPRINT DD   SYSOUT=*                                                
//SYSTSPRT DD   SYSOUT=*                                                
//SYSUDUMP DD   SYSOUT=*                                                
//SYSOUT   DD   SYSOUT=*                                                
//SYSTSIN  DD   *                                                       
  DSN SYSTEM (DB9G)                                                     
  RUN PROGRAM(PGM001)-
  PARM('3,000004002')-                                                   
  PLAN(DF01)-                        
  LIBRARY ('XXX.XXX.LOAD')
  END                                                                   
/*     
//*------------------------------------------------------------*
//* PRINT COBOL SOURCE
//* 内容：実行に使用したCOBOLソースをSYSOUTへ出力する
//* 目的：
//*   ・実行対象ソースの内容をスプールで確認するため
//*   ・処理ログとの突合・レビュー用
//* 備考：
//*   ・IEBGENERを使用してソースをそのまま出力
//*   ・SYSINはDUMMY指定（制御文なし）
//*------------------------------------------------------------*
//PRTSRC   EXEC PGM=IEBGENER
//SYSUT1   DD   DISP=SHR,DSN=&SRCLIB(&MEMB)
//SYSUT2   DD   SYSOUT=*
//*SYSUT2  DD   DSN=&&CBLLOG,DISP=(NEW,PASS),
//*             UNIT=SYSDA,
//*             SPACE=(TRK,(5,5)),
//*             DCB=(RECFM=FB,LRECL=80,BLKSIZE=0)
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   DUMMY
//**************************************************************