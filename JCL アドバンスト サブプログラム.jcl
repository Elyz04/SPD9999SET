//**************************************************************
//* JOB定義
//* 内容：COBOLプログラムの
//*       プリコンパイル、コンパイル、リンク、BIND、実行を行う
//* 備考：DB2連携用の標準JOB
//*------------------------------------------------------------*
//* UPDATE :
//*   2026/01/21 : SRCフォーマットに統一
//*------------------------------------------------------------*
//MAIN     JOB NOTIFY=&SYSUID,TIME=(0,20)
//*------------------------------------------------------------*
//* 変数定義
//*------------------------------------------------------------*
// SET LOAD=XXX.XXX.LOAD
// SET DBRMLIB=XXX.XXX.DBRMLIB
// SET SRCLIB=XXX.XXX.CBL
// SET CPYLIB=XXX.XXX.DCLGEN
// SET PGM=PGM001
// SET MEMB=MAIN
// SET WSPC=800
//*------------------------------------------------------------*
//* PRE-COMPILE（DB2プリコンパイル）
//* 内容：COBOLソース内のEXEC SQL文をプリコンパイルし、
//*       DBRMを生成する
//* 備考：DB2 BIND処理に必要
//*------------------------------------------------------------*
//PRECOM1  EXEC PGM=DSNHPC,PARM='HOST(IBMCOB)'
//STEPLIB  DD   DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT
//         DD   DISP=SHR,DSN=DSN910.SDSNLOAD
//DBRMLIB  DD   DISP=SHR,DSN=&DBRMLIB(&PGM)
//SYSIN    DD   DISP=SHR,DSN=&SRCLIB(&PGM)
//SYSLIB   DD   DISP=SHR,DSN=&CPYLIB
//SYSCIN   DD   DISP=(NEW,PASS),DSN=&&PRECOM,
//              UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE)
//SYSUT1   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSUT2   DD   SPACE=(800,(&WSPC,&WSPC),,,ROUND),UNIT=SYSDA
//SYSPRINT DD   SYSOUT=*
//*------------------------------------------------------------*
//* COMPILE（COBOLコンパイル）
//* 内容：プリコンパイル済みCOBOLソースをコンパイルする
//* 備考：リンク編集用のオブジェクトを生成
//*------------------------------------------------------------*
//COMP     EXEC PGM=IGYCRCTL,COND=(4,LT),
//              PARM='NODYNAM,LIB,OBJECT,RES,APOST,MAP,XREF,NOSEQUENCE'
//STEPLIB  DD   DISP=SHR,DSN=IGY410.SIGYCOMP
//SYSIN    DD   DISP=SHR,DSN=&SRCLIB(&MEMB)
//         DD   DISP=(OLD,DELETE),DSN=&&PRECOM
//SYSLIB   DD   DISP=SHR,DSN=&CPYLIB
//SYSLMOD  DD   DISP=SHR,DSN=&LOAD
//SYSLIN   DD   DISP=(MOD,PASS),DSN=&&PLKSET,
//              UNIT=SYSDA,SPACE=(800,(&WSPC,&WSPC))
//SYSUT1   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT4   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT5   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT6   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT7   DD   UNIT=VIO,SPACE=(CYL,(1,1))
//SYSPRINT DD   SYSOUT=*
//*------------------------------------------------------------*
//* LINKEDIT（ロードモジュール作成）
//* 内容：オブジェクトをリンクし、ロードモジュールを生成
//* 備考：実行可能形式を作成
//*------------------------------------------------------------*
//LKED     EXEC PGM=HEWL,PARM='MAP',COND=(4,LT)
//SYSLIB   DD   DISP=SHR,DSN=CEE.SCEELKED
//         DD   DISP=SHR,DSN=DSN910.SDSNLOAD
//         DD   DISP=SHR,DSN=ISP.SISPLOAD
//         DD   DISP=SHR,DSN=GDDM.SADMMOD
//SYSLIN   DD   DISP=(OLD,DELETE),DSN=&&PLKSET
//SYSLMOD  DD   DISP=(MOD,KEEP),DSN=&LOAD(&MEMB),
//              SPACE=(32000,(30,30)),
//              DCB=(RECFM=U,LRECL=80,BLKSIZE=3200)
//SYSUT1   DD   SPACE=(1024,(50,50)),UNIT=SYSDA,
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//SYSPRINT DD   SYSOUT=*
//*------------------------------------------------------------*
//* BIND（DB2バインド）
//* 内容：DBRMをPLANにバインドする
//* 備考：PLANは実行時に使用
//*------------------------------------------------------------*
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD   DISP=SHR,DSN=DSN910.SDSNLOAD
//DBRMLIB  DD   DISP=SHR,DSN=&DBRMLIB
//SYSPRINT DD   SYSOUT=*
//SYSTSPRT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSDUMP  DD   SYSOUT=*
//SYSTSIN  DD   *
  DSN SYSTEM(DB9G)
    BIND PLAN(DF02)
         MEMBER(PGM001)
         QUALIFIER(DF)
         ENCODING(EBCDIC)
         ISOLATION(CS)
         ACTION(REPLACE)
  END
/*
//*------------------------------------------------------------*
//* RUN（プログラム実行）
//* 内容：作成したロードモジュールを実行
//* 実行結果：SYSOUTに処理ログを出力
//*------------------------------------------------------------*
//RUNSTP   EXEC PGM=IKJEFT01,REGION=0M,COND=(4,LT)
//STEPLIB  DD   DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT
//         DD   DISP=SHR,DSN=DSN910.SDSNLOAD
//         DD   DISP=SHR,DSN=&LOAD
//SYSPRINT DD   SYSOUT=*
//SYSTSPRT DD   SYSOUT=*
//SYSUDUMP DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSTSIN  DD   *
  DSN SYSTEM(DB9G)
    RUN PROGRAM(MAIN)
        PLAN(DF02)
  END
/*
//**************************************************************