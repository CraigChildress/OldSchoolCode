//CRAIGCC JOB  (CRAIGC),
//             'REFORMAT ZIPCODE FILE',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1)
//********************************************************************
//* CREATE WORKFILES: HANDLE RPG BLOCK SIZE LIMIT.
//********************************************************************
//STEP01  EXEC PGM=IEFBR14
//NEWF1    DD  DSN=&DS1,
//             DISP=(NEW,CATLG),
//             UNIT=3350,VOL=SER=PUB000,
//             SPACE=(TRK,(400,10)),
//             DCB=(LRECL=80,BLKSIZE=9440,RECFM=FB)
//NEWF2    DD  DSN=&DS2,
//             DISP=(NEW,CATLG),
//             UNIT=3350,VOL=SER=PUB010,
//             SPACE=(TRK,(400,10)),
//             DCB=(LRECL=70,BLKSIZE=9380,RECFM=FB)
//********************************************************************
//* COPY FROM ORIGNAL ZIPCODE FILE TO INPUT WORKFILE.
//********************************************************************
//STEP02  EXEC PGM=IEBGENER
//SYSUT1  DD   DSN=CRAIGC.NEWAPP.ZIPCODE,
//             DISP=(OLD,KEEP),
//             DCB=(LRECL=80,BLKSIZE=19040,RECFM=FB)
//SYSUT2   DD  DSN=&&DS1,
//             DISP=(OLD,KEEP),
//             DCB=(LRECL=80,BLKSIZE=9440,RECFM=FB)
//SYSIN    DD  DUMMY
//SYSPRINT DD  SYSOUT=A
//SYSDUMP  DD  DUMMY
//********************************************************************
//* CREATE AND RUN REFORMATTER
//********************************************************************
//REFMT    EXEC RPGECLG
//RPG.SYSUT3 DD UNIT=SYSDA
//RPG.SYSUT2 DD UNIT=SYSDA
//RPG.SYSUT1 DD UNIT=SYSDA
//RPG.SYSGO DD  UNIT=SYSDA
//RPG.SYSIN DD  DSN=CRAIGC.NEWAPP.RPG(REFMTR),
//              DISP=(SHR)
//GO.CARDIN DD DSN=&&DS1,
//             DISP=(OLD,KEEP),
//             DCB=(LRECL=80,BLKSIZE=9440,RECFM=FB)
//GO.ZIPCODE DD DSN=&&DS2,
//             DISP=(OLD,KEEP),
//             DCB=(LRECL=70,BLKSIZE=9380,RECFM=FB)
//********************************************************************
//* POPULATE A PERMANENT NEW ZIPCODE TABLE.
//********************************************************************
//STEP03   EXEC PGM=IEBGENER
//SYSUT1  DD   DSN=&&DS2,
//             DISP=(OLD,KEEP),
//             DCB=(LRECL=70,BLKSIZE=9380,RECFM=FB)
//SYSUT2  DD   DSN=CRAIGC.NEWAPP.ZIPTBL,
//             UNIT=3375,VOL=SER=PUB011,
//             DISP=(NEW,CATLG),
//             SPACE=(TRK,(90,10)),
//             DCB=(LRECL=70,BLKSIZE=17570,RECFM=FB)
//SYSPRINT DD  SYSOUT=A
//SYSIN    DD  DUMMY
//SYSDUMP  DD  DUMMY
//*========================================================
//STEP04   EXEC PGM=IEFBR14
//FILE1   DD   DSN=&&DS1,
//             UNIT=3350,VOL=SER=PUB000,
//             DISP=(OLD,DELETE)
//FILE2   DD   DSN=&&DS2,
//             UNIT=3350,VOL=SER=PUB010,
//             DISP=(OLD,DELETE)
