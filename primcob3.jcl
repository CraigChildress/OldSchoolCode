//DISPADR  JOB (COBOL),
//             'Display city from zipcodes',
//             CLASS=A,
//             MSGCLASS=H,
//             REGION=8M,TIME=1440,
//             MSGLEVEL=(1,1)
//********************************************************************
//*
//* Name: SYS2.JCLLIB(PRIMCOB3)
//*
//* Desc: Sieve of Eratosthenes programmed in COBOL.
//*       All prime numbers up to the value entered via
//*       //GO.SYSIN DD are computed. An assembler subroutine
//*       is used to overcome COBOL 32767 array index
//*       and 131068 array length limitations.
//*
//********************************************************************
//ARRAY   EXEC ASMFC,PARM.ASM=(OBJ,NODECK,NOESD,NOLIST,NOXREF)
//ASM.SYSIN DD *
INIPRIME TITLE 'Provide Array to COBOL Program'                         00000100
         ENTRY ISPRIME,SETPRIME,RLSPRIME entry points                   00000200
*********************************************************************** 00000300
***                                                                 *** 00000400
*** Program:  INIPRIME                                              *** 00000500
***                                                                 *** 00000600
*** Purpose:  Provide dynamic array to COBOL Sieve of Eratosthenes  *** 00000700
***                                                                 *** 00000800
*********************************************************************** 00000900
INIPRIME CSECT ,                main entry point and program name       00001000
*                                                                       00001100
* Obtain storage and initialize array                                   00001200
*                                                                       00001300
         SAVE  (14,12),,*       save registers                          00001400
         LR    R12,R15          establish module addressability         00001500
         USING INIPRIME,R12     tell assembler of base                  00001600
         LA    R2,SAVEA         chain ..                                00001700
         ST    R13,4(,R2)         .. the ..                             00001800
         ST    R2,8(,R13)           .. save ..                          00001900
         LR    R13,R2                 .. areas                          00002000
         L     R2,0(,R1)        array size address                      00002100
         L     R7,0(,R2)        array size                              00002200
         LA    R7,1(,R7)        COBOL Sieve will overshoot by one       00002300
         ST    R7,SIZE          remember array size                     00002400
         GETMAIN R,LV=(R7)      obtain storage                          00002500
         ST    R1,ARRAY         remember address of gotten storage      00002600
         LR    R6,R1            address of array for MVCL               00002700
         XR    R8,R8            clear R8 for MVCL                       00002800
         L     R9,INIT          get initialization pattern for MVCL     00002900
         MVCL  R6,R8            initialize array                        00003000
         B     RETURN           return to caller                        00003100
*                                                                       00003200
* Query array element                                                   00003300
*                                                                       00003400
ISPRIME  SAVE  (14,12),,*       save registers                          00003500
         USING ISPRIME,R15      tell assembler of temporary base        00003600
         L     R12,LOADED       establish original base                 00003700
         DROP  R15              drop temporary base                     00003800
         LA    R2,SAVEA         chain ..                                00003900
         ST    R13,4(,R2)         .. the ..                             00004000
         ST    R2,8(,R13)           .. save ..                          00004100
         LR    R13,R2                 .. areas                          00004200
         LM    R2,R3,0(R1)      array index and return value addresses  00004300
         L     R2,0(,R2)        index value                             00004400
         BCTR  R2,0             COBOL index counts from 1, decrement    00004500
         L     R7,ARRAY         array address                           00004600
         LA    R7,0(R2,R7)      array element address                   00004700
         MVC   1(1,R3),0(R7)    return array element                    00004800
         B     RETURN           return to caller                        00004900
*                                                                       00005000
* Set array element                                                     00005100
*                                                                       00005200
SETPRIME SAVE  (14,12),,*       save registers                          00005300
         USING SETPRIME,R15     tell assembler of temporary base        00005400
         L     R12,LOADED       establish original base                 00005500
         DROP  R15              drop temporary base                     00005600
         LA    R2,SAVEA         chain ..                                00005700
         ST    R13,4(,R2)         .. the ..                             00005800
         ST    R2,8(,R13)           .. save ..                          00005900
         LR    R13,R2                 .. areas                          00006000
         LM    R2,R3,0(R1)      array index and return value addresses  00006100
         L     R2,0(,R2)        index value                             00006200
         BCTR  R2,0             COBOL index counts from 1, decrement    00006300
         L     R7,ARRAY         array address                           00006400
         LA    R7,0(R2,R7)      array element address                   00006500
         MVC   0(1,R7),1(R3)    set array element                       00006600
         B     RETURN           return to caller                        00006700
*                                                                       00006800
* Release array                                                         00006900
*                                                                       00007000
RLSPRIME SAVE  (14,12),,*       save registers                          00007100
         USING RLSPRIME,R15     tell assembler of temporary base        00007200
         L     R12,LOADED       establish original base                 00007300
         DROP  R15              drop temporary base                     00007400
         LA    R2,SAVEA         chain ..                                00007500
         ST    R13,4(,R2)         .. the ..                             00007600
         ST    R2,8(,R13)           .. save ..                          00007700
         LR    R13,R2                 .. areas                          00007800
         L     R1,ARRAY         array address                           00007900
         L     R7,SIZE          array size                              00008000
         FREEMAIN R,LV=(R7),A=(R1) release storage                      00008100
*                                                                       00008200
* Return to caller                                                      00008300
*                                                                       00008400
RETURN   L     R13,4(,R13)      caller's save area pointer              00008500
         RETURN (14,12),RC=0    restore registers and return            00008600
*                                                                       00008700
* Data area                                                             00008800
*                                                                       00008900
SAVEA    DS    18F              save area                               00009000
LOADED   DC    A(INIPRIME)      our own address                         00009100
INIT     DC    X'01000000'      array initialization pattern            00009200
ARRAY    DS    F                array address                           00009300
SIZE     DS    F                array size                              00009400
R0       EQU   0                Register  0                             00009500
R1       EQU   1                Register  1                             00009600
R2       EQU   2                Register  2                             00009700
R3       EQU   3                Register  3                             00009800
R4       EQU   4                Register  4                             00009900
R5       EQU   5                Register  5                             00010000
R6       EQU   6                Register  6                             00010100
R7       EQU   7                Register  7                             00010200
R8       EQU   8                Register  8                             00010300
R9       EQU   9                Register  9                             00010400
R10      EQU   10               Register 10                             00010500
R11      EQU   11               Register 11                             00010600
R12      EQU   12               Register 12                             00010700
R13      EQU   13               Register 13                             00010800
R14      EQU   14               Register 14                             00010900
R15      EQU   15               Register 15                             00011000
         END   INIPRIME         end of INIPRIME                         00011100
/*
//ASM.SYSGO DD UNIT=VIO,SPACE=(800,(1,1)),DISP=(,PASS),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800),DSN=&&INIPRIME
//PRIMES   EXEC COBUCG,
//         PARM.COB='FLAGW,LOAD,SUPMAP,SIZE=2048K,BUF=1024K'
//COB.SYSPUNCH DD DUMMY
//COB.SYSIN    DD *
   10 * //////////////////////////////////////////////////////////         PRIME
   20 * // Name: Peter M. Maurer                                           PRIME
   30 * // Program: Sieve of Eratosthenes                                  PRIME
   40 * // Due: Never                                                      PRIME
   50 * // Language: COBOL                                                 PRIME
   60 * //                                                                 PRIME
   70 * // Changes:                                                        PRIME
   80 * // - Juergen Winkelmann, 2014/10/25, o adaption to IBM OS COBOL    PRIME
   90 * //                                   o read limit from SYSIN       PRIME
  100 * //                                   o n**2 (sqrt) shortcut        PRIME
  110 * //                                   o skip even numbers           PRIME
  120 * //                                   o compact output format       PRIME
  130 * //                                   o dynamic prime flags         PRIME
  140 * //////////////////////////////////////////////////////////         PRIME
  150 ***                                                                  PRIME
  160 ***                                                                  PRIME
  170 ***                                                                  PRIME
  180  IDENTIFICATION DIVISION.                                            PRIME
  190  PROGRAM-ID.  'PRIMES'.                                              PRIME
  200 ***                                                                  PRIME
  210 ***                                                                  PRIME
  220 ***                                                                  PRIME
  230  ENVIRONMENT DIVISION.                                               PRIME
  240 **                                                                   PRIME
  250 **                                                                   PRIME
  260  CONFIGURATION SECTION.                                              PRIME
  270  SOURCE-COMPUTER.  IBM-360.                                          PRIME
  280  OBJECT-COMPUTER.  IBM-360.                                          PRIME
  290 **                                                                   PRIME
  300 **                                                                   PRIME
  310  INPUT-OUTPUT SECTION.                                               PRIME
  320  FILE-CONTROL.                                                       PRIME
  330      SELECT PRIMES-SYSIN                                             PRIME
  340         ASSIGN TO UT-S-SYSIN.                                        PRIME
  350 ***                                                                  PRIME
  360 ***                                                                  PRIME
  370 ***                                                                  PRIME
  380  DATA DIVISION.                                                      PRIME
  390 **                                                                   PRIME
  400 **                                                                   PRIME
  410  FILE SECTION.                                                       PRIME
  420  FD  PRIMES-SYSIN                                                    PRIME
  430      RECORDING MODE IS F                                             PRIME
  440      RECORD CONTAINS 80 CHARACTERS                                   PRIME
  450      BLOCK  CONTAINS  1 RECORDS                                      PRIME
  460      LABEL RECORDS ARE OMITTED                                       PRIME
  470      DATA RECORD IS PRIMES-SYSIN-RECORD.                             PRIME
  480  01  PRIMES-SYSIN-RECORD.                                            PRIME
  490   02 PRIMES-SYSIN-NUMBER PIC 99999999 OCCURS 10.                     PRIME
  500 **                                                                   PRIME
  510 **                                                                   PRIME
  520  WORKING-STORAGE SECTION.                                            PRIME
  530      77 I PIC 99999999 COMP VALUE 1.                                 PRIME
  540      77 J PIC 99999999 COMP.                                         PRIME
  550      77 K PIC 99999999 COMP VALUE 1.                                 PRIME
  560      77 N PIC 99999999 COMP.                                         PRIME
  570      77 N-2 PIC 99999999 COMP.                                       PRIME
  580      77 SQRTN PIC 99999999 COMP.                                     PRIME
  590      77 PRODUCT PIC 99999999 COMP.                                   PRIME
  600      77 ISPRIME PIC 9 VALUE 1 COMP.                                  PRIME
  610      77 NOTPRIME PIC 9 VALUE 0 COMP.                                 PRIME
  620      01 BLANK-LINE PIC A(160) VALUE ' '.                             PRIME
  630      01 OUT-INTEGER.                                                 PRIME
  640       02 SHOWIT PIC ZZZZZZZZ OCCURS 20.                              PRIME
  650      01 OUT REDEFINES OUT-INTEGER.                                   PRIME
  660       02 OUT-LINE PIC X(160).                                        PRIME
  670 ***                                                                  PRIME
  680 ***                                                                  PRIME
  690 ***                                                                  PRIME
  700  PROCEDURE DIVISION.                                                 PRIME
  710 **                                                                   PRIME
  720 **                                                                   PRIME
  730  MAIN-PART.                                                          PRIME
  740      OPEN INPUT PRIMES-SYSIN.                                        PRIME
  750      READ PRIMES-SYSIN AT END DISPLAY '** EOF on SYSIN **'.          PRIME
  760      MOVE PRIMES-SYSIN-NUMBER (1) TO N.                              PRIME
  770      CLOSE PRIMES-SYSIN.                                             PRIME
  780      SUBTRACT 2 FROM N GIVING N-2.                                   PRIME
  790 *                                                                    PRIME
  800      PERFORM NEXT-SQUARE UNTIL SQRTN GREATER N.                      PRIME
  810      MOVE I TO SQRTN.                                                PRIME
  820 *                                                                    PRIME
  830      MOVE 3 TO I.                                                    PRIME
  840      CALL 'INIPRIME' USING N.                                        PRIME
  850      PERFORM CHECK-NUMBER UNTIL I GREATER SQRTN OR EQUAL SQRTN.      PRIME
  860 *                                                                    PRIME
  870      MOVE 3 TO I.                                                    PRIME
  880      MOVE 2 TO J.                                                    PRIME
  890      MOVE J TO SHOWIT (K).                                           PRIME
  900      PERFORM PRINT UNTIL I GREATER N.                                PRIME
  910      CALL 'RLSPRIME'                                                 PRIME
  920 *                                                                    PRIME
  930      MOVE K TO SHOWIT (1).                                           PRIME
  940      MOVE N TO SHOWIT (2).                                           PRIME
  950      DISPLAY ' '.                                                    PRIME
  960      DISPLAY SHOWIT (1), ' primes up to ', SHOWIT (2), ' found.'.    PRIME
  970      STOP RUN.                                                       PRIME
  980 **                                                                   PRIME
  990 **                                                                   PRIME
 1000  CHECK-NUMBER.                                                       PRIME
 1010      PERFORM ADVANCE UNTIL I GREATER THAN SQRTN OR EQUAL TO SQRT     PRIME
 1020 -     N OR ISPRIME EQUAL TO 1.                                       PRIME
 1030      IF ISPRIME EQUAL TO 1                                           PRIME
 1040          ADD I I GIVING J                                            PRIME
 1050          MULTIPLY I BY I GIVING PRODUCT                              PRIME
 1060          PERFORM CROSS-OUT UNTIL PRODUCT GREATER THAN N.             PRIME
 1070      MOVE 0 TO ISPRIME.                                              PRIME
 1080 **                                                                   PRIME
 1090 **                                                                   PRIME
 1100  ADVANCE.                                                            PRIME
 1110      ADD 2 TO I.                                                     PRIME
 1120      CALL 'ISPRIME' USING I ISPRIME.                                 PRIME
 1130 **                                                                   PRIME
 1140 **                                                                   PRIME
 1150  CROSS-OUT.                                                          PRIME
 1160      CALL 'SETPRIME' USING PRODUCT NOTPRIME.                         PRIME
 1170      ADD J TO PRODUCT.                                               PRIME
 1180 **                                                                   PRIME
 1190 **                                                                   PRIME
 1200  NEXT-SQUARE.                                                        PRIME
 1210      ADD 1 TO I.                                                     PRIME
 1220      MULTIPLY I BY I GIVING SQRTN.                                   PRIME
 1230 **                                                                   PRIME
 1240 **                                                                   PRIME
 1250  PRINT.                                                              PRIME
 1260      CALL 'ISPRIME' USING I ISPRIME.                                 PRIME
 1270      IF ISPRIME EQUAL TO 1                                           PRIME
 1280          MOVE I TO SHOWIT (J)                                        PRIME
 1290          ADD 1 TO K                                                  PRIME
 1300          ADD 1 TO J                                                  PRIME
 1310          IF J GREATER 20                                             PRIME
 1320              DISPLAY OUT-LINE                                        PRIME
 1330              MOVE BLANK-LINE TO OUT-LINE                             PRIME
 1340              MOVE 1 TO J.                                            PRIME
 1350      IF I GREATER N-2 AND J NOT EQUAL 1 DISPLAY OUT-LINE.            PRIME
 1360      ADD 2 TO I.                                                     PRIME
/*
//COB.SYSLIB   DD DSNAME=SYS1.COBLIB,DISP=SHR
//GO.SYSLIN   DD
//            DD DSN=&&INIPRIME,DISP=(OLD,DELETE)
//GO.SYSOUT   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=161,BLKSIZE=16100)
//GO.SYSIN    DD *
    2000
/*
//
