       IDENTIFICATION DIVISION.                                         00000100
       PROGRAM-ID.  'ARRAY2'.                                           00000200
      *================================================                 00000300
      * PARSE A FEW CSV FIELDS INTO DISCRETE COBOL                      00000400
      * FIELDS AND WRITE TO AN OUTFILE.                                 00000500
      * CORRECT ISSUES WITH ARRAY1, BUT KEEP AS BACKUP                  00000600
      *================================================                 00000700
       ENVIRONMENT DIVISION.                                            00000800
       CONFIGURATION SECTION.                                           00000900
       SOURCE-COMPUTER.  IBM-360.                                       00001000
       OBJECT-COMPUTER.  IBM-360.                                       00001100
       INPUT-OUTPUT SECTION.                                            00001200
       FILE-CONTROL.                                                    00001300
           SELECT TERM-IN ASSIGN TO UT-S-SYSIN.                         00001400
           SELECT TERM-OUT ASSIGN TO UT-S-BLK3600.                      00001500
                                                                        00001600
  007  DATA DIVISION.                                                   00001700
       FILE SECTION.                                                    00001800
                                                                        00001900
       FD  TERM-IN                                                      00002000
           RECORD CONTAINS 80 CHARACTERS.                               00002100
       01  REC-IN.                                                      00002200
           05  REC80     PIC X(80).                                     00002300
                                                                        00002400
       FD  TERM-OUT                                                     00002500
           RECORD CONTAINS 80 CHARACTERS                                00002600
           BLOCK CONTAINS 45 RECORDS.                                   00002700
       01  REC-OUT.                                                     00002800
           05  BOWLER    PIC A(10) VALUE SPACES.                        00002900
           05  TEAM-ID   PIC XX    VALUE SPACES.                        00003000
           05  WEEK-NO   PIC XX    VALUE SPACES.                        00003100
           05  SCORE-1   PIC XXX   VALUE ZEROES.                        00003200
           05  SCORE-2   PIC XXX   VALUE ZEROES.                        00003300
           05  SCORE-3   PIC XXX   VALUE ZEROES.                        00003400
           05  DAY-AVG   PIC XXX   VALUE ZEROES.                        00003500
           05  FILLER    PIC X(54).                                     00003600
                                                                        00003700
  008  WORKING-STORAGE SECTION.                                         00003800
                                                                        00003900
       01  TBL-DIM.                                                     00004000
           05  WS-DIM PIC 999 OCCURS 7 TIMES.                           00004100
                                                                        00004200
       01  TBL-STRING.                                                  00004300
           05 STRING PIC X OCCURS 256 TIMES.                            00004400
                                                                        00004500
       01  WC   PIC 999 VALUE 0.                                        00004600
       01  I    PIC 999 VALUE 0.                                        00004700
       01  M    PIC 999 VALUE 0.                                        00004800
       01  LEN  PIC 999 VALUE 0.                                        00004900
       01  POS  PIC 999 VALUE 0.                                        00005000
       01  EOF  PIC X.                                                  00005100
       88  INPUT-EOF VALUE 'Y'.                                         00005200
                                                                        00005300
       01  TBL-C20.                                                     00005400
           05 WS-C20   PIC X OCCURS 20 TIMES.                           00005500
       01  TBL-OUT.                                                     00005600
           05 WS-OUT   PIC X OCCURS 20 TIMES.                           00005700
                                                                        00005800
       01  WS-COMMA PIC X VALUE ','.                                    00005900
                                                                        00006000
       PROCEDURE DIVISION.                                              00006100
       MAIN.                                                            00006200
           PERFORM INIT.                                                00006300
           OPEN INPUT TERM-IN.                                          00006400
           OPEN OUTPUT TERM-OUT.                                        00006500
           PERFORM READFILE.                                            00006600
           IF NOT INPUT-EOF                                             00006700
               PERFORM PROCESS-RECORDS UNTIL INPUT-EOF.                 00006800
           PERFORM CLEANUP.                                             00006900
                                                                        00007000
       INIT.                                                            00007100
           MOVE '010002002003003003003' TO TBL-DIM.                     00007200
           MOVE ZEROES TO SCORE-1, SCORE-2, SCORE-3, DAY-AVG.           00007300
                                                                        00007400
       READFILE.                                                        00007500
           READ TERM-IN AT END                                          00007600
               MOVE 'Y' TO EOF.                                         00007700
                                                                        00007800
       PROCESS-RECORDS.                                                 00007900
           MOVE 1 TO LEN, WC, I, M, POS.                                00008000
           MOVE REC80  TO TBL-STRING.                                   00008100
           PERFORM PARSE-DATA VARYING POS FROM 1 BY 1                   00008200
             UNTIL POS > 80.                                            00008300
           PERFORM READFILE.                                            00008400
                                                                        00008500
       PARSE-DATA.                                                      00008600
           MOVE STRING(POS) TO WS-C20(I).                               00008700
           IF WS-C20(I) = WS-COMMA THEN                                 00008800
             PERFORM FOUND-COMMA                                        00008900
             MOVE 1 TO I                                                00009000
           ELSE                                                         00009100
             ADD 1 TO I.                                                00009200
                                                                        00009300
       FOUND-COMMA.                                                     00009400
             MOVE SPACE TO WS-C20(I)                                    00009500
             COMPUTE LEN = I - 1                                        00009600
             MOVE SPACES TO TBL-OUT                                     00009700
             MOVE 1 TO M                                                00009800
             PERFORM MOVE-ARRAY VARYING M FROM 1 BY 1                   00009900
               UNTIL M > LEN                                            00010000
             PERFORM POP-WORD.                                          00010100
                                                                        00010200
       MOVE-ARRAY.                                                      00010300
           MOVE WS-C20(M) TO WS-OUT(M).                                 00010400
                                                                        00010500
       POP-WORD.                                                        00010600
           DISPLAY TBL-OUT, WC, I.                                      00010700
           IF WC = 1 THEN                                               00010800
             MOVE TBL-OUT   TO BOWLER.                                  00010900
           IF WC = 2 THEN                                               00011000
             MOVE TBL-OUT    TO TEAM-ID.                                00011100
           IF WC = 3 THEN                                               00011200
             MOVE TBL-OUT     TO WEEK-NO.                               00011300
           IF WC = 4 THEN                                               00011400
             MOVE TBL-OUT     TO SCORE-1.                               00011500
           IF WC = 5 THEN                                               00011600
             MOVE TBL-OUT     TO SCORE-2.                               00011700
           IF WC = 6 THEN                                               00011800
             MOVE TBL-OUT     TO SCORE-3.                               00011900
           IF WC = 7 THEN                                               00012000
             MOVE TBL-OUT     TO DAY-AVG                                00012100
             PERFORM WRITEFILE.                                         00012200
           ADD 1 TO WC.                                                 00012300
                                                                        00012400
       WRITEFILE.                                                       00012500
           WRITE REC-OUT.                                               00012600
           MOVE 1 TO WC, I.                                             00012700
           MOVE SPACES TO TBL-C20, TBL-OUT.                             00012800
           MOVE 80 TO POS.                                              00012900
           MOVE ZEROES TO SCORE-1, SCORE-2, SCORE-3, DAY-AVG.           00013000
                                                                        00013100
       CLEANUP.                                                         00013200
           CLOSE TERM-IN, TERM-OUT.                                     00013300
           STOP RUN.                                                    00013400
