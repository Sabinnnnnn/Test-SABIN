       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MIG999.
       AUTHOR.        Henrik Jensen.
       INSTALLATION.  BEC Roskilde.
       DATE-WRITTEN.  20 Mar 2025.

      ******************************************************************
      *
      *  Program description:
      *  --------------------
      *
      *
      *
      *
      ******************************************************************
      *
      *  Speciel conditions:
      *  Dependencies:
      *  Checkpoint/restart:      Yes
      *
      ******************************************************************
      *  Further description:
      *  --------------------
      *  References to further descriptions.
      *
      *
      ******************************************************************
      *  Changes:
      *  --------
      *  Date     Id          Init Description
      *  20250320             HRJ  Established
      *
      *
      ******************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT IBM-SAMP-ACT                   ASSIGN TO TTDF1010.
       DATA DIVISION.

      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*

       FD  IBM-SAMP-ACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  IBM-SAMP-ACT-RECORD                   PIC X(81).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       01  DIVERSE.
         03  PROGRAM-NAVN                   PIC  X(08) VALUE 'MIG999'.
         03  EOF-IBM-SAMP-ACT-SW            PIC  9     VALUE 0.
           88  EOF-IBM-SAMP-ACT                        VALUE 1.
           88  NOT-EOF-IBM-SAMP-ACT                    VALUE 0.
         03  TTDF1010-ROWS-INSERTED         PIC S9(09) COMP-3 VALUE 0.

           COPY TEKABNAR.
           COPY COMPILED.
           COPY USP101AR.
      ***  DB2 COPYBOOKS ***
      ***  DB2 SQLCA ***
           COPY SQLCA.
           COPY SQLDIAG.

       01  MTDF1010.
           COPY MTDF1010 REPLACING ==:PFX:==    BY ==TDF101M==
                                   ==:ROWSET:== BY ==100==.
           COPY DTDF1010.
       01  TTDF1010.
           COPY TTDF1010 REPLACING ==:PFX:==    BY ==TDF101==.

      ***  Areas for checkpoint/restart ***
           COPY ARCDB2WK.
        05  FILLER                     PIC X.
        05  ARC-CHECKPOINTED-AREA-SLUT PIC X.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
           PERFORM DISPLAY-START
      *    INITIER CHECKPOINT/RESTART
           COPY ARCDB2XR.

           PERFORM READ-FILE-IBM-SAMP-ACT

           DISPLAY PROGRAM-NAVN 'END'
           GOBACK
           .

       READ-FILE-IBM-SAMP-ACT SECTION.
      *----------------------------------------------------------------*
      *    Read and handle IBM-SAMP-ACT
      *----------------------------------------------------------------*

           SET NOT-EOF-IBM-SAMP-ACT TO TRUE
           MOVE 0 TO TDF101M-ROW-COUNT

           OPEN INPUT IBM-SAMP-ACT

           PERFORM READ-IBM-SAMP-ACT

           PERFORM
              WITH TEST BEFORE
              UNTIL EOF-IBM-SAMP-ACT

              PERFORM PROCESS-IBM-SAMP-ACT
              PERFORM READ-IBM-SAMP-ACT

           END-PERFORM

           CLOSE IBM-SAMP-ACT

           IF TDF101M-ROW-LOOP > 0
              PERFORM INSERT-TTDF1010
           END-IF
           .

       READ-IBM-SAMP-ACT SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*

           READ IBM-SAMP-ACT INTO TTDF1010
              AT END SET EOF-IBM-SAMP-ACT TO TRUE
           END-READ
           .

       PROCESS-IBM-SAMP-ACT SECTION.
      *----------------------------------------------------------------*
      *    Handle one record from IBM-SAMP-ACT
      *----------------------------------------------------------------*

           DISPLAY 'TTDF1010=' TTDF1010

           IF TDF101M-ROW-COUNT + 1 > TDF101M-ROW-SET
              PERFORM INSERT-TTDF1010
              MOVE 0 TO TDF101M-ROW-COUNT
           END-IF

           COMPUTE TDF101M-ROW-COUNT = TDF101M-ROW-COUNT + 1

           MOVE TDF101-ACTNO
             TO TDF101M-ACTNO (TDF101M-ROW-COUNT)
           MOVE TDF101-ACTKWD
             TO TDF101M-ACTKWD (TDF101M-ROW-COUNT)
           MOVE TDF101-ACTDESC-LEN
             TO TDF101M-ACTDESC-LEN (TDF101M-ROW-COUNT)
           MOVE TDF101-ACTDESC-TEXT
             TO TDF101M-ACTDESC-TEXT (TDF101M-ROW-COUNT)
           MOVE TDF101-AJOUR-ID
             TO TDF101M-AJOUR-ID (TDF101M-ROW-COUNT)
           MOVE TDF101-AJOUR-ID-TYP
             TO TDF101M-AJOUR-ID-TYP (TDF101M-ROW-COUNT)
           MOVE TDF101-AJOUR-PGM
             TO TDF101M-AJOUR-PGM (TDF101M-ROW-COUNT)
           MOVE TDF101-AJOUR-TMS
             TO TDF101M-AJOUR-TMS (TDF101M-ROW-COUNT)
           .

       INSERT-TTDF1010 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*

           EXEC SQL
              INSERT
              INTO TTDF1010SAMP_ACT
              ( ACTNO
              , ACTKWD
              , ACTDESC
              , AJOUR_ID
              , AJOUR_ID_TYP
              , AJOUR_PGM
              , AJOUR_TMS
              )
              VALUES
              (:TDF101M-ACTNO
              ,:TDF101M-ACTKWD
              ,RTRIM(:TDF101M-ACTDESC)
              ,:TDF101M-AJOUR-ID
              ,:TDF101M-AJOUR-ID-TYP
              ,:TDF101M-AJOUR-PGM
              ,:TDF101M-AJOUR-TMS
              )
              FOR :TDF101M-ROW-COUNT ROWS
           END-EXEC

           EVALUATE SQLCODE
              WHEN 0
                 COMPUTE TTDF1010-ROWS-INSERTED =
                         TTDF1010-ROWS-INSERTED +
                         SQLERRD(3)
              WHEN +354
              WHEN OTHER
                 IF SQLCODE = +354
                    PERFORM SQL-GET-DIAGNOSTICS
                 END-IF
                 MOVE 0001 TO TEKABN-ABEND-CODE
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'DB2 ERROR #0002 IN INSERT TTDF1010SAMP_ACT'
                 MOVE '#0001 INSERT TTDF1010SAMP_ACT'
                   TO USP101-SQL-ID
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'TDF101M-ROW-COUNT="'
                         TDF101M-ROW-COUNT '"'
                 PERFORM
                    VARYING TDF101M-ROW-LOOP
                    FROM    1 BY 1
                    UNTIL   TDF101M-ROW-LOOP > TDF101M-ROW-COUNT

                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'ACTNO="'
                            TDF101M-ACTNO (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'ACTKWD="'
                            TDF101-ACTKWD (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'ACTDESC="'
                            TDF101-ACTDESC (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'AJOUR_ID="'
                            TDF101-AJOUR-ID (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'AJOUR_ID_TYP="'
                            TDF101-AJOUR-ID-TYP (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'AJOUR_PGM="'
                            TDF101-AJOUR-PGM (TDF101M-ROW-LOOP) '"'
                    DISPLAY PROGRAM-NAVN ' '
                            FUNCTION CURRENT-DATE (1:14) ' - '
                           'AJOUR_TMS="'
                            TDF101-AJOUR-TMS (TDF101M-ROW-LOOP) '"'
                 END-PERFORM
                 PERFORM SQL-FEJL-RUTINE
           END-EVALUATE
           .

       DISPLAY-START SECTION.
      *----------------------------------------------------------------*
      *    Show a message that tHE PROGRAM IS Started and show the
      *    compile-time.
      *----------------------------------------------------------------*

           MOVE PROGRAM-NAVN TO COMPILED-PROGRAM
           MOVE WHEN-COMPILED TO COMPILED-WHEN-COMPILED
           MOVE CORR COMPILED-WHEN-COMPILED TO COMPILED-DATA-VIS
           DISPLAY COMPILED-DATA-VIS
           SET COMPILED-TIME-VIST TO TRUE
           .

       SQL-GET-DIAGNOSTICS SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*

           EXEC SQL
                GET DIAGNOSTICS
                   :NUMBERS = NUMBER
           END-EXEC

           PERFORM
              VARYING CONDITION-NUMBER
              FROM    1 BY 1
              UNTIL   CONDITION-NUMBER > NUMBERS

              EXEC SQL
                   GET DIAGNOSTICS CONDITION
                      :DB2-DIAGNOSTICS.CONDITION-NUMBER
                      :DB2-DIAGNOSTICS.DB2-ROW-NUMBER = DB2_ROW_NUMBER
                     ,:DB2-DIAGNOSTICS.DB2-RETURNED-SQLCODE =
                       DB2_RETURNED_SQLCODE
                     ,:DB2-DIAGNOSTICS.DB2-MESSAGE-ID = DB2_MESSAGE_ID
                     ,:DB2-DIAGNOSTICS.MESSAGE-TEXT = MESSAGE_TEXT
              END-EXEC

              IF DB2-RETURNED-SQLCODE = +100
                 EXIT SECTION
              END-IF

              IF DB2-RETURNED-SQLCODE NOT = +354
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'CONDITION-NUMBER="'
                         CONDITION-NUMBER '"'
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'DB2-ROW-NUMBER="'
                         DB2-ROW-NUMBER '"'
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'DB2-RETURNED-SQLCODE="'
                         DB2-RETURNED-SQLCODE '"'
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'DB2-MESSAGE-ID="'
                         DB2-MESSAGE-ID '"'
                 DISPLAY PROGRAM-NAVN ' '
                         FUNCTION CURRENT-DATE (1:14) ' - '
                        'MESSAGE-TEXT="'
                         MESSAGE-TEXT-TEXT(1:MESSAGE-TEXT-LEN) '"'
              END-IF

           END-PERFORM
           .

       SQL-FEJL-RUTINE SECTION.
      *----------------------------------------------------------------*
      * DB2 ERrORHANDLING
      *----------------------------------------------------------------*
           DISPLAY 'DB2 ERROR'
           CALL 'CTEKCALL' USING CUSP1010 SQLCA USP101-AREA
           PERFORM ABEND
           .

      *----------------------------------------------------------------*
      * Checkpoint/restart
      * ARC-CHECKPOINT SECTION.
      *----------------------------------------------------------------*
           COPY ARCDB2CK.
       ABEND SECTION.
      *----------------------------------------------------------------*
      * Abend
      *----------------------------------------------------------------*
           DISPLAY PROGRAM-NAVN ' ended in error.'
           CALL 'CTEKCALL' USING CTEKABN0 TEKABN-AREA
           .
