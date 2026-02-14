       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-YEAR            PIC 9(4).
       01 WS-LEAP-DIVISOR    PIC 9    VALUE 4.
       01 WS-LEAP-QUOTIENT   PIC 9(4).
       01 WS-LEAP-REMAINDER  PIC 9.
       01 WS-RESULT          PIC 9    VALUE 0.
      *
       PROCEDURE DIVISION.
       LEAP.
           DIVIDE WS-YEAR BY WS-LEAP-DIVISOR
              GIVING WS-LEAP-REMAINDER
              REMAINDER WS-LEAP-REMAINDER.
           IF (WS-LEAP-REMAINDER = 0)
              MOVE 1 TO WS-RESULT 
           END-IF.
           CONTINUE.
       LEAP-EXIT.
           EXIT.