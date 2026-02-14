       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-YEAR                 PIC 9(4).
       01 DIVISORS.
          05 WS-LEAP-DIVISOR-4    PIC 9    VALUE 4.
          05 WS-LEAP-DIVISOR-100  PIC 9    VALUE 100.
          05 WS-LEAP-DIVISOR-400  PIC 9    VALUE 400.
       01 WS-LEAP-QUOTIENT        PIC 9(4).
       01 WS-LEAP-REMAINDER       PIC 9.
       01 WS-RESULT               PIC 9    VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       LEAP.
      *
           DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-100
              GIVING WS-LEAP-QUOTIENT
              REMAINDER WS-LEAP-REMAINDER.
           IF (WS-LEAP-REMAINDER = ZERO)
              DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-400
                 GIVING WS-LEAP-QUOTIENT
                 REMAINDER WS-LEAP-REMAINDER
              IF (WS-LEAP-REMAINDER = ZERO)
                 MOVE 1 TO WS-RESULT
              END-IF
           ELSE
              DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-4
                 GIVING WS-LEAP-QUOTIENT
                 REMAINDER WS-LEAP-REMAINDER
              IF (WS-LEAP-REMAINDER = ZERO)
                 MOVE 1 TO WS-RESULT
              END-IF
           END-IF.
      *
       LEAP-EXIT.
           EXIT.