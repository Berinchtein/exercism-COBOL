       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-YEAR                   PIC 9(4).
       01 DIVISORS.
          05 WS-LEAP-DIVISOR-4      PIC 9    VALUE 4.
          05 WS-LEAP-DIVISOR-100    PIC 9(3) VALUE 100.
          05 WS-LEAP-DIVISOR-400    PIC 9(3) VALUE 400.
       01 REMAINDERS.                                                                              
          05 WS-LEAP-REMAINDER-4    PIC 9.
          05 WS-LEAP-REMAINDER-100  PIC 9(3).
          05 WS-LEAP-REMAINDER-400  PIC 9(3).
       01 WS-RESULT                 PIC 9    VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       LEAP.
      *
           DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-4
              GIVING WS-RESULT
              REMAINDER WS-LEAP-REMAINDER-4.
           DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-100
              GIVING WS-RESULT
              REMAINDER WS-LEAP-REMAINDER-100.
           DIVIDE WS-YEAR BY WS-LEAP-DIVISOR-400
              GIVING WS-RESULT
              REMAINDER WS-LEAP-REMAINDER-400.
      *
           IF (WS-LEAP-REMAINDER-4 IS ZERO)
              IF (WS-LEAP-REMAINDER-100 IS NOT ZERO)
                 MOVE 1 TO WS-RESULT 
              ELSE
                 IF (WS-LEAP-REMAINDER-400 IS ZERO)
                    MOVE 1 TO WS-RESULT
                 ELSE
                    MOVE 0 TO WS-RESULT 
                 END-IF
              END-IF
           ELSE
              MOVE 0 TO WS-RESULT
           END-IF.
      *
           CONTINUE.
      *
       LEAP-EXIT.
           EXIT.