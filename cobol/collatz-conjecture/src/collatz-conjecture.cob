       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLLATZ-CONJECTURE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-NUMBER  PIC S9(8).
       01 WS-STEPS   PIC 9(4)  VALUE 0.
       01 WS-ERROR   PIC X(35).
      *
       PROCEDURE DIVISION.
      *
       COLLATZ-CONJECTURE.
           IF (WS-NUMBER < 1)
              MOVE "Only positive integers are allowed" TO WS-ERROR
           ELSE
              PERFORM
                 WITH TEST BEFORE
                 UNTIL WS-NUMBER = 1
                      IF (FUNCTION MOD(WS-NUMBER 2) = 0)
                         DIVIDE WS-NUMBER BY 2 GIVING WS-NUMBER
                      ELSE
                         MULTIPLY WS-NUMBER BY 3 GIVING WS-NUMBER
                         ADD 1 TO WS-NUMBER 
                      END-IF
                      ADD 1 TO WS-STEPS
              END-PERFORM
           END-IF.
           DISPLAY WS-STEPS.
           DISPLAY WS-ERROR.
           EXIT.
      *
       INITIALIZE-VALUES.
           MOVE ZEROES TO WS-STEPS. *> Redundant
           MOVE SPACES TO WS-ERROR.
      *