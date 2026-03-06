       IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-X         PIC 99V9.
       01 WS-Y         PIC 99V9.
       01 WS-DISTANCE  PIC 99V99999 VALUE ZERO.
       01 WS-RESULT    PIC 99.
      *
       PROCEDURE DIVISION.
      *
       DARTS.
           COMPUTE WS-DISTANCE =
              FUNCTION SQRT((WS-X ** 2) +(WS-Y ** 2)).
           DISPLAY WS-DISTANCE.
           IF (WS-DISTANCE <= 10.0)
              IF (WS-DISTANCE <= 5.0)
                 IF (WS-DISTANCE <= 1.0)
                    MOVE 10 TO WS-RESULT
                 ELSE
                    MOVE 5 TO WS-RESULT
                 END-IF
              ELSE
                 MOVE 1 TO WS-RESULT
              END-IF
           ELSE
              MOVE 0 TO WS-RESULT
           END-IF.
      *