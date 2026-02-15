       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO-FER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-NAME    PIC X(16) VALUE SPACES.
       01 WS-RESULT  PIC X(64) VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *
       TWO-FER.
           IF (WS-NAME IS EQUAL TO SPACES)
              MOVE "One for you, one for me." TO WS-RESULT
           ELSE
              STRING "One for " DELIMITED BY SIZE
                     FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
                     ", one for me." DELIMITED BY SIZE
                 INTO WS-RESULT
           END-IF.
           DISPLAY WS-RESULT.
      *    
