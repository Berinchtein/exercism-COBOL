       IDENTIFICATION DIVISION.
       PROGRAM-ID. RAINDROPS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-NUMBER         PIC 9(4).
       01 WS-RESULT         PIC X(20) VALUE SPACES.
       01 WS-POINTER-FIELD  PIC 9(2)  VALUE 1.
      *
       PROCEDURE DIVISION.
      *
       RAINDROPS.
           MOVE 5 TO WS-NUMBER.
           PERFORM INITIALIZE-VALUES.
           IF (FUNCTION MOD(WS-NUMBER, 3) = 0)
              DISPLAY 3
              STRING "Pling" DELIMITED BY SIZE
                     WS-RESULT DELIMITED BY SIZE
                 INTO WS-RESULT
                 WITH POINTER WS-POINTER-FIELD
           END-IF.
           IF (FUNCTION MOD(WS-NUMBER, 5) = 0)
              STRING "Plang" DELIMITED BY SIZE
                     WS-RESULT DELIMITED BY SIZE
                 INTO WS-RESULT
                 WITH POINTER WS-POINTER-FIELD
           END-IF.
           IF (FUNCTION MOD(WS-NUMBER, 7) = 0)
              STRING "Plong" DELIMITED BY SIZE
                     WS-RESULT DELIMITED BY SIZE
                 INTO WS-RESULT
                 WITH POINTER WS-POINTER-FIELD
           END-IF.
           IF (FUNCTION MOD(WS-NUMBER, 3) NOT = 0
              AND FUNCTION MOD(WS-NUMBER, 5) NOT = 0
              AND FUNCTION MOD(WS-NUMBER, 7) NOT = 0)
              MOVE WS-NUMBER TO WS-RESULT
           END-IF.
           DISPLAY WS-RESULT.
           EXIT.
      *
       INITIALIZE-VALUES.
           MOVE SPACES TO WS-RESULT.
           MOVE 1 TO WS-POINTER-FIELD.
      *
      