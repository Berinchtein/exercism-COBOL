       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAMMING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-DNA-1            PIC X(32).
       01 WS-DNA-1-TABLE REDEFINES WS-DNA-1.
          05 WS-DNA-1-STRAND  PIC X OCCURS 32 TIMES.
       01 WS-DNA-1-LENGTH     PIC 9(2)  VALUE 0.
      *    
       01 WS-DNA-2            PIC X(32).
       01 WS-DNA-2-TABLE REDEFINES WS-DNA-2.
          05 WS-DNA-2-STRAND  PIC X OCCURS 32 TIMES.
       01 WS-DNA-2-LENGTH     PIC 9(2)  VALUE 0.
      *    
       01 WS-ITERATOR         PIC 9(2)  VALUE 0.
       01 WS-HAMMING          PIC 9(2)  VALUE 0.
       01 WS-ERROR            PIC X(31) VALUE SPACES.
      *
       PROCEDURE DIVISION.
      *
       HAMMING.
           PERFORM INITIALIZE-VALUES.
           IF (WS-DNA-1-LENGTH IS NOT EQUAL TO WS-DNA-2-LENGTH)
              MOVE "Strands must be of equal length" TO WS-ERROR
           ELSE
              PERFORM COUNT-HAMMING-DISTANCE
                 WITH TEST BEFORE
                 VARYING WS-ITERATOR FROM 1 BY 1
                 UNTIL WS-ITERATOR > WS-DNA-1-LENGTH
           END-IF.
           DISPLAY "Hamming Distance is: " WS-HAMMING.
           DISPLAY WS-ERROR.
           EXIT.
      *    
       INITIALIZE-VALUES.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1))
              TO WS-DNA-1-LENGTH.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-2))
              TO WS-DNA-2-LENGTH.
           MOVE 0 TO WS-ITERATOR.
           MOVE 0 TO WS-HAMMING.
           MOVE SPACES TO WS-ERROR.
      *    
       COUNT-HAMMING-DISTANCE.
           IF (WS-DNA-1-STRAND(WS-ITERATOR) IS NOT EQUAL TO
              WS-DNA-2-STRAND(WS-ITERATOR))
              ADD 1 TO WS-HAMMING 
           END-IF.
      *
