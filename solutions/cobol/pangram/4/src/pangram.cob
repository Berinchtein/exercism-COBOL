        IDENTIFICATION DIVISION.
        PROGRAM-ID. PANGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * 
       01 WS-SENTENCE                   PIC X(60).
       01 WS-RESULT                     PIC 9.
       01 WS-LETTER-COUNT               PIC 99.
          88 PANGRAM-ACQUIRED                     VALUE 26.
       01 WS-SPECIFIC-LETTER-OCCURENCE  PIC 99.
      *
       01 ALPHABET-TABLE-VALUES.
          05 FILLER                     PIC X     VALUE "A". 
          05 FILLER                     PIC X     VALUE "B". 
          05 FILLER                     PIC X     VALUE "C". 
          05 FILLER                     PIC X     VALUE "D". 
          05 FILLER                     PIC X     VALUE "E". 
          05 FILLER                     PIC X     VALUE "F". 
          05 FILLER                     PIC X     VALUE "G". 
          05 FILLER                     PIC X     VALUE "H". 
          05 FILLER                     PIC X     VALUE "I". 
          05 FILLER                     PIC X     VALUE "J". 
          05 FILLER                     PIC X     VALUE "K". 
          05 FILLER                     PIC X     VALUE "L". 
          05 FILLER                     PIC X     VALUE "M". 
          05 FILLER                     PIC X     VALUE "N". 
          05 FILLER                     PIC X     VALUE "O". 
          05 FILLER                     PIC X     VALUE "P". 
          05 FILLER                     PIC X     VALUE "Q". 
          05 FILLER                     PIC X     VALUE "R". 
          05 FILLER                     PIC X     VALUE "S". 
          05 FILLER                     PIC X     VALUE "T". 
          05 FILLER                     PIC X     VALUE "U". 
          05 FILLER                     PIC X     VALUE "V". 
          05 FILLER                     PIC X     VALUE "W". 
          05 FILLER                     PIC X     VALUE "X". 
          05 FILLER                     PIC X     VALUE "Y". 
          05 FILLER                     PIC X     VALUE "Z". 
      *   
       01 ALPHABET-TABLE REDEFINES ALPHABET-TABLE-VALUES.
          05 ALPHABET-LETTER            PIC X OCCURS 26 TIMES
                INDEXED BY ALPHABETIC-TABLE-INDEX.
      *   
        PROCEDURE DIVISION.
      * 
       PANGRAM.
           PERFORM CHECK-LETTER
              WITH TEST AFTER
              VARYING ALPHABETIC-TABLE-INDEX FROM 1 BY 1
              UNTIL ALPHABETIC-TABLE-INDEX >= 26.
           IF (PANGRAM-ACQUIRED)
              MOVE 1 TO WS-RESULT
           ELSE
              MOVE 0 TO WS-RESULT
           END-IF.
           EXIT.
      *
       CHECK-LETTER.
           INSPECT WS-SENTENCE TALLYING WS-SPECIFIC-LETTER-OCCURENCE
              FOR ALL ALPHABET-LETTER(ALPHABETIC-TABLE-INDEX).
           INSPECT WS-SENTENCE TALLYING WS-SPECIFIC-LETTER-OCCURENCE
              FOR ALL FUNCTION
              LOWER-CASE(ALPHABET-LETTER(ALPHABETIC-TABLE-INDEX)).
           IF (WS-SPECIFIC-LETTER-OCCURENCE IS GREATER THAN ZERO)
              ADD 1 TO WS-LETTER-COUNT.
      *