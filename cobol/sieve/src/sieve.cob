       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIEVE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-LIMIT                   PIC 9999.  
       01 WS-RESULT                  PIC 999 OCCURS 1000 TIMES. 
       01 WS-COUNT                   PIC 9999.
       01 WS-ITERATOR                PIC 9999.
      *
       01 NUMBER-TABLE.
          05 NUMBER OCCURS 1000 TIMES INDEXED BY NUMBER-TABLE-INDEX.
             10 NUMBER-VALUE         PIC 9999.
             10 PRIME-NUMBER-SWITCH  PIC X    VALUE "Y".
      *
       PROCEDURE DIVISION.
      *
       SIEVE.
           PERFORM INITIALIZE-VALUES.
           PERFORM
              WITH TEST AFTER
              VARYING WS-COUNT FROM 2 BY 1
              UNTIL WS-COUNT >= WS-LIMIT 
                   MOVE WS-COUNT TO NUMBER-VALUE(NUMBER-TABLE-INDEX)
                   ADD 1 TO NUMBER-TABLE-INDEX
                   PERFORM DETECT-PRIME-NUMBER
           END-PERFORM.
      *    
       INITIALIZE-VALUES.
           MOVE ZEROES TO WS-RESULT.
           MOVE ZEROES TO NUMBER-TABLE.
           MOVE 2 TO WS-COUNT.
           MOVE ZERO TO WS-ITERATOR.
           MOVE 1 TO NUMBER-TABLE-INDEX.
      *    
       DETECT-PRIME-NUMBER.
           IF WS-COUNT IS NOT EQUAL TO 2 AND 3
              IF FUNCTION MOD(WS-COUNT, 2) IS NOT EQUAL TO ZERO
                 AND FUNCTION MOD(WS-COUNT, 3) IS NOT EQUAL TO ZERO 
                 PERFORM
                    WITH TEST BEFORE
                    VARYING WS-ITERATOR FROM 3 BY 3
                    UNTIL WS-ITERATOR >= FUNCTION SQRT(WS-COUNT)
                         IF FUNCTION MOD(WS-COUNT, WS-ITERATOR)
                            IS EQUAL TO ZERO
                            MOVE "N"
                               TO PRIME-NUMBER-SWITCH
                               (NUMBER-TABLE-INDEX)
                 END-PERFORM
              END-IF.
      *       