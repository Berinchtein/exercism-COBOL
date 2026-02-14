        IDENTIFICATION DIVISION.
        PROGRAM-ID. ISOGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * 
       01 WS-PHRASE                     PIC X(60).
       01 WS-PHRASE-TABLE REDEFINES WS-PHRASE.
          05 WS-PHRASE-CHAR             PIC X OCCURS 60 TIMES.
      *
       01 WS-RESULT                     PIC 9     VALUE 1.
          88 ISOGRAM-FOUND                        VALUE 1.
       01 WS-ITERATOR-1                 PIC 99.
       01 WS-ITERATOR-2                 PIC 99.
      * 
        PROCEDURE DIVISION.
      * 
       ISOGRAM.
           MOVE 'isogram' TO WS-PHRASE.
           MOVE 1 TO WS-RESULT.
           PERFORM
              WITH TEST AFTER
              VARYING WS-ITERATOR-1 FROM 1 BY 1
              UNTIL WS-ITERATOR-1 >= LENGTH OF WS-PHRASE
              OR NOT ISOGRAM-FOUND
      *          
                   PERFORM
                      WITH TEST AFTER
                      VARYING WS-ITERATOR-2 FROM 1 BY 1
                      UNTIL WS-ITERATOR-2 >=(WS-ITERATOR-1 - 1)
                      OR NOT ISOGRAM-FOUND
      *       
                           DISPLAY WS-PHRASE-CHAR(WS-ITERATOR-1)
                           DISPLAY WS-PHRASE-CHAR(WS-ITERATOR-2)
                           IF (WS-PHRASE-CHAR(WS-ITERATOR-1)
                              = WS-PHRASE-CHAR(WS-ITERATOR-2))
                              MOVE 0 TO WS-RESULT
      *
                   END-PERFORM 
      *
           END-PERFORM.