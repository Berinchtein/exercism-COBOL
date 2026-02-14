        IDENTIFICATION DIVISION.
        PROGRAM-ID. ISOGRAM.
        ENVIRONMENT DIVISION.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      * 
       01 ISOGRAM-PHRASE-SWITCH         PIC X     VALUE "Y".
          88 IS-ISOGRAM-PHRASE                    VALUE "Y".
      * 
       01 WS-PHRASE                     PIC X(60).
       01 WS-PHRASE-TABLE.
          05 WS-PHRASE-CHAR             PIC X OCCURS 60 TIMES.
      * 
       01 WS-CHARACTER-OCCURENCE-TABLE.
          05 WS-CHARACTER-OCCURENCE     PIC 9 OCCURS 60 TIMES.
      *
       01 WS-RESULT                     PIC 99.
       01 WS-ITERATOR-1                 PIC 99.
       01 WS-ITERATOR-2                 PIC 99.
      * 
        PROCEDURE DIVISION.
      * 
       ISOGRAM.
           PERFORM
              WITH TEST AFTER
              VARYING WS-ITERATOR-1 FROM 1 BY 1
              UNTIL WS-ITERATOR-1 >= LENGTH OF WS-PHRASE
              OR NOT IS-ISOGRAM-PHRASE
      *          
                   PERFORM
                      WITH TEST AFTER
                      VARYING WS-ITERATOR-2 FROM 1 BY 1
                      UNTIL WS-ITERATOR-2 >=(WS-ITERATOR-1 - 1)
                      OR NOT IS-ISOGRAM-PHRASE
      *       
                           IF (WS-PHRASE-CHAR(WS-ITERATOR-1)
                              = WS-PHRASE-CHAR(WS-ITERATOR-2))
                              MOVE "N" TO ISOGRAM-PHRASE-SWITCH
      *
                   END-PERFORM 
      *
           END-PERFORM.