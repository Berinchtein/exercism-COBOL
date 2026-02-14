       IDENTIFICATION DIVISION.

        

          
          
        PROGRAM-ID. PANGRAM.

        

          
          
        ENVIRONMENT DIVISION.

        

          
          
        DATA DIVISION.

        

          
          
        WORKING-STORAGE SECTION.

        

          
          
       01 WS-SENTENCE  PIC X(60).

        

          
          
       01 WS-RESULT    PIC 9.

        

          
          
       01 WS-ORD       PIC 99.

        

          
          
       01 WS-CNT       PIC 99.

        

          
          
        PROCEDURE DIVISION.

        

          
          
       PANGRAM.

        

          
          
           MOVE 1 TO WS-RESULT.

        

          
          
           MOVE FUNCTION LOWER-CASE(WS-SENTENCE) TO WS-SENTENCE.

        

          
          
           PERFORM VARYING WS-ORD FROM 1 BY 1
              UNTIL WS-ORD = 26 OR WS-RESULT = 0

        

          
          
                   MOVE 0 TO WS-CNT

        

          
          
                   INSPECT WS-SENTENCE TALLYING WS-CNT
                      FOR ALL FUNCTION CHAR(98 + WS-ORD)

        

          
          
                   IF WS-CNT = 0 THEN

        

          
          
                      MOVE 0 TO WS-RESULT

        

          
          
                   END-IF

        

          
          
           END-PERFORM.