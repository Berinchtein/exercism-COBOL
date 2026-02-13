       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 SWITCHES.
          05 FIRST-CONDITION-FOUND-SWITCH
                                        PIC X     VALUE "N".
             88 FIRST-CONDITION-FOUND             VALUE "Y".
          05 SECOND-CONDITION-FOUND-SWITCH
                                        PIC X     VALUE "N".
             88 SECOND-CONDITION-FOUND            VALUE "Y".
      *    
       01 WS-RESULT                     PIC 99    VALUE 0.
       01 WS-CATEGORY                   PIC X(15).
       01 WS-DICE                       PIC 9(5).
       01 WS-DICE-STRING                PIC X(5).
       01 WS-MAX-DICE-VALUE             PIC 9     VALUE 6.
       01 WS-CURRENT-DICE-FACE          PIC 9     VALUE 0.
       01 WS-CURRENT-DICE-FACE-STRING   PIC X.
       01 WS-COUNT                      PIC 9     VALUE 0.
      *
       01 CATEGORY-TABLE-VALUES.
          05 FILLER                     PIC X(15) VALUE 'ones'.
          05 FILLER                     PIC X(15) VALUE 'twos'.
          05 FILLER                     PIC X(15) VALUE 'threes'.
          05 FILLER                     PIC X(15) VALUE 'fours'.
          05 FILLER                     PIC X(15) VALUE 'fives'.
          05 FILLER                     PIC X(15) VALUE 'sixes'.
          05 FILLER                     PIC X(15) VALUE 'full house'.
          05 FILLER                     PIC X(15) VALUE 'four of a kind'
           .
          05 FILLER                     PIC X(15) VALUE
                'little straight'.
          05 FILLER                     PIC X(15) VALUE 'big straight'.
          05 FILLER                     PIC X(15) VALUE 'choice'.
          05 FILLER                     PIC X(15) VALUE 'yacht'.
      *
       01 CATEGORY-TABLE REDEFINES CATEGORY-TABLE-VALUES.
          05 CATEGORY                   PIC X(15) OCCURS 12 TIMES
                INDEXED BY CATEGORY-TABLE-INDEX.
      *
          PROCEDURE DIVISION.
      *   
       YACHT.
           MOVE WS-DICE TO WS-DICE-STRING.
           EVALUATE WS-CATEGORY 
           WHEN CATEGORY(1)
                PERFORM ONES-CATEGORY
           WHEN CATEGORY(2)
                PERFORM TWOS-CATEGORY
           WHEN CATEGORY(3)
                PERFORM THREES-CATEGORY
           WHEN CATEGORY(4)
                PERFORM FOURS-CATEGORY
           WHEN CATEGORY(5)
                PERFORM FIVES-CATEGORY
           WHEN CATEGORY(6)
                PERFORM SIXES-CATEGORY
           WHEN CATEGORY(7)
                PERFORM FULL-HOUSE-CATEGORY
           WHEN CATEGORY(8)
                PERFORM FOUR-OF-A-KIND-CATEGORY
           WHEN CATEGORY(9)
                PERFORM LITTLE-STRAIGHT-CATEGORY
           WHEN CATEGORY(10)
                PERFORM BIG-STRAIGHT-CATEGORY
           WHEN CATEGORY(11)
                PERFORM CHOICE-CATEGORY
           WHEN CATEGORY(12)
                PERFORM YATCH-CATEGORY
           END-EVALUATE.
           EXIT.
      *
       ONES-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE-STRING
              TALLYING WS-COUNT FOR ALL '1'.
           COMPUTE WS-RESULT = 1 * WS-COUNT.
      *    
       TWOS-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE-STRING
              TALLYING WS-COUNT FOR ALL '2'.
           COMPUTE WS-RESULT = 2 * WS-COUNT.
      *
       THREES-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-COUNT
              TALLYING WS-COUNT FOR ALL '3'.
           COMPUTE WS-RESULT = 3 * WS-COUNT.
      *    
       FOURS-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-CATEGORY
              TALLYING WS-COUNT FOR ALL '4'.
           COMPUTE WS-RESULT = 4 * WS-COUNT.
      *    
       FIVES-CATEGORY.
           MOVE 0 TO WS-COUNT.
           INSPECT WS-CATEGORY
              TALLYING WS-COUNT FOR ALL '5'.
           COMPUTE WS-RESULT = 5 * WS-COUNT.
      *   
       SIXES-CATEGORY.
           MOVE 0 TO WS-COUNT.
           INSPECT WS-CATEGORY
              TALLYING WS-COUNT FOR ALL '6'.
           COMPUTE WS-RESULT = 5 * WS-COUNT.
      *
       FULL-HOUSE-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
              UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
              OR FIRST-CONDITION-FOUND
                   MOVE 0 TO WS-COUNT 
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STRING
                   INSPECT WS-DICE-STRING TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STRING
                   IF (WS-COUNT = 3)
                      MOVE "Y" TO FIRST-CONDITION-FOUND-SWITCH
                   END-IF
           END-PERFORM.
           IF (FIRST-CONDITION-FOUND)
              PERFORM
                 WITH TEST AFTER
                 VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
                 UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
                 OR SECOND-CONDITION-FOUND
                      MOVE 0 TO WS-COUNT
                      MOVE WS-CURRENT-DICE-FACE
                         TO WS-CURRENT-DICE-FACE-STRING
                      INSPECT WS-DICE-STRING TALLYING WS-COUNT FOR ALL
                         WS-CURRENT-DICE-FACE-STRING
                      IF (WS-COUNT = 2)
                         MOVE "Y" TO SECOND-CONDITION-FOUND-SWITCH
                      END-IF
              END-PERFORM
           END-IF.
           IF (FIRST-CONDITION-FOUND AND SECOND-CONDITION-FOUND)
              COMPUTE WS-RESULT = FUNCTION NUMVAL(WS-DICE-STRING(1:1))
                 + FUNCTION NUMVAL(WS-DICE-STRING(2:1))
                 + FUNCTION NUMVAL(WS-DICE-STRING(3:1))
                 + FUNCTION NUMVAL(WS-DICE-STRING(4:1))
                 + FUNCTION NUMVAL(WS-DICE-STRING(5:1))
           END-IF.
      *
       FOUR-OF-A-KIND-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
              UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
              OR FIRST-CONDITION-FOUND
                   MOVE 0 TO WS-COUNT
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STRING
                   INSPECT WS-DICE-STRING TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STRING
                   IF (WS-COUNT >= 4)
                      MOVE "Y" TO FIRST-CONDITION-FOUND-SWITCH
                   END-IF
           END-PERFORM.
           IF (FIRST-CONDITION-FOUND)
              COMPUTE WS-RESULT = WS-CURRENT-DICE-FACE * 4
           END-IF.
      *    
       LITTLE-STRAIGHT-CATEGORY.
           MOVE 30 TO WS-RESULT.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
              UNTIL WS-CURRENT-DICE-FACE >=(WS-MAX-DICE-VALUE - 1)
              OR WS-RESULT = 0
                   MOVE 0 TO WS-COUNT 
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STRING
                   INSPECT WS-DICE-STRING TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STRING 
                   IF (WS-COUNT NOT = 1)
                      MOVE 0 TO WS-RESULT
                   END-IF
           END-PERFORM.
      *    
       BIG-STRAIGHT-CATEGORY.
           MOVE 30 TO WS-RESULT.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 2 BY 1
              UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
              OR WS-RESULT = 0
                   MOVE 0 TO WS-COUNT 
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STRING
                   INSPECT WS-DICE-STRING TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STRING
                   IF (WS-COUNT NOT = 1)
                      MOVE 0 TO WS-RESULT
                   END-IF
           END-PERFORM.
      *    
       CHOICE-CATEGORY.
           COMPUTE WS-RESULT = FUNCTION NUMVAL(WS-DICE-STRING(1:1))
              + FUNCTION NUMVAL(WS-DICE-STRING(2:1))
              + FUNCTION NUMVAL(WS-DICE-STRING(3:1))
              + FUNCTION NUMVAL(WS-DICE-STRING(4:1))
              + FUNCTION NUMVAL(WS-DICE-STRING(5:1)).
      *
       YATCH-CATEGORY.
           IF (FUNCTION NUMVAL(WS-DICE-STRING(1:1))
              = FUNCTION NUMVAL(WS-DICE-STRING(2:1))
              AND FUNCTION NUMVAL(WS-DICE-STRING(2:1))
              = FUNCTION NUMVAL(WS-DICE-STRING(3:1))
              AND FUNCTION NUMVAL(WS-DICE-STRING(3:1))
              = FUNCTION NUMVAL(WS-DICE-STRING(4:1))
              AND FUNCTION NUMVAL(WS-DICE-STRING(4:1))
              = FUNCTION NUMVAL(WS-DICE-STRING(5:1)))
              MOVE 50 TO WS-RESULT
           END-IF.
      *    
