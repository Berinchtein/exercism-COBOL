       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 SWITCHES.
          05 CONDITION-1-FOUND-SWITCH  PIC X     VALUE "N".
             88 CONDITION-1-FOUND                VALUE "Y".
          05 CONDITION-2-FOUND-SWITCH  PIC X     VALUE "N".
             88 CONDITION-2-FOUND                VALUE "Y".
      *    
       01 WS-RESULT                    PIC 99    VALUE 0.
       01 WS-CATEGORY                  PIC X(15).
       01 WS-DICE                      PIC 9(5).
      *
       01 WS-DICE-TABLE REDEFINES WS-DICE.
          05 WS-DIGIT OCCURS 5 TIMES   PIC 9.
      *    
       01 WS-MAX-DICE-VALUE            PIC 9     VALUE 6.
       01 WS-CURRENT-DICE-FACE         PIC 9     VALUE 0.
       01 WS-CURRENT-DICE-FACE-STR     PIC X.
       01 WS-COUNT                     PIC 9     VALUE 0.
      *
       01 CATEGORY-TABLE-VALUES.
          05 FILLER                    PIC X(15) VALUE 'ones'.
          05 FILLER                    PIC X(15) VALUE 'twos'.
          05 FILLER                    PIC X(15) VALUE 'threes'.
          05 FILLER                    PIC X(15) VALUE 'fours'.
          05 FILLER                    PIC X(15) VALUE 'fives'.
          05 FILLER                    PIC X(15) VALUE 'sixes'.
          05 FILLER                    PIC X(15) VALUE 'full house'.
          05 FILLER                    PIC X(15) VALUE 'four of a kind'
           .
          05 FILLER                    PIC X(15)
                                                 VALUE 'little straight'
                                                                      .    
          05 FILLER                    PIC X(15) VALUE 'big straight'.
          05 FILLER                    PIC X(15) VALUE 'choice'.
          05 FILLER                    PIC X(15) VALUE 'yacht'.
      *
       01 CATEGORY-TABLE REDEFINES CATEGORY-TABLE-VALUES.
          05 CATEGORY                  PIC X(15) OCCURS 12 TIMES
                INDEXED BY CATEGORY-TABLE-INDEX.
      *
          PROCEDURE DIVISION.
      *   
       YACHT.
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
                PERFORM YACHT-CATEGORY
           END-EVALUATE.
           EXIT.
      *
       ONES-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '1'.
           COMPUTE WS-RESULT = 1 * WS-COUNT.
           EXIT.
      *    
       TWOS-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '2'.
           COMPUTE WS-RESULT = 2 * WS-COUNT.
           EXIT.
      *
       THREES-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '3'.
           COMPUTE WS-RESULT = 3 * WS-COUNT.
           EXIT.
      *    
       FOURS-CATEGORY.
           MOVE 0 TO WS-COUNT. 
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '4'.
           COMPUTE WS-RESULT = 4 * WS-COUNT.
           EXIT.
      *    
       FIVES-CATEGORY.
           MOVE 0 TO WS-COUNT.
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '5'.
           COMPUTE WS-RESULT = 5 * WS-COUNT.
           EXIT.
      *   
       SIXES-CATEGORY.
           MOVE 0 TO WS-COUNT.
           INSPECT WS-DICE TALLYING WS-COUNT FOR ALL '6'.
           COMPUTE WS-RESULT = 5 * WS-COUNT.
           EXIT.
      *
       FULL-HOUSE-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
              UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
              OR CONDITION-1-FOUND
                   MOVE 0 TO WS-COUNT 
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STR
                   INSPECT WS-DICE TALLYING WS-COUNT
                      FOR ALL WS-CURRENT-DICE-FACE-STR
                   IF (WS-COUNT = 3)
                      MOVE "Y" TO CONDITION-1-FOUND-SWITCH
                   END-IF
           END-PERFORM.
           IF (CONDITION-1-FOUND)
              PERFORM
                 WITH TEST AFTER
                 VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
                 UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
                 OR CONDITION-2-FOUND
                      MOVE 0 TO WS-COUNT
                      MOVE WS-CURRENT-DICE-FACE
                         TO WS-CURRENT-DICE-FACE-STR
                      INSPECT WS-DICE TALLYING WS-COUNT FOR ALL
                         WS-CURRENT-DICE-FACE-STR
                      IF (WS-COUNT = 2)
                         MOVE "Y" TO CONDITION-2-FOUND-SWITCH
                      END-IF
              END-PERFORM
           END-IF.
           IF (CONDITION-1-FOUND AND CONDITION-2-FOUND)
              PERFORM
                 WITH TEST AFTER
                 VARYING WS-COUNT FROM 1 BY 1
                 UNTIL WS-COUNT >= 5
                      ADD WS-DIGIT(WS-COUNT) TO WS-RESULT
              END-PERFORM
           END-IF.
           EXIT.
      *
       FOUR-OF-A-KIND-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-CURRENT-DICE-FACE FROM 1 BY 1
              UNTIL WS-CURRENT-DICE-FACE >= WS-MAX-DICE-VALUE
              OR CONDITION-1-FOUND
                   MOVE 0 TO WS-COUNT
                   MOVE WS-CURRENT-DICE-FACE
                      TO WS-CURRENT-DICE-FACE-STR
                   INSPECT WS-DICE TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STR
                   IF (WS-COUNT >= 4)
                      MOVE "Y" TO CONDITION-1-FOUND-SWITCH
                   END-IF
           END-PERFORM.
           IF (CONDITION-1-FOUND)
              COMPUTE WS-RESULT = WS-CURRENT-DICE-FACE * 4
           END-IF.
           EXIT.
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
                      TO WS-CURRENT-DICE-FACE-STR
                   INSPECT WS-DICE TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STR 
                   IF (WS-COUNT NOT = 1)
                      MOVE 0 TO WS-RESULT
                   END-IF
           END-PERFORM.
           EXIT.
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
                      TO WS-CURRENT-DICE-FACE-STR
                   INSPECT WS-DICE TALLYING WS-COUNT FOR ALL
                      WS-CURRENT-DICE-FACE-STR
                   IF (WS-COUNT NOT = 1)
                      MOVE 0 TO WS-RESULT
                   END-IF
           END-PERFORM.
           EXIT.
      *    
       CHOICE-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-COUNT FROM 1 BY 1
              UNTIL WS-COUNT >= 5
                   ADD WS-DIGIT(WS-COUNT) TO WS-RESULT
           END-PERFORM.
           EXIT.

      *
       YACHT-CATEGORY.
           PERFORM
              WITH TEST AFTER
              VARYING WS-COUNT FROM 2 BY 1
              UNTIL WS-COUNT >= 5 OR CONDITION-1-FOUND
                 IF (WS-DIGIT(WS-COUNT) NOT = WS-DIGIT(WS-COUNT - 1))
                    MOVE "Y" TO CONDITION-1-FOUND-SWITCH
                 END-IF
           END-PERFORM.
           IF (NOT CONDITION-1-FOUND)
              MOVE 50 TO WS-RESULT.
           EXIT PROGRAM.
      *