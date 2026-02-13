       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-RESULT                PIC 99    VALUE 0.
       01 DICE-VALUES.
          05 WS-DICE               PIC 9(5)  VALUE 00000.
          05 WS-DICE-ROLL-COUNT    PIC 9     VALUE 5.
          05 WS-MAX-DICE-VALUE     PIC 9     VALUE 6.
       01 MULTIPLIERS.
          05 WS-RANDOM-MULTIPLIER  PIC 9     VALUE 5.
          05 WS-DICE-MULTIPLIER    PIC 9.
      *
       01 CATEGORY-TABLE-VALUES.
          05 FILLER                PIC X(15) VALUE "ONES           ".
          05 FILLER                PIC X(15) VALUE "TWOS           ".
          05 FILLER                PIC X(15) VALUE "THREES         ".
          05 FILLER                PIC X(15) VALUE "FOURS          ".
          05 FILLER                PIC X(15) VALUE "FIVES          ".
          05 FILLER                PIC X(15) VALUE "SIXES          ".
          05 FILLER                PIC X(15) VALUE "FULL HOUSE     ".
          05 FILLER                PIC X(15) VALUE "FOUR OF A KIND ".
          05 FILLER                PIC X(15) VALUE "LITTLE STRAIGHT".
          05 FILLER                PIC X(15) VALUE "CHOICE         ".
      *
       01 CATEGORY-TABLE REDEFINES CATEGORY-TABLE-VALUES.
          05 CATEGORY              PIC X(15) OCCURS 12 TIMES.
      *
          PROCEDURE DIVISION.
      *   
       YACHT.
           PERFORM ROLL-DICE
              WITH TEST AFTER
              VARYING WS-DICE-MULTIPLIER FROM 1 BY 1
              UNTIL WS-DICE-MULTIPLIER >= WS-DICE-ROLL-COUNT.
      *
       ROLL-DICE.
           COMPUTE WS-DICE = WS-DICE +
              (((FUNCTION RANDOM * WS-RANDOM-MULTIPLIER) + 1)
              *(10 **(WS-DICE-MULTIPLIER - 1))).             
      *