       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIFFERENCE-OF-SQUARES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-DIFFERENCE-OF-SQUARES  PIC 9(8).
       01 WS-SUM-OF-SQUARES         PIC 9(8).
       01 WS-SQUARE-OF-SUM          PIC 9(8).
       01 WS-NUMBER                 PIC 9(8).
       01 WS-ITERATOR               PIC 9(8) VALUE 0.
       01 WS-DIFFERENCE             PIC 9(8) VALUE 0.
      *
       PROCEDURE DIVISION.
      *    
       SQUARE-OF-SUM.
           MOVE 0 TO WS-SQUARE-OF-SUM.
           PERFORM
              WITH TEST BEFORE
              VARYING WS-ITERATOR FROM 1 BY 1
              UNTIL WS-ITERATOR > WS-NUMBER
                   ADD WS-ITERATOR TO WS-SQUARE-OF-SUM
           END-PERFORM.
           COMPUTE WS-SQUARE-OF-SUM = WS-SQUARE-OF-SUM ** 2.
      *
       SUM-OF-SQUARES.
           MOVE 0 TO WS-SUM-OF-SQUARES.
           PERFORM
              WITH TEST BEFORE
              VARYING WS-ITERATOR FROM 1 BY 1
              UNTIL WS-ITERATOR > WS-NUMBER
                   COMPUTE WS-SUM-OF-SQUARES = WS-SUM-OF-SQUARES +
                      WS-ITERATOR ** 2
           END-PERFORM.
      *
       DIFFERENCE-OF-SQUARES.
           MOVE 0 TO WS-DIFFERENCE.
           PERFORM INITIALIZE-VALUES.
           SUBTRACT WS-SUM-OF-SQUARES FROM WS-SQUARE-OF-SUM
              GIVING WS-DIFFERENCE.
      *
       INITIALIZE-VALUES.
           MOVE 0 TO WS-SUM-OF-SQUARES.
           MOVE 0 TO WS-SQUARE-OF-SUM.
           MOVE 0 TO WS-ITERATOR.
           MOVE 0 TO WS-DIFFERENCE.
      *