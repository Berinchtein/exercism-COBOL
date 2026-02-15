       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 QUESTION-SWITCH    PIC X     VALUE "N".
          88 IS-QUESTION               VALUE "Y".
       01 UPPER-CASE-SWITCH  PIC X     VALUE "N".
          88 IS-UPPER-CASE             VALUE "Y".
      *
       01 WS-HEYBOB          PIC X(60).
       01 WS-HEYBOB-UPPER    PIC X(60).
       01 WS-HEYBOB-LENGTH   PIC 99    VALUE 0.
       01 WS-RESULT          PIC X(40).
      *
       PROCEDURE DIVISION.
      *
       BOB.
           PERFORM INITIALIZE-VALUES.
           PERFORM CHECK-CONDITIONS.
           PERFORM PROCESS-RESULT.
           EXIT. *> Redundant
      *    
       INITIALIZE-VALUES.
           MOVE "N" TO QUESTION-SWITCH.
           MOVE "N" TO UPPER-CASE-SWITCH.
           MOVE SPACES TO WS-RESULT.
           MOVE FUNCTION UPPER-CASE(WS-HEYBOB) TO WS-HEYBOB-UPPER.
           MOVE FUNCTION LENGTH(
              FUNCTION TRIM(WS-HEYBOB)) TO WS-HEYBOB-LENGTH.
      *
       CHECK-CONDITIONS.
           IF (WS-HEYBOB(WS-HEYBOB-LENGTH:1) = "?")
              MOVE "Y" TO QUESTION-SWITCH
           END-IF.
           IF (WS-HEYBOB IS EQUAL TO WS-HEYBOB-UPPER)
              MOVE "Y" TO UPPER-CASE-SWITCH
           END-IF.
      *    
       PROCESS-RESULT.
           EVALUATE TRUE 
           WHEN(IS-QUESTION AND IS-UPPER-CASE)
                MOVE "Calm down, I know what I'm doing!" TO WS-RESULT
           WHEN(IS-QUESTION AND NOT IS-UPPER-CASE)
                MOVE "Sure." TO WS-RESULT
           WHEN(NOT IS-QUESTION AND IS-UPPER-CASE)
                MOVE "Whoa, chill out!" TO WS-RESULT
           WHEN(NOT IS-QUESTION AND NOT IS-UPPER-CASE)
                MOVE "Whatever." TO WS-RESULT
           END-EVALUATE.
           IF (WS-HEYBOB IS EQUAL TO SPACES)
              MOVE "Fine. Be that way!" TO WS-RESULT
           END-IF.
      *