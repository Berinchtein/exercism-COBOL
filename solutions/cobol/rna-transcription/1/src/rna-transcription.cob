       IDENTIFICATION DIVISION.
       PROGRAM-ID. RNA-TRANSCRIPTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01 WS-COMPLEMENT        PIC X(64).
       01 WS-DNA               PIC X(4)  VALUE "ACGT".
      *
       PROCEDURE DIVISION.
      *
       RNA-TRANSCRIPTION.
           INSPECT WS-COMPLEMENT REPLACING
              ALL 'G' BY 'C'
              ALL 'C' BY 'G'
              ALL 'T' BY 'A'
              ALL 'A' BY 'U'.
      *       
      