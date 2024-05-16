       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestCompanyAdminSystem.
       AUTHOR. lek..

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TestInput ASSIGN TO "test_input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TestInput.
       01 TestRecord.
           05 TestOption PIC 9.
           05 TestMachineNum PIC 9(5).

       WORKING-STORAGE SECTION.
       01 ws-TestIndex PIC 9(3) VALUE 1.
       01 ws-TestList.
           05 ws-TestOption PIC 9 OCCURS 100 TIMES.
           05 ws-TestMachineNum PIC 9(5) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       Begin.
           OPEN OUTPUT TestInput
           PERFORM VARYING ws-TestIndex FROM 1 BY 1 UNTIL
            ws-TestIndex > 100
              MOVE ws-TestOption(ws-TestIndex) TO TestOption
              MOVE ws-TestMachineNum(ws-TestIndex) TO TestMachineNum
              WRITE TestRecord
           END-PERFORM
           CLOSE TestInput
           STOP RUN.
