       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompanyAdminSystem.
       AUTHOR. lek.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MachineFile ASSIGN TO "machines.txt"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MachineFile.
       01 MachineRecord.
           05 MachineNum PIC 9(5).



       WORKING-STORAGE SECTION.
       01 ws-MenuOption PIC 9.
       01 ws-MachineNumber PIC 9(5) VALUE 0.
       01 ws-MachineIndex PIC 9(3) VALUE 1.
       01 ws-MachineList.
           05 ws-MachineNum PIC 9(5) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       Begin.
       PERFORM DisplayMenu UNTIL ws-MenuOption = 3
       STOP RUN.

       DisplayMenu.
       DISPLAY "Company Admin System"
       DISPLAY "1. Add Machine Number"
       DISPLAY "2. Check Machine Numbers"
       DISPLAY "3. Exit"
       ACCEPT ws-MenuOption
       PERFORM MenuAction.

       MenuAction.
       EVALUATE ws-MenuOption
        WHEN 1
            PERFORM AddMachine
        WHEN 2
            PERFORM CheckMachineNumbers
        WHEN 3
            EXIT PROGRAM
        WHEN OTHER
            DISPLAY "Invalid option. Please try again."
       END-EVALUATE.

       AddMachine.
       DISPLAY "Enter Machine Number to add: "
       ACCEPT ws-MachineNumber
       PERFORM AppendMachineToFile
       ADD 1 TO ws-MachineIndex
       PERFORM DisplayMenu.

       CheckMachineNumbers.
           OPEN INPUT MachineFile
           READ MachineFile INTO MachineRecord
           AT END
              DISPLAY "No machine numbers stored yet."
           NOT AT END
               PERFORM UNTIL ws-MachineIndex > 100
                 DISPLAY MachineNum
                 READ MachineFile INTO MachineRecord
                 AT END
                     EXIT PERFORM
            END-PERFORM
           CLOSE MachineFile
           PERFORM DisplayMenu.

       AppendMachineToFile.
           OPEN EXTEND MachineFile
           WRITE MachineRecord FROM ws-MachineNumber
           CLOSE MachineFile.
           STOP RUN.
