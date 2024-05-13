       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompanyAdminSystem.
       AUTHOR. lek.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MachineFile ASSIGN TO "machines.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

           SELECT TempMachineFile ASSIGN TO "temp_machines.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MachineFile.
       01 MachineRecord.
           05 MachineNum PIC 9(5).
           05 MachineTyp PIC X(5).
           05 MachineManu PIC X(5).

       FD TempMachineFile.
       01 TempMachineRecord.
           05 TempMachineNum PIC 9(5).
           05 TempMachineTyp PIC X(5).
           05 TempMachineManu PIC X(5).

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
       DISPLAY "3. Delete Machine Number"
       DISPLAY "4. Exit"
       ACCEPT ws-MenuOption
       PERFORM MenuAction.

       MenuAction.
       EVALUATE ws-MenuOption
        WHEN 1
            PERFORM AddMachine
        WHEN 2
            PERFORM CheckMachineNumbers
        WHEN 3
            PERFORM DeleteMachine
        WHEN 4
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

       DeleteMachine.
           DISPLAY "Enter Machine Number to delete: "
           ACCEPT ws-MachineNumber 
           OPEN INPUT MachineFile
           OPEN OUTPUT TempMachineFile
           READ MachineFile INTO MachineRecord
               AT END
                  DISPLAY "No machine numbers stored yet."
               NOT AT END
                   PERFORM UNTIL ws-MachineIndex > 100
                       IF MachineNum NOT EQUAL TO ws-MachineNumber 
                             MOVE MachineNum TO TempMachineNum
                             MOVE MachineTyp TO TempMachineTyp
                             MOVE MachineManu TO TempMachineManu
                             WRITE TempMachineRecord
                       END-IF 
                       READ MachineFile INTO MachineRecord
                          AT END
                              EXIT PERFORM 
                   END-PERFORM
           CLOSE MachineFile 
           CLOSE TempMachineFile
           DELETE "machines.txt"
           RENAME "temp_machines.txt" TO "machines.txt"
           PERFORM DisplayMenu.

       AppendMachineToFile.
           OPEN EXTEND MachineFile
           WRITE MachineRecord FROM ws-MachineNumber
           CLOSE MachineFile.
           STOP RUN.
