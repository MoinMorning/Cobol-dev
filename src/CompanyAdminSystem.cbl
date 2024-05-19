       IDENTIFICATION DIVISION.
       PROGRAM-ID. CompanyAdminSystem.
       AUTHOR. lek.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MachineFile
           ASSIGN TO "machines.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ws-MachineFile-Status.

           SELECT TempMachineFile
           ASSIGN TO 
           "temp_machines.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MachineFile.
       01 MachineRecord.
           05 MachineNum PIC X(5).
           05 MachineTyp PIC X(5).
           05 MachineManu PIC X(5).
           05 MachineUsername PIC X(20).
           05 MachineSpec PIC X(50).

       FD TempMachineFile.
       01 TempMachineRecord.
           05 TempMachineNum PIC X(5).
           05 TempMachineTyp PIC X(5).
           05 TempMachineManu PIC X(5).

       WORKING-STORAGE SECTION.
       01 ws-MachineFile-Status PIC 99. 
       01 ws-MenuOption PIC 9.
       01 ws-MachineNumber PIC 9(5) VALUE 0.
       01 ws-MachineIndex PIC 9(3) VALUE 1.
       01 ws-MachineList.
           05 ws-MachineNum PIC 9(5) OCCURS 100 TIMES.
       01 Username PIC X(20).
       01 ws-MachineSpec PIC X(50).

       PROCEDURE DIVISION.
           
           OPEN INPUT MachineFile
           IF ws-MachineFile-Status NOT EQUAL 00
              DISPLAY "Error opening MachineFile: " 
              ws-MachineFile-Status
             STOP RUN.
           
       
       Begin.
           PERFORM DisplayMenu UNTIL ws-MenuOption = 5
           STOP RUN.

       DisplayMenu.
           DISPLAY "Company Admin System"
           DISPLAY "1. Add Machine Number"
           DISPLAY "2. Check Machine Numbers"
           DISPLAY "3. Delete Machine Number"
           DISPLAY "4. Search Machine Number"
           DISPLAY "5. Exit"
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
            PERFORM SearchMachine  
           WHEN 5
            EXIT PROGRAM
       END-EVALUATE.

       AddMachine.
           
           DISPLAY "Enter Machine Number to add: "
           ACCEPT ws-MachineNumber
           IF ws-MachineNumber LESS THAN 1 OR ws-MachineNumber GREATER 
           THAN 99999
           DISPLAY "Invalid Machine Number. Please enter a number betwe"
           "en 1 and 99999."
           GO TO DisplayMenu 
           END-IF.

           DISPLAY "Enter Username: "
           ACCEPT Username
           IF Username EQUAL SPACES
           DISPLAY "Username cannot be empty."
           GO TO DisplayMenu
           END-IF. 


           DISPLAY "Enter Machine Specifications: "
           ACCEPT ws-MachineSpec 

           IF ws-MachineSpec EQUAL SPACES
           DISPLAY "Machine Specifications cannot be empty."
           GO TO DisplayMenu
           END-IF.

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
           PERFORM DisplayMenu.
        
        SearchMachine.
           DISPLAY "Enter Machine Number to search: "
           ACCEPT ws-MachineNumber 
           OPEN INPUT MachineFile
           READ MachineFile INTO MachineRecord
               AT END
            DISPLAY "No machine numbers stored yet."
               NOT AT END
            PERFORM UNTIL ws-MachineIndex > 100
                IF MachineNum EQUAL TO ws-MachineNumber 
                    DISPLAY "Machine Number found: " MachineNum
                    EXIT PERFORM
                END-IF 
                READ MachineFile INTO MachineRecord
                    AT END
                        DISPLAY "Machine Number not found."
                        EXIT PERFORM 
            END-PERFORM
            CLOSE MachineFile
            PERFORM DisplayMenu.
       
       AppendMachineToFile.
           OPEN EXTEND MachineFile
           MOVE ws-MachineNumber TO MachineNum
           MOVE Username TO MachineUsername  OF MachineRecord
           MOVE MachineSpec  TO MachineSpec OF MachineRecord
           WRITE MachineRecord
           CLOSE MachineFile.


  
           COMPUTE ws-MachineIndex = ws-MachineIndex + 1 ON SIZE ERROR
           DISPLAY "Error: Overflow occurred when incrementing"
           "ws-MachineIndex."
           STOP RUN
           END-COMPUTE.
