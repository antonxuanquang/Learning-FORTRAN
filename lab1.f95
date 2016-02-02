!**************************************************************************!
!*  Program Name:    Lab# 1                                               *!
!*                                                                        *!
!*  Student Name:    Quang Nguyen and Jim Plunger                         *!
!*  Semester:        Spring 2016                                          *!
!*  Class & Section: CoSc 30403                                           *!
!*  Instructor:      Dr. James Comer                                      *!
!*  Due Date:        Feb 2nd, 2016                                        *!
!*                                                                        *!
!*  Program Overview:                                                     *!
!*      _This program takes input from a sudo command file then builds a  *!
!*      array resembling linked list to store all the information         *!
!*      _The sudo command file has the following commands: IN - input a   *!
!*      information of a faculty member; DE - remove stored information   *!
!*      of a certain faculty; PA, PI, PT, PD - print all or some          *!
!*      faculties' information; UP, UT, UD - update information of a      *!
!*      faculty member                                                    *!
!*                                                                        *!
!*  Input:                                                                *!
!*      A sudo command file                                               *!
!*                                                                        *!
!*  Output:                                                               *!
!*      appropriate user feedback indicating successful completion of     *!
!*      each of the commands (insert, delete, update, or print)           *!
!*                                                                        *!
!*  Program Limitations:                                                  *!
!*      (1) list of faculty can only store 100 faculty objects            *!
!*                                                                        *!
!*  Significant Program Variables:                                        *!
!*      faculty_list           - holds the information of all faculty     *!
!*                               information                              *!
!*      head_index             - entry index to the first faculty         *!
!*                               object in the linked list                *!
!*                                                                        *!
!**************************************************************************!
PROGRAM MAIN
		USE CLASS_FACULTY
		USE PRINT_HELPER
		USE UPDATE_HELPER
		USE INSERT_AND_DELETE


		CHARACTER(LEN=66) 				:: INPUT_STRING
		CHARACTER*2						:: COMMAND
		TYPE(FACULTY), DIMENSION (100)  :: FACULTY_LIST
		INTEGER                         :: FILL_LOOP_COUNTER, HEAD_INDEX


        !nullify all the
        DO FILL_LOOP_COUNTER=0, 100,1
            FACULTY_LIST(FILL_LOOP_COUNTER)%IDNUM = -99999999
            FACULTY_LIST(FILL_LOOP_COUNTER)%NEXT_INDEX = -99
        end do

		OPEN(5, STATUS="OLD", FILE="Lab1Data.txt")
		OPEN(6, FILE="lab1ANS.txt")

		WRITE(6, *) "*** PROGRAM STARTED ***"
        ! start reading loops
		! read a line at a time
	50	READ(5, 100, END=999) INPUT_STRING
	100		FORMAT(A66)

			! read out first 2 letter to determine which command to do
			READ(INPUT_STRING, 200) COMMAND
	200		FORMAT(A2)

			! start if
			IF (COMMAND == "IN") THEN
				HEAD_INDEX = INSERT_OP(INPUT_STRING, FACULTY_LIST,HEAD_INDEX)
			ELSE IF (COMMAND == "DE") THEN
				CALL DELETE_OP(INPUT_STRING, FACULTY_LIST,HEAD_INDEX)

			ELSE IF (COMMAND == "UD") THEN
				CALL UPDATE_DEPT_OP(INPUT_STRING, FACULTY_LIST, HEAD_INDEX)
			ELSE IF (COMMAND == "UT") THEN
				CALL UPDATE_TITLE_OP(INPUT_STRING, FACULTY_LIST, HEAD_INDEX)
			ELSE IF (COMMAND == "UP") THEN
				CALL UPDATE_PAY_OP(INPUT_STRING, FACULTY_LIST, HEAD_INDEX)

			ELSE IF (COMMAND == "PA") THEN
				 CALL PRINT_ALL_OP(FACULTY_LIST, HEAD_INDEX)
			ELSE IF (COMMAND == "PI") THEN
				CALL PRINT_ID_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
			ELSE IF (COMMAND == "PT") THEN
				CALL PRINT_TITLE_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
			ELSE IF (COMMAND == "PD") THEN
				CALL PRINT_DEPT_OP(INPUT_STRING,FACULTY_LIST,HEAD_INDEX)
			END IF

		GO TO 50


	999	WRITE (6, *) "END OF DATA REACHED"
        CLOSE(5)
        CLOSE(6)
END






