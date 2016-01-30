PROGRAM MAIN
		USE CLASS_FACULTY
		USE PRINT_HELPER
		USE UPDATE_HELPER
		USE INSERT_AND_DELETE


		CHARACTER(LEN=66) 				:: INPUT_STRING
		CHARACTER*2						:: COMMAND
		TYPE(FACULTY), DIMENSION (100)  :: FACULTY_LIST
		INTEGER                         :: FILL_LOOP_COUNTER, HEAD_INDEX

        DO FILL_LOOP_COUNTER=0, 100,1
            FACULTY_LIST(FILL_LOOP_COUNTER)%IDNUM = -99999999 != INSERT_OP("-99999999 Z.NOBODY NULL NOTITLE 0.00",FACULTY_LIST,INDEX_LIST)
            FACULTY_LIST(FILL_LOOP_COUNTER)%NEXT_INDEX = -1
        end do

		OPEN(5, STATUS="OLD", FILE="Lab1Data.txt")
		!OPEN(6, FILE="lab1ANS.txt")

		WRITE(6, *) "*** PROGRAM STARTED ***"
! start reading loops
		! read a line at a time
	50	READ(5, 100, END=999) INPUT_STRING
	100		FORMAT(A66)

			! read out first 2 letter to determine which command to do
			READ(INPUT_STRING, 200) COMMAND
	200		FORMAT(A2)

			WRITE(*, *) "COMMAND = ", COMMAND

			! start if
			IF (COMMAND == "IN") THEN
				HEAD_INDEX = INSERT_OP(INPUT_STRING, FACULTY_LIST,HEAD_INDEX)
			ELSE IF (COMMAND == "DE") THEN
				CALL DELETE_OP(INPUT_STRING, FACULTY_LIST,HEAD_INDEX)


			ELSE IF (COMMAND == "UD") THEN
				CALL UPDATE_DEPT_OP(INPUT_STRING)
			ELSE IF (COMMAND == "UT") THEN
				CALL UPDATE_TITLE_OP(INPUT_STRING)
			ELSE IF (COMMAND == "UP") THEN
				CALL UPDATE_PAY_OP(INPUT_STRING)

			ELSE IF (COMMAND == "PA") THEN
				!CALL PRINT_ALL_OP(INPUT_STRING)
			ELSE IF (COMMAND == "PI") THEN
				CALL PRINT_ID_OP(INPUT_STRING)
			ELSE IF (COMMAND == "PT") THEN
				CALL PRINT_TITLE_OP(INPUT_STRING)
			ELSE IF (COMMAND == "PD") THEN
				CALL PRINT_DEPT_OP(INPUT_STRING)
			END IF

		GO TO 50


	999	WRITE (6, *) "END OF DATA REACHED"
	CLOSE(5)
	CLOSE(6)
END






