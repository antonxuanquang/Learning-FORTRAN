MODULE INSERT_AND_DELETE

	USE NODE
	USE CLASS_FACULTY
	
	
	IMPLICIT NONE
	CONTAINS

	SUBROUTINE INSERT_OP(INPUT_STRING, HEAD)
			CHARACTER*66 			:: INPUT_STRING
			INTEGER					:: IDNUM
			CHARACTER*13			:: NAME
			CHARACTER*4				:: DEPT
			CHARACTER*9				:: RANK
			REAL					:: PAY
			TYPE(NODE), POINTER 	:: HEAD, ELEM
			TYPE(FACULTY)			:: FACULTY

			READ(STR, 300) IDNUM, NAME, DEPT, RANK, PAY
		300	FORMAT(3X, I8, 1X, A13, 1X, A4, 11X, A9, 8X, F9.2)
			WRITE(6, *) "** In INSERT_OP w/ ID = ", IDNUM, " NAME = ", NAME, " **"
			
			ALLOCATE(FACULTY)
				FACULTY%IDNUM = IDNUM
				FACULTY%NAME = NAME
				FACULTY%DEPT = DEPT
				FACULTY%RANK = RANK
				FACULTY%PAY = PAY
			ALLOCATE(ELEM)
				ELEM%VALUE = FACULTY

			HEAD => INSERTNODE(HEAD, ELEM)
			
	END SUBROUTINE INSERT_OP

		

	SUBROUTINE DELETE_OP(INPUT_STRING)
			CHARACTER*66 	INPUT_STRING
			INTEGER 		IDNUM

			READ(STR, 400) IDNUM
		400	FORMAT(3X, I8)
			WRITE(6, *) "** In DELETE_OP w/ ID = ", IDNUM, " **"
			RETURN
	END SUBROUTINE DELETE_OP

END MODULE INSERT_AND_DELETE