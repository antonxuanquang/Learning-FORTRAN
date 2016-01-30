MODULE CLASS_FACULTY
	IMPLICIT NONE

	TYPE FACULTY
			INTEGER					IDNUM
			CHARACTER*4				DEPT
			CHARACTER*9				RANK
			CHARACTER*13			NAME
			REAL					PAY
			INTEGER                 NEXT_INDEX
	END TYPE FACULTY

    CONTAINS

	CHARACTER(100) FUNCTION PRINT_FACULTY(FAC_MEM)
        IMPLICIT NONE
        CHARACTER(100) RESULT
        CHARACTER (8) ID_NUMBER
        CHARACTER(10) PAY_NUMBER
        TYPE(FACULTY) FAC_MEM
        WRITE(ID_NUMBER,'(i8)')FAC_MEM%IDNUM
        WRITE(PAY_NUMBER,'(f9.2)')FAC_MEM%PAY
        PRINT_FACULTY = ID_NUMBER // ' '// FAC_MEM%NAME// ' '// FAC_MEM%DEPT// ' '// FAC_MEM%RANK// ' '// PAY_NUMBER
    END FUNCTION PRINT_FACULTY

    INTEGER FUNCTION NEXT(FAC_MEM)
        IMPLICIT NONE
        TYPE(FACULTY) FAC_MEM
        NEXT = FAC_MEM%NEXT_INDEX
    END FUNCTION

END MODULE CLASS_FACULTY

