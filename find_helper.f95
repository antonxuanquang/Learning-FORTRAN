MODULE FINDER
    USE CLASS_FACULTY
    IMPLICIT NONE

    CONTAINS


    ! return first empty index in faculty list array
    INTEGER FUNCTION FIND_EMPTY_INDEX(FACULTY_LIST)
        INTEGER					            :: EMPTY_INDEX, I
        TYPE(FACULTY)	            		:: FACULTY_MEMBER
        TYPE(FACULTY), DIMENSION(100)       :: FACULTY_LIST

        EMPTY_INDEX = -1

        DO I = 1,100,1
            IF (FACULTY_LIST(I)%IDNUM <= 0) THEN
                EMPTY_INDEX = I
                EXIT
            END IF
        END DO
        WRITE(*,*)
        FIND_EMPTY_INDEX = EMPTY_INDEX
    END FUNCTION


    ! return index in faculty array by inputing id number
    INTEGER FUNCTION FIND_FACULTY_MEMBER(IDNUM, FACULTY_LIST, HEAD_INDEX)

        INTEGER                             :: IDNUM,I,HEAD_INDEX, INDEX, CURRENT
        TYPE(FACULTY), DIMENSION(100)       :: FACULTY_LIST
        TYPE(FACULTY)	            		:: FACULTY_MEMBER

        INDEX = -1;
        CURRENT = HEAD_INDEX
        DO WHILE (CURRENT /= -99)
            IF (FACULTY_LIST(CURRENT)%IDNUM == IDNUM) THEN
                INDEX = CURRENT
                EXIT
            END IF
            CURRENT = NEXT(FACULTY_LIST(CURRENT))
        END DO
        FIND_FACULTY_MEMBER = INDEX
    END FUNCTION

    ! return index of previous faculty array by inputing id number
    INTEGER FUNCTION FIND_PREVIOUS_FACULTY(IDNUM, FACULTY_LIST, HEAD_INDEX)

        INTEGER                             :: IDNUM,I,HEAD_INDEX, INDEX, CURRENT,PREVIOUS
        TYPE(FACULTY), DIMENSION(100)       :: FACULTY_LIST
        TYPE(FACULTY)	            		:: FACULTY_MEMBER

        INDEX = -1;
        CURRENT = HEAD_INDEX
        DO WHILE (CURRENT /= -99)
            IF (FACULTY_LIST(CURRENT)%IDNUM == IDNUM) THEN
                INDEX = PREVIOUS
                EXIT
            END IF
            PREVIOUS = CURRENT
            CURRENT = NEXT(FACULTY_LIST(CURRENT))
        END DO
        FIND_PREVIOUS_FACULTY = INDEX
    END FUNCTION

END MODULE FINDER
