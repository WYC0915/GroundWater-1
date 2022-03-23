! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

MODULE s_gtpartyp
CONTAINS

  SUBROUTINE gtpartyp(neq)
! -------------------------------------------------------------------------
! Purpose:    Identify the left and right boundaries of piece-wise
!             linear parameters
!
! Author:     R. Dach
!
! Created:    24-Oct-2010
! Last mod:   __-___-____
!
! Changes:    __-___-____ __:
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------
! Used modules:
    USE m_bern
    USE d_par,  ONLY: isParTyp,                  &
                      parType_linear,            &
                      parType_linearLeftPoint,   &
                      parType_linearMiddlePoint, &
                      parType_linearRightPoint
    USE d_neq,  ONLY: t_neq

! No implicits
    IMPLICIT NONE

! In/Out:
    TYPE(t_neq)        :: neq   ! NEQ strcuture

! Local variables
    INTEGER(i4b)       :: iPar, jPar, lPar, rPar

! Loop the parameters
! -------------------
  DO iPar = 1, neq%misc%nPar
    IF (  neq%par(iPar)%type == parType_linear  .AND. &
        ! Potential parameter types
        ( neq%par(iPar)%locq(1) ==  1 .OR.  &
         (neq%par(iPar)%locq(1) ==  2 .AND. neq%par(iPar)%locq(5) == 6) .OR. &
          neq%par(iPar)%locq(1) ==  6 .OR.  &
          neq%par(iPar)%locq(1) == 10 .OR.  &
          neq%par(iPar)%locq(1) == 19 ) ) THEN

! Define the order of the parameters
! ----------------------------------
      lPar = 0
      rPar = 0
      DO jPar = 1,neq%misc%nPar
        IF ( jPar == iPar ) CYCLE

! Identify the related parameters
        IF ( neq%par(jPar)%locq(1) /= neq%par(iPar)%locq(1) ) CYCLE
        IF ( neq%par(jPar)%name    /= neq%par(iPar)%name    ) CYCLE
        IF ( .NOT. isParTyp(neq%par(jPar),parType_linear)   ) CYCLE
        IF ((neq%par(jPar)%locq(1) ==  1 .AND. &
             neq%par(jPar)%locq(3) == neq%par(iPar)%locq(3)) .OR. &
            (neq%par(jPar)%locq(1) ==  6 .AND. &
             neq%par(jPar)%locq(4) == neq%par(iPar)%locq(4)) .OR. &
            (neq%par(jPar)%locq(1) == 10 .AND. &
             neq%par(jPar)%locq(4) == neq%par(iPar)%locq(4)) .OR. &
            (neq%par(jPar)%locq(1) == 19 .AND. &
             neq%par(jPar)%locq(4) == neq%par(iPar)%locq(4) .AND. &
             neq%par(jPar)%locq(5) == neq%par(iPar)%locq(5))) THEN

! Identify the left neigbour
          IF ( neq%par(jpar)%time%mean < neq%par(ipar)%time%mean ) THEN
            IF (lPar == 0) THEN
              lPar = jPar
            ELSE IF (neq%par(ipar)%time%mean - neq%par(jpar)%time%mean < &
                     neq%par(ipar)%time%mean - neq%par(lpar)%time%mean) THEN
              lPar = jPar
            ENDIF
          ENDIF

! Identify the right neigbour
          IF ( neq%par(jpar)%time%mean > neq%par(ipar)%time%mean ) THEN
            IF (rPar == 0) THEN
              rPar = jPar
            ELSE IF (neq%par(jpar)%time%mean - neq%par(ipar)%time%mean < &
                     neq%par(rpar)%time%mean - neq%par(ipar)%time%mean) THEN
              rPar = jPar
            ENDIF
          ENDIF
        ENDIF
      ENDDO

! Make the conclusion
      IF ( lPar == 0 .AND. rPar /= 0 ) THEN
        neq%par(ipar)%type  = parType_linearLeftPoint
        neq%par(ipar)%omega = neq%par(rpar)%time%mean - &
                              neq%par(ipar)%time%mean
      ELSE IF ( lPar /= 0 .AND. rPar /= 0 ) THEN
        neq%par(ipar)%type  = parType_linearMiddlePoint
        neq%par(ipar)%omega = DMIN1(neq%par(rpar)%time%mean - &
                                    neq%par(ipar)%time%mean,  &
                                    neq%par(ipar)%time%mean - &
                                    neq%par(lpar)%time%mean)
      ELSE IF ( lPar /= 0 .AND. rPar == 0 ) THEN
        neq%par(ipar)%type  = parType_linearRightPoint
        neq%par(ipar)%omega = neq%par(ipar)%time%mean - &
                              neq%par(lpar)%time%mean
      ENDIF
    ENDIF
  ENDDO

  END SUBROUTINE gtpartyp

END MODULE s_gtpartyp
