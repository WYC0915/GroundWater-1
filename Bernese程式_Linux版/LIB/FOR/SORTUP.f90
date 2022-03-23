MODULE s_SORTUP
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.0
! -------------------------------------------------------------------------

  SUBROUTINE sortup(file)

! -------------------------------------------------------------------------
! Purpose:    Sort elements of .STA-file in ascending order:
!             Sorts all types (001 - 005): first for flags, second for
!             station names and third for timint(1)
!
! Author:     A. Steinbach
!
! Created:    26-Nov-2007
! Last mod.:  26-Nov-2007
!
! Changes:    06-Jan-2011 SL: use m_bern with ONLY
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern,   ONLY: i4b
  USE d_stacrx, ONLY: t_stacrux, t_renamSta, t_stainfo, t_staprob, t_coovel, &
                      t_statype

  IMPLICIT NONE

! List of parameters
! ------------------
! IN/OUT:
  TYPE(t_stacrux)                 :: file

! kal Variables
! ---------------
  TYPE(t_renamSta)                :: renamSta
  TYPE(t_stainfo)                 :: stainfo
  TYPE(t_staprob)                 :: staprob
  TYPE(t_coovel)                  :: coovel
  TYPE(t_statype)                 :: statype

  INTEGER(i4b)                    :: i, j, k

! Sort structure for flags, stanams and timint(1)
! -----------------------------------------------
! TYPE 001
! --------
  DO i = 1, file%nrenam-1
    k = i
    DO j = i+1, file%nrenam
      IF ( (file%renamsta(j)%flg         <  file%renamsta(k)%flg)          .OR. &
           (file%renamsta(j)%flg         == file%renamsta(k)%flg    .AND. &
            file%renamsta(j)%stanam      <  file%renamsta(k)%stanam)       .OR. &
           (file%renamsta(j)%flg         == file%renamsta(k)%flg    .AND. &
            file%renamsta(j)%stanam      == file%renamsta(k)%stanam .AND. &
            file%renamsta(j)%timint%t(1) <  file%renamsta(k)%timint%t(1) )) THEN
          k = j
      ENDIF
    ENDDO

    IF (k /= i) THEN
      renamsta            = file%renamsta(i)
      file%renamsta(i)    = file%renamsta(k)
      file%renamsta(k)    = renamsta
    ENDIF
  ENDDO

! TYPE 002
! --------
  DO i = 1, file%ninfo-1
    k = i
    DO j = i+1, file%ninfo
      IF ( (file%stainfo(j)%flg         <  file%stainfo(k)%flg)          .OR. &
           (file%stainfo(j)%flg         == file%stainfo(k)%flg    .AND. &
            file%stainfo(j)%stanam      <  file%stainfo(k)%stanam)       .OR. &
           (file%stainfo(j)%flg         == file%stainfo(k)%flg    .AND. &
            file%stainfo(j)%stanam      == file%stainfo(k)%stanam .AND. &
            file%stainfo(j)%timint%t(1) <  file%stainfo(k)%timint%t(1) )) THEN
          k = j
      ENDIF
    ENDDO

    IF (k /= i) THEN
      stainfo            = file%stainfo(i)
      file%stainfo(i)    = file%stainfo(k)
      file%stainfo(k)    = stainfo
    ENDIF
  ENDDO

! TYPE 003
! --------
  DO i = 1, file%nprob-1
    k = i
    DO j = i+1, file%nprob
      IF ( (file%staprob(j)%flg         <  file%staprob(k)%flg)          .OR. &
           (file%staprob(j)%flg         == file%staprob(k)%flg    .AND. &
            file%staprob(j)%stanam      <  file%staprob(k)%stanam)       .OR. &
           (file%staprob(j)%flg         == file%staprob(k)%flg    .AND. &
            file%staprob(j)%stanam      == file%staprob(k)%stanam .AND. &
            file%staprob(j)%timint%t(1) <  file%staprob(k)%timint%t(1) )) THEN
          k = j
      ENDIF
    ENDDO

    IF (k /= i) THEN
      staprob            = file%staprob(i)
      file%staprob(i)    = file%staprob(k)
      file%staprob(k)    = staprob
    ENDIF
  ENDDO

! TYPE 004
! --------
  DO i = 1, file%ncoovel-1
    k = i
    DO j = i+1, file%ncoovel
      IF (file%coovel(j)%stanam(1)   <  file%coovel(k)%stanam(1)      .OR. &
          (file%coovel(j)%stanam(1)  == file%coovel(k)%stanam(1) .AND. &
           file%coovel(j)%stanam(2)  <  file%coovel(k)%stanam(2))) THEN
          k = j
      ENDIF
    ENDDO

    IF (k /= i) THEN
      coovel            = file%coovel(i)
      file%coovel(i)    = file%coovel(k)
      file%coovel(k)    = coovel
    ENDIF
  ENDDO

! TYPE 005
! --------
  DO i = 1, file%nstatype-1
    k = i
    DO j = i+1, file%nstatype
      IF ( (file%statype(j)%flg         <  file%statype(k)%flg)          .OR. &
           (file%statype(j)%flg         == file%statype(k)%flg    .AND. &
            file%statype(j)%stanam      <  file%statype(k)%stanam)       .OR. &
           (file%statype(j)%flg         == file%statype(k)%flg    .AND. &
            file%statype(j)%stanam      == file%statype(k)%stanam .AND. &
            file%statype(j)%timint%t(1) <  file%statype(k)%timint%t(1) )) THEN
          k = j
      ENDIF
    ENDDO

    IF (k /= i) THEN
      statype            = file%statype(i)
      file%statype(i)    = file%statype(k)
      file%statype(k)    = statype
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE sortup

END MODULE
