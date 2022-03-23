MODULE s_NEQSORT
CONTAINS


! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE neqsort(neq)

! -------------------------------------------------------------------------
! Purpose:    This subroutine sorts a normal equation
!
! Author:     M. Meindl
!
! Created:    13-Nov-2002
! Last mod.:  30-Nov-2010
!
! Changes:    23-May-2003 CU: Remove srtIdx
!             10-Jun-2003 MM: Round to full sec for troposphere
!             14-Aug-2003 HU: Satellite pcv added (typ 25)
!             22-Jan-2004 SS/MM: Sort SAP parameters
!             22-Sep-2005 RD: Use new modules D_NEQ.f90 and D_PAR.f90
!             04-Oct-2006 AG: Sort SAO improved
!             16-Nov-2006 AG: Sort SAO on PRN
!             17-Nov-2006 AG: Sort SAO/SAP on PRN if satellite specific
!             25-Jan-2008 RD: Add RAO/RAP parameters
!             04-May-2009 RD: Scaling of loading models added
!             06-May-2009 RD: Sort SAP by SVN (instead of PRN)
!             28-May-2008 DT: Sort RCL / Time biases
!             08-Apr-2009 DT: Sort Range biases (26)
!             10-May-2009 RD: Receiver clock offsets/biases implemented
!             10-Jun-2009 RD: Add epoch satellite/receiver clocks
!             04-Jan-2010 SL: HOI scaling parameters (Case 27) added
!             12-Aug-2010 DT: Technique added for sorting SAO
!             30-Nov-2010 DT: Add Helmert parameters (28)
!             30-Nov-2010 MM: GNSS-specific parameters
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

  USE m_bern
  USE m_time,   ONLY: t_timint
  USE m_global, ONLY: g_syssvn
  USE d_par,    ONLY: t_par, maxParTyp, is_techn
  USE d_neq,    ONLY: t_neq
  USE s_svn2prn
  USE f_ikf
  IMPLICIT NONE

! List of parameters
! ------------------
  TYPE(t_neq)                            :: neq

! Local variables
! ---------------
  INTEGER(i4b), DIMENSION(maxParTyp)     :: numPar
  INTEGER(i4b), DIMENSION(maxParTyp)     :: sOrder
  INTEGER(i4b), DIMENSION(maxParTyp)     :: sStart
  INTEGER(i4b)                           :: iPar
  INTEGER(i4b)                           :: iTyp
  INTEGER(i4b)                           :: iOrd
  INTEGER(i4b)                           :: iIndex
  INTEGER(i4b)                           :: iFrom
  INTEGER(i4b)                           :: idx1
  INTEGER(i4b)                           :: idx2
  INTEGER(i4b)                           :: iStart
  INTEGER(i4b)                           :: iEnd
  INTEGER(i4b)                           :: grpnr1, grpnr2
  INTEGER(i4b)                           :: frq1, frq2
  INTEGER(i4b)                           :: sys1, sys2
  INTEGER(i4b)                           :: cor1, cor2
  INTEGER(i4b)                           :: zen1, zen2
  INTEGER(i4b)                           :: azi1, azi2
  INTEGER(i4b)                           :: irc
  INTEGER(i4b)                           :: techn1,techn2
  REAL(r8b)                              :: tmp
  REAL(r8b)                              :: t1, t2
  TYPE(t_par)                            :: tmpPar
  TYPE(t_timint)                         :: timint
  INTEGER(i4b), DIMENSION(neq%misc%nPar) :: index
  LOGICAL                                :: sorted
  CHARACTER(LEN=4)                       :: svnnr, svnnr1, svnnr2

! Some initializations
! --------------------
  numPar(:) = 0
  iIndex    = 0
  sStart(:) = 0
  sOrder(:) = 0
  sOrder(1:20) = (/1,30,6,19,8,22,27,2,26,3,11,12,25,5,18,10,16,28,23,24/)


! Count number of parameters
! --------------------------
  DO iPar=1,neq%misc%nPar
    numPar(neq%par(iPar)%locq(1)) = numPar(neq%par(iPar)%locq(1))+1
  END DO


! First step using locq(1)
! ------------------------
  DO iOrd=1,maxParTyp
    iTyp = sOrder(iOrd)
    IF (iTyp==0) CYCLE
    IF (numPar(iTyp)==0) CYCLE

    DO iPar=1, neq%misc%nPar
      IF (neq%par(iPar)%locq(1)==iTyp) THEN
        iIndex = iIndex+1
        index(iIndex) = iPar
      END IF
    END DO
    sStart(iTyp) = iIndex-numPar(iTyp)+1
  END DO


! Special sorting for parameter types
! -----------------------------------
  DO iTyp=1,maxParTyp
    sorted = .FALSE.
    iStart = sStart(iTyp)
    iEnd   = sStart(iTyp)+numPar(iTyp)-1

    SELECT CASE (iTyp)

! Coordinates and velocities
      CASE(1)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))       .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)   .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
                 neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Receiver clock offsets
      CASE(2)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF ( neq%par(idx1)%locq(6)>neq%par(idx2)%locq(6) .OR.  &

                (neq%par(idx1)%locq(6)==neq%par(idx2)%locq(6).AND. &
                 neq%par(idx1)%name>neq%par(idx2)%name)      .OR.  &

                (neq%par(idx1)%locq(6)==neq%par(idx2)%locq(6).AND. &
                 neq%par(idx1)%name==neq%par(idx2)%name      .AND. &
                 neq%par(idx1)%locq(6)<5                     .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4)).OR.  &

                (neq%par(idx1)%locq(6)==neq%par(idx2)%locq(6).AND. &
                 neq%par(idx1)%name==neq%par(idx2)%name      .AND. &
                 neq%par(idx1)%locq(6)<5                     .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4).AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean).OR. &

                (neq%par(idx1)%locq(6)==neq%par(idx2)%locq(6).AND. &
                 neq%par(idx1)%name==neq%par(idx2)%name      .AND. &
                 neq%par(idx1)%locq(6)==5                    .AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO



! Orbital parameters
      CASE(3)
!        DO WHILE (.NOT. sorted)
!          sorted = .TRUE.
!          DO iOrd=iStart, iEnd-1
!            idx1 = index(iOrd)
!            idx2 = index(iOrd+1)
!            IF (neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3)        .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)      .AND. &
!                 neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2))      .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)      .AND. &
!                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)      .AND. &
!                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))) THEN
!
!            IF (neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3)        .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)      .AND. &
!                 neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2))) THEN
!              index(iOrd)   = idx2
!              index(iOrd+1) = idx1
!              sorted        = .FALSE.
!            END IF
!          END DO
!        END DO


! Receiver antenna offsets
      CASE(5)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)

            frq1 = MOD(neq%par(idx1)%locq(4) , 100)
            frq2 = MOD(neq%par(idx2)%locq(4) , 100)
            sys1 = MOD(neq%par(idx1)%locq(5) , 100)
            sys2 = MOD(neq%par(idx2)%locq(5) , 100)
            cor1 = neq%par(idx1)%locq(4) / 100
            cor2 = neq%par(idx2)%locq(4) / 100

            IF (neq%par(idx1)%name    > neq%par(idx2)%name          .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)> neq%par(idx2)%locq(2))      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)> neq%par(idx2)%locq(3))      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 >  sys2                                )      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 == sys2 .AND. frq1 >  frq2             )      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 == sys2 .AND. frq1 == frq2 .AND. cor1 > cor2 )) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Troposphere parameters
      CASE(6)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)

            ! round to full second
            t1   = anint(neq%par(idx1)%time%mean*86400d0)/86400.d0
            t2   = anint(neq%par(idx2)%time%mean*86400d0)/86400.d0

            IF (neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                t1>t2)                                              .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 t1==t2                                             .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Differential code biases
      CASE(8)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            ! sort group wise
            IF (neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2)) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
              CYCLE
            ELSE IF (neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)) THEN
              ! satellite dcbs
              IF ((neq%par(idx1)%locq(2)==1                         .AND. &
                   neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3))     .OR.  &
                  (neq%par(idx1)%locq(2)==1                         .AND. &
                   neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)     .AND. &
                   neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)) THEN
                index(iOrd)   = idx2
                index(iOrd+1) = idx1
                sorted        = .FALSE.
                CYCLE
              END IF
              ! receiver dcbs
              IF ((neq%par(idx1)%locq(2)==2                         .AND. &
                   neq%par(idx1)%name>neq%par(idx2)%name)           .OR.  &
                  (neq%par(idx1)%locq(2)==2                         .AND. &
                   neq%par(idx1)%name>neq%par(idx2)%name            .AND. &
                   neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)) THEN
                index(iOrd)   = idx2
                index(iOrd+1) = idx1
                sorted        = .FALSE.
                CYCLE
              END IF
            END IF
          END DO
        END DO


! Earth rotation parameters
      CASE(10)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%time%mean>neq%par(idx2)%time%mean     .OR.  &
                (neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Stochastic pulses
      CASE(11)
!        DO WHILE (.NOT. sorted)
!          sorted = .TRUE.
!          DO iOrd=iStart, iEnd-1
!            idx1 = index(iOrd)
!            idx2 = index(iOrd+1)
!            IF (neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3)         .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
!                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)   .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
!                 neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
!                 neq%par(idx1)%locq(5)>neq%par(idx2)%locq(5))) THEN
!              index(iOrd)   = idx2
!              index(iOrd+1) = idx1
!              sorted        = .FALSE.
!            END IF
!          END DO
!        END DO


! Satellite antenna offsets
      CASE(12)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
! Sort on PRN if satellite specific
            IF (neq%par(idx1)%locq(4)==0 .AND. neq%par(idx2)%locq(4) == 0) THEN
              WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(idx1)%locq(5)/100)), &
                                                  neq%par(idx1)%locq(5)
              CALL svn2prn(6,svnnr,neq%par(idx1)%time%mean, &
                           grpnr1,timint,irc)
              WRITE(svnnr,"(A1,I3)")g_syssvn(INT(neq%par(idx2)%locq(5)/100)), &
                                                  neq%par(idx2)%locq(5)
              CALL svn2prn(6,svnnr,neq%par(idx2)%time%mean, &
                           grpnr2,timint,irc)
            ELSE
              grpnr1= neq%par(idx1)%locq(5)
              grpnr2= neq%par(idx2)%locq(5)
            ENDIF

! Check for technique (default: GNSS; else: SLR LRA)
            techn1 = 1
            techn2 = 1
            IF ( is_techn(neq%par(idx1),slr=1) ) techn1 = 3
            IF ( is_techn(neq%par(idx2),slr=1) ) techn2 = 3

            IF ( techn1 > techn2                                    .OR.  &
                (techn1==techn2                                     .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4) )      .OR.  &
                (techn1==techn2                                     .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 grpnr1>grpnr2)                                     .OR.  &
                (techn1==techn2                                     .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 grpnr1==grpnr2                                     .AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)   .OR.  &
                (techn1==techn2                                     .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 grpnr1==grpnr2                                     .AND. &
                 neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
                 neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Center of mass coordinates
      CASE(16)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%time%mean>neq%par(idx2)%time%mean     .OR.  &
                (neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
                 neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Receiver antenna pattern
      CASE(18)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)

            frq1 = MOD(neq%par(idx1)%locq(4) , 100)
            frq2 = MOD(neq%par(idx2)%locq(4) , 100)
            sys1 = MOD(neq%par(idx1)%locq(5) , 100)
            sys2 = MOD(neq%par(idx2)%locq(5) , 100)
            zen1 = neq%par(idx1)%locq(4) / 100
            zen2 = neq%par(idx2)%locq(4) / 100
            azi1 = neq%par(idx1)%locq(5) / 100
            azi2 = neq%par(idx2)%locq(5) / 100

            IF (neq%par(idx1)%name    > neq%par(idx2)%name          .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)> neq%par(idx2)%locq(2))      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)> neq%par(idx2)%locq(3))      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 >  sys2                                )      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 == sys2 .AND. frq1 >  frq2             )      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 == sys2 .AND. frq1 == frq2                    .AND. &
                 zen1 >  zen2                                )      .OR.  &
                (neq%par(idx1)%name   ==neq%par(idx2)%name          .AND. &
                 neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2)       .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)       .AND. &
                 sys1 == sys2 .AND. frq1 == frq2                    .AND. &
                 zen1 == zen2 .AND. azi1 >  azi2             )) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Global ionosphere model parameters
      CASE(19)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))       .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 ABS(neq%par(idx1)%locq(5))>ABS(neq%par(idx2)%locq(5))).OR.&
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                 ABS(neq%par(idx1)%locq(5))==ABS(neq%par(idx2)%locq(5)).AND.&
                 neq%par(idx1)%locq(5)<neq%par(idx2)%locq(5))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Scaling factors for Vienna grid files
      CASE(22)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF ( neq%par(idx1)%locq(2) >neq%par(idx2)%locq(2)        .OR.  &
                (neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2) .AND. &
                 neq%par(idx1)%name    >neq%par(idx2)%name)          .OR.  &
                (neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2) .AND. &
                 neq%par(idx1)%name   ==neq%par(idx2)%name    .AND. &
                 neq%par(idx1)%locq(3) >neq%par(idx2)%locq(3))       .OR.  &
                (neq%par(idx1)%locq(2)==neq%par(idx2)%locq(2) .AND. &
                 neq%par(idx1)%name   ==neq%par(idx2)%name    .AND. &
                 neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3) .AND. &
                 neq%par(idx1)%locq(4) >neq%par(idx2)%locq(4))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Receiver clocks
      CASE(23)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)   .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%time%mean==neq%par(idx2)%time%mean   .AND. &
                 neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Satellite clocks
      CASE(24)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF (neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
                (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                 neq%par(idx1)%time%mean>neq%par(idx2)%time%mean)) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO



! Satellite antenna patterns
      CASE(25)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
! Sort on PRN if satellite specific
            IF (neq%par(idx1)%locq(2)==0 .AND. neq%par(idx2)%locq(2) == 0) THEN
              WRITE(svnnr1,"(A1,I3)")g_syssvn(INT(neq%par(idx1)%locq(3)/100)), &
                                                  neq%par(idx1)%locq(3)
              CALL svn2prn(6,svnnr1,neq%par(idx1)%time%mean, &
                           grpnr1,timint,irc)
              WRITE(svnnr2,"(A1,I3)")g_syssvn(INT(neq%par(idx2)%locq(3)/100)), &
                                                  neq%par(idx2)%locq(3)
              CALL svn2prn(6,svnnr2,neq%par(idx2)%time%mean, &
                           grpnr2,timint,irc)
            ELSE
              grpnr1= neq%par(idx1)%locq(3)
              grpnr2= neq%par(idx2)%locq(3)
              svnnr1= ' '
              svnnr2= ' '
            ENDIF

!            IF (neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3)        .OR.  &
            IF (grpnr1 > grpnr2                                    .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)      .AND. &
                (grpnr1 == grpnr2 .AND. svnnr1 > svnnr2     )      .OR.  &
                (grpnr1 == grpnr2 .AND. svnnr1 == svnnr2           .AND. &
                 neq%par(idx1)%locq(5)>neq%par(idx2)%locq(5))      .OR.  &
!                (neq%par(idx1)%locq(3)==neq%par(idx2)%locq(3)      .AND. &
                (grpnr1 == grpnr2 .AND. svnnr1 == svnnr2           .AND. &
                 neq%par(idx1)%locq(5)==neq%par(idx2)%locq(5)      .AND. &
                 neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! SLR Range Biases
      CASE(26)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)

            IF ( neq%par(idx1)%name > neq%par(idx2)%name         .OR.  &
                 (neq%par(idx1)%name == neq%par(idx2)%name       .AND. &
                  neq%par(idx1)%locq(5) > neq%par(idx2)%locq(5)) .OR.  &
                 (neq%par(idx1)%name == neq%par(idx2)%name       .AND. &
                  neq%par(idx1)%locq(5) == neq%par(idx2)%locq(5) .AND. &
                  neq%par(idx1)%locq(4) > neq%par(idx2)%locq(4))      ) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF

          END DO
        END DO


! HOI scaling factors
      CASE(27)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF(neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
               (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO

! GNSS-specific parameters
      CASE(30)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)
            IF(neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
               (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                neq%par(idx1)%locq(4)>neq%par(idx2)%locq(4))       .OR.  &
               (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                neq%par(idx1)%locq(4)==neq%par(idx2)%locq(4)       .AND. &
                neq%par(idx1)%locq(3)>neq%par(idx2)%locq(3)) ) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


! Helmert parameters
      CASE(28)
        DO WHILE (.NOT. sorted)
          sorted = .TRUE.
          DO iOrd=iStart, iEnd-1
            idx1 = index(iOrd)
            idx2 = index(iOrd+1)

            IF(neq%par(idx1)%name>neq%par(idx2)%name               .OR.  &
               (neq%par(idx1)%name==neq%par(idx2)%name             .AND. &
                neq%par(idx1)%locq(2)>neq%par(idx2)%locq(2))) THEN
              index(iOrd)   = idx2
              index(iOrd+1) = idx1
              sorted        = .FALSE.
            END IF
          END DO
        END DO


    END SELECT
  END DO


! Sort NQ0 according to index array
! ---------------------------------
  DO iIndex=1, neq%misc%nPar-1
    iFrom = index(iIndex)
    IF (iFrom==iIndex) CYCLE


! Sort aNor
! ---------
! switch aNor columns
    DO iPar=1, neq%misc%nPar
      idx1 = ikf(iFrom, iPar)
      idx2 = ikf(iIndex, iPar)
      tmp  = neq%aNor(idx1)
      neq%aNor(idx1) = neq%aNor(idx2)
      neq%aNor(idx2) = tmp
    END DO

! exchange diagonal and off diagonal element
    idx1 = ikf(iFrom, iIndex)
    idx2 = MIN(iFrom, iIndex)
    idx2 = ikf(idx2, idx2)
    tmp  = neq%aNor(idx1)
    neq%aNor(idx1) = neq%aNor(idx2)
    neq%aNor(idx2) = tmp


! Sort bNor, par
! --------------
! switch two elements of bNor
    tmp = neq%bNor(iFrom)
    neq%bNor(iFrom)  = neq%bNor(iIndex)
    neq%bNor(iIndex) = tmp

! switch two elements of par
    tmpPar = neq%par(iFrom)
    neq%par(iFrom)  = neq%par(iIndex)
    neq%par(iIndex) = tmpPar


! Update index array
! ------------------
    index(iIndex) = iIndex
    DO idx1=iIndex+1, neq%misc%nPar
      IF (index(idx1)==iIndex) THEN
        index(idx1) = iFrom
        EXIT
      END IF
    END DO

  END DO


! End subroutine
! --------------
END SUBROUTINE neqsort


END MODULE
