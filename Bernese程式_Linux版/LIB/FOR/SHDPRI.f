      MODULE s_SHDPRI
      CONTAINS

C*
      SUBROUTINE SHDPRI(NSACHG,SATCHG,NUMCHG,CHGTIM,
     1                  IPRCHS,IPRCHG,IARC,TARC,IDXCHG,mooecl)
CC
CC NAME       :  SHDPRI
CC
CC PURPOSE    :  PRINT SHADOW ENTRY AND EXIT TIMES
CC
CC PARAMETERS :
CC         IN :  NSACHG : NUMBER OF ECLIPSING SATELLITES       I*4
CC               SATCHG(I),I=1,..,NSACHG: SATELLITE NUMBERS    I*4
CC                        OF ECLIPSING SAT.
CC               NUMCHG(I),I=1,..,NSACHG: NUMBER OF            I*4
CC                        LIGHT/SHADOW CHANGES FOR EACH SAT
CC               CHGTIM(2,I,J),I=1,..,NSACHG, J=1,..,NUMCHG(I) R*8
CC                        ECLIPSE TIMES
CC                        CHGTIM(1,...): ECL. ENTRY TIME (MJD)
CC                        CHGTIM(2,...): ECL. EXIT TIME (MJD)
CC               IPRCHS : PRINT FLAG FOR ECLIPSING SATELLITE   I*4
CC                        =0: NOT ALL ECLIPSING SATELLITES
CC                            CAN BE PRINTED
CC                        =1: PRINTING OK (COMPLETE)
CC               IPRCHG(I),I=1,..,NSACHG: PRINT INDEX FOR ECL. I*4
CC                        SATELLITES
CC                        =0: NOT ALL ECLIPSE TIMES CAN BE
CC                            PRINTED
CC                        =1: PRINTING OK (COMPLETE)
CC               IARC   : ARC NUMBER                           I*4
CC               TARC(J),J=1,2: ARC START AND END TIME (MJD)   R*8
CC               mooecl : Structure containing lunar eclipses  t_mooecl
CC      LOCAL :  IDXCHG(I),I=1,..,NSACHG: INDEX ARRAY FOR      I*4
CC                        ORDERING OF ECLIPSING SATELLITES
CC
CC REMARKS    :  ---
CC
CC AUTHOR     :  M. ROTHACHER
CC
CC VERSION    :  3.5
CC
CC CREATED    :  28-SEP-93
CC
CC CHANGES    :  03-OCT-00 : RD: HANDLE ECLIPSE FOR ONLY ONE EPOCH
CC               13-JAN-02 : HU: PRINT LUNAR ECLIPSE INFO
CC               14-JAN-02 : HU: ERROR CONCERING MOON SHADOW PROTOCOL CORRECTED
CC               26-JAN-02 : HU: ERROR CORRECTED
CC               23-JUN-05 : MM: IMPLICIT NONE AND DECLARATIONS ADDED
CC               21-JUL-06 : HU: INDEX ERROR GETTING MAGNITUDE OF ECLIPSE
CC               10-AUG-10 : RD: USE TIMST2 INSTEAD OF TIMSTR
CC               28-JAN-11 : SL: USE M_BERN WITH ONLY
CC
CC COPYRIGHT  :  ASTRONOMICAL INSTITUTE
CC               UNIVERSITY OF BERN
CC               SWITZERLAND
CC
C*
      USE m_bern,   ONLY: lfnErr, lfnPrt
      USE d_shadow, ONLY: t_mooecl,maxmev
      USE s_iordup
      USE s_dordup
      USE s_timst2
      IMPLICIT NONE
C
C DECLARATIONS INSTEAD OF IMPLICIT
C --------------------------------
      INTEGER*4 IA    , IARC  , ICHG  , IEPO  , IEVOLD, INDEPO, INDSAT,
     1          IPRCHS, ISACHG, ISAT  , ITPASS, ITPMIN, ITPSEC, JEPO  ,
     2          MXCCHG, MXCECL, NEVENT, NSACHG, NSAT
C
      REAL*8    DTPASS, ECL
C
CCC       IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*40 TSTRNG
      CHARACTER*6  MXNECL,MXNCHG,eclstr
      CHARACTER*1  EVENTF
C
      REAL*8       CHGTIM(2,MXCECL,MXCCHG),TARC(2),epoch(2)
C
      INTEGER*4    SATCHG(MXCECL),NUMCHG(MXCECL),IPRCHG(MXCECL)
      INTEGER*4    IDXCHG(MXCECL),ievent(2)
      INTEGER*4    IDXEPO(MAXMEV)
C
      TYPE(t_mooecl) mooecl
C
C MAXIMUM DIMENSION COMMONS
C -------------------------
      COMMON/MCMECL/MXCECL,MXNECL
      COMMON/MCMCHG/MXCCHG,MXNCHG
C
cc      INCLUDE 'COMLFNUM.inc'
C
C PRINT SHADOW ENTRY AND EXIT TIMES FOR ALL SATELLITES
C ----------------------------------------------------
      IF (NSACHG.GT.0) THEN
C
C PRINT WARNING, IF NOT ALL ECLIPSING SATELLITES COULD BE SAVED
C
        IF (IPRCHS.EQ.0) THEN
          WRITE(LFNERR,901) MXCECL
901       FORMAT(/,' *** SR SHDPRI: TOO MANY ECLIPSING ',
     1             'SATELLITES',/,
     2         16X,'NOT ALL ECLIP. SATEL. WILL BE PRINTED',/,
     3         16X,'MAX. NUMBER ALLOWED:',I3,/)
        ENDIF
C
C PRINT WARNING, IF NOT ALL ECLIPSING TIMES COULD BE SAVED
C
        DO 10 ISACHG=1,NSACHG
          IF (IPRCHG(ISACHG).EQ.0) THEN
            WRITE(LFNERR,902) SATCHG(ISACHG),MXCCHG
902         FORMAT(/,' *** SR SHDPRI: TOO MANY ECLIPSES',/,
     1           16X,'NOT ALL ECLIPSES WILL BE PRINTED',/,
     2           16X,'SATELLITE          :',I3,/,
     3           16X,'MAX. NUMBER ALLOWED:',I3,/)
          ENDIF
10      CONTINUE
C
C ORDER ECLIPSING SATELLITES
C
        CALL IORDUP(SATCHG,NSACHG,IDXCHG)
C
C WRITE TITLE LINES AND NUMBER OF ECLIPSING SATELLITES
C
        WRITE(LFNPRT,1010) IARC,NSACHG
1010    FORMAT(' ',79('-'),/,' ECLIPSING SATELLITES',12X,
     1       'ARC NUMBER:',I3,/,' ',79('-'),//,
     2       ' NUMBER OF ECLIPSING SATELLITES:',I3,//,
     3       ' SAT  ECLIPSE  DUR.(MIN)       ENTERING',14X,
     4       'EXITING',/,
     5       ' ---  -------  ---------',2(2X,19('-')))
C
C LOOP OVER ALL ECLIPSING SATELLITES
C ----------------------------------
        DO 200 ISACHG=1,NSACHG
          INDSAT=IDXCHG(ISACHG)
          WRITE(LFNPRT,'( )')
C
C LOOP OVER ECLIPSES OF ONE SATELLITES
C
          DO 100 ICHG=1,NUMCHG(INDSAT)
            EVENTF=' '
C
C IN SHADOW AT BEGINNING OF ARC
            IF (CHGTIM(1,INDSAT,ICHG).EQ.0.D0) THEN
              EVENTF='B'
              CHGTIM(1,INDSAT,ICHG)=TARC(1)
            ENDIF
C
C NO END OF SHADOW WITHIN ARC
            IF (CHGTIM(2,INDSAT,ICHG).EQ.0.D0) THEN
              EVENTF='E'
              CHGTIM(2,INDSAT,ICHG)=TARC(2)
            ENDIF
C
C PRINT ECLIPSE TIMES
            CALL TIMST2(1,2,CHGTIM(1:2,INDSAT,ICHG),TSTRNG)
            DTPASS=CHGTIM(2,INDSAT,ICHG)-CHGTIM(1,INDSAT,ICHG)
            IF  (DTPASS.LT.0D0) DTPASS=0D0
            ITPASS=IDNINT(DTPASS*86400.D0)
            ITPMIN=ITPASS/60
            ITPSEC=ITPASS-ITPMIN*60
            IF (ICHG.EQ.1) THEN
              WRITE(LFNPRT,1020) SATCHG(INDSAT),ICHG,ITPMIN,ITPSEC,
     1                           EVENTF,TSTRNG
1020          FORMAT(I4,I6,I9,':',I2.2,1X,A1,2X,A)
            ELSE
              WRITE(LFNPRT,1030) ICHG,ITPMIN,ITPSEC,EVENTF,TSTRNG
1030          FORMAT(4X,I6,I9,':',I2.2,1X,A1,2X,A)
            ENDIF
C
100       CONTINUE
C
200     CONTINUE
C
C WRITE EMPTY LINE AT THE END
        WRITE(LFNPRT,1040)
1040    FORMAT(/,' ',79('-'),//)
      ENDIF

      IF (mooecl%iecl==1 .AND. mooecl%nsat > 0) THEN
        DO isat=1,mooecl%nsat
          CALL DORDUP(mooecl%sat(isat)%tevent(1),
     1                mooecl%sat(isat)%nevent,idxepo)
C
C Get magnitude of eclipse
          DO iepo=1,mooecl%sat(isat)%nevent
            indepo=idxepo(iepo)
            IF (mooecl%sat(isat)%ievent(indepo)==-2) ia=iepo
            IF (mooecl%sat(isat)%ievent(indepo)== 0)
     1                           ecl=mooecl%sat(isat)%eclmin(indepo)
            IF (mooecl%sat(isat)%ievent(indepo)==+2) THEN
              DO jepo=MAX(ia,1),iepo
                indepo=idxepo(jepo)
                mooecl%sat(isat)%eclmin(indepo)=ecl
              ENDDO
            ENDIF
          ENDDO
C
C Remove minimum eclipse epochs
          DO iepo=1,mooecl%sat(isat)%nevent
            indepo=idxepo(iepo)
            IF (mooecl%sat(isat)%ievent(indepo)== 0) THEN
              mooecl%sat(isat)%ievent(indepo)=-99
            ENDIF
          ENDDO
C
C Remove events within Earth shadow
          ievold=-99
          DO iepo=1,mooecl%sat(isat)%nevent
            indepo=idxepo(iepo)
            IF (ievold==-3 .AND.
     1          mooecl%sat(isat)%ievent(indepo) /=3) THEN
              mooecl%sat(isat)%ievent(indepo)=-99
            ELSE
              ievold=mooecl%sat(isat)%ievent(indepo)
            ENDIF
          ENDDO
C
C Remove Earth shadow passages which are unrelated to Moon's shadow
C -> Earth shadow
          ievold=99
          DO iepo=1,mooecl%sat(isat)%nevent
            indepo=idxepo(iepo)
            IF (mooecl%sat(isat)%ievent(indepo)==-99) CYCLE
            IF (ievold >= 2 .AND.
     1          mooecl%sat(isat)%ievent(indepo) ==-3) THEN
              mooecl%sat(isat)%ievent(indepo) =-4
            ENDIF
            ievold=mooecl%sat(isat)%ievent(indepo)
          ENDDO
C Earth shadow ->
          ievold=-99
          DO iepo=mooecl%sat(isat)%nevent,1,-1
            indepo=idxepo(iepo)
            IF (mooecl%sat(isat)%ievent(indepo)==-99) CYCLE
            IF (ievold <= -2 .AND.
     1          mooecl%sat(isat)%ievent(indepo) == 3) THEN
              mooecl%sat(isat)%ievent(indepo) = 4
            ENDIF
            ievold=mooecl%sat(isat)%ievent(indepo)
          ENDDO
C Remove -44 pairs
          ievold=-99
          DO iepo=1,mooecl%sat(isat)%nevent
            indepo=idxepo(iepo)
            IF (mooecl%sat(isat)%ievent(indepo)==-99) CYCLE
            IF (ievold==-4 .AND.
     1          mooecl%sat(isat)%ievent(indepo) ==4) THEN
              mooecl%sat(isat)%ievent(indepo)  =-99
              mooecl%sat(isat)%ievent(indepo-1)=-99
            ENDIF
            ievold=mooecl%sat(isat)%ievent(indepo)
          ENDDO
C Remove -4,4 at end of interval
          IF (mooecl%sat(isat)%ievent(1) == 4) THEN
            mooecl%sat(isat)%ievent(1)=-99
          ENDIF
          IF (mooecl%sat(isat)%ievent(mooecl%sat(isat)%nevent)==-4) THEN
            mooecl%sat(isat)%ievent(mooecl%sat(isat)%nevent)=-99
          ENDIF
        ENDDO
C
C Remove satellites without events
        nsat=0
        DO isat=1,mooecl%nsat
          nevent=0
          DO iepo=1,mooecl%sat(isat)%nevent
            IF (mooecl%sat(isat)%ievent(iepo)/=-99) nevent=nevent+1
          ENDDO
          IF (nevent==0) THEN
            mooecl%sat(isat)%svn=0
          ELSE
            nsat=nsat+1
          ENDIF
        ENDDO

        IF (nsat>0) THEN
! Order eclipsing satellites
          CALL IORDUP(mooecl%sat(1:mooecl%nsat)%svn,mooecl%nsat,idxchg)
!
! Write title lines and number of eclipsing satellites
          WRITE(LFNPRT,"(' ',79('-'),/,' ECLIPSES BY THE MOON',12X,
     1       'ARC NUMBER:',I3,/,' ',79('-'),//,
     2       ' NUMBER OF ECLIPSING SATELLITES:',I3,//,
     3       ' SAT  ECLIPSE  DUR.(MIN)       ENTERING',14X,
     4       'EXITING',8X,'MAX (%)',/,
     5       ' ---  -------  ---------',2(2X,19('-')),2X,9('-'))")
     6       iarc,nsat

! Loop over all eclipsing satellites
! ----------------------------------
          DO isat=1,mooecl%nsat
            indsat=idxchg(isat)
            IF (mooecl%sat(indsat)%svn == 0) CYCLE
            WRITE(lfnprt,'( )')
C Order epochs of events
            CALL DORDUP(mooecl%sat(indsat)%tevent(1),
     1                  mooecl%sat(indsat)%nevent,idxepo)

C Loop over events
C ----------------
            ichg=1
            epoch(1)=0D0
            DO jepo=1,mooecl%sat(indsat)%nevent
              indepo=idxepo(jepo)
              IF (mooecl%sat(indsat)%ievent(indepo)==-99) CYCLE
              eventf=' '

              epoch(2) =mooecl%sat(indsat)%tevent(indepo)
              ievent(2)=mooecl%sat(indsat)%ievent(indepo)

              IF (epoch(1)>0D0) THEN
C In shadow at beginning or end of interval
                IF (ABS(epoch(1)-tarc(1))<=SPACING(tarc(1))) eventf='B'
                IF (ABS(epoch(2)-tarc(2))<=SPACING(tarc(2))) eventf='E'

! Print eclipse times
! -------------------
                CALL timst2(1,2,epoch,tstrng)
                dtpass=epoch(2)-epoch(1)
                IF (dtpass<0D0) dtpass=0D0
                itpass=IDNINT(dtpass*86400.D0)
                itpmin=itpass/60
                itpsec=itpass-itpmin*60

! magnitude or type of eclipse
                WRITE(eclstr,"(F6.1)")
     1                       mooecl%sat(indsat)%eclmin(indepo)*100
                IF (ievent(1)==-1 .AND. ievent(2)==1) eclstr=' TOTAL'
                IF (ievent(1)==-3 .AND. ievent(2)==3) eclstr=' EARTH'
                IF (ievent(1)==-4 .AND. ievent(2)==4) eclstr=' EARTH'

                IF (ievent(1)<2.OR.ievent(2)>-2) THEN
                  IF (ichg==1) THEN
                    WRITE(lfnprt,
     1                    "(I4,I6,I9,':',I2.2,1X,A1,2X,A40,3X,A6)")
     2                    mooecl%sat(indsat)%svn,ichg,itpmin,itpsec,
     3                    eventf,tstrng,eclstr
                  ELSE
                    WRITE(lfnprt,
     1                    "(4X,I6,I9,':',I2.2,1X,A1,2X,A40,3X,A6)")
     2                    ichg,itpmin,itpsec,eventf,tstrng,eclstr
                  ENDIF
                  ichg=ichg+1
                ENDIF

              ENDIF
              epoch(1) =epoch(2)
              ievent(1)=ievent(2)

!       write(lfnprt,*)mooecl%sat(indsat)%svn,
!     1                mooecl%sat(indsat)%tevent(indepo),
!     1                mooecl%sat(indsat)%ievent(indepo),
!     1                mooecl%sat(indsat)%eclmin(indepo)
            ENDDO

          ENDDO
          WRITE(LFNPRT,"(/,' ',79('-'),//)")
        ENDIF
      ENDIF
C
      RETURN

      END SUBROUTINE

      END MODULE
