C*
       PROGRAM BINMERGE
CC
CC NAME        :  BINMERGE
CC
CC PURPOSE     :  SEE BELOW
CC
CC REMARKS     :  ORIGINAL FROM ftp://ssd.jpl.nasa.gov/pub/eph/planets/fortran
CC                ADAPTED FOR BSW VERSION 5.1
CC
CC AUTHORS     :
CC
CC CREATED     :  __-___-____
CC
CC CHANGES     :  13-AUG-10 : SL: UPDATE AND MINIMIZATION OF DIFFS WRT ORIGINAL,
CC                                NUMBER ADDED TO SET KSIZE
CC                17-AUG-10 : SL: LFNERR ADDED
CC
C*
      USE m_bern, ONLY: lfnErr
c
c    Merge two binary JPL ephemeris files
c
c    NOTE that the user must set the values of NRECL and KSIZE
c      (between the lines of asterisks)
c
c     Input files : INEPH1, INEPH2
c       (the stop-date of INEPH1 must be .ge. the start-date of INEPH2)
c
c     Output file : OUTEPH
c
c     Last updated: 24 March 2008

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      CHARACTER*6 TTL1(14,3),TTL2(14,3),CNAM1(400),CNAM2(400)
      CHARACTER*80 INEPH1,INEPH2,OUTEPH,NUMBER

      DOUBLE PRECISION SS1(3),SS2(3),CVAL1(400),CVAL2(400),data(3000)
      double precision dend(2)

      INTEGER IPT1(3,12),IPT2(3,12),LPT1(3),LPT2(3)

      OPEN(10,FILE='BINMERGE.INP',STATUS='OLD')
      READ(10,"(A)") INEPH1
      READ(10,"(A)") INEPH2
      READ(10,"(A)") OUTEPH
      READ(10,"(A)") NUMBER
      CLOSE(10)

c *************************************************************
c *************************************************************
c
c  ****  User must set values of NRECL and KSIZE           ****
c  ****  defaults are set to zero to force a selection     ****
c
c  NRECL=1 if 'RECL' in the OPEN statement is to be in s.p. words
c  NRECL=4 if 'RECL' in the OPEN statement is to be in bytes
c
      data nrec/0/
c      data nrecl/1/
      data nrecl/4/
c
c  *****  Set the value of KSIZE  *****
c
c   for DE200, use 1652; for DE405, use 2036; for DE406, use 1456
c
      data ksize /0/
      IF(NUMBER .EQ. 'DE200') THEN
        ksize = 1652
      ELSEIF(NUMBER .EQ. 'DE405') THEN
        ksize = 2036
      ELSEIF(NUMBER .EQ. 'DE406') THEN
        ksize = 1456
      ENDIF

c *************************************************************
c *************************************************************

      irecl=ksize*nrecl
      if(irecl .eq. 0) then
        write(lfnErr,*) ' User must set values for NRECL and KSIZE'
        stop
      endif

        OPEN(11,

     *       FILE=INEPH1,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=irecl,
     *       STATUS='OLD')

      READ(11,REC=1)TTL1,CNAM1,SS1,NCON1,AU1,EMRAT1,IPT1,NUMDE1,LPT1
      READ(11,REC=2)CVAL1

        OPEN(12,

     *       FILE=INEPH2,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=irecl,
     *       STATUS='OLD')

      READ(12,REC=1)TTL2,CNAM2,SS2,NCON2,AU2,EMRAT2,IPT2,NUMDE2,LPT2
      READ(12,REC=2)CVAL2

c  *****  Check for allowable time-spans  *****

      if(ss1(1) .ge. ss2(2))write(lfnErr,'(a50)')
     * 'time-spans reversed: switch assignments?'

      if(ss1(2) .lt. ss2(1)) write(lfnErr,'(a50)')
     * 'time-spans neither abut nor overlap'


c  *****  Check for matching ephemerides  *****

      if (numde1 .ne. numde2) write(lfnErr,'(a50)')'NUMDE''s differ'

      if (ncon1 .ne. ncon2) write(lfnErr,'(a50)')'NCON''s differ'

      if (au1 .ne. au2) write(lfnErr,'(a50)')'AU''s differ'

      if (emrat1 .ne. emrat2) write(lfnErr,'(a50)')'EMRAT''s differ'

      do i=1,ncon1
      if (cval1(i) .ne. cval2(i)) write(lfnErr,'(a50)')'CVAL''s differ'
      if (cnam1(i) .ne. cnam2(i)) write(lfnErr,'(a50)')'CNAM''s differ'
      enddo

      do i=1,3
        do j=1,12
          if (ipt1(i,j) .ne. ipt2(i,j))
     .      write(lfnErr,'(a50)')'IPT''s differ'
        enddo
        if (lpt1(i) .ne. lpt2(i)) write(lfnErr,'(a50)')'LPT''s differ'
      enddo


c  *****  Combine and Write initial header info  *****

      dend(1)=ss1(2)
      dend(2)=ss2(2)

      ss1(2)=ss2(2)
      do i=1,14
      ttl1(i,3)=ttl2(i,3)
      enddo


        OPEN(10,

     *       FILE=OUTEPH,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=irecl,
     *       STATUS='NEW')


      WRITE(10,REC=1)TTL1,CNAM1,SS1,NCON1,AU1,EMRAT1,IPT1,NUMDE1,LPT1
      WRITE(10,REC=2)CVAL1
      nrp=2

      ndata=ksize/2

      dz=ss1(1)

      do 6 nfil=11,12

      nrec=2

 5      nrec=nrec+1

c      READ(nfil,REC=NREC,END=6,ERR=98)(DATA(K),K=1,NDATA)

      READ(nfil,REC=NREC,IOSTAT=ios,ERR=98)(DATA(K),K=1,NDATA)
      if(ios .lt. 0) go to 6

      if(data(1) .lt. dz) go to 5
      if(data(1) .gt. dz) write(lfnErr,'(a50)')'non-matching dates'
      dz=data(2)

      nrp=nrp+1
      WRITE(10,REC=NRP,ERR=99)(DATA(K),K=1,NDATA)

      if(data(2) .lt. dend(nfil-10)) go to 5

 6      continue

      close(10)
      close(11)
      close(12)

      stop

 98     write(lfnErr,'(/a18,i3)')'read error on unit',nfil
      stop

 99     write(lfnErr,
     *  '(/''write error on output file, nrec='',i6)')nrp
      stop

      end
