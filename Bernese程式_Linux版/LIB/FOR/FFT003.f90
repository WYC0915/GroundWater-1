MODULE s_FFT003
CONTAINS

! -------------------------------------------------------------------------
! Bernese GPS Software Version 5.1
! -------------------------------------------------------------------------

SUBROUTINE fft003(nSamples,nPow,inTS,AR,AI)

! An example of the fast Fourier transform subroutine with N = 2**M.
! AR and AI are the real and imaginary part of data in the input and
! corresponding Fourier coefficients in the output.
! Copyright (c) Tao Pang 1997.

! -------------------------------------------------------------------------
! Purpose:    Fourier analysis of data arrays.
!
! Author:     Luca Ostini
!
! Created:    18-May-2009
! Last mod.:  18-May-2009
!
! Changes:    18-May-2009 LO: Conversion for BSW 5.0 - f90
!             18-Nov-2009 LO: Major changes for consistency
!
! Copyright:  Astronomical Institute
!             University of Bern
!             Switzerland
! -------------------------------------------------------------------------

! Modules
! -------
  USE m_bern
  USE d_const,    ONLY: Pi

  USE s_alcerr
  USE s_exitrc

 IMPLICIT NONE

! Subroutine Arguments
! ---------------------
 INTEGER(i4b)                           :: nSamples ! Nr of samples
 INTEGER(i4b)                           :: nPow     ! nSamples = 2**nPow
 REAL(r8b),DIMENSION(:),POINTER         :: inTS     ! Input TS in time domain
 REAL(r8b),DIMENSION(:),POINTER         :: AR       ! Output Real Part
 REAL(r8b),DIMENSION(:),POINTER         :: AI       ! Output Imaginary Part

! Local Parameters
! ----------------
 CHARACTER(LEN=shortLineLength) , PARAMETER :: srN = 'fft003'

 INTEGER(i4b)                           :: N,M
 INTEGER(i4b)                           :: N1,N2,I,J,K,L,L1,L2
 REAL(r8b)                              :: A1,A2,Q,U,V

 INTEGER(i4b)                           :: iSam

! Subroutine
! ----------

 ! Copy inTs into AR
 AI(:) = 0.0D0
 DO iSam = 1,nSamples
    AR(iSam) = inTS(iSam)
 END DO

  N = nSamples
  M = nPow

  N2 = N/2
  N1 = 2**M

  IF(N1.NE.N)THEN
     WRITE(lfnerr,'(/,A,/)') ' *** SR FFT003: Indices do not match'
     CALL exitrc(2)
  END IF
!
! Rearrange the data to the bit reversed order
!
  L = 1
  DO K = 1, N-1
     IF (K.LT.L) THEN
        A1    = AR(L)
        A2    = AI(L)
        AR(L) = AR(K)
        AR(K) = A1
        AI(L) = AI(K)
        AI(K) = A2
     END IF
     J   = N2
     DO WHILE (J.LT.L)
        L = L-J
        J = J/2
     END DO
     L = L+J
  END DO
!
! Perform additions at all levels with reordered data
!
  L2 = 1
  DO L = 1, M
     Q  =  0.0
     L1 =  L2
     L2 =  2*L1
     DO K = 1, L1
        U   =  DCOS(Q)
        V   = -DSIN(Q)
        Q   =  Q + Pi/L1
        DO J = K, N, L2
           I     =  J + L1
           A1    =  AR(I)*U-AI(I)*V
           A2    =  AR(I)*V+AI(I)*U
           AR(I) =  AR(J)-A1
           AR(J) =  AR(J)+A1
           AI(I) =  AI(J)-A2
           AI(J) =  AI(J)+A2
        END DO
     END DO
  END DO

 RETURN

END SUBROUTINE fft003

END MODULE s_FFT003
