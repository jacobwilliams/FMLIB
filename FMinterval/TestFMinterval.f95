
!  This is a test program for the FM 1.4 multiple-precision interval arithmetic package.

!  All of the interval arithmetic routines are tested.
!  Precision is set to 30 significant digits and the results are checked to that accuracy.

!  If all tests are completed successfully, this line is printed:

!  1044 cases tested.  No errors were found.

   MODULE TEST_VARS_IA

      USE FMVALS
      USE FMZM
      USE FM_INTERVAL_ARITHMETIC

!             Declare the derived type variables of type (FM), (IM), and (ZM).
!             These are in the form that would be found in a user program.

      TYPE (FM_INTERVAL), SAVE ::  A, B, C, D, RESULT, CORRECT,                   &
                          AVEC(3),  BVEC(3),  CVEC(3),  DVEC(3),                  &
                          AMAT(3,3), BMAT(3,3), CMAT(3,3), DMAT(3,3)

      TYPE (FM), SAVE :: ERROR, MFM1, MFMV(3), MFMV2(3,3), MV4(4), MV8(8)
      TYPE (IM), SAVE :: MIM1, MIMV(3), MIMV2(3,3)
      TYPE (ZM), SAVE :: MZM1, MZMV(3), MZMV2(3,3)

!             These are the variables that are not multiple precision.

      INTEGER, SAVE :: J1, JV(3), JV2(3,3)
      REAL, SAVE :: R1, RSMALL, RV(3), RV2(3,3)
      DOUBLE PRECISION, SAVE :: D1, DSMALL, DV(3), DV2(3,3)
      COMPLEX, SAVE :: C1, CV(3), CV2(3,3)
      COMPLEX (KIND(0.0D0)), SAVE :: CD1, CDV(3), CDV2(3,3)

      CHARACTER(100), SAVE :: ST1, ST2, STRING, STV2(3,3)
      INTEGER, SAVE :: KLOG, KWSAVE, NCASE, NERROR
      REAL, SAVE :: TIME1, TIME2
      LOGICAL, EXTERNAL :: FMCOMPARE

      INTERFACE SUM_IVL
         MODULE PROCEDURE SUM0
         MODULE PROCEDURE SUM1
         MODULE PROCEDURE SUM2
      END INTERFACE

      CONTAINS

      FUNCTION SUM0(A,B)     RESULT (RETURN_VALUE)

!  Function that returns an interval result.

      IMPLICIT NONE
      TYPE (FM_INTERVAL) :: A,B,RETURN_VALUE
      RETURN_VALUE = A + B
      END FUNCTION SUM0

      FUNCTION SUM1(A,B)     RESULT (RETURN_VALUE)

!  Function that returns a interval vector result.

      IMPLICIT NONE
      TYPE (FM_INTERVAL) :: A(3),B(3),RETURN_VALUE(3)
      INTEGER :: J
      DO J = 1, 3
         RETURN_VALUE(J) = A(J) + B(J)
      ENDDO
      END FUNCTION SUM1

      FUNCTION SUM2(A,B)     RESULT (RETURN_VALUE)

!  Function that returns a interval matrix result.

      IMPLICIT NONE
      TYPE (FM_INTERVAL) :: A(3,3),B(3,3),RETURN_VALUE(3,3)
      INTEGER :: J, K
      DO J = 1, 3
         DO K = 1, 3
            RETURN_VALUE(J,K) = A(J,K) + B(J,K)
         ENDDO
      ENDDO
      END FUNCTION SUM2

      SUBROUTINE TEST1

!  Input and output testing.

      IMPLICIT NONE
      INTEGER :: J, K, L

      WRITE (KW,"(/' Testing input and output conversion for intervals.')")

!             NCASE is the number of cases tested.

      NCASE = 1
      RESULT = TO_FM_INTERVAL( TO_FM(14) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_FM(14) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 14
      MFM1 = 14

!             Use the .NOT. because FMCOMPARE returns FALSE for special cases like MD = UNKNOWN,
!             and these should be treated as errors for these tests.

      IF ( (.NOT. FMCOMPARE(RESULT%LEFT,'==',MFM1%MFM)) .OR.  &
           (.NOT. FMCOMPARE(RESULT%RIGHT,'==',MFM1%MFM)) ) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 2
      RESULT = TO_FM_INTERVAL( 15 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 15 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 15
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 3
      RESULT = TO_FM_INTERVAL( 16.0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 16.0 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 16
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 4
      CALL FM_INTERVAL_SP2M(16.0,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 16.0 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 16
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 5
      RESULT = TO_FM_INTERVAL( 17.0D0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 17.0D0 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 17
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 6
      RESULT = TO_FM_INTERVAL( CMPLX( 18.0 , 28.0 ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CMPLX( 18.0 , 28.0 ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 18
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 7
      RESULT = TO_FM_INTERVAL( CMPLX( 19.0D0 , 29.0D0 , KIND(1.0D0) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CMPLX( 19.0D0 , 29.0D0 , KIND(1.0D0) ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 19
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 8
      A = TO_FM_INTERVAL( 20 , 21 )
      RESULT = TO_FM_INTERVAL( A )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( A ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = A
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 9
      RESULT = TO_FM_INTERVAL( TO_IM( 22 ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_IM( 22 ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 22
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 10
      MIM1 = 22
      CALL IM_INTERVAL_I2FM(MIM1,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_IM( 22 ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 22
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 11
      RESULT = TO_FM_INTERVAL( TO_ZM( " 23 + 33 i " ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_ZM( " 23 + 33 i " ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 23
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 12
      RESULT = TO_FM_INTERVAL( " 24 " )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( " 24 " ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = 24
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 13
      JV = (/ 25, 26, 27 /)
      AVEC = TO_FM_INTERVAL( JV )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ 25, 26, 27 /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 14
      RV = (/ 25.0, 26.0, 27.0 /)
      AVEC = TO_FM_INTERVAL( RV )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ 25.0, 26.0, 27.0 /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 15
      DV = (/ 25.0D0, 26.0D0, 27.0D0 /)
      AVEC = TO_FM_INTERVAL( DV )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ 25.0D0, 26.0D0, 27.0D0 /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 16
      CV = (/ CMPLX( 25.0 , 35.0 ), CMPLX( 26.0 , 36.0 ), CMPLX( 27.0 , 37.0 ) /)
      AVEC = TO_FM_INTERVAL( CV )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CMPLX( 25.0 , 35.0 ), CMPLX( 26.0 , 36.0 ),',  &
                     ' CMPLX( 27.0 , 37.0 ) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 17
      CDV = (/ CMPLX( 25.0 , 35.0 , KIND(1.0D0) ), CMPLX( 26.0 , 36.0 , KIND(1.0D0) ),  &
               CMPLX( 27.0 , 37.0 , KIND(1.0D0) ) /)
      AVEC = TO_FM_INTERVAL( CDV )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ CMPLX( 25.0 , 35.0 , KIND(1.0D0) ), '
      WRITE (KLOG,*) '                 CMPLX( 26.0 , 36.0 , KIND(1.0D0) ), '
      WRITE (KLOG,*) '                 CMPLX( 27.0 , 37.0 , KIND(1.0D0) ) /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 18
      BVEC = (/ 25, 26, 27 /)
      AVEC = TO_FM_INTERVAL( BVEC )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ TO_FM_INTERVAL(25), TO_FM_INTERVAL(26),',  &
                     ' TO_FM_INTERVAL(27) /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 19
      AVEC = TO_FM_INTERVAL( TO_FM( (/ 25, 26, 27 /) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_FM( (/ 25, 26, 27 /) ) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 20
      AVEC = TO_FM_INTERVAL( TO_IM( (/ 25, 26, 27 /) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_IM( (/ 25, 26, 27 /) ) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 21
      AVEC = TO_FM_INTERVAL( TO_ZM( (/ 25, 26, 27 /) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_ZM( (/ 25, 26, 27 /) ) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 22
      AVEC = TO_FM_INTERVAL( (/ "25", "26", "27" /) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( (/ "25", "26", "27" /) ) '
      DO J = 25, 27
         RESULT = AVEC(J-24)
         CORRECT = J
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 23
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( JV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( JV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 24
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( RV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( RV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 25
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( DV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( DV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 26
      DO J = 1, 3
         DO K = 1, 3
            CV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( CV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 27
      DO J = 1, 3
         DO K = 1, 3
            CDV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( CDV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CDV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 28
      DO J = 1, 3
         DO K = 1, 3
            BMAT(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( BMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( BMAT ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 29
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( MFMV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( MFMV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 30
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( MIMV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( MIMV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 31
      DO J = 1, 3
         DO K = 1, 3
            MZMV2(J,K) = (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( MZMV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( MZMV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 32
      DO J = 1, 3
         DO K = 1, 3
            WRITE ( STV2(J,K) , "(I5)" ) (J+30)*K
         ENDDO
      ENDDO
      AMAT = TO_FM_INTERVAL( STV2 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( STV2 ) '
      DO J = 1, 3
         WRITE (KLOG,"(A,I4)") '        Row ',J
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = (J+30)*K
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 33
      RESULT = TO_FM_INTERVAL( 41 , 42 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 41 , 42 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CALL FMI2M(41,CORRECT%LEFT)
      CALL FMI2M(42,CORRECT%RIGHT)
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 34
      RESULT = TO_FM_INTERVAL( 41.0 , 42.0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 41.0 , 42.0 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 35
      RESULT = TO_FM_INTERVAL( 41.0D0 , 42.0D0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( 41.0D0 , 42.0D0 ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 36
      RESULT = TO_FM_INTERVAL( CMPLX( 41.0 , 51.0 ) , CMPLX( 42.0 , 52.0 ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CMPLX( 41.0 , 51.0 ) , CMPLX( 42.0 , 52.0 ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 37
      RESULT = TO_FM_INTERVAL( CMPLX( 41.0D0 , 51.0D0 , KIND(1.0D0) ) ,  &
                            CMPLX( 42.0D0 , 52.0D0 , KIND(1.0D0) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( CMPLX( 41.0D0 , 51.0D0 , KIND(1.0D0) )',  &
                                  ' CMPLX( 42.0D0 , 52.0D0 , KIND(1.0D0) ) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 38
      RESULT = TO_FM_INTERVAL( TO_FM(41) , TO_FM(42) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_FM(41) , TO_FM(42) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 39
      RESULT = TO_FM_INTERVAL( TO_IM(41) , TO_IM(42) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_IM(41) , TO_IM(42) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 40
      RESULT = TO_FM_INTERVAL( TO_ZM(41) , TO_ZM(42) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( TO_ZM(41) , TO_ZM(42) ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 41
      RESULT = TO_FM_INTERVAL( "41" , "42" )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_INTERVAL( "41" , "42" ) '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 41 , 42 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM_INTERVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 42
      A = TO_FM_INTERVAL( 43 , 45 )
      RESULT = TO_FM(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM(A) '
      CALL FM_FORM('F40.35',TO_FM(A),STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 44 , 44 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_FM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 43
      MFMV = TO_FM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,  &
                       TO_FM_INTERVAL( 47 , 48 ) /) )
      DV = (/ 44.0D0, 45.0D0, 47.5D0 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,',  &
                               ' TO_FM_INTERVAL( 47 , 48 ) /) ) '
      DO J = 1, 3
         RESULT = MFMV(J)
         CALL FM_FORM('F40.35',MFMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         CORRECT = DV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_FM',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 44
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+50)*K , (J+55)*K )
            DV2(J,K) = (2*J+105)*K/2.0D0
         ENDDO
      ENDDO
      MFMV2 = TO_FM( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM( AMAT ) '
      DO J = 1, 3
         WRITE (KLOG,*) ' Row ',J
         DO K = 1, 3
            RESULT = MFMV2(J,K)
            CALL FM_FORM('F40.35',MFMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            CORRECT = DV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_FM',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 45
      A = TO_FM_INTERVAL( 43 , 45 )
      RESULT = TO_IM(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_IM(A) '
      CALL IM_FORM('I10',TO_IM(A),STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 44 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_IM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 46
      MIMV = TO_IM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,  &
                       TO_FM_INTERVAL( 47 , 48 ) /) )
      JV = (/ 44, 45, 47 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_IM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,',  &
                               ' TO_FM_INTERVAL( 47 , 48 ) /) ) '
      DO J = 1, 3
         RESULT = MIMV(J)
         CALL IM_FORM('I10',MIMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         CORRECT = JV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_IM',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 47
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+50)*K , (J+55)*K )
            JV2(J,K) = (2*J+105)*K/2
         ENDDO
      ENDDO
      MIMV2 = TO_IM( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_IM( AMAT ) '
      DO J = 1, 3
         WRITE (KLOG,*) ' Row ',J
         DO K = 1, 3
            RESULT = MIMV2(J,K)
            CALL IM_FORM('I10',MIMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            CORRECT = JV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_IM',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 48
      A = TO_FM_INTERVAL( 43 , 45 )
      RESULT = TO_ZM(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_ZM(A) '
      CALL ZM_FORM('F30.25','F30.25',TO_ZM(A),STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 44 , 44 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' TO_ZM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 49
      MZMV = TO_ZM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,  &
                       TO_FM_INTERVAL( 47 , 48 ) /) )
      RV = (/ 44.0, 45.0, 47.5 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_ZM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,',  &
                               ' TO_FM_INTERVAL( 47 , 48 ) /) ) '
      DO J = 1, 3
         RESULT = MZMV(J)
         CALL ZM_FORM('F30.25','F30.25',MZMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         CORRECT = RV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' TO_ZM',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 50
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+50)*K , (J+55)*K )
            RV2(J,K) = ( (J+50)*K + (J+55)*K ) / 2.0
         ENDDO
      ENDDO
      MZMV2 = TO_ZM( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_ZM( AMAT ) '
      DO J = 1, 3
         WRITE (KLOG,*) ' Row ',J
         DO K = 1, 3
            RESULT = MZMV2(J,K)
            CALL ZM_FORM('F30.25','F30.25',MZMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            CORRECT = RV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' TO_ZM',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 51
      OPEN(42,FILE="RWtest")
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+50)*K , (J+55)*K )
            CALL FM_INTERVAL_WRITE(42,AMAT(J,K))
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' CALL FM_INTERVAL_WRITE( AMAT )  and  CALL FM_INTERVAL_READ( AMAT ) '
      CLOSE(42)
      OPEN(42,FILE="RWtest")
      DO J = 1, 3
         WRITE (KLOG,*) ' Row ',J
         DO K = 1, 3
            CALL FM_INTERVAL_READ(42,BMAT(J,K))
            RESULT = BMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            CORRECT = AMAT(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' RWtest',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      CLOSE(42)

      NCASE = 52
      OPEN(42,FILE="RWtest")
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+50)*K , (J+55)*K )
            L = KW
            KW = 42
            CALL FMPRINT_INTERVAL(AMAT(J,K))
            KW = L
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' CALL FM_INTERVAL_WRITE( AMAT )  and  CALL FM_INTERVAL_READ( AMAT ) '
      CLOSE(42)
      OPEN(42,FILE="RWtest")
      DO J = 1, 3
         WRITE (KLOG,*) ' Row ',J
         DO K = 1, 3
            CALL FM_INTERVAL_READ(42,BMAT(J,K))
            RESULT = BMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            CORRECT = AMAT(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' RWtest',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      CLOSE(42)

      RETURN
      END SUBROUTINE TEST1

      SUBROUTINE TEST2

!  +, -, *, / testing.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing +, -, *, /.')")

!             NCASE is the number of cases tested.

      NCASE = 53
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A + B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) + ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 5 , 12 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 54
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A - B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) - ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , 3 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 55
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_SUB(A,B,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) - ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , 3 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 56
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A
      CALL FM_INTERVAL_SUB_R1(RESULT,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) - ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , 3 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 57
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B
      CALL FM_INTERVAL_SUB_R2(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) - ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , 3 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 58
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 59
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_MPY(A,B,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 60
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A
      CALL FM_INTERVAL_MPY_R1(RESULT,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 61
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B
      CALL FM_INTERVAL_MPY_R2(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 62
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , 10 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 63
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( -7 , -2 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) * ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , -6 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 64
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -21 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 65
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) * ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -21 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 66
      A = TO_FM_INTERVAL( -5 , 3 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , 3 ) * ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , 21 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 67
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) * ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , 21 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 68
      A = TO_FM_INTERVAL( -5 , 3 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , 3 ) * ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -21 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 69
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -2 , -7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) * ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , 21 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 70
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , -6 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 71
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) * ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -35 , 10 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 72
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( -7 , -2 )
      RESULT = A * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) * ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 35 )
      IF (.NOT.(RESULT == CORRECT)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 73
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 74
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_DIV(A,B,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 75
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A
      CALL FM_INTERVAL_DIV_R1(RESULT,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 76
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B
      CALL FM_INTERVAL_DIV_R2(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 77
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 78
      A = TO_FM_INTERVAL( 3 , 5 )
      B = TO_FM_INTERVAL( -7 , -2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 5 ) / ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -TO_FM(5)/2 , -TO_FM(3)/7 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 79
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -TO_FM(3)/2 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 80
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) / ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 81
      A = TO_FM_INTERVAL( -5 , 3 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , 3 ) / ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 82
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) / ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 83
      A = TO_FM_INTERVAL( -5 , 3 )
      B = TO_FM_INTERVAL( -7 , 2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , 3 ) / ( -7 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 84
      A = TO_FM_INTERVAL( -3 , 5 )
      B = TO_FM_INTERVAL( -7 , -2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -3 , 5 ) / ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -TO_FM(5)/2 , TO_FM(3)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 85
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -TO_FM(5)/2 , -TO_FM(3)/7 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 86
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( -2 , 7 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) / ( -2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(IS_OVERFLOW(ERROR))) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 87
      A = TO_FM_INTERVAL( -5 , -3 )
      B = TO_FM_INTERVAL( -7 , -2 )
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -5 , -3 ) / ( -7 , -2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(5)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 88
      A = 2
      B = TO_FM(3)
      RESULT = A / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) / ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(2)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 89
      A = 2
      B = 1/TO_FM(3)
      RESULT = A + 1/TO_FM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) + ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(3) , 2+1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 90
      A = 2
      B = 1/TO_FM(3)
      CALL FM_INTERVAL_ADD(A,B,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) + ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(3) , 2+1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 91
      A = 2
      B = 1/TO_FM(3)
      RESULT = A
      CALL FM_INTERVAL_ADD_R1(RESULT,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) + ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(3) , 2+1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 92
      A = 2
      B = 1/TO_FM(3)
      RESULT = B
      CALL FM_INTERVAL_ADD_R2(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) + ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(3) , 2+1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 93
      A = 2
      B = 1/TO_FM(3)
      RESULT = B
      CALL FM_INTERVAL_ADDI(RESULT,2)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) + ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(3) , 2+1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 94
      A = 2
      B = 1/TO_FM(6)
      RESULT = 1/TO_FM(6) + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 1/6 , 1/6 ) + ( 2 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2+1/TO_FM(6) , 2+1/TO_FM(6) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 95
      A = 2
      B = 1/TO_FM(3)
      RESULT = A - 1/TO_FM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) - ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2-1/TO_FM(3) , 2-1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 96
      A = 2
      B = 1/TO_FM(6)
      RESULT = 1/TO_FM(6) - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 1/6 , 1/6 ) - ( 2 , 2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 1/TO_FM(6)-2 , 1/TO_FM(6)-2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 97
      A = 13
      B = 1/TO_FM(3)
      RESULT = A * (1/TO_FM(3))
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 13 , 13 ) * ( 1/3 , 1/3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 13*1/TO_FM(3) , 13*1/TO_FM(3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 98
      A = 13
      B = 1/TO_FM(6)
      RESULT = (1/TO_FM(6)) * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 1/6 , 1/6 ) * ( 13 , 13 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 1/TO_FM(6)*13 , 1/TO_FM(6)*13 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 99
      A = 13
      B = 1/TO_FM(6)
      CALL FM_INTERVAL_MPYI(B,13,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 1/6 , 1/6 ) * 13 = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 1/TO_FM(6)*13 , 1/TO_FM(6)*13 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 100
      A = 13
      B = 1/TO_FM(6)
      RESULT = B
      CALL FM_INTERVAL_MPYI_R1(RESULT,13)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 1/6 , 1/6 ) * 13 = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 1/TO_FM(6)*13 , 1/TO_FM(6)*13 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 101
      A = 17
      B = TO_FM(11)
      RESULT = A / TO_FM(11)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 17 ) / ( 11 , 11 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 17/TO_FM(11) , 17/TO_FM(11) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 102
      A = 17
      B = TO_FM(13)
      RESULT = TO_FM(13) / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 13 , 13 ) / ( 17 , 17 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(13)/17 , TO_FM(13)/17 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 103
      A = TO_FM_INTERVAL( 17 , 18 )
      B = TO_FM_INTERVAL( TO_FM(1)/3 , TO_FM(1)/2 )
      RESULT = A ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 18 ) ** ( 1/3 , 1/2 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(17)**(TO_FM(1)/3) , TO_FM(18)**(TO_FM(1)/2) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 104
      A = 17
      B = TO_FM(1)/2
      RESULT = A ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 17 ) ** ( 0.5 , 0.5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(17)**(TO_FM(1)/2) , TO_FM(17)**(TO_FM(1)/2) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 105
      A = 17.0
      B = TO_FM(1)/2
      RESULT = A ** (TO_FM(1)/2)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 17 ) ** ( 0.5 , 0.5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(17)**(TO_FM(1)/2) , TO_FM(17)**(TO_FM(1)/2) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 106
      A = 17.0D0
      B = TO_FM(1)/2
      RESULT = TO_FM(17) ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 17 ) ** ( 0.5 , 0.5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(17)**(TO_FM(1)/2) , TO_FM(17)**(TO_FM(1)/2) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 107
      A = 17.0D0
      B = TO_FM(1)/3
      CALL FM_INTERVAL_RATIONAL_POWER(A,1,3,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 17 , 17 ) ** (1/3) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(17)**(TO_FM(1)/3) , TO_FM(17)**(TO_FM(1)/3) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 108
      A = 2.0
      B = TO_IM(5)
      RESULT = A ** TO_IM(5)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) ** ( 5 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)**5 , TO_FM(2)**5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 109
      A = 2.0D0
      B = 6
      RESULT = TO_IM(2) ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) ** ( 6 , 6 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)**6 , TO_FM(2)**6 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 110
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 2 ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) ** ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4, 128 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 111
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 2.0 ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) ** ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4, 128 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 112
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 2.0D0 ** B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 2 ) ** ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4, 128 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 113
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B ** 2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) ** 2 = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4 , 49 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 116
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B ** 2.0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) ** 2.0 = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4 , 49 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 117
      A = 2.0D0
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B ** 2.0D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) ** 2.0D0 = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4 , 49 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' Power',2,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST2

      SUBROUTINE TEST3

!  +, -, *, / testing.

      IMPLICIT NONE
      INTEGER :: J

      NCASE = 118
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 3 + B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 3 ) + ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(5) , TO_FM(10) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = 3
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 119
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B + 3
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) + ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_IM(5), TO_IM(10) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 120
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0 + B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0 , -4.0 ) + ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 121
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B + 6.0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) + ( 6.0 , 6.0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 8, 13 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 6
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 122
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B + 3.0D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) + ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 5, 10 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 123
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0D0 + B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) + ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 124
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B + TO_IM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) + ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 5, 10 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 125
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = TO_IM(-4) + B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) + ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Addition',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 126
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT =  +B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) '  + ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 3 , 5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' Addition',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 127
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 3 - B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 3 ) - ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-4) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = 3
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 128
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B - 3
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) - ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_IM(-1), TO_IM(4) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 129
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0 - B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0 , -4.0 ) - ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -9 , -7 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 130
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B - 6.0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) - ( 6.0 , 6.0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          A = 6
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 131
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B - 3.0D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) - ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1 , 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 132
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0D0 - B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) - ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -9 , -7 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 133
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B - TO_IM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) - ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1, 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 134
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = TO_IM(-4) - B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) - ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -9 , -7 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Subtraction',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 135
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT =  -B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) '  - ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -5 , -3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' Subtraction',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 136
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 3 * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 3 ) * ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(6) , TO_FM(21) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = 3
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 137
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B * 3
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) * ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(6) , TO_FM(21) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 138
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0 * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0 , -4.0 ) * ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -20 , -12 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 139
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B * 6.0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) * ( 6.0 , 6.0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 12 , 42 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 6
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 140
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B * 3.0D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) * ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 21 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 141
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0D0 * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) * ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -20 , -12 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 142
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B * TO_IM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) * ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 6 , 21 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 143
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = TO_IM(-4) * B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) * ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -20 , -12 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Multiplication',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 144
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = 3 / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 3 , 3 ) / ( 2 , 7 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(3)/7 , TO_FM(3)/2 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 145
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B / 3
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 146
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_DIVI(B,3,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 147
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B
      CALL FM_INTERVAL_DIVI_R1(RESULT,3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3 , 3 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 148
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0 / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0 , -4.0 ) / ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-4)/3 , TO_FM(-4)/5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 149
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B / 6.0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 6.0 , 6.0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/6 , TO_FM(7)/6 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 6
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 150
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B / 3.0D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 151
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_DP2M(3.0D0,A)
      RESULT = B / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 152
      A = TO_FM_INTERVAL( 2 , 7 )
      CALL FM_INTERVAL_EQ(A,B)
      CALL FM_INTERVAL_DP2M(3.0D0,A)
      RESULT = B / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 153
      A = TO_FM_INTERVAL( 2 , 7 )
      J = NDIG
      CALL FM_INTERVAL_EQU(A,B,J,J)
      CALL FM_INTERVAL_DP2M(3.0D0,A)
      RESULT = B / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 154
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = -4.0D0 / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) / ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-4)/3 , TO_FM(-4)/5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 155
      B = TO_FM_INTERVAL( 2 , 7 )
      RESULT = B / TO_IM(3)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( 2 , 7 ) / ( 3.0D0 , 3.0D0 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(2)/3 , TO_FM(7)/3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          B = 3
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 156
      B = TO_FM_INTERVAL( 3 , 5 )
      RESULT = TO_IM(-4) / B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' ( -4.0D0 , -4.0D0 ) / ( 3 , 5 ) = '
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-4)/3 , TO_FM(-4)/5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = -4
          CALL ERRPRTFM(' Division',2,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST3

      SUBROUTINE TEST4

!  Conversion testing.

      IMPLICIT NONE
      INTEGER :: J, K

      NCASE = 157
      B = TO_FM_INTERVAL( 2 , 7 )
      J1 = TO_INT( B )
      RESULT = J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_INT( TO_FM_INTERVAL( 2 , 7 ) ) = '
      WRITE (KLOG,*) ' ',J1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' TO_INT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 158
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      JV = TO_INT( AVEC )
      CVEC = (/ 4, 5, 5 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_INT( AVEC ) = '
      DO J = 1, 3
         RESULT = JV(J)
         CORRECT = CVEC(J)
         WRITE (KLOG,*) ' ',JV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             A = AVEC(J)
             CALL ERRPRTFM(' TO_INT',1,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 159
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+30)*K , (J+30)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( ((J+30)*K + (J+30)*K + K + 1)/2 )
         ENDDO
      ENDDO
      JV2 = TO_INT( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_INT( AMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = JV2(J,K)
            CORRECT = CMAT(J,K)
            WRITE (KLOG,*) ' ',JV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                A = AMAT(J,K)
                CALL ERRPRTFM(' TO_INT',1,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 160
      B = TO_FM_INTERVAL( 2 , 7 )
      R1 = TO_SP( B )
      RESULT = R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SP( TO_FM_INTERVAL( 2 , 7 ) ) = '
      WRITE (KLOG,*) ' ',R1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < RSMALL)) THEN
          A = B
          CALL ERRPRTFM(' TO_SP',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 161
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      RV = TO_SP( AVEC )
      CVEC = (/ 4.5, 5.0, 5.5 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SP( AVEC ) = '
      DO J = 1, 3
         RESULT = RV(J)
         CORRECT = CVEC(J)
         WRITE (KLOG,*) ' ',RV(J)
         IF (.NOT.(ABS(RESULT-CORRECT) < RSMALL)) THEN
             A = AVEC(J)
             CALL ERRPRTFM(' TO_SP',1,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 162
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( ((J+20)*K + (J+20)*K + K + 1)/2.0 )
         ENDDO
      ENDDO
      RV2 = TO_SP( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SP( AMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = RV2(J,K)
            CORRECT = CMAT(J,K)
            WRITE (KLOG,*) ' ',RV2(J,K)
            IF (.NOT.(ABS(RESULT-CORRECT) < RSMALL)) THEN
                A = AMAT(J,K)
                CALL ERRPRTFM(' TO_SP',1,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 163
      B = TO_FM_INTERVAL( 2 , 7 )
      D1 = TO_DP( B )
      RESULT = D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DP( TO_FM_INTERVAL( 2 , 7 ) ) = '
      WRITE (KLOG,*) ' ',D1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < DSMALL)) THEN
          A = B
          CALL ERRPRTFM(' TO_DP',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 164
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      DV = TO_DP( AVEC )
      CVEC = (/ 4.5D0, 5.0D0, 5.5D0 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DP( AVEC ) = '
      DO J = 1, 3
         RESULT = DV(J)
         CORRECT = CVEC(J)
         WRITE (KLOG,*) ' ',DV(J)
         IF (.NOT.(ABS(RESULT-CORRECT) < DSMALL)) THEN
             A = AVEC(J)
             CALL ERRPRTFM(' TO_DP',1,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 165
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( ((J+20)*K + (J+20)*K + K + 1)/2.0D0 )
         ENDDO
      ENDDO
      DV2 = TO_DP( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DP( AMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = DV2(J,K)
            CORRECT = CMAT(J,K)
            WRITE (KLOG,*) ' ',DV2(J,K)
            IF (.NOT.(ABS(RESULT-CORRECT) < DSMALL)) THEN
                A = AMAT(J,K)
                CALL ERRPRTFM(' TO_DP',1,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 166
      B = TO_FM_INTERVAL( 2 , 7 )
      C1 = TO_SPZ( B )
      RESULT = C1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SPZ( TO_FM_INTERVAL( 2 , 7 ) ) = '
      WRITE (KLOG,*) ' ',C1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' TO_SPZ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 167
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      CV = TO_SPZ( AVEC )
      CVEC = (/ 4.5, 5.0, 5.5 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SPZ( AVEC ) = '
      DO J = 1, 3
         RESULT = CV(J)
         CORRECT = CVEC(J)
         WRITE (KLOG,*) ' ',CV(J)
         IF (.NOT.(ABS(RESULT-CORRECT) < RSMALL)) THEN
             A = AVEC(J)
             CALL ERRPRTFM(' TO_SPZ',1,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 168
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( ((J+20)*K + (J+20)*K + K + 1)/2.0 )
         ENDDO
      ENDDO
      CV2 = TO_SPZ( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_SPZ( AMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CV2(J,K)
            CORRECT = CMAT(J,K)
            WRITE (KLOG,*) ' ',CV2(J,K)
            IF (.NOT.(ABS(RESULT-CORRECT) < RSMALL)) THEN
                A = AMAT(J,K)
                CALL ERRPRTFM(' TO_SPZ',1,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 169
      B = TO_FM_INTERVAL( 2 , 7 )
      CD1 = TO_DPZ( B )
      RESULT = CD1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DPZ( TO_FM_INTERVAL( 2 , 7 ) ) = '
      WRITE (KLOG,*) ' ',CD1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < DSMALL)) THEN
          A = B
          CALL ERRPRTFM(' TO_DPZ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 170
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      CDV = TO_DPZ( AVEC )
      CVEC = (/ 4.5D0, 5.0D0, 5.5D0 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DPZ( AVEC ) = '
      DO J = 1, 3
         RESULT = CDV(J)
         CORRECT = CVEC(J)
         WRITE (KLOG,*) ' ',CDV(J)
         IF (.NOT.(ABS(RESULT-CORRECT) < DSMALL)) THEN
             A = AVEC(J)
             CALL ERRPRTFM(' TO_DPZ',1,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 171
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( ((J+20)*K + (J+20)*K + K + 1)/2.0D0 )
         ENDDO
      ENDDO
      CDV2 = TO_DPZ( AMAT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_DPZ( AMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CDV2(J,K)
            CORRECT = CMAT(J,K)
            WRITE (KLOG,*) ' ',CDV2(J,K)
            IF (.NOT.(ABS(RESULT-CORRECT) < DSMALL)) THEN
                A = AMAT(J,K)
                CALL ERRPRTFM(' TO_DPZ',1,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 172
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(B) = ',IS_OVERFLOW(B)
      WRITE (KLOG,*) ' '
      IF (IS_OVERFLOW(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_OVERFLOW',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 173
      B = TO_FM_INTERVAL( ' -OVERFLOW ' , ' -3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(B) = ',IS_OVERFLOW(B)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_OVERFLOW(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_OVERFLOW',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 174
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(BVEC) = ',IS_OVERFLOW(BVEC)
      WRITE (KLOG,*) ' '
      IF (IS_OVERFLOW(BVEC)) THEN
          CALL ERRPRTFM(' IS_OVERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 175
      BVEC(2) = TO_FM_INTERVAL( ' -OVERFLOW ' , ' -3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(BVEC) = ',IS_OVERFLOW(BVEC)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_OVERFLOW(BVEC)) THEN
          CALL ERRPRTFM(' IS_OVERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 176
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(BMAT) = ',IS_OVERFLOW(BMAT)
      WRITE (KLOG,*) ' '
      IF (IS_OVERFLOW(BMAT)) THEN
          CALL ERRPRTFM(' IS_OVERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 177
      BMAT(2,3) = TO_FM_INTERVAL( ' -3.1 ' , ' OVERFLOW ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_OVERFLOW(BMAT) = ',IS_OVERFLOW(BMAT)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_OVERFLOW(BMAT)) THEN
          CALL ERRPRTFM(' IS_OVERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 178
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(B) = ',IS_UNDERFLOW(B)
      WRITE (KLOG,*) ' '
      IF (IS_UNDERFLOW(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_UNDERFLOW',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 179
      B = TO_FM_INTERVAL( ' -UNDERFLOW ' , ' 3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(B) = ',IS_UNDERFLOW(B)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNDERFLOW(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_UNDERFLOW',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 180
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(BVEC) = ',IS_UNDERFLOW(BVEC)
      WRITE (KLOG,*) ' '
      IF (IS_UNDERFLOW(BVEC)) THEN
          CALL ERRPRTFM(' IS_UNDERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 181
      BVEC(2) = TO_FM_INTERVAL( ' -UNDERFLOW ' , ' 3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(BVEC) = ',IS_UNDERFLOW(BVEC)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNDERFLOW(BVEC)) THEN
          CALL ERRPRTFM(' IS_UNDERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 182
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(BMAT) = ',IS_UNDERFLOW(BMAT)
      WRITE (KLOG,*) ' '
      IF (IS_UNDERFLOW(BMAT)) THEN
          CALL ERRPRTFM(' IS_UNDERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 183
      BMAT(2,3) = TO_FM_INTERVAL( ' -3.1 ' , ' UNDERFLOW ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNDERFLOW(BMAT) = ',IS_UNDERFLOW(BMAT)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNDERFLOW(BMAT)) THEN
          CALL ERRPRTFM(' IS_UNDERFLOW',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 184
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(B) = ',IS_UNKNOWN(B)
      WRITE (KLOG,*) ' '
      IF (IS_UNKNOWN(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_UNKNOWN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 185
      B = TO_FM_INTERVAL( ' UNKNOWN ' , ' 3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(B) = ',IS_UNKNOWN(B)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNKNOWN(B)) THEN
          A = B
          CALL ERRPRTFM(' IS_UNKNOWN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 186
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(BVEC) = ',IS_UNKNOWN(BVEC)
      WRITE (KLOG,*) ' '
      IF (IS_UNKNOWN(BVEC)) THEN
          CALL ERRPRTFM(' IS_UNKNOWN',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 187
      BVEC(2) = TO_FM_INTERVAL( ' UNKNOWN ' , ' 3.1 ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(BVEC) = ',IS_UNKNOWN(BVEC)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNKNOWN(BVEC)) THEN
          CALL ERRPRTFM(' IS_UNKNOWN',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 188
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(BMAT) = ',IS_UNKNOWN(BMAT)
      WRITE (KLOG,*) ' '
      IF (IS_UNKNOWN(BMAT)) THEN
          CALL ERRPRTFM(' IS_UNKNOWN',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 189
      BMAT(2,3) = TO_FM_INTERVAL( ' -3.1 ' , ' UNKNOWN ' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' IS_UNKNOWN(BMAT) = ',IS_UNKNOWN(BMAT)
      WRITE (KLOG,*) ' '
      IF (.NOT.IS_UNKNOWN(BMAT)) THEN
          CALL ERRPRTFM(' IS_UNKNOWN',0,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST4

      SUBROUTINE TEST5

!  = assignment testing.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing = assignment for intervals.')")

      NCASE = 190
      B = TO_FM_INTERVAL( 2 , 7 )
      J1 = B
      RESULT = J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' J1 = TO_FM_INTERVAL( 2 , 7 ) '
      WRITE (KLOG,*) ' ',J1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 191
      B = TO_FM_INTERVAL( 2 , 7 )
      R1 = B
      RESULT = R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' R1 = TO_FM_INTERVAL( 2 , 7 ) '
      WRITE (KLOG,*) ' ',R1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 192
      B = TO_FM_INTERVAL( 2 , 7 )
      D1 = B
      RESULT = D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' D1 = TO_FM_INTERVAL( 2 , 7 ) '
      WRITE (KLOG,*) ' ',D1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5D0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 193
      B = TO_FM_INTERVAL( 2 , 7 )
      C1 = B
      RESULT = C1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' C1 = TO_FM_INTERVAL( 2 , 7 ) '
      WRITE (KLOG,*) ' ',C1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 194
      B = TO_FM_INTERVAL( 2 , 7 )
      CD1 = B
      RESULT = CD1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' CD1 = TO_FM_INTERVAL( 2 , 7 ) '
      WRITE (KLOG,*) ' ',CD1
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 4.5D0 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 195
      B = TO_FM_INTERVAL( 2 , 7 )
      MFM1 = B
      RESULT = MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MFM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL FM_FORM('F40.35',MFM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 196
      B = TO_FM_INTERVAL( 2 , 7 )
      MFM1 = B
      RESULT = MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MFM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL FM_FORM('F40.35',MFM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CALL FM_INTERVAL_ST2M('4.5',CORRECT)
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 197
      B = TO_FM_INTERVAL( 2 , 7 )
      MIM1 = B
      RESULT = MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MIM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL IM_FORM('I10',MIM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 198
      B = TO_FM_INTERVAL( 2 , 7 )
      CALL IM_INTERVAL_FM2I(B,MIM1)
      RESULT = MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MIM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL IM_FORM('I10',MIM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 199
      B = TO_FM_INTERVAL( 2 , 7 )
      MZM1 = B
      RESULT = MZM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MZM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL ZM_FORM('F30.25','F30.25',MZM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 200
      B = TO_FM_INTERVAL( 2 , 7 )
      C = 0
      CALL ZM_INTERVAL_COMPLEX(B,C,MZM1)
      RESULT = MZM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' MZM1 = TO_FM_INTERVAL( 2 , 7 ) '
      CALL ZM_FORM('F30.25','F30.25',MZM1,STRING)
      WRITE (KLOG,*) ' ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          A = B
          CALL ERRPRTFM(' = ',1,A,'A',B,'B',B,'B')
      ENDIF

!             Array equal assignments.

!             (1) rank 1  =  rank 0

      NCASE = 201
      AVEC = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = 29 '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29' )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 202
      AVEC = 29.5
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = 29.5 '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29.5' )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 203
      AVEC = 29.5D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = 29.5D0 '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29.5' )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 204
      AVEC = ( 29.5 , 30.5 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = ( 29.5 , 30.5 ) '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29.5' )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 205
      AVEC = ( 29.5D0 , 30.5D0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = ( 29.5D0 , 30.5D0 ) '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29.5' )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 206
      JV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " JV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = JV(J)
         CORRECT = TO_FM_INTERVAL( '30' )
         WRITE (KLOG,*) ' ',JV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 207
      RV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = RV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         WRITE (KLOG,*) ' ',RV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 208
      DV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " DV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = DV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         WRITE (KLOG,*) ' ',DV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 209
      CV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = CV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         WRITE (KLOG,*) ' ',CV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 210
      CDV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CDV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = CDV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         WRITE (KLOG,*) ' ',CDV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 211
      MFMV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = MFMV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         CALL FM_FORM('F40.35',MFMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 212
      MFM1 = TO_FM_INTERVAL( '29' , '32' )
      AVEC = MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MFM1 "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 213
      MIM1 = TO_FM_INTERVAL( '29' , '32' )
      AVEC = MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MIM1 "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '30' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 214
      MZM1 = TO_FM_INTERVAL( '29' , '32' )
      AVEC = MZM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MZM1 "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 215
      B = TO_FM_INTERVAL( '29' , '32' )
      AVEC = B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = B "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '29' , '32' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 216
      MIMV = TO_FM_INTERVAL( '29' , '32' )
      AVEC = MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIMV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '30' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 217
      MZMV = TO_FM_INTERVAL( '29' , '32' )
      AVEC = MZMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MZMV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 218
      MIMV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIMV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = MIMV(J)
         CORRECT = TO_FM_INTERVAL( '30' )
         CALL IM_FORM('I10',MIMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 219
      MZMV = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MZMV = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         RESULT = MZMV(J)
         CORRECT = TO_FM_INTERVAL( '30.5' )
         CALL ZM_FORM('F30.25','F30.25',MZMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST5

      SUBROUTINE TEST6

!  = assignment testing.

      IMPLICIT NONE
      INTEGER :: J

!             Array equal assignments.

!             (2) rank 1  =  rank 1

      NCASE = 220
      JV = (/ 31, 32, 33 /)
      AVEC = JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = JV '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( JV(J) )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 221
      RV = (/ 31.0, 32.5, 33.0 /)
      AVEC = RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = RV '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( RV(J) )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 222
      DV = (/ 31.0D0, 32.5D0, 33.0D0 /)
      AVEC = DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = DV '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( DV(J) )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 223
      CV = (/ CMPLX( 25.0 , 35.0 ), CMPLX( 26.0 , 36.0 ), CMPLX( 27.0 , 37.0 ) /)
      AVEC = CV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = CV '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( CV(J) )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 224
      CDV = (/ CMPLX( 25.0 , 35.0 , KIND(1.0D0) ), CMPLX( 26.0 , 36.0 , KIND(1.0D0) ),  &
               CMPLX( 27.0 , 37.0 , KIND(1.0D0) ) /)
      AVEC = CDV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AVEC = CDV '
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = TO_FM_INTERVAL( CDV(J) )
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 225
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      JV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " JV = AVEC "
      DO J = 1, 3
         RESULT = JV(J)
         CORRECT = TO_INT(AVEC(J))
         WRITE (KLOG,*) ' ',JV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 226
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      RV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RV = AVEC "
      DO J = 1, 3
         RESULT = RV(J)
         CORRECT = TO_SP(AVEC(J))
         WRITE (KLOG,*) ' ',RV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 227
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      DV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " DV = AVEC "
      DO J = 1, 3
         RESULT = DV(J)
         CORRECT = TO_DP(AVEC(J))
         WRITE (KLOG,*) ' ',DV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 228
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      CV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CV = AVEC "
      DO J = 1, 3
         RESULT = CV(J)
         CORRECT = TO_SPZ(AVEC(J))
         WRITE (KLOG,*) ' ',CV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 229
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      CDV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CDV = AVEC "
      DO J = 1, 3
         RESULT = CDV(J)
         CORRECT = TO_DPZ(AVEC(J))
         WRITE (KLOG,*) ' ',CDV(J)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 230
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      MFMV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMV = AVEC "
      DO J = 1, 3
         RESULT = MFMV(J)
         CORRECT = TO_FM(AVEC(J))
         CALL FM_FORM('F40.35',MFMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 231
      MFMV = TO_FM( (/ '33.3' , '33.5' , '33.7' /) )
      AVEC = MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MFMV "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 232
      BVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      AVEC = BVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = BVEC "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = BVEC(J)
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 233
      MIMV = (/ 43 , 44, 45 /)
      AVEC = MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MIMV "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 234
      MZMV = TO_ZM( (/ TO_FM_INTERVAL( 43 , 45 ) , TO_FM_INTERVAL( 44 , 46 ) ,  &
                       TO_FM_INTERVAL( 47 , 48 ) /) )
      AVEC = MZMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MZMV "
      DO J = 1, 3
         RESULT = AVEC(J)
         CORRECT = MZMV(J)
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 235
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      MIMV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIMV = AVEC "
      DO J = 1, 3
         RESULT = MIMV(J)
         CORRECT = TO_IM(AVEC(J))
         CALL IM_FORM('I10',MIMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 236
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 7 ) , TO_FM_INTERVAL( 4 , 7 ) /)
      MZMV = AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MZMV = AVEC "
      DO J = 1, 3
         RESULT = MZMV(J)
         CORRECT = TO_ZM(AVEC(J))
         CALL ZM_FORM('F30.25','F30.25',MZMV(J),STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST6

      SUBROUTINE TEST7

!  = assignment testing.

      IMPLICIT NONE
      INTEGER :: J, K

!             Array equal assignments.

!             (3) rank 2  =  rank 0

      NCASE = 237
      AMAT = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = 29 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29' )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 238
      AMAT = 29.5
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = 29.5 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29.5' )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 239
      AMAT = 29.5D0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = 29.5D0 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29.5' )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 240
      AMAT = ( 29.5 , 30.5 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = ( 29.5 , 30.5 ) '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29.5' )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 241
      AMAT = ( 29.5D0 , 30.5D0 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = ( 29.5D0 , 30.5D0 ) '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29.5' )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 242
      JV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " JV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = JV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30' )
            WRITE (KLOG,*) ' ',JV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 243
      RV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = RV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            WRITE (KLOG,*) ' ',RV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 244
      DV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " DV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = DV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            WRITE (KLOG,*) ' ',DV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 245
      CV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            WRITE (KLOG,*) ' ',CV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 246
      CDV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CDV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CDV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            WRITE (KLOG,*) ' ',CDV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 247
      MFMV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MFMV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            CALL FM_FORM('F40.35',MFMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 248
      MFM1 = TO_FM_INTERVAL( '29' , '32' )
      AMAT = MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MFM1 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 249
      MIM1 = TO_FM_INTERVAL( '29' , '32' )
      AMAT = MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MIM1 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '30' )
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 250
      MZM1 = TO_FM_INTERVAL( '29' , '32' )
      AMAT = MZM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MZM1 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 251
      B = TO_FM_INTERVAL( '29' , '32' )
      AMAT = B
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = B "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( '29' , '32' )
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 252
      MIMV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIMV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MIMV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30' )
            CALL IM_FORM('I10',MIMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 253
      MZMV2 = TO_FM_INTERVAL( '29' , '32' )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MZMV2 = TO_FM_INTERVAL( '29' , '32' ) "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MZMV2(J,K)
            CORRECT = TO_FM_INTERVAL( '30.5' )
            CALL ZM_FORM('F30.25','F30.25',MZMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST7

      SUBROUTINE TEST8

!  = assignment testing.

      IMPLICIT NONE
      INTEGER :: J, K

!             Array equal assignments.

!             (4) rank 2  =  rank 2

      NCASE = 254
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (2*J+105)*K/2
         ENDDO
      ENDDO
      AMAT = JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( JV2(J,K) )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 255
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( RV2(J,K) )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 256
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( DV2(J,K) )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 257
      DO J = 1, 3
         DO K = 1, 3
            CV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = CV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = CV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( CV2(J,K) )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 258
      DO J = 1, 3
         DO K = 1, 3
            CDV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = CDV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' AMAT = CDV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = TO_FM_INTERVAL( CDV2(J,K) )
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 259
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+25)*K )
         ENDDO
      ENDDO
      JV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " JV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = JV2(J,K)
            CORRECT = TO_INT(AMAT(J,K))
            WRITE (KLOG,*) ' ',JV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 260
      RV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = RV2(J,K)
            CORRECT = TO_SP(AMAT(J,K))
            WRITE (KLOG,*) ' ',RV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 261
      DV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " DV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = DV2(J,K)
            CORRECT = TO_DP(AMAT(J,K))
            WRITE (KLOG,*) ' ',DV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 262
      CV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CV2(J,K)
            CORRECT = TO_SPZ(AMAT(J,K))
            WRITE (KLOG,*) ' ',CV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 263
      CDV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CDV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CDV2(J,K)
            CORRECT = TO_DPZ(AMAT(J,K))
            WRITE (KLOG,*) ' ',CDV2(J,K)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 264
      MFMV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MFMV2(J,K)
            CORRECT = TO_FM(AMAT(J,K))
            CALL FM_FORM('F40.35',MFMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 265
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MFMV2 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 266
      DO J = 1, 3
         DO K = 1, 3
            BMAT(J,K) = TO_FM_INTERVAL( (J+27)*K , (J+32)*K )
         ENDDO
      ENDDO
      AMAT = BMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = BMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = BMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 267
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MIMV2 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 268
      DO J = 1, 3
         DO K = 1, 3
            MZMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      AMAT = MZMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = MZMV2 "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = AMAT(J,K)
            CORRECT = MZMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 269
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+25)*K )
         ENDDO
      ENDDO
      MIMV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIMV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MIMV2(J,K)
            CORRECT = TO_IM(AMAT(J,K))
            CALL IM_FORM('I10',MIMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 270
      MZMV2 = AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MZMV2 = AMAT "
      DO J = 1, 3
         DO K = 1, 3
            RESULT = MZMV2(J,K)
            CORRECT = TO_ZM(AMAT(J,K))
            CALL ZM_FORM('F30.25','F30.25',MZMV2(J,K),STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST8

      SUBROUTINE TEST9

!  == comparison testing.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing == comparison for intervals.')")

      WRITE (KLOG,*) ' '
      WRITE (KLOG,*) ' Comparison involving two intervals are considered equal'
      WRITE (KLOG,*) ' only when both endpoints are equal.'
      WRITE (KLOG,*) ' When an interval is compared to a non-interval, as with'
      WRITE (KLOG,*) '    TO_FM_INTERVAL( 29 , 30 ) == 29'
      WRITE (KLOG,*) ' the non-interval is converted to an interval with both'
      WRITE (KLOG,*) ' endpoints the same, and then they are compared.'
      WRITE (KLOG,*) ' '

      NCASE = 271
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' == ', TRIM(STRING), '  is  ',J1 == B
      IF (.NOT.( J1 == B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 272
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' == ', TRIM(STRING), '  is  ',J1 == B
      IF ( J1 == B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 273
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' == ', TRIM(STRING), '  is  ',R1 == B
      IF (.NOT.( R1 == B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 274
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' == ', TRIM(STRING), '  is  ',R1 == B
      IF ( R1 == B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 275
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' == ', TRIM(STRING), '  is  ',D1 == B
      IF (.NOT.( D1 == B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 276
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' == ', TRIM(STRING), '  is  ',D1 == B
      IF ( D1 == B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 277
      C1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', C1, ' == ', TRIM(STRING), '  is  ',C1 == B
      IF (.NOT.( C1 == B )) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 278
      C1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', C1, ' == ', TRIM(STRING), '  is  ',C1 == B
      IF ( C1 == B ) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 279
      CD1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', CD1, ' == ', TRIM(STRING), '  is  ',CD1 == B
      IF (.NOT.( CD1 == B )) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 280
      CD1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', CD1, ' == ', TRIM(STRING), '  is  ',CD1 == B
      IF ( CD1 == B ) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 281
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', J1, '  is  ',B == J1
      IF (.NOT.( B == J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 282
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', J1, '  is  ',B == J1
      IF ( B == J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 283
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', R1, '  is  ',B == R1
      IF (.NOT.( B == R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 284
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', R1, '  is  ',B == R1
      IF ( B == R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 285
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', D1, '  is  ',B == D1
      IF (.NOT.( B == D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 286
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', D1, '  is  ',B == D1
      IF ( B == D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 287
      C1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', C1, '  is  ',B == C1
      IF (.NOT.( B == C1 )) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 288
      C1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', C1, '  is  ',B == C1
      IF ( B == C1 ) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 289
      CD1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', CD1, '  is  ',B == CD1
      IF (.NOT.( B == CD1 )) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 290
      CD1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', CD1, '  is  ',B == CD1
      IF ( B == CD1 ) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 291
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST2), ' == ', TRIM(ST1), '  is  ',B == A
      IF (.NOT.( B == A )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 292
      A = TO_FM_INTERVAL( 29.0 , 30.1 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST2), ' == ', TRIM(ST1), '  is  ',B == A
      IF ( B == A ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 293
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MFM1 == B
      IF (.NOT.( MFM1 == B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 294
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MFM1 == B
      IF ( MFM1 == B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 295
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MFM1
      IF (.NOT.( B == MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 296
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MFM1
      IF ( B == MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 297
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MIM1 == B
      IF (.NOT.( MIM1 == B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 298
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MIM1 == B
      IF ( MIM1 == B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 299
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MIM1
      IF (.NOT.( B == MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 300
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MIM1
      IF ( B == MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 301
      MZM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MZM1 == B
      IF (.NOT.( MZM1 == B )) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 302
      MZM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' == ', TRIM(STRING), '  is  ',MZM1 == B
      IF ( MZM1 == B ) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 303
      MZM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MZM1
      IF (.NOT.( B == MZM1 )) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 304
      MZM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' == ', TRIM(ST1), '  is  ',B == MZM1
      IF ( B == MZM1 ) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' == ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST9

      SUBROUTINE TEST10

!  /= comparison testing.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing /= comparison for intervals.')")

      NCASE = 305
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' /= ', TRIM(STRING), '  is  ',J1 /= B
      IF ( J1 /= B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 306
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' /= ', TRIM(STRING), '  is  ',J1 /= B
      IF (.NOT.(J1 /= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 307
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' /= ', TRIM(STRING), '  is  ',R1 /= B
      IF ( R1 /= B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 308
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' /= ', TRIM(STRING), '  is  ',R1 /= B
      IF (.NOT.(R1 /= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 309
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' /= ', TRIM(STRING), '  is  ',D1 /= B
      IF ( D1 /= B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 310
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' /= ', TRIM(STRING), '  is  ',D1 /= B
      IF (.NOT.(D1 /= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 311
      C1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', C1, ' /= ', TRIM(STRING), '  is  ',C1 /= B
      IF ( C1 /= B ) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 312
      C1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', C1, ' /= ', TRIM(STRING), '  is  ',C1 /= B
      IF (.NOT.(C1 /= B )) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 313
      CD1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', CD1, ' /= ', TRIM(STRING), '  is  ',CD1 /= B
      IF ( CD1 /= B ) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 314
      CD1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', CD1, ' /= ', TRIM(STRING), '  is  ',CD1 /= B
      IF (.NOT.(CD1 /= B )) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 315
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', J1, '  is  ',B /= J1
      IF ( B /= J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 316
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', J1, '  is  ',B /= J1
      IF (.NOT.(B /= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 317
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', R1, '  is  ',B /= R1
      IF ( B /= R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 318
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', R1, '  is  ',B /= R1
      IF (.NOT.(B /= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 319
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', D1, '  is  ',B /= D1
      IF ( B /= D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 320
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', D1, '  is  ',B /= D1
      IF (.NOT.(B /= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 321
      C1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', C1, '  is  ',B /= C1
      IF ( B /= C1 ) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 322
      C1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', C1, '  is  ',B /= C1
      IF (.NOT.(B /= C1 )) THEN
          RESULT = C1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 323
      CD1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', CD1, '  is  ',B /= CD1
      IF ( B /= CD1 ) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 324
      CD1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', CD1, '  is  ',B /= CD1
      IF (.NOT.(B /= CD1 )) THEN
          RESULT = CD1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 325
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST2), ' /= ', TRIM(ST1), '  is  ',B /= A
      IF ( B /= A ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 326
      A = TO_FM_INTERVAL( 29.0 , 30.1 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST2), ' /= ', TRIM(ST1), '  is  ',B /= A
      IF (.NOT.(B /= A )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 327
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MFM1 /= B
      IF ( MFM1 /= B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 328
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MFM1 /= B
      IF (.NOT.(MFM1 /= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 329
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MFM1
      IF ( B /= MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 330
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MFM1
      IF (.NOT.(B /= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 331
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MIM1 /= B
      IF ( MIM1 /= B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 332
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MIM1 /= B
      IF (.NOT.(MIM1 /= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 333
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MIM1
      IF ( B /= MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 334
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('F20.15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MIM1
      IF (.NOT.(B /= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 335
      MZM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MZM1 /= B
      IF ( MZM1 /= B ) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 336
      MZM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' /= ', TRIM(STRING), '  is  ',MZM1 /= B
      IF (.NOT.(MZM1 /= B )) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 337
      MZM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MZM1
      IF ( B /= MZM1 ) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 338
      MZM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL ZM_FORM('F20.15','F20.15',MZM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' /= ', TRIM(ST1), '  is  ',B /= MZM1
      IF (.NOT.(B /= MZM1 )) THEN
          RESULT = MZM1
          CORRECT = B
          CALL ERRPRTFM(' /= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST10

      SUBROUTINE TEST11

!  <= comparison testing.

!  For all comparisons except == and /=, the order is not well defined if intervals overlap.
!  In those cases, the midpoints of the intervals are compared.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing <= comparison for intervals.')")

      WRITE (KLOG,*) ' '
      WRITE (KLOG,*) ' Comparison involving the relative order of two intervals'
      WRITE (KLOG,*) ' is not well defined if the intervals overlap.'
      WRITE (KLOG,*) ' In those cases, the midpoints of the intervals are compared.'
      WRITE (KLOG,*) ' When an interval is compared to a non-interval, as with'
      WRITE (KLOG,*) '    TO_FM_INTERVAL( 29 , 30 ) <= 29'
      WRITE (KLOG,*) ' the non-interval is converted to an interval with both'
      WRITE (KLOG,*) ' endpoints the same, and then they are compared.'
      WRITE (KLOG,*) ' '

      NCASE = 339
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' <= ', TRIM(STRING), '  is  ',J1 <= B
      IF (.NOT.( J1 <= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 340
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' <= ', TRIM(STRING), '  is  ',J1 <= B
      IF (.NOT.( J1 <= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 341
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' <= ', TRIM(STRING), '  is  ',J1 <= B
      IF (.NOT.( J1 <= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 342
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' <= ', TRIM(STRING), '  is  ',J1 <= B
      IF ( J1 <= B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 343
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' <= ', TRIM(STRING), '  is  ',J1 <= B
      IF (.NOT.( J1 <= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 344
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', J1, '  is  ',B <= J1
      IF (.NOT.( B <= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 345
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', J1, '  is  ',B <= J1
      IF ( B <= J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 346
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', J1, '  is  ',B <= J1
      IF (.NOT.( B <= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 347
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', J1, '  is  ',B <= J1
      IF (.NOT.( B <= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 348
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', J1, '  is  ',B <= J1
      IF ( B <= J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 349
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' <= ', TRIM(STRING), '  is  ',R1 <= B
      IF (.NOT.( R1 <= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 350
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' <= ', TRIM(STRING), '  is  ',R1 <= B
      IF (.NOT.( R1 <= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 351
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' <= ', TRIM(STRING), '  is  ',R1 <= B
      IF (.NOT.( R1 <= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 352
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' <= ', TRIM(STRING), '  is  ',R1 <= B
      IF ( R1 <= B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 353
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' <= ', TRIM(STRING), '  is  ',R1 <= B
      IF (.NOT.( R1 <= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 354
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', R1, '  is  ',B <= R1
      IF (.NOT.( B <= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 355
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', R1, '  is  ',B <= R1
      IF ( B <= R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 356
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', R1, '  is  ',B <= R1
      IF (.NOT.( B <= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 357
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', R1, '  is  ',B <= R1
      IF (.NOT.( B <= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 358
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', R1, '  is  ',B <= R1
      IF ( B <= R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 359
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' <= ', TRIM(STRING), '  is  ',D1 <= B
      IF (.NOT.( D1 <= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 360
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' <= ', TRIM(STRING), '  is  ',D1 <= B
      IF (.NOT.( D1 <= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 361
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' <= ', TRIM(STRING), '  is  ',D1 <= B
      IF (.NOT.( D1 <= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 362
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' <= ', TRIM(STRING), '  is  ',D1 <= B
      IF ( D1 <= B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 363
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' <= ', TRIM(STRING), '  is  ',D1 <= B
      IF (.NOT.( D1 <= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 364
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', D1, '  is  ',B <= D1
      IF (.NOT.( B <= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 365
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', D1, '  is  ',B <= D1
      IF ( B <= D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 366
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', D1, '  is  ',B <= D1
      IF (.NOT.( B <= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 367
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', D1, '  is  ',B <= D1
      IF (.NOT.( B <= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 368
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', D1, '  is  ',B <= D1
      IF ( B <= D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 369
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MFM1 <= B
      IF (.NOT.( MFM1 <= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 370
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MFM1 <= B
      IF (.NOT.( MFM1 <= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 371
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MFM1 <= B
      IF (.NOT.( MFM1 <= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 372
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MFM1 <= B
      IF ( MFM1 <= B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 373
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MFM1 <= B
      IF (.NOT.( MFM1 <= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 374
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MFM1
      IF (.NOT.( B <= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 375
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MFM1
      IF ( B <= MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 376
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MFM1
      IF (.NOT.( B <= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 377
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MFM1
      IF (.NOT.( B <= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 378
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MFM1
      IF ( B <= MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 379
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MIM1 <= B
      IF (.NOT.( MIM1 <= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 380
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MIM1 <= B
      IF (.NOT.( MIM1 <= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 381
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MIM1 <= B
      IF (.NOT.( MIM1 <= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 382
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MIM1 <= B
      IF ( MIM1 <= B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 383
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(STRING), '  is  ',MIM1 <= B
      IF (.NOT.( MIM1 <= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 384
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MIM1
      IF (.NOT.( B <= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 385
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MIM1
      IF ( B <= MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 386
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MIM1
      IF (.NOT.( B <= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 387
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MIM1
      IF (.NOT.( B <= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 388
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' <= ', TRIM(ST1), '  is  ',B <= MIM1
      IF ( B <= MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 389
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(ST2), '  is  ',A <= B
      IF (.NOT.( A <= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 390
      A = TO_FM_INTERVAL( 29 , 31 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(ST2), '  is  ',A <= B
      IF ( A <= B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 391
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(ST2), '  is  ',A <= B
      IF (.NOT.( A <= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 392
      A = TO_FM_INTERVAL( 40 , 41 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(ST2), '  is  ',A <= B
      IF ( A <= B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 393
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 40 , 41 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' <= ', TRIM(ST2), '  is  ',A <= B
      IF (.NOT.( A <= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' <= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST11

      SUBROUTINE TEST12

!  < comparison testing.

!  For all comparisons except == and /=, the order is not well defined if intervals overlap.
!  In those cases, the midpoints of the intervals are compared.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing < comparison for intervals.')")

      NCASE = 394
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' < ', TRIM(STRING), '  is  ',J1 < B
      IF ( J1 < B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 395
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' < ', TRIM(STRING), '  is  ',J1 < B
      IF (.NOT.( J1 < B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 396
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' < ', TRIM(STRING), '  is  ',J1 < B
      IF ( J1 < B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 397
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' < ', TRIM(STRING), '  is  ',J1 < B
      IF ( J1 < B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 398
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' < ', TRIM(STRING), '  is  ',J1 < B
      IF (.NOT.( J1 < B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 399
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', J1, '  is  ',B < J1
      IF ( B < J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 400
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', J1, '  is  ',B < J1
      IF ( B < J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 401
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', J1, '  is  ',B < J1
      IF ( B < J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 402
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', J1, '  is  ',B < J1
      IF (.NOT.( B < J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 403
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', J1, '  is  ',B < J1
      IF ( B < J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 404
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' < ', TRIM(STRING), '  is  ',R1 < B
      IF ( R1 < B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 405
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' < ', TRIM(STRING), '  is  ',R1 < B
      IF (.NOT.( R1 < B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 406
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' < ', TRIM(STRING), '  is  ',R1 < B
      IF ( R1 < B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 407
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' < ', TRIM(STRING), '  is  ',R1 < B
      IF ( R1 < B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 408
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' < ', TRIM(STRING), '  is  ',R1 < B
      IF (.NOT.( R1 < B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 409
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', R1, '  is  ',B < R1
      IF ( B < R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 410
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', R1, '  is  ',B < R1
      IF ( B < R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 411
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', R1, '  is  ',B < R1
      IF ( B < R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 412
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', R1, '  is  ',B < R1
      IF (.NOT.( B < R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 413
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', R1, '  is  ',B < R1
      IF ( B < R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 414
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' < ', TRIM(STRING), '  is  ',D1 < B
      IF ( D1 < B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 415
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' < ', TRIM(STRING), '  is  ',D1 < B
      IF (.NOT.( D1 < B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 416
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' < ', TRIM(STRING), '  is  ',D1 < B
      IF ( D1 < B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 417
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' < ', TRIM(STRING), '  is  ',D1 < B
      IF ( D1 < B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 418
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' < ', TRIM(STRING), '  is  ',D1 < B
      IF (.NOT.( D1 < B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 419
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', D1, '  is  ',B < D1
      IF ( B < D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 420
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', D1, '  is  ',B < D1
      IF ( B < D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 421
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', D1, '  is  ',B < D1
      IF ( B < D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 422
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', D1, '  is  ',B < D1
      IF (.NOT.( B < D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 423
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', D1, '  is  ',B < D1
      IF ( B < D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 424
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MFM1 < B
      IF ( MFM1 < B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 425
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MFM1 < B
      IF (.NOT.( MFM1 < B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 426
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MFM1 < B
      IF ( MFM1 < B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 427
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MFM1 < B
      IF ( MFM1 < B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 428
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MFM1 < B
      IF (.NOT.( MFM1 < B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 429
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MFM1
      IF ( B < MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 430
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MFM1
      IF ( B < MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 431
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MFM1
      IF ( B < MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 432
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MFM1
      IF (.NOT.( B < MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 433
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MFM1
      IF ( B < MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 434
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MIM1 < B
      IF ( MIM1 < B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 435
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MIM1 < B
      IF (.NOT.( MIM1 < B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 436
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MIM1 < B
      IF ( MIM1 < B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 437
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MIM1 < B
      IF ( MIM1 < B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 438
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(STRING), '  is  ',MIM1 < B
      IF (.NOT.( MIM1 < B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 439
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MIM1
      IF ( B < MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 440
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MIM1
      IF ( B < MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 441
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MIM1
      IF ( B < MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 442
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MIM1
      IF (.NOT.( B < MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 443
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' < ', TRIM(ST1), '  is  ',B < MIM1
      IF ( B < MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 444
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(ST2), '  is  ',A < B
      IF ( A < B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 445
      A = TO_FM_INTERVAL( 29 , 31 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(ST2), '  is  ',A < B
      IF ( A < B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 446
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(ST2), '  is  ',A < B
      IF (.NOT.( A < B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 447
      A = TO_FM_INTERVAL( 40 , 41 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(ST2), '  is  ',A < B
      IF ( A < B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 448
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 40 , 41 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' < ', TRIM(ST2), '  is  ',A < B
      IF (.NOT.( A < B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' < ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST12

      SUBROUTINE TEST13

!  >= comparison testing.

!  For all comparisons except == and /=, the order is not well defined if intervals overlap.
!  In those cases, the midpoints of the intervals are compared.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing >= comparison for intervals.')")

      NCASE = 449
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' >= ', TRIM(STRING), '  is  ',J1 >= B
      IF (.NOT.( J1 >= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 450
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' >= ', TRIM(STRING), '  is  ',J1 >= B
      IF ( J1 >= B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 451
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' >= ', TRIM(STRING), '  is  ',J1 >= B
      IF (.NOT.( J1 >= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 452
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' >= ', TRIM(STRING), '  is  ',J1 >= B
      IF (.NOT.( J1 >= B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 453
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' >= ', TRIM(STRING), '  is  ',J1 >= B
      IF ( J1 >= B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 454
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', J1, '  is  ',B >= J1
      IF (.NOT.( B >= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 455
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', J1, '  is  ',B >= J1
      IF (.NOT.( B >= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 456
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', J1, '  is  ',B >= J1
      IF (.NOT.( B >= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 457
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', J1, '  is  ',B >= J1
      IF ( B >= J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 458
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', J1, '  is  ',B >= J1
      IF (.NOT.( B >= J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 459
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' >= ', TRIM(STRING), '  is  ',R1 >= B
      IF (.NOT.( R1 >= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 460
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' >= ', TRIM(STRING), '  is  ',R1 >= B
      IF ( R1 >= B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 461
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' >= ', TRIM(STRING), '  is  ',R1 >= B
      IF (.NOT.( R1 >= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 462
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' >= ', TRIM(STRING), '  is  ',R1 >= B
      IF (.NOT.( R1 >= B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 463
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' >= ', TRIM(STRING), '  is  ',R1 >= B
      IF ( R1 >= B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 464
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', R1, '  is  ',B >= R1
      IF (.NOT.( B >= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 465
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', R1, '  is  ',B >= R1
      IF (.NOT.( B >= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 466
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', R1, '  is  ',B >= R1
      IF (.NOT.( B >= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 467
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', R1, '  is  ',B >= R1
      IF ( B >= R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 468
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', R1, '  is  ',B >= R1
      IF (.NOT.( B >= R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 469
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' >= ', TRIM(STRING), '  is  ',D1 >= B
      IF (.NOT.( D1 >= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 470
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' >= ', TRIM(STRING), '  is  ',D1 >= B
      IF ( D1 >= B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 471
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' >= ', TRIM(STRING), '  is  ',D1 >= B
      IF (.NOT.( D1 >= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 472
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' >= ', TRIM(STRING), '  is  ',D1 >= B
      IF (.NOT.( D1 >= B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 473
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' >= ', TRIM(STRING), '  is  ',D1 >= B
      IF ( D1 >= B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 474
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', D1, '  is  ',B >= D1
      IF (.NOT.( B >= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 475
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', D1, '  is  ',B >= D1
      IF (.NOT.( B >= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 476
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', D1, '  is  ',B >= D1
      IF (.NOT.( B >= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 477
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', D1, '  is  ',B >= D1
      IF ( B >= D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 478
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', D1, '  is  ',B >= D1
      IF (.NOT.( B >= D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 479
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MFM1 >= B
      IF (.NOT.( MFM1 >= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 480
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MFM1 >= B
      IF ( MFM1 >= B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 481
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MFM1 >= B
      IF (.NOT.( MFM1 >= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 482
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MFM1 >= B
      IF (.NOT.( MFM1 >= B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 483
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MFM1 >= B
      IF ( MFM1 >= B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 484
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MFM1
      IF (.NOT.( B >= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 485
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MFM1
      IF (.NOT.( B >= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 486
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MFM1
      IF (.NOT.( B >= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 487
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MFM1
      IF ( B >= MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 488
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MFM1
      IF (.NOT.( B >= MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 489
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MIM1 >= B
      IF (.NOT.( MIM1 >= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 490
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MIM1 >= B
      IF ( MIM1 >= B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 491
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MIM1 >= B
      IF (.NOT.( MIM1 >= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 492
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MIM1 >= B
      IF (.NOT.( MIM1 >= B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 493
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(STRING), '  is  ',MIM1 >= B
      IF ( MIM1 >= B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 494
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MIM1
      IF (.NOT.( B >= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 495
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MIM1
      IF (.NOT.( B >= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 496
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MIM1
      IF (.NOT.( B >= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 497
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MIM1
      IF ( B >= MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 498
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' >= ', TRIM(ST1), '  is  ',B >= MIM1
      IF (.NOT.( B >= MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 499
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(ST2), '  is  ',A >= B
      IF (.NOT.( A >= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 500
      A = TO_FM_INTERVAL( 29 , 31 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(ST2), '  is  ',A >= B
      IF (.NOT.( A >= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 501
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(ST2), '  is  ',A >= B
      IF ( A >= B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 502
      A = TO_FM_INTERVAL( 40 , 41 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(ST2), '  is  ',A >= B
      IF (.NOT.( A >= B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 503
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 40 , 41 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' >= ', TRIM(ST2), '  is  ',A >= B
      IF ( A >= B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' >= ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST13

      SUBROUTINE TEST14

!  > comparison testing.

!  For all comparisons except == and /=, the order is not well defined if intervals overlap.
!  In those cases, the midpoints of the intervals are compared.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing > comparison for intervals.')")

      NCASE = 504
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' > ', TRIM(STRING), '  is  ',J1 > B
      IF ( J1 > B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 505
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' > ', TRIM(STRING), '  is  ',J1 > B
      IF ( J1 > B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 506
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' > ', TRIM(STRING), '  is  ',J1 > B
      IF ( J1 > B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 507
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' > ', TRIM(STRING), '  is  ',J1 > B
      IF (.NOT.( J1 > B )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 508
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', J1, ' > ', TRIM(STRING), '  is  ',J1 > B
      IF ( J1 > B ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 509
      J1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', J1, '  is  ',B > J1
      IF ( B > J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 510
      J1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', J1, '  is  ',B > J1
      IF (.NOT.( B > J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 511
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', J1, '  is  ',B > J1
      IF ( B > J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 512
      J1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', J1, '  is  ',B > J1
      IF ( B > J1 ) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 513
      J1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', J1, '  is  ',B > J1
      IF (.NOT.( B > J1 )) THEN
          RESULT = J1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 514
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' > ', TRIM(STRING), '  is  ',R1 > B
      IF ( R1 > B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 515
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' > ', TRIM(STRING), '  is  ',R1 > B
      IF ( R1 > B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 516
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' > ', TRIM(STRING), '  is  ',R1 > B
      IF ( R1 > B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 517
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' > ', TRIM(STRING), '  is  ',R1 > B
      IF (.NOT.( R1 > B )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 518
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', R1, ' > ', TRIM(STRING), '  is  ',R1 > B
      IF ( R1 > B ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 519
      R1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', R1, '  is  ',B > R1
      IF ( B > R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 520
      R1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', R1, '  is  ',B > R1
      IF (.NOT.( B > R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 521
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', R1, '  is  ',B > R1
      IF ( B > R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 522
      R1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', R1, '  is  ',B > R1
      IF ( B > R1 ) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 523
      R1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', R1, '  is  ',B > R1
      IF (.NOT.( B > R1 )) THEN
          RESULT = R1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 524
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' > ', TRIM(STRING), '  is  ',D1 > B
      IF ( D1 > B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 525
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' > ', TRIM(STRING), '  is  ',D1 > B
      IF ( D1 > B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 526
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' > ', TRIM(STRING), '  is  ',D1 > B
      IF ( D1 > B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 527
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' > ', TRIM(STRING), '  is  ',D1 > B
      IF (.NOT.( D1 > B )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 528
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', D1, ' > ', TRIM(STRING), '  is  ',D1 > B
      IF ( D1 > B ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 529
      D1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', D1, '  is  ',B > D1
      IF ( B > D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 530
      D1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', D1, '  is  ',B > D1
      IF (.NOT.( B > D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 531
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', D1, '  is  ',B > D1
      IF ( B > D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 532
      D1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', D1, '  is  ',B > D1
      IF ( B > D1 ) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 533
      D1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', D1, '  is  ',B > D1
      IF (.NOT.( B > D1 )) THEN
          RESULT = D1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 534
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MFM1 > B
      IF ( MFM1 > B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 535
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MFM1 > B
      IF ( MFM1 > B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 536
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MFM1 > B
      IF ( MFM1 > B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 537
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MFM1 > B
      IF (.NOT.( MFM1 > B )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 538
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MFM1 > B
      IF ( MFM1 > B ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 539
      MFM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MFM1
      IF ( B > MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 540
      MFM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MFM1
      IF (.NOT.( B > MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 541
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MFM1
      IF ( B > MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 542
      MFM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MFM1
      IF ( B > MFM1 ) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 543
      MFM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL FM_FORM('F20.15',MFM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MFM1
      IF (.NOT.( B > MFM1 )) THEN
          RESULT = MFM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 544
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MIM1 > B
      IF ( MIM1 > B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 545
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MIM1 > B
      IF ( MIM1 > B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 546
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MIM1 > B
      IF ( MIM1 > B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 547
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MIM1 > B
      IF (.NOT.( MIM1 > B )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 548
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(STRING), '  is  ',MIM1 > B
      IF ( MIM1 > B ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 549
      MIM1 = 29
      B = 29
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MIM1
      IF ( B > MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 550
      MIM1 = 29
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MIM1
      IF (.NOT.( B > MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 551
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MIM1
      IF ( B > MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 552
      MIM1 = 29
      B = TO_FM_INTERVAL( 27 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MIM1
      IF ( B > MIM1 ) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 553
      MIM1 = 29
      B = TO_FM_INTERVAL( 28 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F20.15',B,STRING)
      CALL IM_FORM('I15',MIM1,ST1)
      WRITE (KLOG,*) ' ', TRIM(STRING), ' > ', TRIM(ST1), '  is  ',B > MIM1
      IF (.NOT.( B > MIM1 )) THEN
          RESULT = MIM1
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 554
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(ST2), '  is  ',A > B
      IF ( A > B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 555
      A = TO_FM_INTERVAL( 29 , 31 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(ST2), '  is  ',A > B
      IF (.NOT.( A > B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 556
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 29 , 31 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(ST2), '  is  ',A > B
      IF ( A > B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 557
      A = TO_FM_INTERVAL( 40 , 41 )
      B = TO_FM_INTERVAL( 29 , 30 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(ST2), '  is  ',A > B
      IF (.NOT.( A > B )) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      NCASE = 558
      A = TO_FM_INTERVAL( 29 , 30 )
      B = TO_FM_INTERVAL( 40 , 41 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F15.10',A,ST1)
      CALL FMFORM_INTERVAL('F15.10',B,ST2)
      WRITE (KLOG,*) ' ', TRIM(ST1), ' > ', TRIM(ST2), '  is  ',A > B
      IF ( A > B ) THEN
          RESULT = A
          CORRECT = B
          CALL ERRPRTFM(' > ',0,A,'A',B,'B',B,'B')
      ENDIF
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST14

      SUBROUTINE TEST15

!  Array addition.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing interval array addition.')")

!             (1) rank 1  +  rank 0   or   rank 0  +  rank 1

      NCASE = 559
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = AVEC + J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + J1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + J1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 560
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = AVEC + R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + R1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + R1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 561
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = AVEC + D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + D1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + D1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 562
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = AVEC + MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + MFM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + MFM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 563
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = AVEC + MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + MIM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + MIM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 564
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = AVEC + C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + C '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + C
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 565
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      BVEC = +AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = +AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 566
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = J1 + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = J1 + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = J1 + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 567
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = R1 + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = R1 + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = R1 + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 568
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = D1 + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = D1 + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = D1 + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 569
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = MFM1 + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFM1 + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFM1 + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 570
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = MIM1 + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIM1 + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIM1 + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 571
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = A + JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A + JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A + JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 572
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = A + RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A + RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A + RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 573
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = A + DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A + DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A + DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 574
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = A + MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A + MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A + MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 575
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = A + MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A + MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A + MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 576
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = JV + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV + A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) + A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 577
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = RV + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV + A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) + A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 578
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = DV + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV + A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) + A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 579
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = MFMV + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV + A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) + A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 580
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = MIMV + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV + A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) + A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 581
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = C + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = C + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = C + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST15

      SUBROUTINE TEST16

!  Array addition.

      IMPLICIT NONE
      INTEGER :: J

!             (2) rank 1  +  rank 1

      NCASE = 582
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV = (/ 41 , 43 , 45 /)
      BVEC = AVEC + JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 583
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC + RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 584
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC + DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 585
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC + MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 586
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC + MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 587
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      CVEC = (/ TO_FM_INTERVAL(-2 , 5 ) , TO_FM_INTERVAL( 22 ,23 ) , TO_FM_INTERVAL( -1 , 8 ) /)
      BVEC = AVEC + CVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC + CVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) + CVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 588
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV  = (/ 41 , 43 , 45 /)
      BVEC = JV + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 589
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = RV + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 590
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = DV + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 591
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = MFMV + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 592
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = MIMV + AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV + AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) + AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST16

      SUBROUTINE TEST17

!  Array addition.

      IMPLICIT NONE
      INTEGER :: J, K

!             (3) rank 2  +  rank 0   or   rank 0  +  rank 2

      NCASE = 593
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = AMAT + J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + J1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + J1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 594
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = AMAT + R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + R1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + R1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 595
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = AMAT + D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + D1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + D1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 596
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = AMAT + MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + MFM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + MFM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 597
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = AMAT + MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + MIM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + MIM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 598
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = AMAT + C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + C '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + C
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 599
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = +AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = +AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 600
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = J1 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = J1 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = J1 + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 601
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = R1 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = R1 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = R1 + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 602
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = D1 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = D1 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = D1 + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 603
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = MFM1 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFM1 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFM1 + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 604
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = MIM1 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIM1 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIM1 + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 605
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A + JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A + JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A + JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 606
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A + RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A + RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A + RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 607
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A + DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A + DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A + DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 608
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A + MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A + MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A + MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 609
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A + MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A + MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A + MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 610
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = JV2 + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 + A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) + A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 611
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = RV2 + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 + A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) + A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 612
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = DV2 + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 + A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) + A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 613
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 + A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) + A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 614
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 + A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 + A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) + A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 615
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = C + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = C + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = C + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST17

      SUBROUTINE TEST18

!  Array addition.

      IMPLICIT NONE
      INTEGER :: J, K

!             (4) rank 2  +  rank 2

      NCASE = 616
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT + JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 617
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT + RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 618
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT + DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 619
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT + MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 620
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT + MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 621
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( (J+13)*K , (J+24)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = AMAT + CMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT + CMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) + CMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 622
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = JV2 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 623
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = RV2 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 624
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = DV2 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 625
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 626
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 + AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 + AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) + AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST18

      SUBROUTINE TEST19

!  Array subtraction.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing interval array subtraction.')")

!             (1) rank 1  -  rank 0   or   rank 0  -  rank 1

      NCASE = 627
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = AVEC - J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - J1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - J1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 628
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = AVEC - R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - R1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - R1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 629
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = AVEC - D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - D1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - D1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 630
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = AVEC - MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - MFM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - MFM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 631
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = AVEC - MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - MIM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - MIM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 632
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = AVEC - C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - C '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - C
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 633
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      BVEC = -AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = -AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = -AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 634
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = J1 - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = J1 - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = J1 - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 635
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = R1 - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = R1 - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = R1 - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 636
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = D1 - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = D1 - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = D1 - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 637
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = MFM1 - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFM1 - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFM1 - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 638
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = MIM1 - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIM1 - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIM1 - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 639
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = A - JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A - JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A - JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 640
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = A - RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A - RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A - RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 641
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = A - DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A - DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A - DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 642
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = A - MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A - MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A - MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 643
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = A - MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A - MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A - MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 644
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = JV - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV - A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) - A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 645
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = RV - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV - A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) - A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 646
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = DV - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV - A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) - A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 647
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = MFMV - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV - A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) - A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 648
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = MIMV - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV - A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) - A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 649
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = C - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = C - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = C - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST19

      SUBROUTINE TEST20

!  Array subtraction.

      IMPLICIT NONE
      INTEGER :: J

!             (2) rank 1  -  rank 1

      NCASE = 650
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV = (/ 41 , 43 , 45 /)
      BVEC = AVEC - JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 651
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC - RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 652
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC - DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 653
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC - MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 654
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC - MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 655
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      CVEC = (/ TO_FM_INTERVAL(-2 , 5 ) , TO_FM_INTERVAL( 22 ,23 ) , TO_FM_INTERVAL( -1 , 8 ) /)
      BVEC = AVEC - CVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC - CVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) - CVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 656
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV  = (/ 41 , 43 , 45 /)
      BVEC = JV - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 657
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = RV - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 658
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = DV - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 659
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = MFMV - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 660
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = MIMV - AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV - AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) - AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST20

      SUBROUTINE TEST21

!  Array subtraction.

      IMPLICIT NONE
      INTEGER :: J, K

!             (3) rank 2  -  rank 0   or   rank 0  -  rank 2

      NCASE = 661
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = AMAT - J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - J1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - J1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 662
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = AMAT - R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - R1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - R1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 663
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = AMAT - D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - D1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - D1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 664
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = AMAT - MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - MFM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - MFM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 665
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = AMAT - MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - MIM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - MIM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 666
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = AMAT - C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - C '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - C
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 667
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = -AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = -AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = -AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 668
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = J1 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = J1 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = J1 - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 669
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = R1 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = R1 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = R1 - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 670
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = D1 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = D1 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = D1 - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 671
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = MFM1 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFM1 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFM1 - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 672
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = MIM1 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIM1 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIM1 - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 673
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A - JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A - JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A - JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 674
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A - RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A - RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A - RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 675
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A - DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A - DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A - DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 676
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A - MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A - MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A - MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 677
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A - MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A - MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A - MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 678
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = JV2 - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 - A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) - A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 679
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = RV2 - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 - A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) - A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 680
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = DV2 - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 - A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) - A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 681
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 - A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) - A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 682
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 - A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 - A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) - A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 683
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = C - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = C - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = C - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST21

      SUBROUTINE TEST22

!  Array subtraction.

      IMPLICIT NONE
      INTEGER :: J, K

!             (4) rank 2  -  rank 2

      NCASE = 684
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT - JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 685
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT - RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 686
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT - DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 687
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT - MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 688
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT - MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 689
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( (J+13)*K , (J+24)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = AMAT - CMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT - CMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) - CMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 690
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = JV2 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 691
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = RV2 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 692
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = DV2 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 693
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 694
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 - AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 - AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) - AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST22

      SUBROUTINE TEST23

!  Array multiplication.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing interval array multiplication.')")

!             (1) rank 1  *  rank 0   or   rank 0  *  rank 1

      NCASE = 695
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = AVEC * J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * J1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * J1
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 696
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = AVEC * R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * R1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * R1
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 697
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = AVEC * D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * D1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * D1
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 698
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = AVEC * MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * MFM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * MFM1
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 699
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = AVEC * MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * MIM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * MIM1
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 700
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = AVEC * C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * C '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * C
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 701
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      J1 = 29
      BVEC = J1 * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = J1 * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = J1 * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 702
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      R1 = 29
      BVEC = R1 * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = R1 * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = R1 * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 703
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      D1 = 29
      BVEC = D1 * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = D1 * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = D1 * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 704
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFM1 = 29
      BVEC = MFM1 * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFM1 * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFM1 * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 705
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIM1 = 29
      BVEC = MIM1 * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIM1 * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIM1 * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 706
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = A * JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A * JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A * JV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 707
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = A * RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A * RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A * RV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 708
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = A * DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A * DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A * DV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 709
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = A * MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A * MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A * MFMV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 710
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = A * MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A * MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A * MIMV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 711
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = JV * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV * A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) * A
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 712
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = RV * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV * A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) * A
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 713
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = DV * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV * A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) * A
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 714
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = MFMV * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV * A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) * A
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 715
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = MIMV * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV * A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) * A
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 716
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      C = TO_FM_INTERVAL(-9 , 5 )
      BVEC = C * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = C * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = C * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST23

      SUBROUTINE TEST24

!  Array multiplication.

      IMPLICIT NONE
      INTEGER :: J

!             (2) rank 1  *  rank 1

      NCASE = 717
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV = (/ 41 , 43 , 45 /)
      BVEC = AVEC * JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * JV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 718
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC * RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * RV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 719
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC * DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * DV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 720
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC * MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * MFMV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 721
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC * MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * MIMV(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 722
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      CVEC = (/ TO_FM_INTERVAL(-2 , 5 ) , TO_FM_INTERVAL( 22 ,23 ) , TO_FM_INTERVAL( -1 , 8 ) /)
      BVEC = AVEC * CVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC * CVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) * CVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 723
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      JV  = (/ 41 , 43 , 45 /)
      BVEC = JV * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 724
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = RV * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 725
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = DV * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 726
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = MFMV * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 727
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = MIMV * AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV * AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) * AVEC(J)
         CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST24

      SUBROUTINE TEST25

!  Array multiplication.

      IMPLICIT NONE
      INTEGER :: J, K

!             (3) rank 2  *  rank 0   or   rank 0  *  rank 2

      NCASE = 728
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = AMAT * J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * J1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * J1
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 729
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = AMAT * R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * R1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * R1
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 730
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = AMAT * D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * D1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * D1
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 731
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = AMAT * MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * MFM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * MFM1
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 732
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = AMAT * MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * MIM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * MIM1
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 733
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = AMAT * C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * C '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * C
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 734
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = J1 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = J1 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = J1 * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 735
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = R1 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = R1 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = R1 * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 736
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = D1 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = D1 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = D1 * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 737
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = MFM1 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFM1 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFM1 * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 738
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = MIM1 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIM1 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIM1 * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 739
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A * JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A * JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A * JV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 740
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A * RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A * RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A * RV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 741
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A * DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A * DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A * DV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 742
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A * MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A * MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A * MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 743
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A * MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A * MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A * MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 744
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = JV2 * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 * A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) * A
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 745
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = RV2 * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 * A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) * A
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 746
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = DV2 * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 * A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) * A
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 747
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 * A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) * A
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 748
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 * A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 * A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) * A
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 749
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , 5 )
      BMAT = C * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = C * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = C * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST25

      SUBROUTINE TEST26

!  Array multiplication.

      IMPLICIT NONE
      INTEGER :: J, K

!             (4) rank 2  *  rank 2

      NCASE = 750
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT * JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * JV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 751
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT * RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * RV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 752
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT * DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * DV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 753
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT * MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 754
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT * MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 755
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( (J+13)*K , (J+24)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = AMAT * CMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT * CMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) * CMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 756
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = JV2 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 757
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = RV2 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 758
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = DV2 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 759
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 760
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 * AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 * AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) * AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.32',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST26

      SUBROUTINE TEST27

!  Array division.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing interval array division.')")

!             (1) rank 1  /  rank 0   or   rank 0  /  rank 1

      NCASE = 761
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      J1 = 29
      BVEC = AVEC / J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / J1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / J1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 762
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      R1 = 29
      BVEC = AVEC / R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / R1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / R1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 763
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      D1 = 29
      BVEC = AVEC / D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / D1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / D1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 764
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MFM1 = 29
      BVEC = AVEC / MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / MFM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / MFM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 765
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MIM1 = 29
      BVEC = AVEC / MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / MIM1 '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / MIM1
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 766
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      C = TO_FM_INTERVAL(-9 , -5 )
      BVEC = AVEC / C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / C '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / C
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 767
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      J1 = 29
      BVEC = J1 / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = J1 / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = J1 / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 768
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      R1 = 29
      BVEC = R1 / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = R1 / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = R1 / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 769
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      D1 = 29
      BVEC = D1 / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = D1 / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = D1 / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 770
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MFM1 = 29
      BVEC = MFM1 / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFM1 / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFM1 / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 771
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MIM1 = 29
      BVEC = MIM1 / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIM1 / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIM1 / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 772
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = A / JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A / JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A / JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 773
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = A / RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A / RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A / RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 774
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = A / DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A / DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A / DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 775
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = A / MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A / MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A / MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 776
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = A / MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = A / MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = A / MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 777
      A = TO_FM_INTERVAL( 2 , 7 )
      JV = (/ 29 , 31 , 33 /)
      BVEC = JV / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV / A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) / A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 778
      A = TO_FM_INTERVAL( 2 , 7 )
      RV = (/ 29 , 31 , 33 /)
      BVEC = RV / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV / A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) / A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 779
      A = TO_FM_INTERVAL( 2 , 7 )
      DV = (/ 29 , 31 , 33 /)
      BVEC = DV / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV / A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) / A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 780
      A = TO_FM_INTERVAL( 2 , 7 )
      MFMV = (/ 29 , 31 , 33 /)
      BVEC = MFMV / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV / A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) / A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 781
      A = TO_FM_INTERVAL( 2 , 7 )
      MIMV = (/ 29 , 31 , 33 /)
      BVEC = MIMV / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV / A '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) / A
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 782
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      C = TO_FM_INTERVAL(-9 , -5 )
      BVEC = C / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = C / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = C / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST27

      SUBROUTINE TEST28

!  Array division.

      IMPLICIT NONE
      INTEGER :: J

!             (2) rank 1  /  rank 1

      NCASE = 783
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      JV = (/ 41 , 43 , 45 /)
      BVEC = AVEC / JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / JV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / JV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 784
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC / RV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / RV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / RV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 785
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC / DV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / DV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / DV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 786
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC / MFMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / MFMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / MFMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 787
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = AVEC / MIMV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / MIMV '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / MIMV(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 788
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      CVEC = (/ TO_FM_INTERVAL(-5 ,-2 ) , TO_FM_INTERVAL( 22 ,23 ) , TO_FM_INTERVAL( 4 , 8 ) /)
      BVEC = AVEC / CVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = AVEC / CVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = AVEC(J) / CVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 789
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      JV  = (/ 41 , 43 , 45 /)
      BVEC = JV / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = JV / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = JV(J) / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 790
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      RV  = (/ 41 , 43 , 45 /)
      BVEC = RV / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = RV / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = RV(J) / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 791
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      DV  = (/ 41 , 43 , 45 /)
      BVEC = DV / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = DV / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = DV(J) / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 792
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MFMV  = (/ 41 , 43 , 45 /)
      BVEC = MFMV / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MFMV / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MFMV(J) / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 793
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( 3 , 5 ) , TO_FM_INTERVAL( -8 , -1 ) /)
      MIMV  = (/ 41 , 43 , 45 /)
      BVEC = MIMV / AVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BVEC = MIMV / AVEC '
      DO J = 1, 3
         RESULT = BVEC(J)
         CORRECT = MIMV(J) / AVEC(J)
         CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
         WRITE (KLOG,*) ' ',TRIM(STRING)
         IF (.NOT.(RESULT == CORRECT)) THEN
             CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST28

      SUBROUTINE TEST29

!  Array division.

      IMPLICIT NONE
      INTEGER :: J, K

!             (3) rank 2  /  rank 0   or   rank 0  /  rank 2

      NCASE = 794
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = AMAT / J1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / J1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / J1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 795
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = AMAT / R1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / R1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / R1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 796
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = AMAT / D1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / D1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / D1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 797
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = AMAT / MFM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / MFM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / MFM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 798
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = AMAT / MIM1
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / MIM1 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / MIM1
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 799
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , -5 )
      BMAT = AMAT / C
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / C '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / C
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 800
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      J1 = 29
      BMAT = J1 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = J1 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = J1 / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 801
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      R1 = 29
      BMAT = R1 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = R1 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = R1 / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 802
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      D1 = 29
      BMAT = D1 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = D1 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = D1 / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 803
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MFM1 = 29
      BMAT = MFM1 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFM1 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFM1 / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 804
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      MIM1 = 29
      BMAT = MIM1 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIM1 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIM1 / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 805
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A / JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A / JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A / JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 806
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A / RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A / RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A / RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 807
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A / DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A / DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A / DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 808
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A / MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A / MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A / MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 809
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = A / MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = A / MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = A / MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 810
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = JV2 / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 / A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) / A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 811
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = RV2 / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 / A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) / A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 812
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = DV2 / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 / A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) / A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 813
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 / A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) / A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 814
      A = TO_FM_INTERVAL( 2 , 7 )
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+20)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 / A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 / A '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) / A
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 815
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      C = TO_FM_INTERVAL(-9 , -5 )
      BMAT = C / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = C / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = C / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST29

      SUBROUTINE TEST30

!  Array division.

      IMPLICIT NONE
      INTEGER :: J, K

!             (4) rank 2  /  rank 2

      NCASE = 816
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT / JV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / JV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / JV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 817
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT / RV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / RV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / RV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 818
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT / DV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / DV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / DV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 819
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT / MFMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / MFMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / MFMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 820
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = AMAT / MIMV2
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / MIMV2 '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / MIMV2(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 821
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
            CMAT(J,K) = TO_FM_INTERVAL( (J+13)*K , (J+24)*K + K + 1 )
         ENDDO
      ENDDO
      BMAT = AMAT / CMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = AMAT / CMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = AMAT(J,K) / CMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 822
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = JV2 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = JV2 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = JV2(J,K) / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 823
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            RV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = RV2 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = RV2 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = RV2(J,K) / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 824
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            DV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = DV2 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = DV2 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = DV2(J,K) / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 825
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MFMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MFMV2 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MFMV2 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MFMV2(J,K) / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 826
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            MIMV2(J,K) = (J+10)*K
         ENDDO
      ENDDO
      BMAT = MIMV2 / AMAT
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' BMAT = MIMV2 / AMAT '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = BMAT(J,K)
            CORRECT = MIMV2(J,K) / AMAT(J,K)
            CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
            WRITE (KLOG,*) ' ',TRIM(STRING)
            IF (.NOT.(RESULT == CORRECT)) THEN
                CALL ERRPRTFM(' = ',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST30

      SUBROUTINE TEST31

!  Elementary functions.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing interval functions abs, acos, ..., atan2.')")

      NCASE = 827
      A = TO_FM_INTERVAL( 2 , 3 )
      RESULT = ABS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ABS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ABS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 828
      A = TO_FM_INTERVAL( -2 , 3 )
      RESULT = ABS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ABS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ABS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 829
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = ACOS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ACOS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = ACOS( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ACOS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 830
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = ACOS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ACOS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ACOS( TO_FM( '0.6' ) ) , ACOS( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ACOS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 831
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = ACOS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ACOS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ACOS( TO_FM( '-0.5' ) ) , ACOS( TO_FM( '-0.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ACOS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 832
      A = TO_FM_INTERVAL( '-0.6' , '0.5' )
      RESULT = ACOS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ACOS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ACOS( TO_FM( '0.5' ) ) , ACOS( TO_FM( '-0.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ACOS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 833
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = AINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' AINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' AINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 834
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = AINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' AINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' AINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 835
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = AINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' AINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' AINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 836
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = ANINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ANINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ANINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 837
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = ANINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ANINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -2 , 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ANINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 838
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = ANINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ANINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ANINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 839
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = NINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' NINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' NINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 840
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = ASIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ASIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = ASIN( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ASIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 841
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = ASIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ASIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ASIN( TO_FM( '0.5' ) ) , ASIN( TO_FM( '0.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ASIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 842
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = ASIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ASIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ASIN( TO_FM( '-0.6' ) ) , ASIN( TO_FM( '-0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ASIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 843
      A = TO_FM_INTERVAL( '-0.6' , '0.5' )
      RESULT = ASIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ASIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ASIN( TO_FM( '-0.6' ) ) , ASIN( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ASIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 844
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = ATAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = ATAN( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 845
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = ATAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN( TO_FM( '0.5' ) ) , ATAN( TO_FM( '0.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 846
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = ATAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN( TO_FM( '-0.6' ) ) , ATAN( TO_FM( '-0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 847
      A = TO_FM_INTERVAL( '-0.6' , '0.5' )
      RESULT = ATAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN( TO_FM( '-0.6' ) ) , ATAN( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 848
      A = TO_FM_INTERVAL( '-0.5' , '1.01' )
      RESULT = ATAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN( TO_FM( '-0.5' ) ) , ATAN( TO_FM( '1.01' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 849
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( '0.5' ) , TO_FM( ' 0.8 ' ) ) ,  &
                                ATAN2( TO_FM( '0.6' ) , TO_FM( ' 0.7 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 850
      A = TO_FM_INTERVAL( ' 0.5' , ' 0.6' )
      B = TO_FM_INTERVAL( '-0.8' , '-0.7' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( '0.6' ) , TO_FM( '-0.7 ' ) ) ,  &
                                ATAN2( TO_FM( '0.5' ) , TO_FM( '-0.8 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 851
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      B = TO_FM_INTERVAL( '-0.8' , '-0.7' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( '-0.5' ) , TO_FM( '-0.8 ' ) ) ,  &
                                ATAN2( TO_FM( '-0.6' ) , TO_FM( '-0.7 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 852
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      B = TO_FM_INTERVAL( ' 0.7' , ' 0.8' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( '-0.6' ) , TO_FM( ' 0.7 ' ) ) ,  &
                                ATAN2( TO_FM( '-0.5' ) , TO_FM( ' 0.8 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 853
      A = TO_FM_INTERVAL( '-0.1' , ' 0.2' )
      B = TO_FM_INTERVAL( ' 0.7' , ' 0.8' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( '-0.1' ) , TO_FM( ' 0.7 ' ) ) ,  &
                                ATAN2( TO_FM( ' 0.2' ) , TO_FM( ' 0.7 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 854
      A = TO_FM_INTERVAL( ' 0.1' , ' 0.2' )
      B = TO_FM_INTERVAL( '-0.7' , ' 0.8' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2( TO_FM( ' 0.1' ) , TO_FM( ' 0.8 ' ) ) ,  &
                                ATAN2( TO_FM( ' 0.1' ) , TO_FM( '-0.7 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 855
      A = TO_FM_INTERVAL( '-0.1' , ' 0.2' )
      B = TO_FM_INTERVAL( '-0.7' , '-0.8' )
      RESULT = ATAN2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ATAN2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( ATAN2(-TINY(TO_FM( ' 0 ' )) , TO_FM( '-0.8 ' ) ) ,  &
                                ATAN2( TINY(TO_FM( ' 0 ' )) , TO_FM( '-0.8 ' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ATAN2',2,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST31

      SUBROUTINE TEST32

!  Elementary functions.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing interval functions ceiling, cos, ..., dot_product.')")

      NCASE = 856
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = CEILING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' CEILING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 3 , 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' CEILING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 857
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = CEILING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' CEILING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -2 , 4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' CEILING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 858
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = CEILING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' CEILING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' CEILING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 859
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = COS( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 860
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COS( TO_FM( '0.6' ) ) , COS( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 861
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COS( TO_FM( '-0.6' ) ) , COS( TO_FM( '-0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 862
      A = TO_FM_INTERVAL( '-0.6' , '0.5' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COS( TO_FM( '-0.6' ) ) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 863
      A = TO_FM_INTERVAL( '3.1' , '3.2' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , COS( TO_FM( '3.2' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 864
      A = TO_FM_INTERVAL( '-0.1' , '3.8' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 865
      A = TO_FM_INTERVAL( '9.1' , '9.9' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , COS( TO_FM( '9.9' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 866
      A = TO_FM_INTERVAL( '-22.1' , '-21.9' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , COS( TO_FM( '-22.1' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 867
      A = TO_FM_INTERVAL( '23.4' , '23.5' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COS( TO_FM( '23.4' ) ) , COS( TO_FM( '23.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 868
      A = TO_FM_INTERVAL( '31.4' , '31.7' )
      RESULT = COS(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COS( TO_FM( '31.7' ) ) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 869
      A = TO_FM_INTERVAL( '1.2' , '1.3' )
      RESULT = COSH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COSH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COSH( TO_FM( '1.2' ) ) , COSH( TO_FM( '1.3' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COSH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 870
      A = TO_FM_INTERVAL( '-1.2' , '1.3' )
      RESULT = COSH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COSH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(1) , COSH( TO_FM( '1.3' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COSH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 871
      A = TO_FM_INTERVAL( '-2.2' , '-1.3' )
      RESULT = COSH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COSH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( COSH( TO_FM( '-1.3' ) ) , COSH( TO_FM( '-2.2' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COSH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 872
      A = TO_FM_INTERVAL( '-2.2' , '1.3' )
      RESULT = DBLE(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DBLE(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-2.2' ) , TO_FM( '1.3' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DBLE',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 873
      A = TO_FM_INTERVAL( '2' , '3' )
      B = TO_FM_INTERVAL( '1' , '5' )
      RESULT = DIM(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DIM(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '0' ) , TO_FM( '2' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DIM',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 874
      A = TO_FM_INTERVAL( '-2' , '3' )
      B = TO_FM_INTERVAL( '-7' , '5' )
      RESULT = DIM(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DIM(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '0' ) , TO_FM( '10' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DIM',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 875
      A = TO_FM_INTERVAL( '8' , '9' )
      B = TO_FM_INTERVAL( '2' , '3' )
      RESULT = DIM(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DIM(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '5' ) , TO_FM( '7' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DIM',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 876
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = DINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 877
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = DINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 878
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = DINT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' DINT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DINT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 879
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      BVEC = (/ TO_FM_INTERVAL(-3 , 4 ) , TO_FM_INTERVAL( -1 , 2 ) , TO_FM_INTERVAL(  0 , 6 ) /)
      RESULT = DOT_PRODUCT( AVEC , BVEC )
      CORRECT = 0
      DO J = 1, 3
         CORRECT = CORRECT + AVEC(J) * BVEC(J)
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      WRITE (KLOG,*) '     BVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',BVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
      WRITE (KLOG,*) ' DOT_PRODUCT( AVEC , BVEC ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' DOT_PRODUCT',0,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST32

      SUBROUTINE TEST33

!  Elementary functions.

      IMPLICIT NONE
      INTEGER :: J, K, L

      WRITE (KW,"(/' Testing interval functions epsilon, exp, ..., matmul.')")

      NCASE = 880
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = EPSILON(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.25',RESULT,STRING)
      WRITE (KLOG,*) ' EPSILON(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( EPSILON( TO_FM(-2.1) ) , EPSILON( TO_FM(3.8) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EPSILON',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 881
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      CALL FM_INTERVAL_ULP(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.25',RESULT,STRING)
      WRITE (KLOG,*) ' EPSILON(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( EPSILON( TO_FM(-2.1) ) , EPSILON( TO_FM(3.8) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EPSILON',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 882
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = EXP(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' EXP(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = EXP( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EXP',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 883
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = EXP(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' EXP(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( EXP( TO_FM( '0.5' ) ) , EXP( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EXP',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 884
      A = TO_FM_INTERVAL( '-2.5' , '3.7' )
      RESULT = EXP(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' EXP(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( EXP( TO_FM( '-2.5' ) ) , EXP( TO_FM( '3.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EXP',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 885
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = FLOOR(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FLOOR(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FLOOR',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 886
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = FLOOR(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FLOOR(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -3 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FLOOR',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 887
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = FLOOR(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FLOOR(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -5 , -4 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FLOOR',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 888
      A = TO_FM_INTERVAL( '1.4' , '1.7' )
      RESULT = FRACTION(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FRACTION(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = A
      CORRECT%LEFT%MP(2) = 0
      CORRECT%RIGHT%MP(2) = 0
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FRACTION',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 889
      A = TO_FM_INTERVAL( '0.4' , '1.7' )
      RESULT = FRACTION(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FRACTION(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 0 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FRACTION',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 890
      A = TO_FM_INTERVAL( '-3.4' , '5.7' )
      RESULT = FRACTION(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FRACTION(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -1 , 1 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FRACTION',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 891
      A = TO_FM_INTERVAL( '-3.4' , '5.7' )
      RESULT = HUGE(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' HUGE(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( HUGE( TO_FM(1) ) , HUGE( TO_FM(1) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' HUGE',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 892
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = INT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( 2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 893
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = INT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -2 , 3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 894
      A = TO_FM_INTERVAL( '-4.4' , '-3.7' )
      RESULT = INT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( -4 , -3 )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 895
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = LOG(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = LOG( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 896
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = LOG(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( LOG( TO_FM( '0.5' ) ) , LOG( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 897
      A = TO_FM_INTERVAL( '1234.5' , '1234.6' )
      RESULT = LOG(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( LOG( TO_FM( '1234.5' ) ) , LOG( TO_FM( '1234.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 898
      A = TO_FM_INTERVAL( '0' , '3.7' )
      RESULT = LOG(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , LOG( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                LEFT_ENDPOINT(RESULT) < 0)) THEN
          CALL ERRPRTFM(' LOG',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 899
      A = TO_FM_INTERVAL( '-0.01' , '3.7' )
      RESULT = LOG(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , LOG( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                LEFT_ENDPOINT(RESULT) < 0)) THEN
          CALL ERRPRTFM(' LOG',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 900
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = LOG10(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG10(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = LOG10( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG10',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 901
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = LOG10(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG10(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( LOG10( TO_FM( '0.5' ) ) , LOG10( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG10',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 902
      A = TO_FM_INTERVAL( '1234.5' , '1234.6' )
      RESULT = LOG10(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG10(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( LOG10( TO_FM( '1234.5' ) ) , LOG10( TO_FM( '1234.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG10',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 903
      A = TO_FM_INTERVAL( '0' , '3.7' )
      RESULT = LOG10(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG10(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , LOG10( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                LEFT_ENDPOINT(RESULT) < 0)) THEN
          CALL ERRPRTFM(' LOG10',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 904
      A = TO_FM_INTERVAL( '-0.01' , '3.7' )
      RESULT = LOG10(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG10(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , LOG10( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                LEFT_ENDPOINT(RESULT) < 0)) THEN
          CALL ERRPRTFM(' LOG10',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 905
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+10)*K , (J+10)*K + K + 1 )
            BMAT(J,K) = TO_FM_INTERVAL( (J+11)*K , (J+11)*K + K + 1 )
         ENDDO
      ENDDO
      CMAT = MATMUL( AMAT , BMAT )
      DMAT = 0
      DO J = 1, 3
         DO K = 1, 3
            DO L = 1, 3
               DMAT(J,K) = DMAT(J,K) + AMAT(J,L) * BMAT(L,K)
            ENDDO
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      WRITE (KLOG,*) '     BMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',BMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' MATMUL( AMAT , BMAT ) = '
      DO J = 1, 3
         DO K = 1, 3
            RESULT = CMAT(J,K)
            CORRECT = DMAT(J,K)
            CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
            ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
                    ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
            IF (.NOT.(ERROR < 1.0D-25)) THEN
                CALL ERRPRTFM(' MATMUL',0,A,'A',B,'B',B,'B')
                EXIT
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 906
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+10)*K , (J+10)*K + K + 1 )
         ENDDO
         BVEC(J) = TO_FM_INTERVAL( (J+11)*K , (J+11)*K + K + 1 )
      ENDDO
      CVEC = MATMUL( AMAT , BVEC )
      DVEC = 0
      DO J = 1, 3
         DO L = 1, 3
            DVEC(J) = DVEC(J) + AMAT(J,L) * BVEC(L)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      WRITE (KLOG,*) '     BVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',BVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      WRITE (KLOG,*) ' MATMUL( AMAT , BVEC ) = '
      DO J = 1, 3
         RESULT = CVEC(J)
         CORRECT = DVEC(J)
         CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
         ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
                 ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
         IF (.NOT.(ERROR < 1.0D-25)) THEN
             CALL ERRPRTFM(' MATMUL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      NCASE = 907
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+10)*K , (J+10)*K + K + 1 )
         ENDDO
         BVEC(J) = TO_FM_INTERVAL( (J+11)*K , (J+11)*K + K + 1 )
      ENDDO
      CVEC = MATMUL( BVEC , AMAT )
      DVEC = 0
      DO J = 1, 3
         DO L = 1, 3
            DVEC(J) = DVEC(J) + BVEC(L) * AMAT(L,J)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For BVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',BVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' MATMUL( BVEC , AMAT ) = '
      DO J = 1, 3
         RESULT = CVEC(J)
         CORRECT = DVEC(J)
         CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
         ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
                 ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
         IF (.NOT.(ERROR < 1.0D-25)) THEN
             CALL ERRPRTFM(' MATMUL',0,A,'A',B,'B',B,'B')
             EXIT
         ENDIF
      ENDDO
      WRITE (KLOG,*) ' '

      RETURN
      END SUBROUTINE TEST33

      SUBROUTINE TEST34

!  Elementary functions.

      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing interval functions max, maxval, ..., modulo.')")

      NCASE = 908
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = MAX(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MAX(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.7' , '0.8' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MAX',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 909
      A = TO_FM_INTERVAL( '-0.5' , '0.6' )
      B = TO_FM_INTERVAL( '-0.7' , '0.8' )
      RESULT = MAX(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MAX(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-0.5' , '0.8' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MAX',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 910
      A = TO_FM_INTERVAL( '-0.5' , '0.6' )
      B = TO_FM_INTERVAL( '-0.7' , '0.8' )
      C = TO_FM_INTERVAL( '-0.4' , '0.3' )
      D = TO_FM_INTERVAL( '-0.9' , '0.9' )
      RESULT = MAX(A,B,C,D)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',C,STRING)
      WRITE (KLOG,*) '     C  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',D,STRING)
      WRITE (KLOG,*) '     D  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F38.33',RESULT,STRING)
      WRITE (KLOG,*) ' MAX(A,B,C,D) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-0.4' , '0.9' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MAX',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 911
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 8 ) , TO_FM_INTERVAL( 4 , 5 ) /)
      RESULT = MAXVAL(AVEC)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MAXVAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4' , '8' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MAXVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 912
      AMAT(1,1:3) = (/ TO_FM_INTERVAL( 2, 7 ) , TO_FM_INTERVAL(-3,8 ) , TO_FM_INTERVAL( 4, 5 ) /)
      AMAT(2,1:3) = (/ TO_FM_INTERVAL(-2, 1 ) , TO_FM_INTERVAL(-4,9 ) , TO_FM_INTERVAL(-6,-3 ) /)
      AMAT(3,1:3) = (/ TO_FM_INTERVAL(-7,-4 ) , TO_FM_INTERVAL( 3,8 ) , TO_FM_INTERVAL( 0, 1 ) /)
      RESULT = MAXVAL(AMAT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MAXVAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '4' , '9' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MAXVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 913
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = MIN(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MIN(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '0.6' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MIN',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 914
      A = TO_FM_INTERVAL( '-0.5' , '0.6' )
      B = TO_FM_INTERVAL( '-0.7' , '0.8' )
      RESULT = MIN(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MIN(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-0.7' , '0.6' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MIN',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 915
      A = TO_FM_INTERVAL( '-0.5' , '0.6' )
      B = TO_FM_INTERVAL( '-0.7' , '0.8' )
      C = TO_FM_INTERVAL( '-0.4' , '0.3' )
      D = TO_FM_INTERVAL( '-0.9' , '0.9' )
      RESULT = MIN(A,B,C,D)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',C,STRING)
      WRITE (KLOG,*) '     C  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',D,STRING)
      WRITE (KLOG,*) '     D  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F38.33',RESULT,STRING)
      WRITE (KLOG,*) ' MIN(A,B,C,D) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-0.9' , '0.3' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MIN',3,A,'A',B,'B',C,'C')
      ENDIF

      NCASE = 916
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 8 ) , TO_FM_INTERVAL( 4 , 5 ) /)
      RESULT = MINVAL(AVEC)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MINVAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-3' , '5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MINVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 917
      AMAT(1,1:3) = (/ TO_FM_INTERVAL( 2, 7 ) , TO_FM_INTERVAL(-3,8 ) , TO_FM_INTERVAL( 4, 5 ) /)
      AMAT(2,1:3) = (/ TO_FM_INTERVAL(-2, 1 ) , TO_FM_INTERVAL(-4,9 ) , TO_FM_INTERVAL(-6,-3 ) /)
      AMAT(3,1:3) = (/ TO_FM_INTERVAL(-7,-4 ) , TO_FM_INTERVAL( 3,8 ) , TO_FM_INTERVAL( 0, 1 ) /)
      RESULT = MINVAL(AMAT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MINVAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-7' , '-4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MINVAL',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 918
      A = TO_FM_INTERVAL( '2.5' , '2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '0.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 919
      A = TO_FM_INTERVAL( '2.5' , '2.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '0.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 920
      A = TO_FM_INTERVAL( '2.5' , '3.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '1.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 921
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.0' , '2.0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 922
      A = TO_FM_INTERVAL( '2.5' , '3.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.4' , '1.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 923
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.0' , '2.05' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 924
      A = TO_FM_INTERVAL( '2.5' , '4.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.0' , '2.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 925
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 926
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 927
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 928
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2.05' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 929
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 930
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 931
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 932
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.1' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2.05' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 933
      A = TO_FM_INTERVAL( '2.5' , '3.1' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '1.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 934
      A = TO_FM_INTERVAL( '2.5' , '3.1' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.4' , '1.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 935
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0' , '2' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 936
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0' , '2.05' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 937
      A = TO_FM_INTERVAL( '-11' , '2' )
      B = TO_FM_INTERVAL( '3' , '3' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-3' , '2' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 938
      A = TO_FM_INTERVAL( '-11' , '2' )
      B = TO_FM_INTERVAL( '3' , '4' )
      RESULT = MOD(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MOD(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-4' , '2' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MOD',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 939
      A = TO_FM_INTERVAL( '2.5' , '2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '0.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 940
      A = TO_FM_INTERVAL( '2.5' , '2.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '0.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 941
      A = TO_FM_INTERVAL( '2.5' , '3.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.5' , '1.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 942
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.0' , '2.0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 943
      A = TO_FM_INTERVAL( '2.5' , '3.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.4' , '1.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 944
      A = TO_FM_INTERVAL( '2.5' , '4.7' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.0' , '2.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 945
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.9' , '1.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 946
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0.9' , '1.7' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 947
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0' , '2' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 948
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '2.0' , '2.1' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0' , '2.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 949
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 950
      A = TO_FM_INTERVAL( '-3.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.1' , '-0.4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 951
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 952
      A = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.1' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2.05' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 953
      A = TO_FM_INTERVAL( '2.5' , '3.1' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.5' , '-0.9' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 954
      A = TO_FM_INTERVAL( '2.5' , '3.1' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-1.7' , '-0.9' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 955
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-2.0' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 956
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-2.1' , '-2.0' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2.1' , '0.0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 957
      A = TO_FM_INTERVAL( '-11' , '2' )
      B = TO_FM_INTERVAL( '3' , '4' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '0' , '4' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 958
      A = TO_FM_INTERVAL( '-11' , '2' )
      B = TO_FM_INTERVAL( '-5' , '-4' )
      RESULT = MODULO(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' MODULO(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-5' , '0' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' MODULO',2,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST34

      SUBROUTINE TEST35

!  Elementary functions.

      IMPLICIT NONE
      INTEGER :: J, K, L

      WRITE (KW,"(/' Testing interval functions nearest, product, ..., sin.')")

      NCASE = 959
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '1' , '1' )
      RESULT = NEAREST(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' NEAREST(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '2.5' , '4.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' NEAREST',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 960
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-1' , '-1' )
      RESULT = NEAREST(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' NEAREST(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '2.5' , '4.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' NEAREST',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 961
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-1' , '1' )
      RESULT = NEAREST(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' NEAREST(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '2.5' , '4.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' NEAREST',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 962
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RESULT = PRODUCT( AVEC )
      CORRECT = 1
      DO J = 1, 3
         CORRECT = CORRECT * AVEC(J)
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
      WRITE (KLOG,*) ' PRODUCT( AVEC ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' PRODUCT',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 963
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      RESULT = PRODUCT( AMAT )
      CORRECT = 1
      DO J = 1, 3
         DO K = 1, 3
            CORRECT = CORRECT * AMAT(J,K)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      CALL FMFORM_INTERVAL('F30.10',RESULT,STRING)
      WRITE (KLOG,*) ' PRODUCT( AMAT ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' PRODUCT',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 964
      A = TO_FM_INTERVAL( '-2.1' , '3.8' )
      RESULT = REAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' REAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-2.1' , '3.8' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' REAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 965
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = RRSPACING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' RRSPACING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      B = ABS(A)
      CORRECT = TO_FM_INTERVAL( RRSPACING(LEFT_ENDPOINT(B)) ,  &
                                RRSPACING(RIGHT_ENDPOINT(B)) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' RRSPACING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 966
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = SCALE(A,2)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SCALE(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = A * TO_FM( MBASE ) ** 2
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SCALE',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 967
      A = TO_FM_INTERVAL( '1.7' , '3.8' )
      L = 2
      RESULT = SETEXPONENT(A,L)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SETEXPONENT(A,2) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '1.7' , '3.8' ) * TO_FM( MBASE ) ** (L-1)
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SETEXPONENT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 968
      A = TO_FM_INTERVAL( '0.7' , '3.8' )
      L = 2
      RESULT = SETEXPONENT(A,L)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SETEXPONENT(A,2) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(1) , TO_FM(MBASE) ) * TO_FM( MBASE ) ** (L-1)
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SETEXPONENT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 969
      A = TO_FM_INTERVAL( '-4.7' , '-4.2' )
      L = 3
      RESULT = SETEXPONENT(A,L)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SETEXPONENT(A,2) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-4.7' , '-4.2' ) * TO_FM( MBASE ) ** (L-1)
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SETEXPONENT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 970
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '1' , '1' )
      RESULT = SIGN(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIGN(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '2.5' , '4.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIGN',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 971
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-2' , '-1' )
      RESULT = SIGN(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIGN(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-4.1' , '-2.5' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIGN',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 972
      A = TO_FM_INTERVAL( '2.5' , '4.1' )
      B = TO_FM_INTERVAL( '-1' , '1' )
      RESULT = SIGN(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIGN(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( '-4.1' , '4.1' )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIGN',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 973
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = SIN( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 974
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '0.6' ) ) , SIN( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 975
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '-0.6' ) ) , SIN( TO_FM( '-0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 976
      A = TO_FM_INTERVAL( '1.57' , '1.58' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '1.58' ) ) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 977
      A = TO_FM_INTERVAL( '4.7' , '4.8' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , SIN( TO_FM( '4.8' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 978
      A = TO_FM_INTERVAL( '1.5' , '4.8' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 979
      A = TO_FM_INTERVAL( '10.9' , '11.0' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , SIN( TO_FM( '10.9' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 980
      A = TO_FM_INTERVAL( '9.1' , '9.2' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '9.2' ) ) , SIN( TO_FM( '9.1' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 981
      A = TO_FM_INTERVAL( '-23.6' , '-23.5' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '-23.5' ) ) , TO_FM(1) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 982
      A = TO_FM_INTERVAL( '23.4' , '23.5' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SIN( TO_FM( '23.4' ) ) , SIN( TO_FM( '23.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 983
      A = TO_FM_INTERVAL( '36.1' , '36.2' )
      RESULT = SIN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM(-1) , SIN( TO_FM( '36.2' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN',1,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST35

      SUBROUTINE TEST36

!  Elementary functions.

      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing interval functions sinh, sqrt, ..., tanh.')")

      NCASE = 984
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = SINH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SINH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = SINH( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SINH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 985
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = SINH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SINH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SINH( TO_FM( '0.5' ) ) , SINH( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SINH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 986
      A = TO_FM_INTERVAL( '-2.5' , '3.7' )
      RESULT = SINH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SINH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SINH( TO_FM( '-2.5' ) ) , SINH( TO_FM( '3.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SINH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 987
      A = TO_FM_INTERVAL( '2.1' , '3.8' )
      RESULT = SPACING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SPACING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      B = ABS(A)
      CORRECT = TO_FM_INTERVAL( SPACING(LEFT_ENDPOINT(B)) ,  &
                                SPACING(RIGHT_ENDPOINT(B)) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SPACING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 988
      A = TO_FM_INTERVAL( '0.2' , '3.8' )
      RESULT = SPACING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SPACING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      B = ABS(A)
      CORRECT = TO_FM_INTERVAL( SPACING(LEFT_ENDPOINT(B)) ,  &
                                SPACING(RIGHT_ENDPOINT(B)) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SPACING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 989
      A = TO_FM_INTERVAL( '-3.8' , '-0.2' )
      RESULT = SPACING(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.30',RESULT,STRING)
      WRITE (KLOG,*) ' SPACING(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      B = ABS(A)
      CORRECT = TO_FM_INTERVAL( SPACING(LEFT_ENDPOINT(B)) ,  &
                                SPACING(RIGHT_ENDPOINT(B)) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SPACING',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 990
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = SQRT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = SQRT( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 991
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = SQRT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SQRT( TO_FM( '0.5' ) ) , SQRT( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 992
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      CALL FM_INTERVAL_SQRT(A,RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SQRT( TO_FM( '0.5' ) ) , SQRT( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 993
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = A
      CALL FM_INTERVAL_SQRT_R1(RESULT)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SQRT( TO_FM( '0.5' ) ) , SQRT( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 994
      A = TO_FM_INTERVAL( '1234.5' , '1234.6' )
      RESULT = SQRT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( SQRT( TO_FM( '1234.5' ) ) , SQRT( TO_FM( '1234.6' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 995
      A = TO_FM_INTERVAL( '0' , '3.7' )
      RESULT = SQRT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '0' ) , SQRT( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 996
      A = TO_FM_INTERVAL( '-0.01' , '3.7' )
      RESULT = SQRT(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SQRT(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '0' ) , SQRT( TO_FM( '3.7' ) ) )
      ERROR = ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SQRT',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 997
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RESULT = SUM( AVEC )
      CORRECT = 0
      DO J = 1, 3
         CORRECT = CORRECT + AVEC(J)
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
      WRITE (KLOG,*) ' SUM( AVEC ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SUM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 998
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      RESULT = SUMVEC1( AVEC )
      CORRECT = 0
      DO J = 1, 3
         CORRECT = CORRECT + AVEC(J)
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
      WRITE (KLOG,*) ' SUMVEC1( AVEC ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SUM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 999
      AVEC = (/ TO_FM_INTERVAL( 2 , 7 ) , TO_FM_INTERVAL( -3 , 5 ) , TO_FM_INTERVAL( -8 , 1 ) /)
      CALL SUMVEC2( AVEC , RESULT )
      CORRECT = 0
      DO J = 1, 3
         CORRECT = CORRECT + AVEC(J)
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AVEC  = '
      DO J = 1, 3
         CALL FMFORM_INTERVAL('F40.35',AVEC(J),STRING)
         WRITE (KLOG,*) '   ',TRIM(STRING)
      ENDDO
      CALL FMFORM_INTERVAL('F30.25',RESULT,STRING)
      WRITE (KLOG,*) ' CALL SUMVEC2( AVEC , RESULT ) gives RESULT = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SUM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1000
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( (J+20)*K , (J+20)*K + K + 1 )
         ENDDO
      ENDDO
      RESULT = SUM( AMAT )
      CORRECT = 0
      DO J = 1, 3
         DO K = 1, 3
            CORRECT = CORRECT + AMAT(J,K)
         ENDDO
      ENDDO
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' For AMAT  = '
      DO J = 1, 3
         DO K = 1, 3
            CALL FMFORM_INTERVAL('F40.35',AMAT(J,K),STRING)
            WRITE (KLOG,*) '   ',TRIM(STRING)
         ENDDO
      ENDDO
      CALL FMFORM_INTERVAL('F30.10',RESULT,STRING)
      WRITE (KLOG,*) ' SUM( AMAT ) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SUM',0,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1001
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TAN( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1002
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TAN( TO_FM( '0.6' ) ) , TAN( TO_FM( '0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1003
      A = TO_FM_INTERVAL( '-0.6' , '-0.5' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TAN( TO_FM( '-0.6' ) ) , TAN( TO_FM( '-0.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1004
      A = TO_FM_INTERVAL( '1.57' , '1.58' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1005
      A = TO_FM_INTERVAL( '4.7' , '4.8' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1006
      A = TO_FM_INTERVAL( '1.5' , '4.8' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1007
      A = TO_FM_INTERVAL( '10.9' , '11.0' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1008
      A = TO_FM_INTERVAL( '9.1' , '9.2' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TAN( TO_FM( '9.2' ) ) , TAN( TO_FM( '9.1' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1009
      A = TO_FM_INTERVAL( '-23.6' , '-23.5' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1010
      A = TO_FM_INTERVAL( '23.4' , '23.5' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TAN( TO_FM( '23.4' ) ) , TAN( TO_FM( '23.5' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1011
      A = TO_FM_INTERVAL( '36.1' , '36.2' )
      RESULT = TAN(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TAN(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TO_FM( '-OVERFLOW' ) , TO_FM( 'OVERFLOW' ) )
      IF (.NOT.(LEFT_ENDPOINT(RESULT)  < 0 .AND. IS_OVERFLOW(LEFT_ENDPOINT(RESULT)) .AND.  &
                RIGHT_ENDPOINT(RESULT) > 0 .AND. IS_OVERFLOW(RIGHT_ENDPOINT(RESULT)))) THEN
          CALL ERRPRTFM(' TAN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1012
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = TANH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TANH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TANH( TO_FM( '0.5' ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TANH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1013
      A = TO_FM_INTERVAL( '0.5' , '0.7' )
      RESULT = TANH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TANH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TANH( TO_FM( '0.5' ) ) , TANH( TO_FM( '0.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TANH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1014
      A = TO_FM_INTERVAL( '-2.5' , '3.7' )
      RESULT = TANH(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TANH(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TANH( TO_FM( '-2.5' ) ) , TANH( TO_FM( '3.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TANH',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1015
      A = TO_FM_INTERVAL( '-2.5' , '3.7' )
      RESULT = TINY(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' TINY(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_INTERVAL( TINY( TO_FM( '-2.5' ) ) , TINY( TO_FM( '3.7' ) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' TINY',1,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST36

      FUNCTION SUMVEC1(A)     RESULT (RETURN_VALUE)
      USE FM_INTERVAL_ARITHMETIC
      IMPLICIT NONE
      TYPE (FM_INTERVAL) :: A(3), RETURN_VALUE
      INTEGER :: J

      RETURN_VALUE = 0
      DO J = 1, 3
         RETURN_VALUE = RETURN_VALUE + A(J)
      ENDDO

      END FUNCTION SUMVEC1

      SUBROUTINE SUMVEC2(A,B)
      USE FM_INTERVAL_ARITHMETIC
      IMPLICIT NONE
      TYPE (FM_INTERVAL) :: A(3), B
      INTEGER :: J

      B = 0
      DO J = 1, 3
         B = B + A(J)
      ENDDO

      END SUBROUTINE SUMVEC2

      SUBROUTINE TEST37

!  Special functions.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing interval functions beta, binomial, ..., fresnel_s.')")

      NCASE = 1016
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = BETA(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' BETA(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4 = (/ BETA( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  ),  &
               BETA( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) ),  &
               BETA( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  ),  &
               BETA( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) )   &
            /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4 ) , MAXVAL( MV4 ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' BETA',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1017
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = BINOMIAL(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' BINOMIAL(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4 = (/ BINOMIAL( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  ),  &
               BINOMIAL( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) ),  &
               BINOMIAL( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  ),  &
               BINOMIAL( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) )   &
            /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4 ) , MAXVAL( MV4 ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' BINOMIAL',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1018
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = FACTORIAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FACTORIAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ FACTORIAL( LEFT_ENDPOINT(A) ), FACTORIAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FACTORIAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1019
      A = TO_FM_INTERVAL( '0.5' , '0.5' )
      RESULT = GAMMA(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' GAMMA(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ GAMMA( LEFT_ENDPOINT(A) ), GAMMA( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' GAMMA',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1020
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = GAMMA(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' GAMMA(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ GAMMA( LEFT_ENDPOINT(A) ), GAMMA( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' GAMMA',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1021
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      C = TO_FM_INTERVAL( '0.3' , '0.4' )
      RESULT = INCOMPLETE_BETA(A,B,C)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',C,STRING)
      WRITE (KLOG,*) '     C  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INCOMPLETE_BETA(A,B,C) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV8 = (/ INCOMPLETE_BETA( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  , LEFT_ENDPOINT(C) ) ,  &
               INCOMPLETE_BETA( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) , LEFT_ENDPOINT(C) ) ,  &
               INCOMPLETE_BETA( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  , LEFT_ENDPOINT(C) ) ,  &
               INCOMPLETE_BETA( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) , LEFT_ENDPOINT(C) ) ,  &
               INCOMPLETE_BETA( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  , RIGHT_ENDPOINT(C) ),  &
               INCOMPLETE_BETA( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) , RIGHT_ENDPOINT(C) ),  &
               INCOMPLETE_BETA( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  , RIGHT_ENDPOINT(C) ),  &
               INCOMPLETE_BETA( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) , RIGHT_ENDPOINT(C) )   &
            /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV8 ) , MAXVAL( MV8 ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INCOMPLETE_BETA',3,A,'A',B,'B',C,'C')
      ENDIF

      NCASE = 1022
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = INCOMPLETE_GAMMA1(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INCOMPLETE_GAMMA1(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4 = (/ INCOMPLETE_GAMMA1( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  ),  &
               INCOMPLETE_GAMMA1( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) ),  &
               INCOMPLETE_GAMMA1( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  ),  &
               INCOMPLETE_GAMMA1( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) )   &
            /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4 ) , MAXVAL( MV4 ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INCOMPLETE_GAMMA1',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1023
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      B = TO_FM_INTERVAL( '0.7' , '0.8' )
      RESULT = INCOMPLETE_GAMMA2(A,B)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '     B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' INCOMPLETE_GAMMA2(A,B) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4 = (/ INCOMPLETE_GAMMA2( LEFT_ENDPOINT(A)  , LEFT_ENDPOINT(B)  ),  &
               INCOMPLETE_GAMMA2( LEFT_ENDPOINT(A)  , RIGHT_ENDPOINT(B) ),  &
               INCOMPLETE_GAMMA2( RIGHT_ENDPOINT(A) , LEFT_ENDPOINT(B)  ),  &
               INCOMPLETE_GAMMA2( RIGHT_ENDPOINT(A) , RIGHT_ENDPOINT(B) )   &
            /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4 ) , MAXVAL( MV4 ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' INCOMPLETE_GAMMA2',2,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1024
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = LOG_GAMMA(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG_GAMMA(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ LOG_GAMMA( LEFT_ENDPOINT(A) ), LOG_GAMMA( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG_GAMMA',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1025
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = POLYGAMMA(2,A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' POLYGAMMA(2,A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ POLYGAMMA(2, LEFT_ENDPOINT(A) ), POLYGAMMA(2, RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' POLYGAMMA',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1026
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = POCHHAMMER(A,9)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('ES40.25',RESULT,STRING)
      WRITE (KLOG,*) ' POCHHAMMER(A,9) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ POCHHAMMER( LEFT_ENDPOINT(A) , 9 ), POCHHAMMER( RIGHT_ENDPOINT(A) , 9 )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' POCHHAMMER',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1027
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = PSI(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' PSI(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ PSI( LEFT_ENDPOINT(A) ), PSI( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' PSI',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1028
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = BESSEL_J(2,A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' BESSEL_J(2,A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ BESSEL_J(2, LEFT_ENDPOINT(A) ), BESSEL_J(2, RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' BESSEL_J',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1029
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = BESSEL_Y(2,A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' BESSEL_Y(2,A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ BESSEL_Y(2, LEFT_ENDPOINT(A) ), BESSEL_Y(2, RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' BESSEL_Y',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1030
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = COS_INTEGRAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COS_INTEGRAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ COS_INTEGRAL( LEFT_ENDPOINT(A) ), COS_INTEGRAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COS_INTEGRAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1031
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = COSH_INTEGRAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' COSH_INTEGRAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ COSH_INTEGRAL( LEFT_ENDPOINT(A) ), COSH_INTEGRAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' COSH_INTEGRAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1032
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = EXP_INTEGRAL_EI(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' EXP_INTEGRAL_EI(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ EXP_INTEGRAL_EI( LEFT_ENDPOINT(A) ), EXP_INTEGRAL_EI( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EXP_INTEGRAL_EI',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1033
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = EXP_INTEGRAL_EN(2,A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' EXP_INTEGRAL_EN(2,A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ EXP_INTEGRAL_EN(2, LEFT_ENDPOINT(A) ), EXP_INTEGRAL_EN(2, RIGHT_ENDPOINT(A) ) /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' EXP_INTEGRAL_EN',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1034
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = FRESNEL_C(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FRESNEL_C(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ FRESNEL_C( LEFT_ENDPOINT(A) ), FRESNEL_C( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FRESNEL_C',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1035
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = FRESNEL_S(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' FRESNEL_S(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ FRESNEL_S( LEFT_ENDPOINT(A) ), FRESNEL_S( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' FRESNEL_S',1,A,'A',B,'B',B,'B')
      ENDIF

      RETURN
      END SUBROUTINE TEST37

      SUBROUTINE TEST38

!  Special functions.

      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing interval functions erf, erfc, ..., sinh_integral.')")

      NCASE = 1036
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = ERF(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ERF(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ ERF( LEFT_ENDPOINT(A) ), ERF( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ERF',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1037
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = ERFC(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' ERFC(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ ERFC( LEFT_ENDPOINT(A) ), ERFC( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' ERFC',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1038
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = LOG_ERFC(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG_ERFC(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ LOG_ERFC( LEFT_ENDPOINT(A) ), LOG_ERFC( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG_ERFC',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1039
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = LOG_INTEGRAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' LOG_INTEGRAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ LOG_INTEGRAL( LEFT_ENDPOINT(A) ), LOG_INTEGRAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' LOG_INTEGRAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1040
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = SIN_INTEGRAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SIN_INTEGRAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ SIN_INTEGRAL( LEFT_ENDPOINT(A) ), SIN_INTEGRAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SIN_INTEGRAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1041
      A = TO_FM_INTERVAL( '0.5' , '0.6' )
      RESULT = SINH_INTEGRAL(A)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F40.35',RESULT,STRING)
      WRITE (KLOG,*) ' SINH_INTEGRAL(A) = ',TRIM(STRING)
      WRITE (KLOG,*) ' '
      MV4(1:2) = (/ SINH_INTEGRAL( LEFT_ENDPOINT(A) ), SINH_INTEGRAL( RIGHT_ENDPOINT(A) )  /)
      CORRECT = TO_FM_INTERVAL( MINVAL( MV4(1:2) ) , MAXVAL( MV4(1:2) ) )
      ERROR = ABS( LEFT_ENDPOINT(RESULT) -  LEFT_ENDPOINT(CORRECT)) +  &
              ABS(RIGHT_ENDPOINT(RESULT) - RIGHT_ENDPOINT(CORRECT))
      IF (.NOT.(ERROR < 1.0D-25)) THEN
          CALL ERRPRTFM(' SINH_INTEGRAL',1,A,'A',B,'B',B,'B')
      ENDIF

      NCASE = 1042
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_IVL(A,B)"
      A = TO_FM_INTERVAL( 5, 11 )
      B = TO_FM_INTERVAL( 7, 13 )
      C = SUM_IVL(A,B)
      D = TO_FM_INTERVAL( 12, 24 )
      KW = KLOG
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A)") ' Function for A + B'
      CALL FMFORM_INTERVAL('F10.5',A,STRING)
      WRITE (KLOG,*) ' For A, B  = ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',B,STRING)
      WRITE (KLOG,*) '             ',TRIM(STRING)
      CALL FMFORM_INTERVAL('F10.5',C,STRING)
      WRITE (KLOG,*) '        gave ',TRIM(STRING)
      IF ( (.NOT. FMCOMPARE(C%LEFT,'==',D%LEFT)) .OR.  &
           (.NOT. FMCOMPARE(C%RIGHT,'==',D%RIGHT)) ) THEN
           CALL ERRPRTFM(' interval-valued function call',0,A,'A',B,'B',B,'B')
      ENDIF
      KW = KWSAVE

      NCASE = 1043
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_IVL(A,B)  for a,b,c vectors."
      DO J = 1, 3
         AVEC(J) = TO_FM_INTERVAL( 4+J, 10+J )
         BVEC(J) = TO_FM_INTERVAL( 40+J, 51+J )
      ENDDO
      CVEC = SUM_IVL(AVEC,BVEC)
      DO J = 1, 3
         DVEC(J) = TO_FM_INTERVAL( 4+J, 10+J ) + TO_FM_INTERVAL( 40+J, 51+J )
      ENDDO
      DO J = 1, 3
         IF ( (.NOT. FMCOMPARE(CVEC(J)%LEFT,'==',DVEC(J)%LEFT)) .OR.  &
              (.NOT. FMCOMPARE(CVEC(J)%RIGHT,'==',DVEC(J)%RIGHT)) ) THEN
              CALL ERRPRTFM(' interval-valued vector function call',0,A,'A',B,'B',B,'B')
         ENDIF
      ENDDO

      NCASE = 1044
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_IVL(A,B)  for a,b,c matrices."
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_INTERVAL( 4+J+K, 10+J+K )
            BMAT(J,K) = TO_FM_INTERVAL( 40+J, 51+J )
         ENDDO
      ENDDO
      CMAT = SUM_IVL(AMAT,BMAT)
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_INTERVAL( 4+J+K, 10+J+K ) + TO_FM_INTERVAL( 40+J, 51+J )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            IF ( (.NOT. FMCOMPARE(CMAT(J,K)%LEFT,'==',DMAT(J,K)%LEFT)) .OR.  &
                 (.NOT. FMCOMPARE(CMAT(J,K)%RIGHT,'==',DMAT(J,K)%RIGHT)) ) THEN
                 CALL ERRPRTFM(' interval-valued vector function call',0,A,'A',B,'B',B,'B')
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END SUBROUTINE TEST38


      SUBROUTINE ERRPRTFM(OPERATION,NARGS,M1,NAME1,M2,NAME2,M3,NAME3)

!  Print error messages.

!  OPERATION is the operation being tested.
!  NARGS is the number of input arguments.
!  M1, M2, M3, are the values of the input arguments.
!  NAME1, 2, 3 are the variable names of the input arguments.
!  RESULT is the result of the operation.
!  CORRECT is the expected result.

      USE FM_INTERVAL_ARITHMETIC
      IMPLICIT NONE

      TYPE (FM_INTERVAL) :: M1,M2,M3

      CHARACTER(*) :: NAME1,NAME2,NAME3
      CHARACTER(*) :: OPERATION
      INTEGER :: NARGS

      NERROR = NERROR + 1
      WRITE (KW,  &
                "(//' Error in case',I5,'.  The operation',' being tested was: ',A)"  &
                ) NCASE,OPERATION
      WRITE (KLOG,  &
                "(//' Error in case',I5,'.  The operation',' being tested was: ',A)"  &
                ) NCASE,OPERATION

!                 Temporarily change KW to KLOG so FMPRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      IF (NARGS >= 1) THEN
                WRITE (KLOG,"(1X,A,' =')") NAME1
                CALL FMPRINT_INTERVAL(M1)
      ENDIF
      IF (NARGS >= 2) THEN
                WRITE (KLOG,"(1X,A,' =')") NAME2
                CALL FMPRINT_INTERVAL(M2)
      ENDIF
      IF (NARGS >= 3) THEN
                WRITE (KLOG,"(1X,A,' =')") NAME3
                CALL FMPRINT_INTERVAL(M3)
      ENDIF
      WRITE (KLOG,"(1X,A,' =')") 'Result'
      CALL FMPRINT_INTERVAL(RESULT)
      WRITE (KLOG,"(1X,A)") 'Should be'
      CALL FMPRINT_INTERVAL(CORRECT)
      KW = KWSAVE
      RETURN
      END SUBROUTINE ERRPRTFM

   END MODULE TEST_VARS_IA

      PROGRAM TEST
      USE TEST_VARS_IA
      IMPLICIT NONE

!             Write output to the standard FM output (unit KW, defined in subroutine FMSET),
!             and also to the file TestFMinterval.out.

      KLOG = 18
      OPEN (KLOG,FILE='TestFMinterval.out')
      KWSAVE = KW
      KW = KLOG

!             Set precision to give at least 25 significant digits and initialize the FM package.
!             This call also checks many of the initialization values used in module FMVALS
!             (file FMSAVE.f95).  Set KW = KLOG for this call so that any messages concerning these
!             values will appear in file TestFMinterval.OUT.

      CALL FM_SET(25)
      CALL FM_SETVAR(' JFORM2 = 35 ')
      KW = KWSAVE

      CALL CPU_TIME(TIME1)

!             NERROR is the number of errors found.

      NERROR = 0
      RSMALL = EPSILON(1.0)*100.0
      DSMALL = EPSILON(1.0D0)*100.0

!             Test input conversion to intervals.

      CALL TEST1

!             Test add, subtract, multiply, divide and power.

      CALL TEST2
      CALL TEST3

!             Test conversion to and from the interval type.

      CALL TEST4

!             Test = assignment.

      CALL TEST5
      CALL TEST6
      CALL TEST7
      CALL TEST8

!             Test == comparison.

      CALL TEST9

!             Test /= comparison.

      CALL TEST10

!             Test <= comparison.

      CALL TEST11

!             Test < comparison.

      CALL TEST12

!             Test >= comparison.

      CALL TEST13

!             Test > comparison.

      CALL TEST14

!             Test array addition.

      CALL TEST15
      CALL TEST16
      CALL TEST17
      CALL TEST18

!             Test array subtraction.

      CALL TEST19
      CALL TEST20
      CALL TEST21
      CALL TEST22

!             Test array multiplication.

      CALL TEST23
      CALL TEST24
      CALL TEST25
      CALL TEST26

!             Test array division.

      CALL TEST27
      CALL TEST28
      CALL TEST29
      CALL TEST30

!             Test functions abs, acos, ..., atan2

      CALL TEST31

!             Test functions ceiling, cos, ..., dot_product

      CALL TEST32

!             Test functions epsilon, exp, ...,  matmul

      CALL TEST33

!             Test functions max, maxval, ..., modulo

      CALL TEST34

!             Test functions nearest, product, ..., sin

      CALL TEST35

!             Test functions sinh, sqrt, ... tanh

      CALL TEST36

!             Test functions beta, binomial, ..., fresnel_s

      CALL TEST37

!             Test functions erf, erfc, ... sinh_integral

      CALL TEST38

!             End of tests.

      CALL CPU_TIME(TIME2)

      IF (NERROR == 0) THEN
          WRITE (KW, "(///1X,I5,' cases tested.  No errors were found.'/)" ) NCASE
          WRITE (KLOG, "(///1X,I5,' cases tested.  No errors were found.'/)" ) NCASE
      ELSE IF (NERROR == 1) THEN
          WRITE (KW, "(///1X,I5,' cases tested.  1 error was found.'/)" ) NCASE
          WRITE (KLOG, "(///1X,I5,' cases tested.  1 error was found.'/)" ) NCASE
      ELSE
          WRITE (KW, "(///1X,I5,' cases tested.',I4,' errors were found.'/)" ) NCASE,NERROR
          WRITE (KLOG, "(///1X,I5,' cases tested.',I4,' errors were found.'/)" ) NCASE,NERROR
      ENDIF

      IF (NERROR >= 1) THEN
          KWSAVE = KW
          KW = KLOG

!             Write some of the variables in module FMVALS.

          CALL FMVARS
          KW = KWSAVE
      ENDIF

      WRITE (KW,*) ' '
      WRITE (KW,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMinterval.'
      WRITE (KW,*) ' '
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMinterval.'
      WRITE (KLOG,*) ' '

      WRITE (KW,*)' End of run.'

      STOP
      END PROGRAM TEST
