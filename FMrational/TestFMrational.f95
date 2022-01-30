
!  This is a test program for the FM 1.4 multiple-precision rational arithmetic package.

!  All of the rational arithmetic routines are tested.

!  If all tests are completed successfully, this line is printed:

!  499 cases tested.  No errors were found.

   MODULE SUM_R

   INTERFACE SUM_RAT
      MODULE PROCEDURE SUM0
      MODULE PROCEDURE SUM1
      MODULE PROCEDURE SUM2
   END INTERFACE

   CONTAINS

      FUNCTION SUM0(A,B)     RESULT (RETURN_VALUE)

!  Function that returns a rational result.

      USE FM_RATIONAL_ARITHMETIC
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: A,B,RETURN_VALUE
      RETURN_VALUE = A + B
      END FUNCTION SUM0

      FUNCTION SUM1(A,B)     RESULT (RETURN_VALUE)

!  Function that returns a rational vector result.

      USE FM_RATIONAL_ARITHMETIC
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: A(3),B(3),RETURN_VALUE(3)
      INTEGER :: J
      DO J = 1, 3
         RETURN_VALUE(J) = A(J) + B(J)
      ENDDO
      END FUNCTION SUM1

      FUNCTION SUM2(A,B)     RESULT (RETURN_VALUE)

!  Function that returns a rational matrix result.

      USE FM_RATIONAL_ARITHMETIC
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: A(3,3),B(3,3),RETURN_VALUE(3,3)
      INTEGER :: J, K
      DO J = 1, 3
         DO K = 1, 3
            RETURN_VALUE(J,K) = A(J,K) + B(J,K)
         ENDDO
      ENDDO
      END FUNCTION SUM2

   END MODULE SUM_R

   MODULE TEST_RATIONAL

      USE FMVALS
      USE FMZM
      USE FM_RATIONAL_ARITHMETIC

      TYPE (FM_RATIONAL), SAVE ::  A, B, C, D, RESULT, CORRECT,                   &
                          AVEC(3),  BVEC(3),  CVEC(3),  DVEC(3),                  &
                          AMAT(3,3), BMAT(3,3), CMAT(3,3), DMAT(3,3)

!             Declare the derived type variables of type (FM), (IM).
!             These are in the form that would be found in a user program.

      TYPE (FM), SAVE :: MFM1, MFM2, MFMVEC(3), MFMMAT(3,3)
      TYPE (IM), SAVE :: MIM1, MIM2, MIM3, MIMVEC(3), MIMMAT(3,3), AMAT_IM(3,6)

!             These are the variables that are not multiple precision.

      INTEGER, SAVE :: J1, JV(3), JV2(3,3)
      REAL, SAVE :: R1, RSMALL, RV(3), RV2(3,3)
      DOUBLE PRECISION, SAVE :: D1, DSMALL, DV(3), DV2(3,3)
      COMPLEX, SAVE :: C1, CV(3), CV2(3,3)
      COMPLEX (KIND(0.0D0)), SAVE :: CD1, CDV(3), CDV2(3,3)

      CHARACTER(100), SAVE :: ST1, ST2, STV2(3,3)
      INTEGER, SAVE :: KLOG, KWSAVE, ML(2), NCASE, NERROR
      REAL, SAVE :: TIME1, TIME2
      LOGICAL, EXTERNAL :: IMCOMPARE

      CONTAINS

      SUBROUTINE TEST1
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing input and output conversion for rationals.')")
      KWSAVE = KW

!             NCASE is the number of cases tested.

      NCASE = 1
      RESULT = TO_FM_RATIONAL( 2, 3 )
      CORRECT = 0
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( 2, 3 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM1 = 2
      CALL IMEQ(MIM1%MIM,CORRECT%NUMERATOR)
      MIM1 = 3
      CALL IMEQ(MIM1%MIM,CORRECT%DENOMINATOR)

!             Use the .NOT. because FMCOMPARE returns FALSE for special cases like MD = UNKNOWN,
!             and these should be treated as errors for these tests.

      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 2
      RESULT = TO_FM_RATIONAL( -2, 3 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( -2, 3 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM1 = -2
      CALL IMEQ(MIM1%MIM,CORRECT%NUMERATOR)
      MIM1 = 3
      CALL IMEQ(MIM1%MIM,CORRECT%DENOMINATOR)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 3
      RESULT = TO_FM_RATIONAL( 2, -3 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( 2, -3 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM1 = -2
      CALL IMEQ(MIM1%MIM,CORRECT%NUMERATOR)
      MIM1 = 3
      CALL IMEQ(MIM1%MIM,CORRECT%DENOMINATOR)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 4
      RESULT = TO_FM_RATIONAL( -2, -3 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( -2, -3 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM1 = 2
      CALL IMEQ(MIM1%MIM,CORRECT%NUMERATOR)
      MIM1 = 3
      CALL IMEQ(MIM1%MIM,CORRECT%DENOMINATOR)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 5
      RESULT = TO_FM_RATIONAL( 12, 36 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( 12, 36 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM1 = 1
      CALL IMEQ(MIM1%MIM,CORRECT%NUMERATOR)
      MIM1 = 3
      CALL IMEQ(MIM1%MIM,CORRECT%DENOMINATOR)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 6
      RESULT = TO_FM_RATIONAL( 84, 36 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) ' TO_FM_RATIONAL( 84, 36 ) '
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( 7, 3 )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 7
      RESULT = TO_FM_RATIONAL( TO_IM('3141592653589776'), TO_IM('271828182829') )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " TO_FM_RATIONAL( TO_IM('3141592653589776'), TO_IM('271828182829') ) "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' TO_FM_RATIONAL')
      ENDIF

      NCASE = 8
      A = TO_FM_RATIONAL( TO_IM('3141592653589776'), TO_IM('271828182829') )
      RESULT = A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 9
      A = TO_FM_RATIONAL( '3141592653589776' ) / TO_IM('271828182829')
      RESULT = A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A = TO_FM_RATIONAL( '3141592653589776' ) / TO_IM('271828182829') "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 10
      A = TO_FM_RATIONAL( '3141592653589776 / 271828182829' )
      RESULT = A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A = TO_FM_RATIONAL( '3141592653589776 / 271828182829' ) "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 11
      A = TO_FM_RATIONAL( '3141592653589776', '271828182829' )
      RESULT = A
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A = TO_FM_RATIONAL( '3141592653589776', '271828182829' ) "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 12
      RESULT = 314159
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = 314159 "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('314159'), TO_IM('1') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 13
      RESULT = -314159
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = -314159 "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-314159'), TO_IM('1') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 14
      RESULT = TO_IM('3141592653589793')
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_IM('3141592653589793') "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('3141592653589793'), TO_IM('1') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 15
      RESULT = TO_IM('-3141592653589793')
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_IM('-3141592653589793') "
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-3141592653589793'), TO_IM('1') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 16
      RESULT = TO_FM_RATIONAL( 31, 47 )
      MIM1 = RATIONAL_NUMERATOR( RESULT )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIM1 = RATIONAL_NUMERATOR( RESULT ) "
      KW = KLOG
      CALL IM_PRINT(MIM1)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM2 = 31
      IF ( (.NOT. IMCOMPARE(MIM1%MIM,'==',MIM2%MIM)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 17
      RESULT = TO_FM_RATIONAL( 31, 47 )
      MIM1 = RATIONAL_DENOMINATOR( TO_FM_RATIONAL( 31, 47 ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MIM1 = RATIONAL_DENOMINATOR( TO_FM_RATIONAL( 31, 47 ) ) "
      KW = KLOG
      CALL IM_PRINT(MIM1)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MIM2 = 47
      IF ( (.NOT. IMCOMPARE(MIM1%MIM,'==',MIM2%MIM)) ) THEN
          CALL ERRPRTRM(' = assignment')
      ENDIF

      NCASE = 18
      AVEC = 31
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = 31"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( 31 )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 19
      AVEC = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 20
      AVEC = TO_IM('101341698502896')
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = TO_IM('101341698502896')"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_IM('101341698502896')
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 21
      JV(1:3) = (/ 31, -41, 59 /)
      AVEC = (/ 31, -41, 59 /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = (/ 31, -41, 59 /)"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( JV(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 22
      JV(1:3) = (/ 31, -41, 59 /)
      AVEC = TO_FM_RATIONAL( (/ 31, -41, 59 /) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = TO_FM_RATIONAL( (/ 31, -41, 59 /) )"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( JV(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 23
      JV(1:3) = (/ 31, -41, 59 /)
      AVEC = JV
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = JV(1:3)"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( JV(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 24
      MIMVEC(1:3) = (/ 31, -41, 59 /)
      AVEC = TO_FM_RATIONAL( TO_IM( (/ 31, -41, 59 /) ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = TO_IM( (/ 31, -41, 59 /) )"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( MIMVEC(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 25
      MIMVEC(1:3) = (/ 31, -41, 59 /)
      AVEC = TO_IM( (/ 31, -41, 59 /) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = TO_IM( (/ 31, -41, 59 /) )"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( MIMVEC(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 26
      MIMVEC(1:3) = (/ 31, -41, 59 /)
      AVEC = MIMVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = MIMVEC(1:3)"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = TO_FM_RATIONAL( MIMVEC(J) )
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 27
      BVEC(1:3) = (/ TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') ),  &
                     TO_FM_RATIONAL( -41, 43 ),                                        &
                     TO_FM_RATIONAL( 314, 159 )   /)
      AVEC = (/ TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') ),  &
                     TO_FM_RATIONAL( -41, 43 ),                                   &
                     TO_FM_RATIONAL( 314, 159 )   /)
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = (/ TO_FM_RATIONAL( TO_IM('101341698502896'), ..."
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = BVEC(J)
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 28
      BVEC(1:3) = (/ TO_FM_RATIONAL( TO_IM('101341698502896'), TO_IM('8768651059') ),  &
                     TO_FM_RATIONAL( -41, 43 ),                                        &
                     TO_FM_RATIONAL( 314, 159 )   /)
      AVEC = BVEC
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AVEC = BVEC"
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " AVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(AVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = BVEC(J)
         IF ( (.NOT. IMCOMPARE(AVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(AVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' = assignment')
         ENDIF
      ENDDO

      NCASE = 29
      JV2 = 314
      AMAT = 314
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = 314"
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = TO_FM_RATIONAL( JV2(J,K) )
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 30
      JV2(1,1:3) = (/ 1, 2, 3 /)
      JV2(2,1:3) = (/ 4, 5, 6 /)
      JV2(3,1:3) = (/ 7, 8, 9 /)
      AMAT = RESHAPE(  (/ 1, 4, 7,    &
                          2, 5, 8,    &
                          3, 6, 9 /)  &
           , (/ 3,3 /)  )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = RESHAPE(  (/ 1, 4, 7, ..."
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = TO_FM_RATIONAL( JV2(J,K) )
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 31
      MIMMAT = 159
      AMAT = TO_IM( 159 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = TO_IM( 159 )"
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = TO_FM_RATIONAL( MIMMAT(J,K) )
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 32
      MIMMAT(1,1:3) = (/ 1, 2, 3 /)
      MIMMAT(2,1:3) = (/ 4, 5, 6 /)
      MIMMAT(3,1:3) = (/ 7, 8, 9 /)
      AMAT = TO_IM(  RESHAPE(  (/ 1, 4, 7,    &
                                  2, 5, 8,    &
                                  3, 6, 9 /)  &
                   , (/ 3,3 /)  )             &
                  )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = TO_IM(  RESHAPE(  (/ 1, 4, 7, ..."
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = TO_FM_RATIONAL( MIMMAT(J,K) )
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 33
      MIMMAT(1,1:3) = (/ 1, 2, 3 /)
      MIMMAT(2,1:3) = (/ 4, 5, 6 /)
      MIMMAT(3,1:3) = (/ 7, 8, 9 /)
      AMAT = TO_FM_RATIONAL( TO_IM(  RESHAPE(  (/ 1, 4, 7,    &
                                                  2, 5, 8,    &
                                                  3, 6, 9 /)  &
                                   , (/ 3,3 /)  )             &
                         ) )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = TO_IM(  RESHAPE(  (/ 1, 4, 7, ..."
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = TO_FM_RATIONAL( MIMMAT(J,K) )
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 34
      A = TO_FM_RATIONAL( -314, 159 )
      AMAT = TO_FM_RATIONAL( -314, 159 )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = TO_FM_RATIONAL( -314, 159 )"
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = A
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 35
      BMAT(1,1:3) = (/ 1, 2, 3 /)
      BMAT(2,1:3) = (/ 4, 5, 6 /)
      BMAT(3,1:3) = (/ 7, 8, 9 /)
      AMAT = TO_FM_RATIONAL(  RESHAPE(  (/ 1, 4, 7,    &
                                           2, 5, 8,    &
                                           3, 6, 9 /)  &
                            , (/ 3,3 /)  )             &
                           )
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " AMAT = TO_FM_RATIONAL(  RESHAPE(  (/ 1, 4, 7, ..."
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") " AMAT(",J,",",K,") = "
            CALL FM_PRINT_RATIONAL(AMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            CORRECT = BMAT(J,K)
            IF ( (.NOT. IMCOMPARE(AMAT(J,K)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(AMAT(J,K)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' = assignment')
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END SUBROUTINE TEST1



      SUBROUTINE TEST2
      IMPLICIT NONE
      INTEGER :: K

      WRITE (KW,"(/' Testing addition of rationals.')")



      NCASE = 36
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = +TO_FM_RATIONAL(7,9) "
      RESULT = +TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('7'), TO_IM('9') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 37
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + B   (= TO_FM_RATIONAL(5,6) + TO_FM_RATIONAL(7,9) ) "
      A = TO_FM_RATIONAL(5,6)
      B = TO_FM_RATIONAL(7,9)
      RESULT = A + B
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('29'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 38
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(5,6) + TO_FM_RATIONAL(7,9) "
      RESULT = TO_FM_RATIONAL(5,6) + TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('29'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 39
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-5,6) + TO_FM_RATIONAL(7,9) "
      RESULT = TO_FM_RATIONAL(-5,6) + TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-1'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 40
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'), "
      WRITE (KLOG,*) "                         TO_IM('289333333333333333213632'))       + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'), "
      WRITE (KLOG,*) "                         TO_IM('3719999999999999999632464')) "
      RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'),          &
                              TO_IM('289333333333333333213632'))         +     &
               TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'),          &
                              TO_IM('3719999999999999999632464'))
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('49176954732510288060077777439647119341563786034605'),  &
                               TO_IM('241111111111111110987538222222222222232077632'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 41
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + K  ( = TO_FM_RATIONAL(51234,62345) + 3141) "
      A = TO_FM_RATIONAL(51234,62345)
      K = 3141
      RESULT = A + K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('195876879'),  &
                               TO_IM('62345'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 42
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) + 41 "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = A + K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('94244274435124542380243386095439'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 43
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) + 41 "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = A + K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('88299205905277231882875135290563'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 44
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) - 41 "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = -41
      RESULT = A + K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-94244274435124542380243386095439'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 45
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K + A  ( = 3141 + TO_FM_RATIONAL(51234,62345) ) "
      A = TO_FM_RATIONAL(51234,62345)
      K = 3141
      RESULT = K + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('195876879'),  &
                               TO_IM('62345'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 46
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K + A  ( = 41 + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = K + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('94244274435124542380243386095439'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 47
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K + A  ( = 41 + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = K + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('88299205905277231882875135290563'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 48
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K + A  ( = -41 + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = -41
      RESULT = K + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-94244274435124542380243386095439'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 49
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 + A  ( = 314159 + TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314159
      RESULT = MIM1 + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2586970988474'),  &
                               TO_IM('8234567'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 50
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 + A  ( = TO_IM('265129767915894430221715901488988') + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 51
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 + A  ( = TO_IM('265129767915894430221715901488988') + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 52
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 + A  ( = TO_IM('-265129767915894430221715901488988') + "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = MIM1 + A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 53
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + MIM1  ( = TO_FM_RATIONAL(7654321,8234567) + 314159 ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314159
      RESULT = A + MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2586970988474'),  &
                               TO_IM('8234567'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 54
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) +  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "

      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A + MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 55
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) +  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A + MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF

      NCASE = 56
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A + MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) +  "
      WRITE (KLOG,*) "          TO_IM('-265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = A + MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' addition of rationals')
      ENDIF







      WRITE (KW,"(/' Testing subtraction of rationals.')")



      NCASE = 57
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = -TO_FM_RATIONAL(7,9) "
      RESULT = -TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-7'), TO_IM('9') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 58
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - B   (= TO_FM_RATIONAL(5,6) - TO_FM_RATIONAL(7,9) ) "
      A = TO_FM_RATIONAL(5,6)
      B = TO_FM_RATIONAL(7,9)
      RESULT = A - B
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('1'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 59
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(5,6) - TO_FM_RATIONAL(7,9) "
      RESULT = TO_FM_RATIONAL(5,6) - TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('1'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 60
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-5,6) - TO_FM_RATIONAL(7,9) "
      RESULT = TO_FM_RATIONAL(-5,6) - TO_FM_RATIONAL(7,9)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-29'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 61
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'), "
      WRITE (KLOG,*) "                         TO_IM('289333333333333333213632'))       - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'), "
      WRITE (KLOG,*) "                         TO_IM('3719999999999999999632464')) "
      RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'),          &
                              TO_IM('289333333333333333213632'))         -     &
               TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'),          &
                              TO_IM('3719999999999999999632464'))
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('14471879286694101507788888761290466392318244185047'),  &
                               TO_IM('80370370370370370329179407407407407410692544'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 62
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - K  ( = TO_FM_RATIONAL(51234,62345) - 3141) "
      A = TO_FM_RATIONAL(51234,62345)
      K = 3141
      RESULT = A - K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-195774411'),  &
                               TO_IM('62345'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 63
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) - 41 "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = A - K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-88299205905277231882875135290563'),  &
                               TO_IM(' 2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 64
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) - 41 "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = A - K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-94244274435124542380243386095439'),  &
                               TO_IM(' 2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 65
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683')) - (-41) ) "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -41
      RESULT = A - K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('88299205905277231882875135290563'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 66
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K - A  ( = 3141 - TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      K = 3141
      RESULT = K - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('25857120626'),  &
                               TO_IM('8234567'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 67
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K - A  ( = 41 - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 41
      RESULT = K - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('88299205905277231882875135290563'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 68
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K - A  ( = 41 - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = 41
      RESULT = K - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('94244274435124542380243386095439'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 69
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K - A  ( = -41 - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -41
      RESULT = K - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-88299205905277231882875135290563'),  &
                               TO_IM(' 2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 70
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 - A  ( = 314159 - TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314159
      RESULT = MIM1 - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2586955679832'),  &
                               TO_IM('8234567'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 71
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 - A  ( = TO_IM('265129767915894430221715901488988') - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 72
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 - A  ( = TO_IM('265129767915894430221715901488988') - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 73
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 - A  ( = TO_IM('-265129767915894430221715901488988') - "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = MIM1 - A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 74
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - MIM1  ( = TO_FM_RATIONAL(7654321,8234567) - 314159 ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314159
      RESULT = A - MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-2586955679832'),  &
                               TO_IM(' 8234567'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 75
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) -  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "

      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A - MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 76
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) -  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A - MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-77663033875116511578492782637666916261000649139603399829917049036'),  &
                               TO_IM(' 292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      NCASE = 77
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A - MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) -  "
      WRITE (KLOG,*) "          TO_IM('-265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = A - MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('77663033875116511578492782637666507858593072936981330181692256028'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' subtraction of rationals')
      ENDIF

      RETURN
      END SUBROUTINE TEST2



      SUBROUTINE TEST3
      IMPLICIT NONE
      INTEGER :: K


      WRITE (KW,"(/' Testing multiplication of rationals.')")

      NCASE = 78
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * B   (= TO_FM_RATIONAL(5,6) * TO_FM_RATIONAL(7,9) ) "
      A = TO_FM_RATIONAL(5,6)
      B = TO_FM_RATIONAL(7,9)
      RESULT = A * B
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('35'), TO_IM('54') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 79
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(5,6) * TO_FM_RATIONAL(15,13) "
      RESULT = TO_FM_RATIONAL(5,6) * TO_FM_RATIONAL(15,13)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('25'), TO_IM('26') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 80
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-5,6) * TO_FM_RATIONAL(15,13) "
      RESULT = TO_FM_RATIONAL(-5,6) * TO_FM_RATIONAL(15,13)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-25'), TO_IM('26') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 81
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'), "
      WRITE (KLOG,*) "                         TO_IM('289333333333333333213632'))       * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'), "
      WRITE (KLOG,*) "                         TO_IM('3719999999999999999632464')) "
      RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'),          &
                              TO_IM('289333333333333333213632'))         *     &
               TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'),          &
                              TO_IM('3719999999999999999632464'))
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2469135802469135802469135802386419753086419753086419604047'),  &
                               TO_IM('1076319999999999999448370624000000000043994549248'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 82
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * K  ( = TO_FM_RATIONAL(51234,62345) * 3145) "
      A = TO_FM_RATIONAL(51234,62345)
      K = 3145
      RESULT = A * K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('870978'),  &
                               TO_IM('337'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 83
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) * 210 "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = A * K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('624232195633967602223666334511980'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 84
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) * 210 "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = A * K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-624232195633967602223666334511980'),  &
                               TO_IM(' 2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 85
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683')) * (-210) ) "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -210
      RESULT = A * K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('624232195633967602223666334511980'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 86
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K * A  ( = 3146 * TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      K = 3146
      RESULT = K * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2189135806'),  &
                               TO_IM('748597'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 87
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K * A  ( = 210 * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = K * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('624232195633967602223666334511980'),  &
                               TO_IM('2226140004151241149550225870561'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 88
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K * A  ( = 270191 * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = 270191
      RESULT = K * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-2972534264923655248684125402438'),  &
                               TO_IM('8239134553524140883857071'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 89
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K * A  ( = -13 * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -13
      RESULT = K * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2972534264923655248684125402438'),  &
                               TO_IM('171241538780864703811555836197'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 90
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 * A  ( = 314160 * TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314160
      RESULT = MIM1 * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('218607407760'),  &
                               TO_IM('748597'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 91
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 * A  ( = TO_IM('265129767915894037158473471010136') * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894037158473471010136')
      RESULT = MIM1 * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('129265653161071213310929946006912920883168063808'),  &
                               TO_IM('699394504889023'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 92
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 * A  ( = TO_IM('265129767915894430221715901488988') * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-54139817768485563161148990841175291201523319409532726682645697952'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 93
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 * A  ( = TO_IM('-265129767915894037158473471010136') * "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894037158473471010136')
      RESULT = MIM1 * A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('129265653161071213310929946006912920883168063808'),  &
                               TO_IM('699394504889023'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 94
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * MIM1  ( = TO_FM_RATIONAL(7654321,8234567) * 314160 ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314160
      RESULT = A * MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('218607407760'),  &
                               TO_IM('748597'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 95
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) *  "
      WRITE (KLOG,*) "          TO_IM('265129767915894037158473471010136')  "

      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894037158473471010136')
      RESULT = A * MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('129265653161071213310929946006912920883168063808'),  &
                               TO_IM('699394504889023'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 96
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) *  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A * MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-54139817768485563161148990841175291201523319409532726682645697952'),  &
                               TO_IM(' 292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF

      NCASE = 97
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A * MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) *  "
      WRITE (KLOG,*) "          TO_IM('-265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = A * MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('54139817768485563161148990841175291201523319409532726682645697952'),  &
                               TO_IM('292924610033804664316183459326539'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' multiplication of rationals')
      ENDIF







      WRITE (KW,"(/' Testing division of rationals.')")

      NCASE = 98
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / B   (= TO_FM_RATIONAL(5,6) / TO_FM_RATIONAL(7,9) ) "
      A = TO_FM_RATIONAL(5,6)
      B = TO_FM_RATIONAL(7,9)
      RESULT = A / B
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('15'), TO_IM('14') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 99
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(5,6) / TO_FM_RATIONAL(15,13) "
      RESULT = TO_FM_RATIONAL(5,6) / TO_FM_RATIONAL(15,13)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('13'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 100
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-5,6) / TO_FM_RATIONAL(15,13) "
      RESULT = TO_FM_RATIONAL(-5,6) / TO_FM_RATIONAL(15,13)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL( TO_IM('-13'), TO_IM('18') )
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 101
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'), "
      WRITE (KLOG,*) "                         TO_IM('289333333333333333213632'))       / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'), "
      WRITE (KLOG,*) "                         TO_IM('3719999999999999999632464')) "
      RESULT = TO_FM_RATIONAL(TO_IM('55555555555555555555555555123'),          &
                              TO_IM('289333333333333333213632'))         /     &
               TO_FM_RATIONAL(TO_IM('44444444444444444444444444789'),          &
                              TO_IM('3719999999999999999632464'))
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('46296296296296296291722221861759259259259259294873'),  &
                               TO_IM('2880658436213991768355555577887860082304526739732'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 102
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / K  ( = TO_FM_RATIONAL(51234,62345) / 3144) "
      A = TO_FM_RATIONAL(51234,62345)
      K = 3144
      RESULT = A / K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('8539'),  &
                               TO_IM('32668780'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 103
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) / 210 "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = A / K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('70774625355325124968669652439'),  &
                               TO_IM('11130700020756205747751129352805'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 104
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) / 210 "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = A / K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-70774625355325124968669652439'),  &
                               TO_IM(' 11130700020756205747751129352805'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 105
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / K  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683')) / (-210) ) "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -210
      RESULT = A / K
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('70774625355325124968669652439'),  &
                               TO_IM('11130700020756205747751129352805'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 106
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K / A  ( = 3154 / TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      K = 3154
      RESULT = K / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('1366938122'),  &
                               TO_IM('402859'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 107
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K / A  ( = 210 / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683')) "
      A = TO_FM_RATIONAL(TO_IM('8917602794770965746052376207314'),  &
                         TO_IM('6678420012453723448650677611683'))
      K = 210
      RESULT = K / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('11130700020756205747751129352805'),  &
                               TO_IM('70774625355325124968669652439'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 108
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K / A  ( = 13712269 / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM(' 6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = 13712269
      RESULT = K / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-2226140004151241149550225870561'),  &
                               TO_IM(' 216779167978957767579102'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 109
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = K / A  ( = -7499 / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'), "
      WRITE (KLOG,*) "                         TO_IM('6678420012453723448650677611683'))  "
      A = TO_FM_RATIONAL(TO_IM('-8917602794770965746052376207314'),  &
                         TO_IM(' 6678420012453723448650677611683'))
      K = -7499
      RESULT = K / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2226140004151241149550225870561'),  &
                               TO_IM('396390754090366081968812562'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 110
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 / A  ( = 314165 / TO_FM_RATIONAL(7654321,8234567) ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314165
      RESULT = MIM1 / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('136158565345'),  &
                               TO_IM('402859'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 111
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 / A  ( = TO_IM('265129767915894430221715118517693') / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189524'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189524'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715118517693')
      RESULT = MIM1 / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('5974641810208868168278956128472612919586437971124324359'),  &
                               TO_IM('15709263326091525210236'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 112
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 / A  ( = TO_IM('265129767915894430221715901488988') / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = MIM1 / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-19415758468779127894623195659416678014949215259573091251451163133'),  &
                               TO_IM(' 51050300947025327758706028099126'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 113
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIM1 / A  ( = TO_IM('-265129767915894037158473471010136') / "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894037158473471010136')
      RESULT = MIM1 / A
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('9707879234389549555074471883353238914887523037214474161325349913'),  &
                               TO_IM('25525150473512663879353014049563'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 114
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / MIM1  ( = TO_FM_RATIONAL(7654321,8234567) / 314165 ) "
      A = TO_FM_RATIONAL(7654321,8234567)
      MIM1 = 314165
      RESULT = A / MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('402859'),  &
                               TO_IM('136158565345'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 115
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) /  "
      WRITE (KLOG,*) "          TO_IM('265129767915894037158473471010136')  "

      A = TO_FM_RATIONAL(TO_IM('612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894037158473471010136')
      RESULT = A / MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('25525150473512663879353014049563'),  &
                               TO_IM('9707879234389549555074471883353238914887523037214474161325349913'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 116
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) /  "
      WRITE (KLOG,*) "          TO_IM('265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('265129767915894430221715901488988')
      RESULT = A / MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-51050300947025327758706028099126'),  &
                               TO_IM(' 19415758468779127894623195659416678014949215259573091251451163133'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      NCASE = 117
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = A / MIM1  ( =  "
      WRITE (KLOG,*) "          TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'), "
      WRITE (KLOG,*) "                         TO_IM('878773830101413992948550377979617')) /  "
      WRITE (KLOG,*) "          TO_IM('-265129767915894430221715901488988')  "
      A = TO_FM_RATIONAL(TO_IM('-612603611364303933104472337189512'),  &
                         TO_IM('878773830101413992948550377979617'))
      MIM1 = TO_IM('-265129767915894430221715901488988')
      RESULT = A / MIM1
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('51050300947025327758706028099126'),  &
                               TO_IM('19415758468779127894623195659416678014949215259573091251451163133'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' division of rationals')
      ENDIF

      END SUBROUTINE TEST3



      SUBROUTINE TEST4
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing rational addition array operations.')")
      NCASE = 118
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC + BVEC "
      AVEC = (/ TO_FM_RATIONAL( 31, 97 ), TO_FM_RATIONAL( 37, 97 ), TO_FM_RATIONAL( 41, 97 ) /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = AVEC + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 5456, 7663 ), TO_FM_RATIONAL( 6512, 7663 ),  &
                TO_FM_RATIONAL( 7216, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 119
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A + BVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = A + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 5456, 7663 ), TO_FM_RATIONAL( 6038, 7663 ),  &
                TO_FM_RATIONAL( 6426, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 120
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC + B "
      AVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      B = TO_FM_RATIONAL( 31, 97 )
      CVEC = AVEC + B
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 5456, 7663 ), TO_FM_RATIONAL( 6038, 7663 ),  &
                TO_FM_RATIONAL( 6426, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 121
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV + BVEC "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = JV + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 122
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = (/ 31, 37, 41 /) + BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = (/ 31, 37, 41 /) + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 123
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + JV "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 124
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + (/ 31, 37, 41 /) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + (/ 31, 37, 41 /)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 125
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + 31 "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2486, 79 ),  &
                TO_FM_RATIONAL( 2490, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 126
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = 31 + BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = 31 + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2486, 79 ),  &
                TO_FM_RATIONAL( 2490, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 127
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A + JV "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = A + JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 3038, 97 ), TO_FM_RATIONAL( 3620, 97 ),  &
                TO_FM_RATIONAL( 4008, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 128
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV + A "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = JV + A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 3038, 97 ), TO_FM_RATIONAL( 3620, 97 ),  &
                TO_FM_RATIONAL( 4008, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 129
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC + BVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = MIMVEC + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 130
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM( (/ 31, 37, 41 /) ) + BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM( (/ 31, 37, 41 /) ) + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 131
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + MIMVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 132
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + TO_IM( (/ 31, 37, 41 /) ) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + TO_IM( (/ 31, 37, 41 /) )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2960, 79 ),  &
                TO_FM_RATIONAL( 3280, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 133
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC + TO_IM(31) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC + TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2486, 79 ),  &
                TO_FM_RATIONAL( 2490, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 134
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM(31) + BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM(31) + BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2480, 79 ), TO_FM_RATIONAL( 2486, 79 ),  &
                TO_FM_RATIONAL( 2490, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 135
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A + MIMVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = A + MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 3038, 97 ), TO_FM_RATIONAL( 3620, 97 ),  &
                TO_FM_RATIONAL( 4008, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 136
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC + A "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = MIMVEC + A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 3038, 97 ), TO_FM_RATIONAL( 3620, 97 ),  &
                TO_FM_RATIONAL( 4008, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 137
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + BMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
            BMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT + BMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 273*J+370*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 138
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + A "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = AMAT + A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 3007+79*J+79*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 139
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A + AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A + AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 3007+79*J+79*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 140
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT + JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 81*J+82*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 141
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 + AMAT "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = JV2 + AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 81*J+82*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 142
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 + A "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = JV2 + A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 143
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A + JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A + JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 144
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT + A "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = MIMMAT + A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 145
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A + MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A + MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 146
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + 31 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT + 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 147
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = 31 + AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = 31 + AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 148
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT + MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 81*J+82*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 149
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT + AMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = MIMMAT + AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 81*J+82*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 150
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT + TO_IM(31) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT + TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 151
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = TO_IM(31) + AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = TO_IM(31) + AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 152
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = +AVEC "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 2*J+31, 79 )
      ENDDO
      CVEC = +AVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DVEC(J) = TO_FM_RATIONAL( 2*J+31, 79 )
      ENDDO
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A,I1,A)") ' CVEC(',J,',',K,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- addition of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 153
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = +AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = +AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- addition of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      END SUBROUTINE TEST4



      SUBROUTINE TEST5
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing rational subtraction array operations.')")
      NCASE = 154
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC - BVEC "
      AVEC = (/ TO_FM_RATIONAL( 31, 97 ), TO_FM_RATIONAL( 37, 97 ), TO_FM_RATIONAL( 41, 97 ) /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = AVEC - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( -558, 7663 ), TO_FM_RATIONAL( -666, 7663 ),  &
                TO_FM_RATIONAL( -738, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 155
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A - BVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = A - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( -558, 7663 ), TO_FM_RATIONAL( -1140, 7663 ),  &
                TO_FM_RATIONAL( -1528, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 156
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC - B "
      AVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      B = TO_FM_RATIONAL( 31, 97 )
      CVEC = AVEC - B
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 558, 7663 ), TO_FM_RATIONAL( 1140, 7663 ),  &
                TO_FM_RATIONAL( 1528, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 157
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV - BVEC "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = JV - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 158
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = (/ 31, 37, 41 /) - BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = (/ 31, 37, 41 /) - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 159
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - JV "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                 TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 160
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - (/ 31, 37, 41 /) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - (/ 31, 37, 41 /)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                 TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 161
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - 31 "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( -2418, 79 ), TO_FM_RATIONAL( -2412, 79 ),  &
                TO_FM_RATIONAL( -2408, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 162
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = 31 - BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = 31 - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2412, 79 ),  &
                TO_FM_RATIONAL( 2408, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 163
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A - JV "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = A - JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( -2976, 97 ), TO_FM_RATIONAL( -3558, 97 ),  &
                TO_FM_RATIONAL( -3946, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 164
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV - A "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = JV - A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2976, 97 ), TO_FM_RATIONAL( 3558, 97 ),  &
                TO_FM_RATIONAL( 3946, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 165
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC - BVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = MIMVEC - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 166
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM( (/ 31, 37, 41 /) ) - BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM( (/ 31, 37, 41 /) ) - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 167
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - MIMVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                 TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 168
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - TO_IM( (/ 31, 37, 41 /) ) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - TO_IM( (/ 31, 37, 41 /) )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( 2418, 79 ), TO_FM_RATIONAL( 2886, 79 ),  &
                 TO_FM_RATIONAL( 3198, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 169
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC - TO_IM(31) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC - TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( -2418, 79 ), TO_FM_RATIONAL( -2412, 79 ),  &
                TO_FM_RATIONAL( -2408, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 170
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM(31) - BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM(31) - BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( -2418, 79 ), TO_FM_RATIONAL( -2412, 79 ),  &
                 TO_FM_RATIONAL( -2408, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 171
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A - MIMVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = A - MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = -(/ TO_FM_RATIONAL( 2976, 97 ), TO_FM_RATIONAL( 3558, 97 ),  &
                 TO_FM_RATIONAL( 3946, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 172
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC - A "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = MIMVEC - A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 2976, 97 ), TO_FM_RATIONAL( 3558, 97 ),  &
                TO_FM_RATIONAL( 3946, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 173
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - BMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
            BMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT - BMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( 115*J+212*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 174
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - A "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = AMAT - A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -3007+79*J+79*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 175
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A - AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A - AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( -3007+79*J+79*K, 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 176
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT - JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( 77*J+76*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 177
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 - AMAT "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = JV2 - AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 77*J+76*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 178
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 - A "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = JV2 - A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 179
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A - JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A - JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( -31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 180
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT - A "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = MIMMAT - A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 181
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A - MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A - MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( -31+79*J+79*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 182
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - 31 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT - 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 183
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = 31 - AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = 31 - AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( -2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 184
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT - MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( 77*J+76*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 185
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT - AMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = MIMMAT - AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 77*J+76*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 186
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT - TO_IM(31) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT - TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 187
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = TO_IM(31) - AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = TO_IM(31) - AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( -2449+2*J+3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 188
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = -AVEC "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 2*J+31, 79 )
      ENDDO
      CVEC = -AVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DVEC(J) = TO_FM_RATIONAL( -2*J-31, 79 )
      ENDDO
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A,I1,A)") ' CVEC(',J,',',K,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- subtraction of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 189
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = -AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = -AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( -2*J-3*K, 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- subtraction of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      END SUBROUTINE TEST5



      SUBROUTINE TEST6
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing rational multiplication array operations.')")
      NCASE = 190
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC * BVEC "
      AVEC = (/ TO_FM_RATIONAL( 31, 97 ), TO_FM_RATIONAL( 37, 97 ), TO_FM_RATIONAL( 41, 97 ) /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = AVEC * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 7663 ), TO_FM_RATIONAL( 1369, 7663 ),  &
                TO_FM_RATIONAL( 1681, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 191
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A * BVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = A * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 7663 ), TO_FM_RATIONAL( 1147, 7663 ),  &
                TO_FM_RATIONAL( 1271, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 192
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC * B "
      AVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      B = TO_FM_RATIONAL( 31, 97 )
      CVEC = AVEC * B
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 7663 ), TO_FM_RATIONAL( 1147, 7663 ),  &
                TO_FM_RATIONAL( 1271, 7663 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 193
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV * BVEC "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = JV * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 194
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = (/ 31, 37, 41 /) * BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = (/ 31, 37, 41 /) * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 195
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * JV "
      JV = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 196
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * (/ 31, 37, 41 /) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * (/ 31, 37, 41 /)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 197
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * 31 "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1147, 79 ),  &
                TO_FM_RATIONAL( 1271, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 198
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = 31 * BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = 31 * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1147, 79 ),  &
                TO_FM_RATIONAL( 1271, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 199
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A * JV "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = A * JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 97 ), TO_FM_RATIONAL( 1147, 97 ),  &
                TO_FM_RATIONAL( 1271, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 200
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV * A "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 37, 41 /)
      CVEC = JV * A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 97 ), TO_FM_RATIONAL( 1147, 97 ),  &
                TO_FM_RATIONAL( 1271, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 201
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC * BVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = MIMVEC * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 202
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM( (/ 31, 37, 41 /) ) * BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM( (/ 31, 37, 41 /) ) * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 203
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * MIMVEC "
      MIMVEC = (/ 31, 37, 41 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 204
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * TO_IM( (/ 31, 37, 41 /) ) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * TO_IM( (/ 31, 37, 41 /) )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1369, 79 ),  &
                TO_FM_RATIONAL( 1681, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 205
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC * TO_IM(31) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC * TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1147, 79 ),  &
                TO_FM_RATIONAL( 1271, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 206
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM(31) * BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM(31) * BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 79 ), TO_FM_RATIONAL( 1147, 79 ),  &
                TO_FM_RATIONAL( 1271, 79 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 207
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A * MIMVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = A * MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 97 ), TO_FM_RATIONAL( 1147, 97 ),  &
                TO_FM_RATIONAL( 1271, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 208
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC * A "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 37, 41 /)
      CVEC = MIMVEC * A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 961, 97 ), TO_FM_RATIONAL( 1147, 97 ),  &
                TO_FM_RATIONAL( 1271, 97 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- multiplication of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 209
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * BMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = -TO_FM_RATIONAL( J+K, 97 )
            BMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT * BMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( (J+K)*(2*J+3*K), 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 210
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * A "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = AMAT * A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 211
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A * AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A * AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 7663 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 212
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT * JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (J+K)*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 213
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 * AMAT "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = JV2 * AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (J+K)*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 214
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 * A "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = JV2 * A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 215
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A * JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A * JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 216
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT * A "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = MIMMAT * A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 217
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A * MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A * MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(J+K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 218
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * 31 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT * 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 219
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = 31 * AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = 31 * AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 220
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT * MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (J+K)*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 221
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT * AMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = MIMMAT * AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (J+K)*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 222
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT * TO_IM(31) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT * TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 223
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = TO_IM(31) * AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = TO_IM(31) * AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*(2*J+3*K), 79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- multiplication of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      END SUBROUTINE TEST6



      SUBROUTINE TEST7
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing rational division array operations.')")
      NCASE = 224
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC / BVEC "
      AVEC = (/ TO_FM_RATIONAL( 31, 97 ), TO_FM_RATIONAL( 31, 97 ), TO_FM_RATIONAL( 31, 97 ) /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = AVEC / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 97 ), TO_FM_RATIONAL( 2449, 3589 ),  &
                TO_FM_RATIONAL( 2449, 3977 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 225
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A / BVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = A / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 97 ), TO_FM_RATIONAL( 2449, 3589 ),  &
                TO_FM_RATIONAL( 2449, 3977 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 226
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = AVEC / B "
      AVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      B = TO_FM_RATIONAL( 31, 97 )
      CVEC = AVEC / B
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 97, 79 ), TO_FM_RATIONAL( 3589, 2449 ),  &
                TO_FM_RATIONAL( 3977, 2449 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 227
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV / BVEC "
      JV = (/ 31, 33, 35 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = JV / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2607, 37 ),  &
                TO_FM_RATIONAL( 2765, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 228
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = (/ 31, 33, 35 /) / BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = (/ 31, 33, 35 /) / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2607, 37 ),  &
                TO_FM_RATIONAL( 2765, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 229
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / JV "
      JV = (/ 31, 33, 35 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2607 ),  &
                TO_FM_RATIONAL( 41, 2765 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 230
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / (/ 31, 33, 35 /) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / (/ 31, 33, 35 /)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2607 ),  &
                TO_FM_RATIONAL( 41, 2765 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 231
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / 31 "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2449 ),  &
                TO_FM_RATIONAL( 41, 2449 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 232
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = 31 / BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = 31 / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2449, 37 ),  &
                TO_FM_RATIONAL( 2449, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 233
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A / JV "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 33, 35 /)
      CVEC = A / JV
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 97 ), TO_FM_RATIONAL( 31, 3201 ),  &
                TO_FM_RATIONAL( 31, 3395 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 234
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = JV / A "
      A = TO_FM_RATIONAL( 31, 97 )
      JV = (/ 31, 33, 35 /)
      CVEC = JV / A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 97, 1 ), TO_FM_RATIONAL( 3201, 31 ),  &
                TO_FM_RATIONAL( 3395, 31 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 235
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC / BVEC "
      MIMVEC = (/ 31, 33, 35 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = MIMVEC / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2607, 37 ),  &
                TO_FM_RATIONAL( 2765, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 236
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM( (/ 31, 33, 35 /) ) / BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM( (/ 31, 33, 35 /) ) / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2607, 37 ),  &
                TO_FM_RATIONAL( 2765, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 237
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / MIMVEC "
      MIMVEC = (/ 31, 33, 35 /)
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2607 ),  &
                TO_FM_RATIONAL( 41, 2765 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 238
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / TO_IM( (/ 31, 33, 35 /) ) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / TO_IM( (/ 31, 33, 35 /) )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2607 ),  &
                TO_FM_RATIONAL( 41, 2765 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 239
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = BVEC / TO_IM(31) "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = BVEC / TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 79 ), TO_FM_RATIONAL( 37, 2449 ),  &
                TO_FM_RATIONAL( 41, 2449 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 240
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = TO_IM(31) / BVEC "
      BVEC = (/ TO_FM_RATIONAL( 31, 79 ), TO_FM_RATIONAL( 37, 79 ), TO_FM_RATIONAL( 41, 79 ) /)
      CVEC = TO_IM(31) / BVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 79, 1 ), TO_FM_RATIONAL( 2449, 37 ),  &
                TO_FM_RATIONAL( 2449, 41 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 241
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = A / MIMVEC "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 33, 35 /)
      CVEC = A / MIMVEC
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 1, 97 ), TO_FM_RATIONAL( 31, 3201 ),  &
                TO_FM_RATIONAL( 31, 3395 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 242
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MIMVEC / A "
      A = TO_FM_RATIONAL( 31, 97 )
      MIMVEC = (/ 31, 33, 35 /)
      CVEC = MIMVEC / A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ TO_FM_RATIONAL( 97, 1 ), TO_FM_RATIONAL( 3201, 31 ),  &
                TO_FM_RATIONAL( 3395, 31 ) /)
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array operations -- division of rationals')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 243
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / BMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = -TO_FM_RATIONAL( J+K, 97 )
            BMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT / BMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = -TO_FM_RATIONAL( 79*(J+K), 97*(2*J+3*K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 244
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / A "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = AMAT / A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 79*(J+K), 31*97 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 245
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A / AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( J+K, 97 )
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A / AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*97, 79*(J+K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 246
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT / JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (2*J+3*K), 79*(J+K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 247
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 / AMAT "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = JV2 / AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 79*(J+K), (2*J+3*K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 248
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = JV2 / A "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = JV2 / A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 79*(J+K), 31 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 249
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A / JV2 "
      DO J = 1, 3
         DO K = 1, 3
            JV2(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A / JV2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31, 79*(J+K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 250
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT / A "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = MIMMAT / A
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 79*(J+K), 31 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 251
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = A / MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
         ENDDO
      ENDDO
      A = TO_FM_RATIONAL( 31, 79 )
      CMAT = A / MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31, 79*(J+K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 252
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / 31 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT / 31
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 31*79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 253
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = 31 / AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = 31 / AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*79, 2*J+3*K )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 254
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / MIMMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT / MIMMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79*(J+K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 255
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MIMMAT / AMAT "
      DO J = 1, 3
         DO K = 1, 3
            MIMMAT(J,K) = J + K
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = MIMMAT / AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 79*(J+K), 2*J+3*K )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 256
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = AMAT / TO_IM(31) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = AMAT / TO_IM(31)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 31*79 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 257
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = TO_IM(31) / AMAT "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J+3*K, 79 )
         ENDDO
      ENDDO
      CMAT = TO_IM(31) / AMAT
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 31*79, 2*J+3*K )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array operations -- division of rationals')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      END SUBROUTINE TEST7



      SUBROUTINE TEST8
      IMPLICIT NONE
      INTEGER :: K

      WRITE (KW,"(/' Testing == comparison.')")


      NCASE = 258
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( .NOT.( A == B ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 259
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(7,8)
      IF ( ( A == B ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 260
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      K = 2
      IF ( .NOT.( A == K ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 261
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( A == K ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 262
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      MIM1 = 2
      IF ( .NOT.( A == MIM1 ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 263
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( A == MIM1 ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 264
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      K = 2
      IF ( .NOT.( K == A ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 265
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( K == A ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 266
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      MIM1 = 2
      IF ( .NOT.( MIM1 == A ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF

      NCASE = 267
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( MIM1 == A ) ) THEN
          CALL ERRPRTRM(' == comparison of rationals')
      ENDIF



      WRITE (KW,"(/' Testing /= comparison.')")


      NCASE = 268
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( ( A /= B ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 269
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(7,8)
      IF ( .NOT.( A /= B ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 270
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      K = 2
      IF ( ( A /= K ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 271
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( A /= K ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 272
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      MIM1 = 2
      IF ( ( A /= MIM1 ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 273
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( A /= MIM1 ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 274
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      K = 2
      IF ( ( K /= A ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 275
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( K /= A ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 276
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(18,9)
      MIM1 = 2
      IF ( ( MIM1 /= A ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF

      NCASE = 277
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( MIM1 /= A ) ) THEN
          CALL ERRPRTRM(' /= comparison of rationals')
      ENDIF



      WRITE (KW,"(/' Testing < comparison.')")


      NCASE = 278
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( ( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 279
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( .NOT.( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 280
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(5,9)
      IF ( ( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 281
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( .NOT.( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 282
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( ( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 283
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( .NOT.( A < B ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 284
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( ( A < K ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 285
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( A < K ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 286
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( ( A < K ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 287
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( .NOT.( A < K ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 288
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( .NOT.( K < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 289
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( K < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 290
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( .NOT.( K < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 291
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( ( K < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 292
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( ( A < MIM1 ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 293
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( A < MIM1 ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 294
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( ( A < MIM1 ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 295
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( .NOT.( A < MIM1 ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 296
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( .NOT.( MIM1 < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 297
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( MIM1 < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 298
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( .NOT.( MIM1 < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF

      NCASE = 299
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( ( MIM1 < A ) ) THEN
          CALL ERRPRTRM(' < comparison of rationals')
      ENDIF



      WRITE (KW,"(/' Testing <= comparison.')")


      NCASE = 300
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( .NOT.( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 301
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( .NOT.( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 302
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(5,9)
      IF ( ( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 303
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( .NOT.( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 304
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( ( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 305
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( .NOT.( A <= B ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 306
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( ( A <= K ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 307
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( A <= K ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 308
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( ( A <= K ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 309
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( .NOT.( A <= K ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 310
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( .NOT.( K <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 311
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( K <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 312
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( .NOT.( K <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 313
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( ( K <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 314
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( ( A <= MIM1 ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 315
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( A <= MIM1 ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 316
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( ( A <= MIM1 ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 317
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( .NOT.( A <= MIM1 ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 318
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( .NOT.( MIM1 <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 319
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( MIM1 <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 320
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( .NOT.( MIM1 <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF

      NCASE = 321
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( ( MIM1 <= A ) ) THEN
          CALL ERRPRTRM(' <= comparison of rationals')
      ENDIF



      WRITE (KW,"(/' Testing > comparison.')")


      NCASE = 322
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( ( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 323
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( ( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 324
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(5,9)
      IF ( .NOT.( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 325
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( ( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 326
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( .NOT.( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 327
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( ( A > B ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 328
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( .NOT.( A > K ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 329
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( A > K ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 330
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( .NOT.( A > K ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 331
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( ( A > K ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 332
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( ( K > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 333
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( K > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 334
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( ( K > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 335
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( .NOT.( K > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 336
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( .NOT.( A > MIM1 ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 337
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( A > MIM1 ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 338
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( .NOT.( A > MIM1 ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 339
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( ( A > MIM1 ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 340
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( ( MIM1 > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 341
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( MIM1 > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 342
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( ( MIM1 > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF

      NCASE = 343
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( .NOT.( MIM1 > A ) ) THEN
          CALL ERRPRTRM(' > comparison of rationals')
      ENDIF



      WRITE (KW,"(/' Testing >= comparison.')")


      NCASE = 344
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = A
      IF ( .NOT.( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 345
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( ( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 346
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(5,9)
      IF ( .NOT.( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 347
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(8,9)
      IF ( ( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 348
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( .NOT.( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 349
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      B = TO_FM_RATIONAL(-5,9)
      IF ( ( A >= B ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 350
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( .NOT.( A >= K ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 351
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( ( A >= K ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 352
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( .NOT.( A >= K ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 353
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( ( A >= K ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 354
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = -3
      IF ( ( K >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 355
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      K = 3
      IF ( .NOT.( K >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 356
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = -3
      IF ( ( K >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 357
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      K = 3
      IF ( .NOT.( K >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 358
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( .NOT.( A >= MIM1 ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 359
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( ( A >= MIM1 ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 360
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( .NOT.( A >= MIM1 ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 361
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( ( A >= MIM1 ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 362
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = -3
      IF ( ( MIM1 >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 363
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(7,9)
      MIM1 = 3
      IF ( .NOT.( MIM1 >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 364
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = -3
      IF ( ( MIM1 >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF

      NCASE = 365
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      A = TO_FM_RATIONAL(-7,9)
      MIM1 = 3
      IF ( .NOT.( MIM1 >= A ) ) THEN
          CALL ERRPRTRM(' >= comparison of rationals')
      ENDIF


      END SUBROUTINE TEST8



      SUBROUTINE TEST9
      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing rational functions.')")



      NCASE = 366
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = ABS( TO_FM_RATIONAL(7,9) )"
      RESULT = ABS( TO_FM_RATIONAL(7,9) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' ABS')
      ENDIF

      NCASE = 367
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = ABS( TO_FM_RATIONAL(-7,9) )"
      RESULT = ABS( TO_FM_RATIONAL(-7,9) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' ABS')
      ENDIF

      NCASE = 368
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = ABS( (/ TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(17,29)," // &
                                 " TO_FM_RATIONAL(-31,79) /) )"
      AVEC =   (/ TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(17,29), TO_FM_RATIONAL(-31,79) /)
      BVEC = ABS( (/ TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(17,29), TO_FM_RATIONAL(-31,79) /) )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = ABS( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- ABS')
         ENDIF
      ENDDO

      NCASE = 369
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = ABS( AVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(17,29), TO_FM_RATIONAL(-31,79) /)
      BVEC = ABS( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = ABS( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- ABS')
         ENDIF
      ENDDO

      NCASE = 370
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = ABS( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 2*J + 3*K - 10, 97 )
         ENDDO
      ENDDO
      BMAT = ABS( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( ABS(2*J + 3*K - 10), 97 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- ABS')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 371
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = CEILING( TO_FM_RATIONAL(7,3) )"
      RESULT = CEILING( TO_FM_RATIONAL(7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' CEILING')
      ENDIF

      NCASE = 372
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = CEILING( TO_FM_RATIONAL(-7,3) )"
      RESULT = CEILING( TO_FM_RATIONAL(-7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' CEILING')
      ENDIF

      NCASE = 373
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = CEILING( TO_FM_RATIONAL(-17,29) )"
      RESULT = CEILING( TO_FM_RATIONAL(-17,29) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = 0
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' CEILING')
      ENDIF

      NCASE = 374
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = CEILING( TO_FM_RATIONAL(123456,3) )"
      RESULT = CEILING( TO_FM_RATIONAL(123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' CEILING')
      ENDIF

      NCASE = 375
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = CEILING( TO_FM_RATIONAL(-123456,3) )"
      RESULT = CEILING( TO_FM_RATIONAL(-123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' CEILING')
      ENDIF

      NCASE = 376
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = CEILING( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29)," // &
                                        " TO_FM_RATIONAL(131,79) /) )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /)
      BVEC = CEILING( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /) )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = CEILING( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- CEILING')
         ENDIF
      ENDDO

      NCASE = 377
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = CEILING( AVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /)
      BVEC = CEILING( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = CEILING( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- CEILING')
         ENDIF
      ENDDO

      NCASE = 378
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = CEILING( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = CEILING( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( TO_IM( CEILING( (23*J + 31*K - 103)/TO_FM(3) ) ) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- CEILING')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 379
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DIM( TO_FM_RATIONAL(7,3), TO_FM_RATIONAL(5,3) )"
      RESULT = DIM( TO_FM_RATIONAL(7,3), TO_FM_RATIONAL(5,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 2/3 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DIM')
      ENDIF

      NCASE = 380
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DIM( TO_FM_RATIONAL(7,3), TO_FM_RATIONAL(11,3) )"
      RESULT = DIM( TO_FM_RATIONAL(7,3), TO_FM_RATIONAL(11,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 0 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DIM')
      ENDIF

      NCASE = 381
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DIM( A, B )"
      A = TO_FM_RATIONAL(7,3)
      B = TO_FM_RATIONAL(6,7)
      RESULT = DIM( A, B )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 31/21 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DIM')
      ENDIF

      NCASE = 382
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DIM( B, A )"
      A = TO_FM_RATIONAL(7,3)
      B = TO_FM_RATIONAL(6,7)
      RESULT = DIM( B, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 0 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DIM')
      ENDIF

      NCASE = 383
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DIM( A, A )"
      A = TO_FM_RATIONAL(7,3)
      B = TO_FM_RATIONAL(6,7)
      RESULT = DIM( A, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 0 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DIM')
      ENDIF

      NCASE = 384
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = DIM( AVEC, BVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(17,29), TO_FM_RATIONAL(131,79) /)
      BVEC =   (/ TO_FM_RATIONAL( 13,9), TO_FM_RATIONAL(16,29), TO_FM_RATIONAL(132,79) /)
      CVEC = DIM( AVEC, BVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " CVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(CVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = DIM( AVEC(J), BVEC(J) )
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- DIM')
         ENDIF
      ENDDO

      NCASE = 385
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = DIM( AMAT, BMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
            BMAT(J,K) = TO_FM_RATIONAL( 27*J + 28*K - 103, 3 )
         ENDDO
      ENDDO
      CMAT = DIM( AMAT, BMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( DIM( (23*J + 31*K), (27*J + 28*K) ), 3 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- DIM')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 386
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = FLOOR( TO_FM_RATIONAL(7,3) )"
      RESULT = FLOOR( TO_FM_RATIONAL(7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' FLOOR')
      ENDIF

      NCASE = 387
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = FLOOR( TO_FM_RATIONAL(-7,3) )"
      RESULT = FLOOR( TO_FM_RATIONAL(-7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' FLOOR')
      ENDIF

      NCASE = 388
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = FLOOR( TO_FM_RATIONAL(123456,3) )"
      RESULT = FLOOR( TO_FM_RATIONAL(123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' FLOOR')
      ENDIF

      NCASE = 389
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = FLOOR( TO_FM_RATIONAL(-123456,3) )"
      RESULT = FLOOR( TO_FM_RATIONAL(-123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' FLOOR')
      ENDIF

      NCASE = 390
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = FLOOR( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29)," // &
                                      " TO_FM_RATIONAL(31,79) /) )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /)
      BVEC = FLOOR( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /) )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = FLOOR( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- FLOOR')
         ENDIF
      ENDDO

      NCASE = 391
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = FLOOR( AVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /)
      BVEC = FLOOR( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = FLOOR( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- FLOOR')
         ENDIF
      ENDDO

      NCASE = 392
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = FLOOR( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = FLOOR( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( TO_IM( FLOOR( (23*J + 31*K - 103)/TO_FM(3) ) ) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- FLOOR')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 393
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = INT( TO_FM_RATIONAL(7,3) )"
      RESULT = INT( TO_FM_RATIONAL(7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' INT')
      ENDIF

      NCASE = 394
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = INT( TO_FM_RATIONAL(-7,3) )"
      RESULT = INT( TO_FM_RATIONAL(-7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' INT')
      ENDIF

      NCASE = 395
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = INT( TO_FM_RATIONAL(123456,3) )"
      RESULT = INT( TO_FM_RATIONAL(123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' INT')
      ENDIF

      NCASE = 396
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = INT( TO_FM_RATIONAL(-123456,3) )"
      RESULT = INT( TO_FM_RATIONAL(-123456,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-41152)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' INT')
      ENDIF

      NCASE = 397
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = INT( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29)," // &
                                    " TO_FM_RATIONAL(31,79) /) )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /)
      BVEC = INT( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /) )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = INT( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- INT')
         ENDIF
      ENDDO

      NCASE = 398
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = INT( AVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /)
      BVEC = INT( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = INT( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- INT')
         ENDIF
      ENDDO

      NCASE = 399
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = INT( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = INT( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( (23*J + 31*K - 103) / 3 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- INT')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE


      END SUBROUTINE TEST9



      SUBROUTINE TEST10
      IMPLICIT NONE
      INTEGER :: J, K

      NCASE = 400
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( TO_FM_RATIONAL(31,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MOD( TO_FM_RATIONAL(31,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 401
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( TO_FM_RATIONAL(-31,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MOD( TO_FM_RATIONAL(-31,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 402
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( A, B )"
      A = TO_FM_RATIONAL(31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MOD( A, B )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 403
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( A, B )"
      A = TO_FM_RATIONAL(-31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MOD( A, B )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 404
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( B, A )"
      A = TO_FM_RATIONAL(-31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MOD( B, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -2/3 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 405
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( TO_FM_RATIONAL(3,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MOD( TO_FM_RATIONAL(3,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 406
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( TO_FM_RATIONAL(-3,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MOD( TO_FM_RATIONAL(-3,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 407
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MOD( A, A )"
      A = TO_FM_RATIONAL(7,3)
      B = TO_FM_RATIONAL(6,7)
      RESULT = MOD( A, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 0 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MOD')
      ENDIF

      NCASE = 408
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MOD( AVEC, BVEC )"
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, 3 )
         BVEC(J) = TO_FM_RATIONAL( 29*J - 47, 5 )
      ENDDO
      CVEC = MOD( AVEC, BVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " CVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(CVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = MOD( AVEC(J), BVEC(J) )
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- MOD')
         ENDIF
      ENDDO

      NCASE = 409
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MOD( AMAT, BMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
            BMAT(J,K) = TO_FM_RATIONAL( 27*J + 28*K - 103, 5 )
         ENDDO
      ENDDO
      CMAT = MOD( AMAT, BMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = MOD( AMAT(J,K), BMAT(J,K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- MOD')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 410
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( TO_FM_RATIONAL(31,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MODULO( TO_FM_RATIONAL(31,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 411
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( TO_FM_RATIONAL(-31,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MODULO( TO_FM_RATIONAL(-31,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 5/21 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 412
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( A, B )"
      A = TO_FM_RATIONAL(31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MODULO( A, B )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -5/21 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 413
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( A, B )"
      A = TO_FM_RATIONAL(-31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MODULO( A, B )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 414
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( B, A )"
      A = TO_FM_RATIONAL(-31,7)
      B = TO_FM_RATIONAL(-2,3)
      RESULT = MODULO( B, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' -2/3 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 415
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( TO_FM_RATIONAL(3,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MODULO( TO_FM_RATIONAL(3,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 3/7 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 416
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( TO_FM_RATIONAL(-3,7), TO_FM_RATIONAL(2,3) )"
      RESULT = MODULO( TO_FM_RATIONAL(-3,7), TO_FM_RATIONAL(2,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 5/21 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 417
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MODULO( A, A )"
      A = TO_FM_RATIONAL(7,3)
      B = TO_FM_RATIONAL(6,7)
      RESULT = MODULO( A, A )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(' 0 ')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MODULO')
      ENDIF

      NCASE = 418
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MODULO( AVEC, BVEC )"
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, 3 )
         BVEC(J) = TO_FM_RATIONAL( 29*J - 47, 5 )
      ENDDO
      CVEC = MODULO( AVEC, BVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " CVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(CVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = MODULO( AVEC(J), BVEC(J) )
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- MODULO')
         ENDIF
      ENDDO

      NCASE = 419
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MODULO( AMAT, BMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
            BMAT(J,K) = TO_FM_RATIONAL( 27*J + 28*K - 103, 5 )
         ENDDO
      ENDDO
      CMAT = MODULO( AMAT, BMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = MODULO( AMAT(J,K), BMAT(J,K) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- MODULO')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 420
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(7,3) )"
      RESULT = NINT( TO_FM_RATIONAL(7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 421
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(8,3) )"
      RESULT = NINT( TO_FM_RATIONAL(8,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 422
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(9,2) )"
      RESULT = NINT( TO_FM_RATIONAL(9,2) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(5)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 423
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(-7,3) )"
      RESULT = NINT( TO_FM_RATIONAL(-7,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 424
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(-8,3) )"
      RESULT = NINT( TO_FM_RATIONAL(-8,3) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 425
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(-9,2) )"
      RESULT = NINT( TO_FM_RATIONAL(-9,2) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-5)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 426
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(3500001,1000000) )"
      RESULT = NINT( TO_FM_RATIONAL(3500001,1000000) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(4)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 427
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = NINT( TO_FM_RATIONAL(-3499999,1000000) )"
      RESULT = NINT( TO_FM_RATIONAL(-3499999,1000000) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' NINT')
      ENDIF

      NCASE = 428
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = NINT( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29)," // &
                                     " TO_FM_RATIONAL(31,79) /) )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /)
      BVEC = NINT( (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(31,79) /) )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = NINT( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- NINT')
         ENDIF
      ENDDO

      NCASE = 429
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = NINT( AVEC )"
      AVEC =   (/ TO_FM_RATIONAL(-13,9), TO_FM_RATIONAL(-17,29), TO_FM_RATIONAL(131,79) /)
      BVEC = NINT( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = NINT( AVEC(J) )
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- NINT')
         ENDIF
      ENDDO

      NCASE = 430
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = NINT( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = NINT( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = NINT( TO_FM_RATIONAL( (23*J + 31*K - 103) , 3 ) )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- NINT')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 431
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(7,9) ** 12"
      RESULT = TO_FM_RATIONAL(7,9) ** 12
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('13841287201'),TO_IM('282429536481'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' **')
      ENDIF

      NCASE = 432
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-7,9) ** 19"
      RESULT = TO_FM_RATIONAL(-7,9) ** 19
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-11398895185373143'),TO_IM('1350851717672992089'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' **')
      ENDIF

      NCASE = 433
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-7,9) ** (-16)"
      RESULT = TO_FM_RATIONAL(-7,9) ** (-16)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('1853020188851841'),TO_IM('33232930569601'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' **')
      ENDIF

      NCASE = 434
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-7,9) ** (-17)"
      RESULT = TO_FM_RATIONAL(-7,9) ** (-17)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('-16677181699666569'),TO_IM('232630513987207'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' **')
      ENDIF

      NCASE = 435
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = TO_FM_RATIONAL(-7,9) ** 0"
      RESULT = TO_FM_RATIONAL(-7,9) ** 0
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('1'),TO_IM('1'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' **')
      ENDIF

      NCASE = 436
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = AVEC ** 2 "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, 3 )
      ENDDO
      BVEC = AVEC ** 2
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = AVEC(J) * AVEC(J)
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- **')
         ENDIF
      ENDDO

      NCASE = 437
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = AVEC ** 23 "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 22*J - 43, 7 )
      ENDDO
      BVEC = AVEC ** 23
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = AVEC(J) ** 23
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- **')
         ENDIF
      ENDDO

      NCASE = 438
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BVEC = AVEC ** (-29) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 24*J - 43, 7 )
      ENDDO
      BVEC = AVEC ** (-29)
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " BVEC(",J,") = "
         CALL FM_PRINT_RATIONAL(BVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         CORRECT = AVEC(J) ** (-29)
         IF ( (.NOT. IMCOMPARE(BVEC(J)%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(BVEC(J)%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- **')
         ENDIF
      ENDDO

      NCASE = 439
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = AMAT ** 2 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = AMAT ** 2
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = AMAT(J,K) * AMAT(J,K)
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- **')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 440
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = AMAT ** 19 "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = AMAT ** 19
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = AMAT(J,K) ** 19
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- **')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 441
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " BMAT = AMAT ** (-17) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 3 )
         ENDDO
      ENDDO
      BMAT = AMAT ** (-17)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = AMAT(J,K) ** (-17)
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- **')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE


      END SUBROUTINE TEST10



      SUBROUTINE TEST11
      USE SUM_R
      IMPLICIT NONE
      INTEGER :: J, K

      NCASE = 442
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFM1 = TO_FM( TO_FM_RATIONAL(7,9) )"
      MFM1 = TO_FM( TO_FM_RATIONAL(7,9) )
      KW = KLOG
      CALL FM_PRINT(MFM1)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MFM2 = TO_FM(7)/9
      IF ( .NOT.( MFM1 == MFM2 ) ) THEN
          CALL ERRPRTRM(' TO_FM')
      ENDIF

      NCASE = 443
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFM1 = TO_FM( TO_FM_RATIONAL(-765432,314159) )"
      MFM1 = TO_FM( TO_FM_RATIONAL(-765432,314159) )
      KW = KLOG
      CALL FM_PRINT(MFM1)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      MFM2 = TO_FM(-765432)/314159
      IF ( .NOT.( MFM1 == MFM2 ) ) THEN
          CALL ERRPRTRM(' TO_FM')
      ENDIF

      NCASE = 444
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMVEC = TO_FM( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, 3 )
      ENDDO
      MFMVEC = TO_FM( AVEC )
      DO J = 1, 3
         KW = KLOG
         WRITE (KLOG,"(A,I1,A)") " MFMVEC(",J,") = "
         CALL FM_PRINT(MFMVEC(J))
         KW = KWSAVE
         WRITE (KLOG,*) ' '
         MFM2 = TO_FM(23*J - 43) / 3
         IF ( .NOT.( MFMVEC(J) == MFM2 ) ) THEN
             CALL ERRPRTRM(' array arguments -- TO_FM')
         ENDIF
      ENDDO

      NCASE = 445
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " MFMMAT = TO_FM( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 31 )
         ENDDO
      ENDDO
      MFMMAT = TO_FM( AMAT )
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            KW = KLOG
            WRITE (KLOG,"(A,I1,A,I1,A)") ' MFMMAT(',J,',',K,') ='
            CALL FM_PRINT(MFMMAT(J,K))
            KW = KWSAVE
            WRITE (KLOG,*) ' '
            MFM2 = TO_FM(23*J + 31*K - 103) / 31
            IF ( .NOT.( MFMMAT(J,K) == MFM2 ) ) THEN
                CALL ERRPRTRM(' array arguments -- TO_FM')
            ENDIF
         ENDDO
      ENDDO

      NCASE = 446
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = RATIONAL_APPROX(pi,3)"
      CALL FM_PI(MFM1)
      RESULT = RATIONAL_APPROX(MFM1,3)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(355,113)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' RATIONAL_APPROX')
      ENDIF

      NCASE = 447
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = RATIONAL_APPROX(-pi,6)"
      CALL FM_PI(MFM1)
      RESULT = RATIONAL_APPROX(-MFM1,6)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-833719,265381)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' RATIONAL_APPROX')
      ENDIF

      NCASE = 448
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = RATIONAL_APPROX(pi,20)"
      CALL FM_PI(MFM1)
      RESULT = RATIONAL_APPROX(MFM1,20)
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(TO_IM('2646693125139304345'),TO_IM('842468587426513207'))
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' RATIONAL_APPROX')
      ENDIF

      NCASE = 449
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(71,90) )"
      RESULT = MAX( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(71,90)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 450
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(-71,90) )"
      RESULT = MAX( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(-71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 451
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(71,90) )"
      RESULT = MAX( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(71,90)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 452
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(-71,90) )"
      RESULT = MAX( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(-71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 453
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(22,7), TO_FM_RATIONAL(355,113), "//  &
                                    "TO_FM_RATIONAL(103993,33102) )"
      RESULT = MAX( TO_FM_RATIONAL(22,7), TO_FM_RATIONAL(355,113), TO_FM_RATIONAL(103993,33102) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(22,7)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 454
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAX( TO_FM_RATIONAL(' 37535589513263342053361',' 11947949225808341100193'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('199573010111413366978755',' 63526062133920493691074'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('237108599624676709032116',' 75474011359728834791267'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('436681609736090076010871','139000073493649328482341') )"
      RESULT = MAX( TO_FM_RATIONAL(' 37535589513263342053361',' 11947949225808341100193'),  &
                    TO_FM_RATIONAL('199573010111413366978755',' 63526062133920493691074'),  &
                    TO_FM_RATIONAL('237108599624676709032116',' 75474011359728834791267'),  &
                    TO_FM_RATIONAL('436681609736090076010871','139000073493649328482341') )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL('199573010111413366978755',' 63526062133920493691074')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MAX')
      ENDIF

      NCASE = 455
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(71,90) )"
      RESULT = MIN( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 456
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(-71,90) )"
      RESULT = MIN( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(-71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-71,90)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 457
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(71,90) )"
      RESULT = MIN( TO_FM_RATIONAL(-7,9), TO_FM_RATIONAL(71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-7,9)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 458
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(-71,90) )"
      RESULT = MIN( TO_FM_RATIONAL(7,9), TO_FM_RATIONAL(-71,90) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-71,90)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 459
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(22,7), TO_FM_RATIONAL(355,113), "//  &
                                    "TO_FM_RATIONAL(103993,33102) )"
      RESULT = MIN( TO_FM_RATIONAL(22,7), TO_FM_RATIONAL(355,113), TO_FM_RATIONAL(103993,33102) )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(103993,33102)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 460
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MIN( TO_FM_RATIONAL(' 37535589553263342053361',' 11947949225808341100193'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('199573010111413366978755',' 63526062133920493691074'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('237108599624676709032116',' 75474011359728834791267'),"
      WRITE (KLOG,*) "               TO_FM_RATIONAL('436681609736090076010871','139000073493649328482341') )"
      RESULT = MIN( TO_FM_RATIONAL(' 37535589553263342053361',' 11947949225808341100193'),  &
                    TO_FM_RATIONAL('199573010111413366978755',' 63526062133920493691074'),  &
                    TO_FM_RATIONAL('237108599624676709032116',' 75474011359728834791267'),  &
                    TO_FM_RATIONAL('436681609736090076010871','139000073493649328482341') )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL('237108599624676709032116',' 75474011359728834791267')
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' MIN')
      ENDIF

      NCASE = 461
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( TO_FM_RATIONAL(-7,9) )"
      WRITE (KLOG,*) IS_UNKNOWN( TO_FM_RATIONAL(-7,9) )
      WRITE (KLOG,*) ' '
      IF ( IS_UNKNOWN( TO_FM_RATIONAL(-7,9) ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 462
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( TO_FM_RATIONAL(-7,0) )"
      WRITE (KLOG,*) IS_UNKNOWN( TO_FM_RATIONAL(-7,0) )
      WRITE (KLOG,*) ' '
      IF ( .NOT. IS_UNKNOWN( TO_FM_RATIONAL(-7,0) ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 463
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( TO_FM_RATIONAL(-7,9) / TO_FM_RATIONAL(0) )"
      WRITE (KLOG,*) IS_UNKNOWN( TO_FM_RATIONAL(-7,9) / TO_FM_RATIONAL(0) )
      WRITE (KLOG,*) ' '
      IF ( .NOT. IS_UNKNOWN( TO_FM_RATIONAL(-7,9) / TO_FM_RATIONAL(0) ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 464
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( AVEC )"
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, J - 2 )
      ENDDO
      WRITE (KLOG,*) IS_UNKNOWN( AVEC )
      WRITE (KLOG,*) ' '
      IF ( .NOT. IS_UNKNOWN( AVEC ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 465
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( AVEC )"
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, J + 2 )
      ENDDO
      WRITE (KLOG,*) IS_UNKNOWN( AVEC )
      WRITE (KLOG,*) ' '
      IF ( IS_UNKNOWN( AVEC ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 466
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( AMAT )"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 2*J + 3*K )
         ENDDO
      ENDDO
      WRITE (KLOG,*) IS_UNKNOWN( AMAT )
      WRITE (KLOG,*) ' '
      IF ( IS_UNKNOWN( AMAT ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF

      NCASE = 467
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = IS_UNKNOWN( AMAT )"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 2*J - 3*K )
         ENDDO
      ENDDO
      WRITE (KLOG,*) IS_UNKNOWN( AMAT )
      WRITE (KLOG,*) ' '
      IF ( .NOT. IS_UNKNOWN( AMAT ) ) THEN
          CALL ERRPRTRM(' IS_UNKNOWN')
      ENDIF
      KW = KWSAVE

      NCASE = 468
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = DOT_PRODUCT( AVEC, BVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 23*J - 43, 3 )
         BVEC(J) = TO_FM_RATIONAL( 37*J - 41, 7 )
      ENDDO
      RESULT = DOT_PRODUCT( AVEC, BVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(1999,21)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' DOT_PRODUCT')
      ENDIF

      NCASE = 469
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = MATMUL( AMAT, BMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 5 )
            BMAT(J,K) = TO_FM_RATIONAL( 27*J + 28*K - 103, 7 )
         ENDDO
      ENDDO
      CMAT = MATMUL( AMAT, BMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = 3 * TO_FM_RATIONAL( 2567 - 1127*J - 1148*K + 644*J*K, 35 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- MATMUL')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 470
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MATMUL( AMAT, BVEC ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 5 )
         ENDDO
         BVEC(J) = TO_FM_RATIONAL( 27*J - 49, 7 )
      ENDDO
      CVEC = MATMUL( AMAT, BVEC )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DVEC(J) = 3 * TO_FM_RATIONAL( 353 + 115*J, 35 )
      ENDDO
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A,I1,A)") ' CVEC(',J,',',K,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- MATMUL')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 471
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CVEC = MATMUL( AVEC, BMAT ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 27*J - 49, 7 )
         DO K = 1, 3
            BMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 5 )
         ENDDO
      ENDDO
      CVEC = MATMUL( AVEC, BMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DVEC(J) = 3 * TO_FM_RATIONAL( 129 + 155*J, 35 )
      ENDDO
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A,I1,A)") ' CVEC(',J,',',K,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- MATMUL')
         ENDIF
      ENDDO
      KW = KWSAVE

      NCASE = 472
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXLOC( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( -J*J + 4*J + 11, 79 )
      ENDDO
      J = MAXLOC( AVEC )
      WRITE (KLOG,"(1X,I6)") MAXLOC( AVEC )
      WRITE (KLOG,*) ' '
      K = 2
      IF (J /= K) THEN
          CALL ERRPRTRM(' array arguments -- MAXLOC')
      ENDIF

      NCASE = 473
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXLOC( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (-J*J + 4*J + 11)*(-K*K + 2*K + 13), 41 )
         ENDDO
      ENDDO
      ML = MAXLOC( AMAT )
      WRITE (KLOG,"(1X,2I6)") ML
      WRITE (KLOG,*) ' '
      IF (.NOT.( ML(1) == 2 .AND. ML(2) == 1 )) THEN
          CALL ERRPRTRM(' array arguments -- MAXLOC')
      ENDIF

      NCASE = 474
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXVAL( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 24*J - 43, 7 )
      ENDDO
      RESULT = MAXVAL( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(29,7)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MAXVAL')
      ENDIF

      NCASE = 475
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXVAL( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( -J*J + 4*J + 11, 79 )
      ENDDO
      RESULT = MAXVAL( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(15,79)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MAXVAL')
      ENDIF

      NCASE = 476
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXVAL( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 97 )
         ENDDO
      ENDDO
      RESULT = MAXVAL( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(59,97)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MAXVAL')
      ENDIF

      NCASE = 477
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MAXVAL( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (-J*J + 4*J + 11)*(-K*K + 2*K + 13), 41 )
         ENDDO
      ENDDO
      RESULT = MAXVAL( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(210,41)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MAXVAL')
      ENDIF

      NCASE = 478
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINLOC( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( -J*J + 3*J + 11, 79 )
      ENDDO
      J = MINLOC( AVEC )
      WRITE (KLOG,"(1X,I6)") MINLOC( AVEC )
      WRITE (KLOG,*) ' '
      K = 3
      IF (J /= K) THEN
          CALL ERRPRTRM(' array arguments -- MINLOC')
      ENDIF

      NCASE = 479
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINLOC( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (-J*J + 5*J + 11)*(-K*K + 2*K + 13), 41 )
         ENDDO
      ENDDO
      ML = MINLOC( AMAT )
      WRITE (KLOG,"(1X,2I6)") ML
      WRITE (KLOG,*) ' '
      IF (.NOT.( ML(1) == 1 .AND. ML(2) == 3 )) THEN
          CALL ERRPRTRM(' array arguments -- MINLOC')
      ENDIF

      NCASE = 480
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINVAL( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 24*J - 43, 7 )
      ENDDO
      RESULT = MINVAL( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-19,7)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MINVAL')
      ENDIF

      NCASE = 481
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINVAL( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( J*J - 4*J + 11, 79 )
      ENDDO
      RESULT = MINVAL( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(7,79)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MINVAL')
      ENDIF

      NCASE = 482
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINVAL( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 97 )
         ENDDO
      ENDDO
      RESULT = MINVAL( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-49,97)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MINVAL')
      ENDIF

      NCASE = 483
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = MINVAL( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (J*J - 6*J + 11)*(K*K - 4*K + 13), 61 )
         ENDDO
      ENDDO
      RESULT = MINVAL( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(18,61)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- MINVAL')
      ENDIF

      NCASE = 484
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = PRODUCT( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 24*J - 42, 7*J + 3 )
      ENDDO
      RESULT = PRODUCT( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-27,34)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- PRODUCT')
      ENDIF

      NCASE = 485
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = PRODUCT( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( J*J - 4*J + 11, j+1 )
      ENDDO
      RESULT = PRODUCT( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(56,3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- PRODUCT')
      ENDIF

      NCASE = 486
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = PRODUCT( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 30 )
         ENDDO
      ENDDO
      RESULT = PRODUCT( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(3420053,253125000)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- PRODUCT')
      ENDIF

      NCASE = 487
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = PRODUCT( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (J*J - 6*J + 11)*(K*K - 4*K + 13), 30 )
         ENDDO
      ENDDO
      RESULT = PRODUCT( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(216,125)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- PRODUCT')
      ENDIF

      NCASE = 488
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = SUM( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 24*J - 42, 7*J + 3 )
      ENDDO
      RESULT = SUM( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(-67,340)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- SUM')
      ENDIF

      NCASE = 489
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = SUM( AVEC ) "
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( J*J - 4*J + 11, j+1 )
      ENDDO
      RESULT = SUM( AVEC )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(25,3)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- SUM')
      ENDIF

      NCASE = 490
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = SUM( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 30 )
         ENDDO
      ENDDO
      RESULT = SUM( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(3,2)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- SUM')
      ENDIF

      NCASE = 491
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " RESULT = SUM( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( (J*J - 6*J + 11)*(K*K - 4*K + 13), J + K )
         ENDDO
      ENDDO
      RESULT = SUM( AMAT )
      KW = KLOG
      CALL FM_PRINT_RATIONAL(RESULT)
      KW = KWSAVE
      WRITE (KLOG,*) ' '
      CORRECT = TO_FM_RATIONAL(5861,60)
      IF ( (.NOT. IMCOMPARE(RESULT%NUMERATOR,'==',CORRECT%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(RESULT%DENOMINATOR,'==',CORRECT%DENOMINATOR)) ) THEN
          CALL ERRPRTRM(' array arguments -- SUM')
      ENDIF

      NCASE = 492
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CMAT = TRANSPOSE( AMAT ) "
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J + 31*K - 103, 5 )
         ENDDO
      ENDDO
      CMAT = TRANSPOSE( AMAT )
      KW = KLOG
      WRITE (KLOG,*) ' '
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 23*K + 31*J - 103, 5 )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' CMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(CMAT(J,K))
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- TRANSPOSE')
            ENDIF
         ENDDO
      ENDDO
      KW = KWSAVE

      NCASE = 493
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CALL RM_LIN_SOLVE(AMAT,CVEC,BVEC,3,D)"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J - 31*K + (J+K)**2 - 3, 5 )
         ENDDO
      ENDDO
      BVEC = (/ 31, 41, -59 /)
      BVEC = BVEC / 11
      CALL RM_LIN_SOLVE(AMAT,CVEC,BVEC,3,D)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ 45340, -95105, 49490 /)
      DVEC = DVEC / 11
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' solution CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- RM_LIN_SOLVE')
         ENDIF
      ENDDO
      WRITE (KLOG,"(A)") ' det D ='
      CALL FM_PRINT_RATIONAL(D)
      IF (.NOT. D == TO_FM_RATIONAL(-8,125)) THEN
          CALL ERRPRTRM(' array arguments -- RM_LIN_SOLVE det')
      ENDIF
      KW = KWSAVE

      NCASE = 494
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CALL RM_LIN_SOLVE2(AMAT,CVEC,BVEC,3,D)"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J - 31*K + (J+K)**2 - 3, 5 )
         ENDDO
      ENDDO
      BVEC = (/ 31, 41, -59 /)
      BVEC = BVEC / 11
      CALL RM_LIN_SOLVE2(AMAT,CVEC,BVEC,3,D)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DVEC = (/ 45340, -95105, 49490 /)
      DVEC = DVEC / 11
      DO J = 1, 3
         WRITE (KLOG,"(A,I1,A)") ' solution CVEC(',J,') ='
         CALL FM_PRINT_RATIONAL(CVEC(J))
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
             CALL ERRPRTRM(' array arguments -- RM_LIN_SOLVE2')
         ENDIF
      ENDDO
      WRITE (KLOG,"(A)") ' det D ='
      CALL FM_PRINT_RATIONAL(D)
      IF (.NOT. D == TO_FM_RATIONAL(-8,125)) THEN
          CALL ERRPRTRM(' array arguments -- RM_LIN_SOLVE2 det')
      ENDIF
      KW = KWSAVE

      NCASE = 495
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CALL RM_INVERSE(AMAT,3,BMAT,D)"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J - 31*K + (J+K)**2 - 3, 5 )
         ENDDO
      ENDDO
      CALL RM_INVERSE(AMAT,3,BMAT,D)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DMAT(1,1:3) = (/ -1745, 3390, -1635 /)
      DMAT(2,1:3) = (/  3660,-7110,  3430 /)
      DMAT(3,1:3) = (/ -1905, 3700, -1785 /)
      DMAT = DMAT / 4
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' inverse BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- RM_INVERSE')
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,"(A)") ' det D ='
      CALL FM_PRINT_RATIONAL(D)
      IF (.NOT. D == TO_FM_RATIONAL(-8,125)) THEN
          CALL ERRPRTRM(' array arguments -- RM_INVERSE det')
      ENDIF
      KW = KWSAVE

      NCASE = 496
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " CALL RM_INVERSE2(AMAT_IM,3,BMAT,D)"
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 23*J - 31*K + (J+K)**2 - 3, 5 )
            AMAT_IM(J,K) = 23*J - 31*K + (J+K)**2 - 3
            AMAT_IM(J,K+3) = 0
            IF (J == K) AMAT_IM(J,J+3) = 5
         ENDDO
      ENDDO
      CALL RM_INVERSE2(AMAT_IM,3,BMAT,D)
      KW = KLOG
      WRITE (KLOG,*) ' '
      DMAT(1,1:3) = (/ -1745, 3390, -1635 /)
      DMAT(2,1:3) = (/  3660,-7110,  3430 /)
      DMAT(3,1:3) = (/ -1905, 3700, -1785 /)
      DMAT = DMAT / 4
      DO J = 1, 3
         DO K = 1, 3
            WRITE (KLOG,"(A,I1,A,I1,A)") ' inverse BMAT(',J,',',K,') ='
            CALL FM_PRINT_RATIONAL(BMAT(J,K))
            IF ( (.NOT. IMCOMPARE(BMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(BMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                CALL ERRPRTRM(' array arguments -- RM_INVERSE2')
            ENDIF
         ENDDO
      ENDDO
      WRITE (KLOG,"(A)") ' det D ='
      CALL FM_PRINT_RATIONAL(D)
      IF (.NOT. D == TO_FM_RATIONAL(-8,125)) THEN
          CALL ERRPRTRM(' array arguments -- RM_INVERSE2 det')
      ENDIF
      KW = KWSAVE

      NCASE = 497
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_RAT(A,B)"
      A = TO_FM_RATIONAL( 5, 11 )
      B = TO_FM_RATIONAL( 7, 13 )
      C = SUM_RAT(A,B)
      D = TO_FM_RATIONAL( 142, 143 )
      KW = KLOG
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A)") ' Function for 5/11 + 7/13 ='
      CALL FM_PRINT_RATIONAL(C)
      IF ( (.NOT. IMCOMPARE(C%NUMERATOR,'==',D%NUMERATOR)) .OR.  &
           (.NOT. IMCOMPARE(C%DENOMINATOR,'==',D%DENOMINATOR)) ) THEN
           CALL ERRPRTRM(' rational-valued function call')
      ENDIF
      KW = KWSAVE

      NCASE = 498
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_RAT(A,B)  for a,b,c vectors."
      DO J = 1, 3
         AVEC(J) = TO_FM_RATIONAL( 4+J, 10+J )
         BVEC(J) = TO_FM_RATIONAL( 40+J, 51+J )
      ENDDO
      CVEC = SUM_RAT(AVEC,BVEC)
      DO J = 1, 3
         DVEC(J) = TO_FM_RATIONAL( 4+J, 10+J ) + TO_FM_RATIONAL( 40+J, 51+J )
      ENDDO
      DO J = 1, 3
         IF ( (.NOT. IMCOMPARE(CVEC(J)%NUMERATOR,'==',DVEC(J)%NUMERATOR)) .OR.  &
              (.NOT. IMCOMPARE(CVEC(J)%DENOMINATOR,'==',DVEC(J)%DENOMINATOR)) ) THEN
              CALL ERRPRTRM(' rational-valued vector function call')
         ENDIF
      ENDDO

      NCASE = 499
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(A,I6)") ' NCASE = ',NCASE
      WRITE (KLOG,*) " C = SUM_RAT(A,B)  for a,b,c matrices."
      DO J = 1, 3
         DO K = 1, 3
            AMAT(J,K) = TO_FM_RATIONAL( 4+J+K, 10+J+K )
            BMAT(J,K) = TO_FM_RATIONAL( 40+J, 51+J )
         ENDDO
      ENDDO
      CMAT = SUM_RAT(AMAT,BMAT)
      DO J = 1, 3
         DO K = 1, 3
            DMAT(J,K) = TO_FM_RATIONAL( 4+J+K, 10+J+K ) + TO_FM_RATIONAL( 40+J, 51+J )
         ENDDO
      ENDDO
      DO J = 1, 3
         DO K = 1, 3
            IF ( (.NOT. IMCOMPARE(CMAT(J,K)%NUMERATOR,'==',DMAT(J,K)%NUMERATOR)) .OR.  &
                 (.NOT. IMCOMPARE(CMAT(J,K)%DENOMINATOR,'==',DMAT(J,K)%DENOMINATOR)) ) THEN
                 CALL ERRPRTRM(' rational-valued vector function call')
            ENDIF
         ENDDO
      ENDDO

      END SUBROUTINE TEST11


      SUBROUTINE ERRPRTRM(OPERATION)

!  Print error messages.

!  OPERATION is the operation being tested.

      USE FM_RATIONAL_ARITHMETIC
      IMPLICIT NONE

      CHARACTER(*) :: OPERATION

      NERROR = NERROR + 1
      WRITE (KW,  &
                "(//' Error in case',I5,'.  The operation',' being tested was: ',A)"  &
                ) NCASE,OPERATION
      IF (KLOG /= KW) THEN
          WRITE (KLOG,  &
                    "(//' Error in case',I5,'.  The operation',' being tested was: ',A)"  &
                    ) NCASE,OPERATION
      ENDIF
      RETURN
      END SUBROUTINE ERRPRTRM

   END MODULE TEST_RATIONAL

      PROGRAM TEST
      USE TEST_RATIONAL
      IMPLICIT NONE

!             Write output to the standard FM output (unit KW, defined in subroutine FMSET),
!             and also to the file TestFMRATIONAL.out.

      KLOG = 18
      OPEN (KLOG,FILE='TestFMrational.out')
      KWSAVE = KW
      KW = KLOG

!             Set precision to give at least 50 significant digits and initialize the FM package.
!             This call also checks many of the initialization values used in module FMVALS
!             (file FMSAVE.f95).  Set KW = KLOG for this call so that any messages concerning these
!             values will appear in file TestFMrational.OUT.

      CALL FM_SET(50)
      KW = KWSAVE

      CALL CPU_TIME(TIME1)

!             NERROR is the number of errors found.

      NERROR = 0
      RSMALL = EPSILON(1.0)*100.0
      DSMALL = EPSILON(1.0D0)*100.0

!             Test input conversion to RATIONALs.

      CALL TEST1

!             Test add, subtract, multiply, divide and power.

      CALL TEST2
      CALL TEST3

!             Test array addition.

      CALL TEST4

!             Test array subtraction.

      CALL TEST5

!             Test array multiplication.

      CALL TEST6

!             Test array division.

      CALL TEST7

!             Test rational comparisons.

      CALL TEST8

!             Test rational functions

      CALL TEST9
      CALL TEST10
      CALL TEST11


!             End of tests.

      CALL CPU_TIME(TIME2)

      IF (NERROR == 0) THEN
          WRITE (KW,   "(///1X,I5,' cases tested.  No errors were found.'/)" ) NCASE
          WRITE (KLOG, "(///1X,I5,' cases tested.  No errors were found.'/)" ) NCASE
      ELSE IF (NERROR == 1) THEN
          WRITE (KW,   "(///1X,I5,' cases tested.  1 error was found.'/)" ) NCASE
          WRITE (KLOG, "(///1X,I5,' cases tested.  1 error was found.'/)" ) NCASE
      ELSE
          WRITE (KW,   "(///1X,I5,' cases tested.',I4,' errors were found.'/)" ) NCASE,NERROR
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
      WRITE (KW,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMrational.'
      WRITE (KW,*) ' '
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMrational.'
      WRITE (KLOG,*) ' '

      WRITE (KW,*)' End of run.'

      STOP
      END PROGRAM TEST
