
 MODULE FM_RATIONAL_ARITHMETIC_1


!  FM_rational 1.4                        David M. Smith                        Rational Arithmetic

!  This module extends the definition of basic Fortran arithmetic and function operations so
!  they also apply to multiple precision rationals, using version 1.4 of FM.
!  The multiple precision rational data type is called
!    TYPE (FM_RATIONAL)

!  Each FM rational number A/B consists of two values, with A and B being TYPE(IM) integer multiple
!  precision numbers.  Negative rationals are represented with A being negative.

!  This module supports assignment, arithmetic, comparison, and functions involving FM_RATIONAL
!  numbers.

!  Mixed-mode operations, such as adding FM_RATIONAL to IM or machine integer types, are supported.
!  In general, operations where both the arguments and results are mathematically rational (machine
!  precision integers, TYPE(IM), or TYPE (FM_RATIONAL)) are supported, such as A = 1, A = B - 3, or
!  A = B / X_IM, where A and B are FM_RATIONAL, and X_IM is type IM.

!  Array operations are also supported, so A, B, and X_IM could be 1- or 2-dimensional arrays in the
!  examples above.

!  Mixed-mode comparisons are also supported, as with IF (A == 1), IF (A <= B - 3), or
!  IF (A > B / X_IM).

!  TO_FM_RATIONAL is a function for creating a number of type FM_RATIONAL.
!  This function can have one argument, for the common case of creating a rational number with an
!  integer value.  For TO_FM_RATIONAL(A), A can be a machine integer or array of integers, a type
!  IM value or array, or a character string.

!  There is also a two argument form, TO_FM_RATIONAL(A,B), that can be used to create the fraction
!  A/B when A and B are both machine precision integers, TYPE(IM) multiple precision integers, or
!  character strings representing integers.

!  The one argument character form can be used with a single input string having both parts of the
!  fraction present and separated by '/', as in TO_FM_RATIONAL(' 41 / 314 ').  This might be more
!  readable than the equivalent forms TO_FM_RATIONAL( 41, 314 ) or TO_FM_RATIONAL( '41', '314' ).

!  The TO_FM function from the basic FM package has been extended to convert a type FM_RATIONAL to
!  a type FM number.  The result is an approximation accurate to the current FM precision.

!  RATIONAL_APPROX(A, DIGITS) is a function that converts an FM number A to a rational approximation
!  of type FM_RATIONAL that has no more than DIGITS decimal digits in the top and bottom.
!  Ex:  A = pi, DIGITS = 2   returns the FM_RATIONAL result      22 /      7
!       A = pi, DIGITS = 3   returns the FM_RATIONAL result     355 /    113
!       A = pi, DIGITS = 6   returns the FM_RATIONAL result  833719 / 265381
!  The rational result usually approximates A to about 2*DIGITS significant digits, so
!  DIGITS should not be more than about half the precision carried for A.
!  Ex:  833719 / 265381 = 3.141592653581077771204419306..., and agrees with pi to about 11 s.d.


!  The standard Fortran functions that are available for FM_RATIONAL arguments are the ones that
!  give exact rational results.  So if A and B are type FM_RATIONAL variables, A**B, EXP(A), etc.,
!  are not provided since the results are not generally exact rational numbers.

!  But INT(A), FLOOR(A), MAX(A,B), MOD(A,B), etc., do give rational result and are provided.

!  AVAILABLE OPERATONS:

!     =
!     +
!     -
!     *
!     /
!     **                   A ** J is provided for FM_RATIONAL A and machine integer J.
!     ==
!     /=
!     <
!     <=
!     >
!     >=
!     ABS(A)
!     CEILING(A)
!     DIM(A,B)             Positive difference.  Returns A - B if A > B, zero if not.
!     FLOOR(A)
!     INT(A)
!     IS_UNKNOWN(A)        Returns true if A is unknown.
!     MAX(A,B,...)         Can have from 2 to 10 arguments.
!     MIN(A,B,...)         Can have from 2 to 10 arguments.
!     MOD(A,B)             Result is A - int(A/B) * B
!     MODULO(A,B)          Result is A - floor(A/B) * B
!     NINT(A)

!  Array operations for functions.

!     ABS, CEILING, FLOOR, INT, and NINT can each have a 1- or 2-dimensional array argument.
!     They will return a vector or matrix with the function applied to each element of the
!     argument.

!     DIM, MOD, MODULO work the same way, but the two array arguments for these functions must
!     have the same size and shape.

!     IS_UNKNOWN is a logical function that can be used with an array argument but does not return
!     an array result.  It returns "true" if any element of the input array is FM's special UNKNOWN
!     value, which comes from undefined operations such as dividing by zero.

!  Functions that operate only on arrays.

!     DOT_PRODUCT(X,Y)     Dot product of rank 1 vectors.
!     MATMUL(X,Y)          Matrix multiplication of arrays
!                          Cases for valid argument shapes:
!                          (1)  (n,m) * (m,k) --> (n,k)
!                          (2)    (m) * (m,k) --> (k)
!                          (3)  (n,m) * (m)   --> (n)
!     MAXVAL(X)            Maximum value in the array
!     MINVAL(X)            Minimum value in the array
!     PRODUCT(X)           Product of all values in the array
!     SUM(X)               Sum of all values in the array
!     TRANSPOSE(X)         Matrix transposition.  If X is a rank 2 array with shape (n,m), then
!                          Y = TRANSPOSE(X) has shape (m,n) with Y(i,j) = X(j,i).

    USE FMZM

    TYPE FM_RATIONAL
         TYPE(MULTI) :: NUMERATOR
         TYPE(MULTI) :: DENOMINATOR
    END TYPE

!             Work variables for derived type operations.

    TYPE (IM), SAVE :: R_1, R_2, R_3, R_4, R_5, R_6
    TYPE (FM), SAVE :: F_1, F_2
    TYPE (FM_RATIONAL), SAVE :: MT_RM, MU_RM
    INTEGER, SAVE :: RATIONAL_EXP_MAX = 0, RATIONAL_SKIP_MAX = 100
    LOGICAL, SAVE :: SKIP_GCD = .FALSE.

   INTERFACE TO_FM_RATIONAL
      MODULE PROCEDURE FM_RATIONAL_I
      MODULE PROCEDURE FM_RATIONAL_II
      MODULE PROCEDURE FM_RATIONAL_I1
      MODULE PROCEDURE FM_RATIONAL_I2
      MODULE PROCEDURE FM_RATIONAL_IM
      MODULE PROCEDURE FM_RATIONAL_IMIM
      MODULE PROCEDURE FM_RATIONAL_IM1
      MODULE PROCEDURE FM_RATIONAL_IM2
      MODULE PROCEDURE FM_RATIONAL_ST
      MODULE PROCEDURE FM_RATIONAL_STST
   END INTERFACE

   INTERFACE FM_UNDEF_INP
      MODULE PROCEDURE FM_UNDEF_INP_RATIONAL_RM0
      MODULE PROCEDURE FM_UNDEF_INP_RATIONAL_RM1
      MODULE PROCEDURE FM_UNDEF_INP_RATIONAL_RM2
   END INTERFACE

   INTERFACE RATIONAL_NUMERATOR
      MODULE PROCEDURE RATIONAL_NUMERATOR_IM
   END INTERFACE

   INTERFACE RATIONAL_DENOMINATOR
      MODULE PROCEDURE RATIONAL_DENOMINATOR_IM
   END INTERFACE

   INTERFACE ASSIGNMENT (=)
       MODULE PROCEDURE FMEQ_RATIONAL_RMRM
       MODULE PROCEDURE FMEQ_RATIONAL_RMI
       MODULE PROCEDURE FMEQ_RATIONAL_RMIM

       MODULE PROCEDURE FMEQ_RATIONAL_RM1RM
       MODULE PROCEDURE FMEQ_RATIONAL_RM1RM1
       MODULE PROCEDURE FMEQ_RATIONAL_RM1I
       MODULE PROCEDURE FMEQ_RATIONAL_RM1I1
       MODULE PROCEDURE FMEQ_RATIONAL_RM1IM
       MODULE PROCEDURE FMEQ_RATIONAL_RM1IM1

       MODULE PROCEDURE FMEQ_RATIONAL_RM2RM
       MODULE PROCEDURE FMEQ_RATIONAL_RM2RM2
       MODULE PROCEDURE FMEQ_RATIONAL_RM2I
       MODULE PROCEDURE FMEQ_RATIONAL_RM2I2
       MODULE PROCEDURE FMEQ_RATIONAL_RM2IM
       MODULE PROCEDURE FMEQ_RATIONAL_RM2IM2
   END INTERFACE

   CONTAINS


!                                                      TO_FM_RATIONAL

   FUNCTION FM_RATIONAL_I(TOP)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: TOP,N1,N2
      INTENT (IN) :: TOP
      IF (TOP == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      N1 = ABS(TOP)
      N2 = 1
      IF (TOP > 0) THEN
          CALL IMI2M(N1,RETURN_VALUE%NUMERATOR)
      ELSE
          CALL IMI2M(-N1,RETURN_VALUE%NUMERATOR)
      ENDIF
      CALL IMI2M(N2,RETURN_VALUE%DENOMINATOR)
      CALL FM_MAX_EXP_RM(RETURN_VALUE)
   END FUNCTION FM_RATIONAL_I

   FUNCTION FM_RATIONAL_II(TOP,BOT)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: TOP,BOT,N1,N2
      INTENT (IN) :: TOP,BOT
      IF (BOT == 0) THEN
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      IF (TOP == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      N1 = ABS(TOP)
      N2 = ABS(BOT)
      CALL FMGCDI(N1,N2)
      IF ((TOP > 0 .AND. BOT > 0) .OR. (TOP < 0 .AND. BOT < 0)) THEN
          CALL IMI2M(N1,RETURN_VALUE%NUMERATOR)
      ELSE
          CALL IMI2M(-N1,RETURN_VALUE%NUMERATOR)
      ENDIF
      CALL IMI2M(N2,RETURN_VALUE%DENOMINATOR)
      CALL FM_MAX_EXP_RM(RETURN_VALUE)
   END FUNCTION FM_RATIONAL_II

   FUNCTION FM_RATIONAL_I1(IVAL)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: IVAL
      TYPE (FM_RATIONAL), DIMENSION(SIZE(IVAL)) :: RETURN_VALUE
      INTEGER :: J
      DO J = 1, SIZE(IVAL)
         CALL IMI2M(IVAL(J),RETURN_VALUE(J)%NUMERATOR)
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
         CALL FM_MAX_EXP_RM(RETURN_VALUE(J))
      ENDDO
   END FUNCTION FM_RATIONAL_I1

   FUNCTION FM_RATIONAL_I2(IVAL)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: IVAL
      TYPE (FM_RATIONAL), DIMENSION(SIZE(IVAL,DIM=1),SIZE(IVAL,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      DO J = 1, SIZE(IVAL,DIM=1)
         DO K = 1, SIZE(IVAL,DIM=2)
            CALL IMI2M(IVAL(J,K),RETURN_VALUE(J,K)%NUMERATOR)
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
            CALL FM_MAX_EXP_RM(RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FM_RATIONAL_I2

   FUNCTION FM_RATIONAL_IM(TOP)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: R_SIGN
      TYPE (IM) :: TOP
      INTENT (IN) :: TOP
      CALL FM_UNDEF_INP(TOP)
      CALL IMEQ(TOP%MIM,R_1%MIM)
      CALL IMI2M(1,R_2%MIM)
      IF (IS_UNKNOWN(R_1) .OR. IS_OVERFLOW(R_1)) THEN
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      IF (R_1 == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      R_SIGN = 1
      IF (R_1 < 0) THEN
          R_SIGN = -1
      ENDIF
      CALL IM_ABS(R_1,R_4)
      CALL IM_ABS(R_2,R_5)
      CALL FM_MAX_EXP_IM(R_4,R_5)
      IF (SKIP_GCD .AND. MAX(R_4%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          IF (R_SIGN == -1) R_4%MIM%MP(1) = -1
          CALL IMEQ(R_4%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_5%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_4,R_5,R_3)
          CALL IM_DIV(R_4,R_3,R_1)
          CALL IM_DIV(R_5,R_3,R_2)
          IF (R_SIGN == -1) R_1%MIM%MP(1) = -1
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_2%MIM,RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FM_RATIONAL_IM

   FUNCTION FM_RATIONAL_IMIM(TOP,BOT)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: R_SIGN
      TYPE (IM) :: TOP,BOT
      INTENT (IN) :: TOP,BOT
      CALL FM_UNDEF_INP(TOP)
      CALL FM_UNDEF_INP(BOT)
      CALL IMEQ(TOP%MIM,R_1%MIM)
      CALL IMEQ(BOT%MIM,R_2%MIM)
      IF (R_2 == 0 .OR. IS_UNKNOWN(R_1) .OR. IS_OVERFLOW(R_1) .OR.  &
                        IS_UNKNOWN(R_2) .OR. IS_OVERFLOW(R_2) ) THEN
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      IF (R_1 == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      R_SIGN = 1
      IF ((R_1 > 0 .AND. R_2 < 0) .OR. (R_1 < 0 .AND. R_2 > 0)) THEN
          R_SIGN = -1
      ENDIF
      CALL IM_ABS(R_1,R_4)
      CALL IM_ABS(R_2,R_5)
      CALL FM_MAX_EXP_IM(R_4,R_5)
      IF (SKIP_GCD .AND. MAX(R_4%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          IF (R_SIGN == -1) R_4%MIM%MP(1) = -1
          CALL IMEQ(R_4%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_5%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_4,R_5,R_3)
          CALL IM_DIV(R_4,R_3,R_1)
          CALL IM_DIV(R_5,R_3,R_2)
          IF (R_SIGN == -1) R_1%MIM%MP(1) = -1
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_2%MIM,RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FM_RATIONAL_IMIM

   FUNCTION FM_RATIONAL_IM1(TOP)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: TOP
      TYPE (FM_RATIONAL), DIMENSION(SIZE(TOP)) :: RETURN_VALUE
      INTEGER :: J
      DO J = 1, SIZE(TOP)
         CALL IMEQ(TOP(J)%MIM,RETURN_VALUE(J)%NUMERATOR)
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
         CALL FM_MAX_EXP_RM(RETURN_VALUE(J))
      ENDDO
   END FUNCTION FM_RATIONAL_IM1

   FUNCTION FM_RATIONAL_IM2(TOP)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: TOP
      TYPE (FM_RATIONAL), DIMENSION(SIZE(TOP,DIM=1),SIZE(TOP,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      DO J = 1, SIZE(TOP,DIM=1)
         DO K = 1, SIZE(TOP,DIM=2)
            CALL IMEQ(TOP(J,K)%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
            CALL FM_MAX_EXP_RM(RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FM_RATIONAL_IM2

   FUNCTION FM_RATIONAL_ST(TOP)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: K,R_SIGN
      CHARACTER(*) :: TOP
      INTENT (IN) :: TOP
      K = INDEX(TOP,'/')
      IF (K > 0) THEN
          CALL IMST2M(TOP(1:K-1),R_1%MIM)
          CALL IMST2M(TOP(K+1:LEN(TOP)),R_2%MIM)
      ELSE
          CALL IMST2M(TOP,R_1%MIM)
          CALL IMI2M(1,R_2%MIM)
      ENDIF
      IF (R_2 == 0 .OR. IS_UNKNOWN(R_1) .OR. IS_OVERFLOW(R_1) .OR.  &
                        IS_UNKNOWN(R_2) .OR. IS_OVERFLOW(R_2) ) THEN
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      IF (R_1 == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      R_SIGN = 1
      IF ((R_1 > 0 .AND. R_2 < 0) .OR. (R_1 < 0 .AND. R_2 > 0)) THEN
          R_SIGN = -1
      ENDIF
      CALL IM_ABS(R_1,R_4)
      CALL IM_ABS(R_2,R_5)
      CALL FM_MAX_EXP_IM(R_4,R_5)
      IF (SKIP_GCD .AND. MAX(R_4%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          IF (R_SIGN == -1) R_4%MIM%MP(1) = -1
          CALL IMEQ(R_4%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_5%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_4,R_5,R_3)
          CALL IM_DIV(R_4,R_3,R_1)
          CALL IM_DIV(R_5,R_3,R_2)
          IF (R_SIGN == -1) R_1%MIM%MP(1) = -1
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_2%MIM,RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FM_RATIONAL_ST

   FUNCTION FM_RATIONAL_STST(TOP,BOT)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      INTEGER :: R_SIGN
      CHARACTER(*) :: TOP,BOT
      INTENT (IN) :: TOP,BOT
      CALL IMST2M(TOP,R_1%MIM)
      CALL IMST2M(BOT,R_2%MIM)
      IF (R_2 == 0 .OR. IS_UNKNOWN(R_1) .OR. IS_OVERFLOW(R_1) .OR.  &
                        IS_UNKNOWN(R_2) .OR. IS_OVERFLOW(R_2) ) THEN
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      IF (R_1 == 0) THEN
          CALL IMST2M('0',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('1',RETURN_VALUE%DENOMINATOR)
          RETURN
      ENDIF
      R_SIGN = 1
      IF ((R_1 > 0 .AND. R_2 < 0) .OR. (R_1 < 0 .AND. R_2 > 0)) THEN
          R_SIGN = -1
      ENDIF
      CALL IM_ABS(R_1,R_4)
      CALL IM_ABS(R_2,R_5)
      CALL FM_MAX_EXP_IM(R_4,R_5)
      IF (SKIP_GCD .AND. MAX(R_4%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          IF (R_SIGN == -1) R_4%MIM%MP(1) = -1
          CALL IMEQ(R_4%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_5%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_4,R_5,R_3)
          CALL IM_DIV(R_4,R_3,R_1)
          CALL IM_DIV(R_5,R_3,R_2)
          IF (R_SIGN == -1) R_1%MIM%MP(1) = -1
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMEQ(R_2%MIM,RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FM_RATIONAL_STST

!                                                               RATIONAL_NUMERATOR

   FUNCTION RATIONAL_NUMERATOR_IM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM) :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL IMEQ(MA%NUMERATOR,RETURN_VALUE%MIM)
   END FUNCTION RATIONAL_NUMERATOR_IM

!                                                               RATIONAL_DENOMINATOR

   FUNCTION RATIONAL_DENOMINATOR_IM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM) :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL IMEQ(MA%DENOMINATOR,RETURN_VALUE%MIM)
   END FUNCTION RATIONAL_DENOMINATOR_IM

   SUBROUTINE FM_MAX_EXP_RM(MA)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: NT, NB
      NT = MA%NUMERATOR%MP(2)
      NB = MA%DENOMINATOR%MP(2)
      IF (NT < MEXPOV .AND. NT > RATIONAL_EXP_MAX) RATIONAL_EXP_MAX = NT
      IF (NB < MEXPOV .AND. NB > RATIONAL_EXP_MAX) RATIONAL_EXP_MAX = NB
      END SUBROUTINE FM_MAX_EXP_RM

   SUBROUTINE FM_MAX_EXP_IM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA,MB
      INTEGER :: NT
      NT = MA%MIM%MP(2)
      IF (NT < MEXPOV .AND. NT > RATIONAL_EXP_MAX) RATIONAL_EXP_MAX = NT
      NT = MB%MIM%MP(2)
      IF (NT < MEXPOV .AND. NT > RATIONAL_EXP_MAX) RATIONAL_EXP_MAX = NT
      END SUBROUTINE FM_MAX_EXP_IM

!                                                               FM_PRINT_RATIONAL

   SUBROUTINE FM_PRINT_RATIONAL(MA)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      CHARACTER(100) :: ST1, ST2
      CHARACTER(203) :: STR
      INTENT (IN) :: MA
      INTEGER :: J,KPT

!             If the top and bottom integers can be printed on one line, as  12 / 7
!             in fewer than KSWIDE characters, do it.  Otherwise call IMPRINT twice.

      CALL IMABS(MA%NUMERATOR,R_1%MIM)
      CALL IMABS(MA%DENOMINATOR,R_2%MIM)
      CALL IMMPY(R_1%MIM,R_2%MIM,R_3%MIM)

      IF (TO_IM(10)**(KSWIDE-11) > R_3 .AND. R_1 < TO_IM('1E+99') .AND. R_2 < TO_IM('1E+99')) THEN
          CALL IMFORM('I100',MA%NUMERATOR,ST1)
          CALL IMFORM('I100',MA%DENOMINATOR,ST2)
          STR = ' '
          KPT = 0
          DO J = 1, 100
             IF (ST1(J:J) /= ' ') THEN
                 KPT = KPT + 1
                 STR(KPT:KPT) = ST1(J:J)
             ENDIF
          ENDDO
          STR(KPT+1:KPT+3) = ' / '
          KPT = KPT + 3
          DO J = 1, 100
             IF (ST2(J:J) /= ' ') THEN
                 KPT = KPT + 1
                 STR(KPT:KPT) = ST2(J:J)
             ENDIF
          ENDDO
          IF (MA%NUMERATOR%MP(1) < 0) THEN
              WRITE (KW,"(6X,A)") STR(1:KPT)
          ELSE
              WRITE (KW,"(7X,A)") STR(1:KPT)
          ENDIF
      ELSE
          CALL IMPRINT(MA%NUMERATOR)
          WRITE (KW,"(A)") '    /'
          CALL IMPRINT(MA%DENOMINATOR)
      ENDIF
   END SUBROUTINE FM_PRINT_RATIONAL


   SUBROUTINE FM_UNDEF_INP_RATIONAL_RM0(MA)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTENT (IN) :: MA
      IF (.NOT. ALLOCATED(MA%NUMERATOR%MP)) CALL FM_INPUT_ERROR
      IF (.NOT. ALLOCATED(MA%DENOMINATOR%MP)) CALL FM_INPUT_ERROR
   END SUBROUTINE FM_UNDEF_INP_RATIONAL_RM0

   SUBROUTINE FM_UNDEF_INP_RATIONAL_RM1(MA)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: J
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA)
         IF (.NOT. ALLOCATED(MA(J)%NUMERATOR%MP)) CALL FM_INPUT_ERROR1(J)
         IF (.NOT. ALLOCATED(MA(J)%DENOMINATOR%MP)) CALL FM_INPUT_ERROR1(J)
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_RATIONAL_RM1

   SUBROUTINE FM_UNDEF_INP_RATIONAL_RM2(MA)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: J,K
      INTENT (IN) :: MA
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (.NOT. ALLOCATED(MA(J,K)%NUMERATOR%MP)) CALL FM_INPUT_ERROR2(J,K)
            IF (.NOT. ALLOCATED(MA(J,K)%DENOMINATOR%MP)) CALL FM_INPUT_ERROR2(J,K)
         ENDDO
      ENDDO
   END SUBROUTINE FM_UNDEF_INP_RATIONAL_RM2

   SUBROUTINE FMEQ_RATIONAL(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA
      INTENT (INOUT) :: MB
      CALL IMEQ(MA%NUMERATOR,MB%NUMERATOR)
      CALL IMEQ(MA%DENOMINATOR,MB%DENOMINATOR)
   END SUBROUTINE FMEQ_RATIONAL

!                                                                                        =

   SUBROUTINE FMEQ_RATIONAL_RMRM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FM_UNDEF_INP(MB)
      CALL FMEQ_RATIONAL(MB,MA)
      CALL FM_MAX_EXP_RM(MA)
   END SUBROUTINE FMEQ_RATIONAL_RMRM

   SUBROUTINE FMEQ_RATIONAL_RMI(MA,IVAL)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: IVAL
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      CALL IMI2M(IVAL,MA%NUMERATOR)
      CALL IMI2M(1,MA%DENOMINATOR)
      CALL FM_MAX_EXP_RM(MA)
   END SUBROUTINE FMEQ_RATIONAL_RMI

   SUBROUTINE FMEQ_RATIONAL_RMIM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM) :: MB
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%MIM,MA%NUMERATOR)
      CALL IMI2M(1,MA%DENOMINATOR)
      CALL FM_MAX_EXP_RM(MA)
   END SUBROUTINE FMEQ_RATIONAL_RMIM

!             Array equal assignments for RM.

!             (1) rank 1  =  rank 0

   SUBROUTINE FMEQ_RATIONAL_RM1I(MA,IVAL)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: IVAL,J
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL_RMI(MA(J),IVAL)
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM1I

   SUBROUTINE FMEQ_RATIONAL_RM1RM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: J
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMEQ_RATIONAL_RMRM(MT_RM,MB)
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL_RMRM(MA(J),MT_RM)
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM1RM

   SUBROUTINE FMEQ_RATIONAL_RM1IM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM) :: MB
      INTEGER :: J
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL IMEQ(MB%MIM,R_1%MIM)
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL_RMIM(MA(J),R_1)
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM1IM

!             (2) rank 1  =  rank 1

   SUBROUTINE FMEQ_RATIONAL_RM1I1(MA,IVAL)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER, DIMENSION(:) :: IVAL
      INTEGER :: J
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      IF (SIZE(MA) /= SIZE(IVAL)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,MA(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,MA(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL_RMI(MA(J),IVAL(J))
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM1I1

   SUBROUTINE FMEQ_RATIONAL_RM1RM1(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), ALLOCATABLE, DIMENSION(:) :: TEMP
      INTEGER :: J,N
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,MA(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,MA(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      N = SIZE(MA)

!             To avoid problems when lhs and rhs are overlapping parts of the same array, move MB
!             to a temporary array before re-defining any of MA.

      ALLOCATE(TEMP(N))
      DO J = 1, N
         CALL IMEQ(MB(J)%NUMERATOR,TEMP(J)%NUMERATOR)
         CALL IMEQ(MB(J)%DENOMINATOR,TEMP(J)%DENOMINATOR)
      ENDDO
      DO J = 1, N
         CALL FMEQ_RATIONAL_RMRM(MA(J),TEMP(J))
      ENDDO
      DEALLOCATE(TEMP)
   END SUBROUTINE FMEQ_RATIONAL_RM1RM1

   SUBROUTINE FMEQ_RATIONAL_RM1IM1(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (IM), ALLOCATABLE, DIMENSION(:) :: TEMP
      INTEGER :: J,N
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,MA(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,MA(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      N = SIZE(MA)
      ALLOCATE(TEMP(N))
      DO J = 1, SIZE(MA)
         CALL IMEQ(MB(J)%MIM,TEMP(J)%MIM)
      ENDDO
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL_RMIM(MA(J),TEMP(J))
      ENDDO
      DEALLOCATE(TEMP)
   END SUBROUTINE FMEQ_RATIONAL_RM1IM1

!             (3) rank 2  =  rank 0

   SUBROUTINE FMEQ_RATIONAL_RM2I(MA,IVAL)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: IVAL,J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMI(MA(J,K),IVAL)
         ENDDO
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM2I

   SUBROUTINE FMEQ_RATIONAL_RM2RM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FMEQ_RATIONAL_RMRM(MT_RM,MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMRM(MA(J,K),MT_RM)
         ENDDO
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM2RM

   SUBROUTINE FMEQ_RATIONAL_RM2IM(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM) :: MB
      INTEGER :: J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL IMEQ(MB%MIM,R_1%MIM)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMIM(MA(J,K),R_1)
         ENDDO
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM2IM

!             (4) rank 2  =  rank 2

   SUBROUTINE FMEQ_RATIONAL_RM2I2(MA,IVAL)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(:,:) :: IVAL
      INTEGER :: J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: IVAL
      IF (SIZE(MA,DIM=1) /= SIZE(IVAL,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(IVAL,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,MA(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,MA(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMI(MA(J,K),IVAL(J,K))
         ENDDO
      ENDDO
   END SUBROUTINE FMEQ_RATIONAL_RM2I2

   SUBROUTINE FMEQ_RATIONAL_RM2RM2(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), ALLOCATABLE, DIMENSION(:,:) :: TEMP
      INTEGER :: J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,MA(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,MA(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF

!             To avoid problems when lhs and rhs are overlapping parts of the same array, move MB
!             to a temporary array before re-defining any of MA.

      ALLOCATE(TEMP(SIZE(MA,DIM=1),SIZE(MA,DIM=2)))
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMEQ(MB(J,K)%NUMERATOR,TEMP(J,K)%NUMERATOR)
            CALL IMEQ(MB(J,K)%DENOMINATOR,TEMP(J,K)%DENOMINATOR)
         ENDDO
      ENDDO
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMRM(MA(J,K),TEMP(J,K))
         ENDDO
      ENDDO
      DEALLOCATE(TEMP)
   END SUBROUTINE FMEQ_RATIONAL_RM2RM2

   SUBROUTINE FMEQ_RATIONAL_RM2IM2(MA,MB)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (IM), ALLOCATABLE, DIMENSION(:,:) :: TEMP
      INTEGER :: J,K
      INTENT (INOUT) :: MA
      INTENT (IN) :: MB
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,MA(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,MA(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      ALLOCATE(TEMP(SIZE(MA,DIM=1),SIZE(MA,DIM=2)))
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMEQ(MB(J,K)%MIM,TEMP(J,K)%MIM)
         ENDDO
      ENDDO
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL_RMIM(MA(J,K),TEMP(J,K))
         ENDDO
      ENDDO
      DEALLOCATE(TEMP)
   END SUBROUTINE FMEQ_RATIONAL_RM2IM2

   SUBROUTINE FMADD_RATIONAL_RMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      CALL IM_ADD(R_1,R_5,R_3)
      CALL IM_MPY(R_2,R_4,R_5)
      CALL FM_MAX_EXP_IM(R_3,R_5)
      IF (SKIP_GCD .AND. MAX(R_3%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_3%MIM,MC%NUMERATOR)
          CALL IMEQ(R_5%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_3,R_5,R_1)
          CALL IMDIV(R_3%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_5%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMADD_RATIONAL_RMRM_0

   SUBROUTINE FMSUB_RATIONAL_RMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      CALL IM_SUB(R_5,R_1,R_3)
      CALL IM_MPY(R_2,R_4,R_5)
      CALL FM_MAX_EXP_IM(R_3,R_5)
      IF (SKIP_GCD .AND. MAX(R_3%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_3%MIM,MC%NUMERATOR)
          CALL IMEQ(R_5%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_3,R_5,R_1)
          CALL IMDIV(R_3%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_5%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMSUB_RATIONAL_RMRM_0

   SUBROUTINE FMMPY_RATIONAL_RMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      INTEGER :: IDIV
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_3,R_5)
      CALL IM_MPY(R_2,R_4,R_3)
      CALL FM_MAX_EXP_IM(R_3,R_5)
      IF (SKIP_GCD .AND. MAX(R_3%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_5%MIM,MC%NUMERATOR)
          CALL IMEQ(R_3%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_3,R_5,R_1)
          IF (R_1%MIM%MP(2) == 1 .AND. R_1%MIM%MP(3) == 1) THEN
              CALL IMEQ(R_5%MIM,MC%NUMERATOR)
              CALL IMEQ(R_3%MIM,MC%DENOMINATOR)
          ELSE IF (R_1%MIM%MP(2) == 1 .AND. R_1%MIM%MP(3) < MXBASE) THEN
              IDIV = R_1%MIM%MP(3)
              CALL IMDIVI(R_5%MIM,IDIV,MC%NUMERATOR)
              CALL IMDIVI(R_3%MIM,IDIV,MC%DENOMINATOR)
          ELSE
              CALL IMDIV(R_5%MIM,R_1%MIM,MC%NUMERATOR)
              CALL IMDIV(R_3%MIM,R_1%MIM,MC%DENOMINATOR)
          ENDIF
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMMPY_RATIONAL_RMRM_0

   SUBROUTINE FMDIV_RATIONAL_RMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (MA%NUMERATOR%MP(2) == MUNKNO .OR. MA%DENOMINATOR%MP(2) == MUNKNO .OR.  &
          MB%NUMERATOR%MP(2) == MUNKNO .OR. MB%DENOMINATOR%MP(2) == MUNKNO .OR.  &
          MB%NUMERATOR%MP(3) == 0 ) THEN
          CALL IMST2M('UNKNOWN',MC%NUMERATOR)
          CALL IMST2M('UNKNOWN',MC%DENOMINATOR)
          RETURN
      ENDIF
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_4,R_5)
      IF (SKIP_GCD .AND. MAX(R_4%MIM%MP(2),R_5%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_5%MIM,MC%NUMERATOR)
          CALL IMEQ(R_4%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_4,R_5,R_1)
          CALL IMDIV(R_5%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMDIV_RATIONAL_RMRM_0

 END MODULE FM_RATIONAL_ARITHMETIC_1


 MODULE FM_RATIONAL_ARITHMETIC_2
    USE FM_RATIONAL_ARITHMETIC_1

   INTERFACE OPERATOR (+)
       MODULE PROCEDURE FMADD_RATIONAL_RM
       MODULE PROCEDURE FMADD_RATIONAL_RM1
       MODULE PROCEDURE FMADD_RATIONAL_RM2

       MODULE PROCEDURE FMADD_RATIONAL_RMRM
       MODULE PROCEDURE FMADD_RATIONAL_IRM
       MODULE PROCEDURE FMADD_RATIONAL_RMI
       MODULE PROCEDURE FMADD_RATIONAL_IMRM
       MODULE PROCEDURE FMADD_RATIONAL_RMIM

       MODULE PROCEDURE FMADD_RATIONAL_RM1RM1
       MODULE PROCEDURE FMADD_RATIONAL_RM1RM
       MODULE PROCEDURE FMADD_RATIONAL_RMRM1
       MODULE PROCEDURE FMADD_RATIONAL_I1RM1
       MODULE PROCEDURE FMADD_RATIONAL_RM1I1
       MODULE PROCEDURE FMADD_RATIONAL_I1RM
       MODULE PROCEDURE FMADD_RATIONAL_RMI1
       MODULE PROCEDURE FMADD_RATIONAL_IRM1
       MODULE PROCEDURE FMADD_RATIONAL_RM1I
       MODULE PROCEDURE FMADD_RATIONAL_IM1RM1
       MODULE PROCEDURE FMADD_RATIONAL_IM1RM
       MODULE PROCEDURE FMADD_RATIONAL_IMRM1
       MODULE PROCEDURE FMADD_RATIONAL_RM1IM1
       MODULE PROCEDURE FMADD_RATIONAL_RM1IM
       MODULE PROCEDURE FMADD_RATIONAL_RMIM1

       MODULE PROCEDURE FMADD_RATIONAL_RM2RM2
       MODULE PROCEDURE FMADD_RATIONAL_RM2RM
       MODULE PROCEDURE FMADD_RATIONAL_RMRM2
       MODULE PROCEDURE FMADD_RATIONAL_I2RM2
       MODULE PROCEDURE FMADD_RATIONAL_I2RM
       MODULE PROCEDURE FMADD_RATIONAL_IRM2
       MODULE PROCEDURE FMADD_RATIONAL_RM2I2
       MODULE PROCEDURE FMADD_RATIONAL_RM2I
       MODULE PROCEDURE FMADD_RATIONAL_RMI2
       MODULE PROCEDURE FMADD_RATIONAL_IM2RM2
       MODULE PROCEDURE FMADD_RATIONAL_IM2RM
       MODULE PROCEDURE FMADD_RATIONAL_IMRM2
       MODULE PROCEDURE FMADD_RATIONAL_RM2IM2
       MODULE PROCEDURE FMADD_RATIONAL_RM2IM
       MODULE PROCEDURE FMADD_RATIONAL_RMIM2
   END INTERFACE

   CONTAINS

!                                                                                        +

   FUNCTION FMADD_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMEQ_RATIONAL(MA,RETURN_VALUE)
      IF (RETURN_VALUE%DENOMINATOR%MP(1) < 0) THEN
          RETURN_VALUE%DENOMINATOR%MP(1) = 1
          RETURN_VALUE%NUMERATOR%MP(1) = -RETURN_VALUE%NUMERATOR%MP(1)
      ENDIF
   END FUNCTION FMADD_RATIONAL_RM

   FUNCTION FMADD_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL(MA(J),RETURN_VALUE(J))
         IF (RETURN_VALUE(J)%DENOMINATOR%MP(1) < 0) THEN
             RETURN_VALUE(J)%DENOMINATOR%MP(1) = 1
             RETURN_VALUE(J)%NUMERATOR%MP(1) =  &
             -RETURN_VALUE(J)%NUMERATOR%MP(1)
         ENDIF
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1

   FUNCTION FMADD_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL(MA(J,K),RETURN_VALUE(J,K))
            IF (RETURN_VALUE(J,K)%DENOMINATOR%MP(1) < 0) THEN
                RETURN_VALUE(J,K)%DENOMINATOR%MP(1) = 1
                RETURN_VALUE(J,K)%NUMERATOR%MP(1) =  &
                -RETURN_VALUE(J,K)%NUMERATOR%MP(1)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2

   FUNCTION FMADD_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FMADD_RATIONAL_RMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMADD_RATIONAL_RMRM

   SUBROUTINE FMADD_RATIONAL_RMI_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_ADD(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMADD_RATIONAL_RMI_0

   FUNCTION FMADD_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FMADD_RATIONAL_RMI_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMADD_RATIONAL_RMI

   SUBROUTINE FMADD_RATIONAL_IRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_ADD(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMADD_RATIONAL_IRM_0

   FUNCTION FMADD_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FMADD_RATIONAL_IRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMADD_RATIONAL_IRM

   SUBROUTINE FMADD_RATIONAL_RMIM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_ADD(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMADD_RATIONAL_RMIM_0

   FUNCTION FMADD_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FMADD_RATIONAL_RMIM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMADD_RATIONAL_RMIM

   SUBROUTINE FMADD_RATIONAL_IMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_ADD(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMADD_RATIONAL_IMRM_0

   FUNCTION FMADD_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FMADD_RATIONAL_IMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMADD_RATIONAL_IMRM

   FUNCTION FMADD_RATIONAL_RM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1RM1

   FUNCTION FMADD_RATIONAL_RM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1RM

   FUNCTION FMADD_RATIONAL_RMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMADD_RATIONAL_RMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMRM1

   FUNCTION FMADD_RATIONAL_I1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_IRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_I1RM1

   FUNCTION FMADD_RATIONAL_RM1I1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMI_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1I1

   FUNCTION FMADD_RATIONAL_IRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMADD_RATIONAL_IRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_IRM1

   FUNCTION FMADD_RATIONAL_RM1I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMI_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1I

   FUNCTION FMADD_RATIONAL_RMI1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB)
         CALL FMADD_RATIONAL_RMI_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMI1

   FUNCTION FMADD_RATIONAL_I1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_IRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_I1RM

   FUNCTION FMADD_RATIONAL_IM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_IMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_IM1RM1

   FUNCTION FMADD_RATIONAL_RM1IM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMIM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1IM1

   FUNCTION FMADD_RATIONAL_IMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMADD_RATIONAL_IMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_IMRM1

   FUNCTION FMADD_RATIONAL_RM1IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_RMIM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM1IM

   FUNCTION FMADD_RATIONAL_RMIM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMADD_RATIONAL_RMIM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMIM1

   FUNCTION FMADD_RATIONAL_IM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMADD_RATIONAL_IMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMADD_RATIONAL_IM1RM

   FUNCTION FMADD_RATIONAL_RM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2RM2

   FUNCTION FMADD_RATIONAL_RM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2RM

   FUNCTION FMADD_RATIONAL_RMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMADD_RATIONAL_RMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMRM2

   FUNCTION FMADD_RATIONAL_I2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_IRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_I2RM2

   FUNCTION FMADD_RATIONAL_RM2I2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMI_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2I2

   FUNCTION FMADD_RATIONAL_IRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMADD_RATIONAL_IRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_IRM2

   FUNCTION FMADD_RATIONAL_RM2I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMI_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2I

   FUNCTION FMADD_RATIONAL_RMI2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMADD_RATIONAL_RMI_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMI2

   FUNCTION FMADD_RATIONAL_I2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_IRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_I2RM

   FUNCTION FMADD_RATIONAL_IM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_IMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_IM2RM2

   FUNCTION FMADD_RATIONAL_RM2IM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMIM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2IM2

   FUNCTION FMADD_RATIONAL_IMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMADD_RATIONAL_IMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_IMRM2

   FUNCTION FMADD_RATIONAL_RM2IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_RMIM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RM2IM

   FUNCTION FMADD_RATIONAL_RMIM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMADD_RATIONAL_RMIM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_RMIM2

   FUNCTION FMADD_RATIONAL_IM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMADD_RATIONAL_IMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMADD_RATIONAL_IM2RM


 END MODULE FM_RATIONAL_ARITHMETIC_2


 MODULE FM_RATIONAL_ARITHMETIC_3
    USE FM_RATIONAL_ARITHMETIC_1

   INTERFACE OPERATOR (-)
       MODULE PROCEDURE FMSUB_RATIONAL_RM
       MODULE PROCEDURE FMSUB_RATIONAL_RM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM2

       MODULE PROCEDURE FMSUB_RATIONAL_RMRM
       MODULE PROCEDURE FMSUB_RATIONAL_IRM
       MODULE PROCEDURE FMSUB_RATIONAL_RMI
       MODULE PROCEDURE FMSUB_RATIONAL_IMRM
       MODULE PROCEDURE FMSUB_RATIONAL_RMIM

       MODULE PROCEDURE FMSUB_RATIONAL_RM1RM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM1RM
       MODULE PROCEDURE FMSUB_RATIONAL_RMRM1
       MODULE PROCEDURE FMSUB_RATIONAL_I1RM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM1I1
       MODULE PROCEDURE FMSUB_RATIONAL_I1RM
       MODULE PROCEDURE FMSUB_RATIONAL_RMI1
       MODULE PROCEDURE FMSUB_RATIONAL_IRM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM1I
       MODULE PROCEDURE FMSUB_RATIONAL_IM1RM1
       MODULE PROCEDURE FMSUB_RATIONAL_IM1RM
       MODULE PROCEDURE FMSUB_RATIONAL_IMRM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM1IM1
       MODULE PROCEDURE FMSUB_RATIONAL_RM1IM
       MODULE PROCEDURE FMSUB_RATIONAL_RMIM1

       MODULE PROCEDURE FMSUB_RATIONAL_RM2RM2
       MODULE PROCEDURE FMSUB_RATIONAL_RM2RM
       MODULE PROCEDURE FMSUB_RATIONAL_RMRM2
       MODULE PROCEDURE FMSUB_RATIONAL_I2RM2
       MODULE PROCEDURE FMSUB_RATIONAL_I2RM
       MODULE PROCEDURE FMSUB_RATIONAL_IRM2
       MODULE PROCEDURE FMSUB_RATIONAL_RM2I2
       MODULE PROCEDURE FMSUB_RATIONAL_RM2I
       MODULE PROCEDURE FMSUB_RATIONAL_RMI2
       MODULE PROCEDURE FMSUB_RATIONAL_IM2RM2
       MODULE PROCEDURE FMSUB_RATIONAL_IM2RM
       MODULE PROCEDURE FMSUB_RATIONAL_IMRM2
       MODULE PROCEDURE FMSUB_RATIONAL_RM2IM2
       MODULE PROCEDURE FMSUB_RATIONAL_RM2IM
       MODULE PROCEDURE FMSUB_RATIONAL_RMIM2
   END INTERFACE

   CONTAINS

!                                                                                        -

   FUNCTION FMSUB_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FMEQ_RATIONAL(MA,RETURN_VALUE)
      RETURN_VALUE%NUMERATOR%MP(1) = -RETURN_VALUE%NUMERATOR%MP(1)
      IF (RETURN_VALUE%DENOMINATOR%MP(1) < 0) THEN
          RETURN_VALUE%DENOMINATOR%MP(1) = 1
          RETURN_VALUE%NUMERATOR%MP(1) = -RETURN_VALUE%NUMERATOR%MP(1)
      ENDIF
   END FUNCTION FMSUB_RATIONAL_RM

   FUNCTION FMSUB_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMEQ_RATIONAL(MA(J),RETURN_VALUE(J))
         IF (RETURN_VALUE(J)%NUMERATOR%MP(2) /= MUNKNO .AND.  &
             RETURN_VALUE(J)%NUMERATOR%MP(3) /= 0) THEN
             RETURN_VALUE(J)%NUMERATOR%MP(1) =  &
             -RETURN_VALUE(J)%NUMERATOR%MP(1)
         ENDIF
         IF (RETURN_VALUE(J)%DENOMINATOR%MP(1) < 0) THEN
             RETURN_VALUE(J)%DENOMINATOR%MP(1) = 1
             RETURN_VALUE(J)%NUMERATOR%MP(1) =  &
             -RETURN_VALUE(J)%NUMERATOR%MP(1)
         ENDIF
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1

   FUNCTION FMSUB_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMEQ_RATIONAL(MA(J,K),RETURN_VALUE(J,K))
            IF (RETURN_VALUE(J,K)%NUMERATOR%MP(2) /= MUNKNO .AND.  &
                RETURN_VALUE(J,K)%NUMERATOR%MP(3) /= 0) THEN
                RETURN_VALUE(J,K)%NUMERATOR%MP(1) =  &
                -RETURN_VALUE(J,K)%NUMERATOR%MP(1)
            ENDIF
            IF (RETURN_VALUE(J,K)%DENOMINATOR%MP(1) < 0) THEN
                RETURN_VALUE(J,K)%DENOMINATOR%MP(1) = 1
                RETURN_VALUE(J,K)%NUMERATOR%MP(1) =  &
                -RETURN_VALUE(J,K)%NUMERATOR%MP(1)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2

   FUNCTION FMSUB_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FMSUB_RATIONAL_RMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMSUB_RATIONAL_RMRM

   SUBROUTINE FMSUB_RATIONAL_RMI_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_SUB(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMSUB_RATIONAL_RMI_0

   FUNCTION FMSUB_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FMSUB_RATIONAL_RMI_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMSUB_RATIONAL_RMI

   SUBROUTINE FMSUB_RATIONAL_IRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_SUB(R_5,R_1,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMSUB_RATIONAL_IRM_0

   FUNCTION FMSUB_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FMSUB_RATIONAL_IRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMSUB_RATIONAL_IRM

   SUBROUTINE FMSUB_RATIONAL_RMIM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_SUB(R_1,R_5,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMSUB_RATIONAL_RMIM_0

   FUNCTION FMSUB_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FMSUB_RATIONAL_RMIM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMSUB_RATIONAL_RMIM

   SUBROUTINE FMSUB_RATIONAL_IMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      CALL IM_SUB(R_5,R_1,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMSUB_RATIONAL_IMRM_0

   FUNCTION FMSUB_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FMSUB_RATIONAL_IMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMSUB_RATIONAL_IMRM

   FUNCTION FMSUB_RATIONAL_RM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1RM1

   FUNCTION FMSUB_RATIONAL_RM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1RM

   FUNCTION FMSUB_RATIONAL_RMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMSUB_RATIONAL_RMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMRM1

   FUNCTION FMSUB_RATIONAL_I1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_IRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_I1RM1

   FUNCTION FMSUB_RATIONAL_RM1I1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMI_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1I1

   FUNCTION FMSUB_RATIONAL_IRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMSUB_RATIONAL_IRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IRM1

   FUNCTION FMSUB_RATIONAL_RM1I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMI_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1I

   FUNCTION FMSUB_RATIONAL_RMI1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB)
         CALL FMSUB_RATIONAL_RMI_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMI1

   FUNCTION FMSUB_RATIONAL_I1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_IRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_I1RM

   FUNCTION FMSUB_RATIONAL_IM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_IMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IM1RM1

   FUNCTION FMSUB_RATIONAL_RM1IM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMIM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1IM1

   FUNCTION FMSUB_RATIONAL_IMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMSUB_RATIONAL_IMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IMRM1

   FUNCTION FMSUB_RATIONAL_RM1IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMIM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM1IM

   FUNCTION FMSUB_RATIONAL_RMIM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMSUB_RATIONAL_RMIM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMIM1

   FUNCTION FMSUB_RATIONAL_IM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_IMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IM1RM

   FUNCTION FMSUB_RATIONAL_RM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2RM2

   FUNCTION FMSUB_RATIONAL_RM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2RM

   FUNCTION FMSUB_RATIONAL_RMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMSUB_RATIONAL_RMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMRM2

   FUNCTION FMSUB_RATIONAL_I2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_IRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_I2RM2

   FUNCTION FMSUB_RATIONAL_RM2I2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMI_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2I2

   FUNCTION FMSUB_RATIONAL_IRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMSUB_RATIONAL_IRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IRM2

   FUNCTION FMSUB_RATIONAL_RM2I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMI_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2I

   FUNCTION FMSUB_RATIONAL_RMI2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMSUB_RATIONAL_RMI_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMI2

   FUNCTION FMSUB_RATIONAL_I2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_IRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_I2RM

   FUNCTION FMSUB_RATIONAL_IM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_IMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IM2RM2

   FUNCTION FMSUB_RATIONAL_RM2IM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMIM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2IM2

   FUNCTION FMSUB_RATIONAL_IMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMSUB_RATIONAL_IMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IMRM2

   FUNCTION FMSUB_RATIONAL_RM2IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMIM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RM2IM

   FUNCTION FMSUB_RATIONAL_RMIM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMSUB_RATIONAL_RMIM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_RMIM2

   FUNCTION FMSUB_RATIONAL_IM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_IMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMSUB_RATIONAL_IM2RM


 END MODULE FM_RATIONAL_ARITHMETIC_3


 MODULE FM_RATIONAL_ARITHMETIC_4
    USE FM_RATIONAL_ARITHMETIC_1

   INTERFACE OPERATOR (*)
       MODULE PROCEDURE FMMPY_RATIONAL_RMRM
       MODULE PROCEDURE FMMPY_RATIONAL_IRM
       MODULE PROCEDURE FMMPY_RATIONAL_RMI
       MODULE PROCEDURE FMMPY_RATIONAL_IMRM
       MODULE PROCEDURE FMMPY_RATIONAL_RMIM

       MODULE PROCEDURE FMMPY_RATIONAL_RM1RM1
       MODULE PROCEDURE FMMPY_RATIONAL_RM1RM
       MODULE PROCEDURE FMMPY_RATIONAL_RMRM1
       MODULE PROCEDURE FMMPY_RATIONAL_I1RM1
       MODULE PROCEDURE FMMPY_RATIONAL_RM1I1
       MODULE PROCEDURE FMMPY_RATIONAL_I1RM
       MODULE PROCEDURE FMMPY_RATIONAL_RMI1
       MODULE PROCEDURE FMMPY_RATIONAL_IRM1
       MODULE PROCEDURE FMMPY_RATIONAL_RM1I
       MODULE PROCEDURE FMMPY_RATIONAL_IM1RM1
       MODULE PROCEDURE FMMPY_RATIONAL_IM1RM
       MODULE PROCEDURE FMMPY_RATIONAL_IMRM1
       MODULE PROCEDURE FMMPY_RATIONAL_RM1IM1
       MODULE PROCEDURE FMMPY_RATIONAL_RM1IM
       MODULE PROCEDURE FMMPY_RATIONAL_RMIM1

       MODULE PROCEDURE FMMPY_RATIONAL_RM2RM2
       MODULE PROCEDURE FMMPY_RATIONAL_RM2RM
       MODULE PROCEDURE FMMPY_RATIONAL_RMRM2
       MODULE PROCEDURE FMMPY_RATIONAL_I2RM2
       MODULE PROCEDURE FMMPY_RATIONAL_I2RM
       MODULE PROCEDURE FMMPY_RATIONAL_IRM2
       MODULE PROCEDURE FMMPY_RATIONAL_RM2I2
       MODULE PROCEDURE FMMPY_RATIONAL_RM2I
       MODULE PROCEDURE FMMPY_RATIONAL_RMI2
       MODULE PROCEDURE FMMPY_RATIONAL_IM2RM2
       MODULE PROCEDURE FMMPY_RATIONAL_IM2RM
       MODULE PROCEDURE FMMPY_RATIONAL_IMRM2
       MODULE PROCEDURE FMMPY_RATIONAL_RM2IM2
       MODULE PROCEDURE FMMPY_RATIONAL_RM2IM
       MODULE PROCEDURE FMMPY_RATIONAL_RMIM2
   END INTERFACE

   CONTAINS

!                                                                                        *

   FUNCTION FMMPY_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FMMPY_RATIONAL_RMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMMPY_RATIONAL_RMRM

   SUBROUTINE FMMPY_RATIONAL_RMI_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_1,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMMPY_RATIONAL_RMI_0

   FUNCTION FMMPY_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FMMPY_RATIONAL_RMI_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMMPY_RATIONAL_RMI

   SUBROUTINE FMMPY_RATIONAL_IRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_1,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMMPY_RATIONAL_IRM_0

   FUNCTION FMMPY_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FMMPY_RATIONAL_IRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMMPY_RATIONAL_IRM

   SUBROUTINE FMMPY_RATIONAL_RMIM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_1,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMMPY_RATIONAL_RMIM_0

   FUNCTION FMMPY_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FMMPY_RATIONAL_RMIM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMMPY_RATIONAL_RMIM

   SUBROUTINE FMMPY_RATIONAL_IMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_1,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_2,R_4)
      IF (SKIP_GCD .AND. MAX(R_2%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_2%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_2,R_4,R_1)
          CALL IMDIV(R_4%MIM,R_1%MIM,MC%NUMERATOR)
          CALL IMDIV(R_2%MIM,R_1%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMMPY_RATIONAL_IMRM_0

   FUNCTION FMMPY_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FMMPY_RATIONAL_IMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMMPY_RATIONAL_IMRM

   FUNCTION FMMPY_RATIONAL_RM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1RM1

   FUNCTION FMMPY_RATIONAL_RM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1RM

   FUNCTION FMMPY_RATIONAL_RMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMMPY_RATIONAL_RMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMRM1

   FUNCTION FMMPY_RATIONAL_I1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_IRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_I1RM1

   FUNCTION FMMPY_RATIONAL_RM1I1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMI_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1I1

   FUNCTION FMMPY_RATIONAL_IRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMMPY_RATIONAL_IRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IRM1

   FUNCTION FMMPY_RATIONAL_RM1I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMI_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1I

   FUNCTION FMMPY_RATIONAL_RMI1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB)
         CALL FMMPY_RATIONAL_RMI_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMI1

   FUNCTION FMMPY_RATIONAL_I1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_IRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_I1RM

   FUNCTION FMMPY_RATIONAL_IM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_IMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IM1RM1

   FUNCTION FMMPY_RATIONAL_RM1IM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMIM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1IM1

   FUNCTION FMMPY_RATIONAL_IMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMMPY_RATIONAL_IMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IMRM1

   FUNCTION FMMPY_RATIONAL_RM1IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_RMIM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM1IM

   FUNCTION FMMPY_RATIONAL_RMIM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMMPY_RATIONAL_RMIM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMIM1

   FUNCTION FMMPY_RATIONAL_IM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMMPY_RATIONAL_IMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IM1RM

   FUNCTION FMMPY_RATIONAL_RM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2RM2

   FUNCTION FMMPY_RATIONAL_RM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2RM

   FUNCTION FMMPY_RATIONAL_RMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMMPY_RATIONAL_RMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMRM2

   FUNCTION FMMPY_RATIONAL_I2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_IRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_I2RM2

   FUNCTION FMMPY_RATIONAL_RM2I2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMI_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2I2

   FUNCTION FMMPY_RATIONAL_IRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMMPY_RATIONAL_IRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IRM2

   FUNCTION FMMPY_RATIONAL_RM2I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMI_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2I

   FUNCTION FMMPY_RATIONAL_RMI2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMMPY_RATIONAL_RMI_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMI2

   FUNCTION FMMPY_RATIONAL_I2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_IRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_I2RM

   FUNCTION FMMPY_RATIONAL_IM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_IMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IM2RM2

   FUNCTION FMMPY_RATIONAL_RM2IM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMIM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2IM2

   FUNCTION FMMPY_RATIONAL_IMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMMPY_RATIONAL_IMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IMRM2

   FUNCTION FMMPY_RATIONAL_RM2IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_RMIM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RM2IM

   FUNCTION FMMPY_RATIONAL_RMIM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMMPY_RATIONAL_RMIM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_RMIM2

   FUNCTION FMMPY_RATIONAL_IM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMMPY_RATIONAL_IMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMPY_RATIONAL_IM2RM


 END MODULE FM_RATIONAL_ARITHMETIC_4


 MODULE FM_RATIONAL_ARITHMETIC_5
    USE FM_RATIONAL_ARITHMETIC_1

   INTERFACE OPERATOR (/)
       MODULE PROCEDURE FMDIV_RATIONAL_RMRM
       MODULE PROCEDURE FMDIV_RATIONAL_IRM
       MODULE PROCEDURE FMDIV_RATIONAL_RMI
       MODULE PROCEDURE FMDIV_RATIONAL_IMRM
       MODULE PROCEDURE FMDIV_RATIONAL_RMIM

       MODULE PROCEDURE FMDIV_RATIONAL_RM1RM1
       MODULE PROCEDURE FMDIV_RATIONAL_RM1RM
       MODULE PROCEDURE FMDIV_RATIONAL_RMRM1
       MODULE PROCEDURE FMDIV_RATIONAL_I1RM1
       MODULE PROCEDURE FMDIV_RATIONAL_RM1I1
       MODULE PROCEDURE FMDIV_RATIONAL_I1RM
       MODULE PROCEDURE FMDIV_RATIONAL_RMI1
       MODULE PROCEDURE FMDIV_RATIONAL_IRM1
       MODULE PROCEDURE FMDIV_RATIONAL_RM1I
       MODULE PROCEDURE FMDIV_RATIONAL_IM1RM1
       MODULE PROCEDURE FMDIV_RATIONAL_IM1RM
       MODULE PROCEDURE FMDIV_RATIONAL_IMRM1
       MODULE PROCEDURE FMDIV_RATIONAL_RM1IM1
       MODULE PROCEDURE FMDIV_RATIONAL_RM1IM
       MODULE PROCEDURE FMDIV_RATIONAL_RMIM1

       MODULE PROCEDURE FMDIV_RATIONAL_RM2RM2
       MODULE PROCEDURE FMDIV_RATIONAL_RM2RM
       MODULE PROCEDURE FMDIV_RATIONAL_RMRM2
       MODULE PROCEDURE FMDIV_RATIONAL_I2RM2
       MODULE PROCEDURE FMDIV_RATIONAL_I2RM
       MODULE PROCEDURE FMDIV_RATIONAL_IRM2
       MODULE PROCEDURE FMDIV_RATIONAL_RM2I2
       MODULE PROCEDURE FMDIV_RATIONAL_RM2I
       MODULE PROCEDURE FMDIV_RATIONAL_RMI2
       MODULE PROCEDURE FMDIV_RATIONAL_IM2RM2
       MODULE PROCEDURE FMDIV_RATIONAL_IM2RM
       MODULE PROCEDURE FMDIV_RATIONAL_IMRM2
       MODULE PROCEDURE FMDIV_RATIONAL_RM2IM2
       MODULE PROCEDURE FMDIV_RATIONAL_RM2IM
       MODULE PROCEDURE FMDIV_RATIONAL_RMIM2
   END INTERFACE

   CONTAINS

!                                                                                        /

   FUNCTION FMDIV_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FMDIV_RATIONAL_RMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMDIV_RATIONAL_RMRM

   SUBROUTINE FMDIV_RATIONAL_RMI_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_1,R_4)
      IF (SKIP_GCD .AND. MAX(R_1%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_1%MIM,MC%NUMERATOR)
          CALL IMEQ(R_4%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_1,R_4,R_5)
          CALL IMDIV(R_1%MIM,R_5%MIM,MC%NUMERATOR)
          CALL IMDIV(R_4%MIM,R_5%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMDIV_RATIONAL_RMI_0

   FUNCTION FMDIV_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FMDIV_RATIONAL_RMI_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMDIV_RATIONAL_RMI

   SUBROUTINE FMDIV_RATIONAL_IRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_1,R_4)
      IF (SKIP_GCD .AND. MAX(R_1%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_1%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_1,R_4,R_5)
          CALL IMDIV(R_4%MIM,R_5%MIM,MC%NUMERATOR)
          CALL IMDIV(R_1%MIM,R_5%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMDIV_RATIONAL_IRM_0

   FUNCTION FMDIV_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FMDIV_RATIONAL_IRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMDIV_RATIONAL_IRM

   SUBROUTINE FMDIV_RATIONAL_RMIM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MC
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_1,R_4)
      IF (SKIP_GCD .AND. MAX(R_1%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_1%MIM,MC%NUMERATOR)
          CALL IMEQ(R_4%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_1,R_4,R_5)
          CALL IMDIV(R_1%MIM,R_5%MIM,MC%NUMERATOR)
          CALL IMDIV(R_4%MIM,R_5%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMDIV_RATIONAL_RMIM_0

   FUNCTION FMDIV_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      TYPE (IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FMDIV_RATIONAL_RMIM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMDIV_RATIONAL_RMIM

   SUBROUTINE FMDIV_RATIONAL_IMRM_0(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,MC
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_4)
      CALL FM_MAX_EXP_IM(R_1,R_4)
      IF (SKIP_GCD .AND. MAX(R_1%MIM%MP(2),R_4%MIM%MP(2)) < RATIONAL_SKIP_MAX) THEN
          CALL IMEQ(R_4%MIM,MC%NUMERATOR)
          CALL IMEQ(R_1%MIM,MC%DENOMINATOR)
      ELSE
          CALL IM_GCD(R_1,R_4,R_5)
          CALL IMDIV(R_4%MIM,R_5%MIM,MC%NUMERATOR)
          CALL IMDIV(R_1%MIM,R_5%MIM,MC%DENOMINATOR)
      ENDIF
      IF (MC%DENOMINATOR%MP(1) < 0) THEN
          MC%DENOMINATOR%MP(1) = 1
          MC%NUMERATOR%MP(1) = -MC%NUMERATOR%MP(1)
      ENDIF
   END SUBROUTINE FMDIV_RATIONAL_IMRM_0

   FUNCTION FMDIV_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MB,RETURN_VALUE
      TYPE (IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FMDIV_RATIONAL_IMRM_0(MA,MB,RETURN_VALUE)
   END FUNCTION FMDIV_RATIONAL_IMRM

   FUNCTION FMDIV_RATIONAL_RM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1RM1

   FUNCTION FMDIV_RATIONAL_RM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1RM

   FUNCTION FMDIV_RATIONAL_RMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMDIV_RATIONAL_RMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMRM1

   FUNCTION FMDIV_RATIONAL_I1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_IRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_I1RM1

   FUNCTION FMDIV_RATIONAL_RM1I1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMI_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1I1

   FUNCTION FMDIV_RATIONAL_IRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMDIV_RATIONAL_IRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IRM1

   FUNCTION FMDIV_RATIONAL_RM1I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMI_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1I

   FUNCTION FMDIV_RATIONAL_RMI1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB)
         CALL FMDIV_RATIONAL_RMI_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMI1

   FUNCTION FMDIV_RATIONAL_I1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_IRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_I1RM

   FUNCTION FMDIV_RATIONAL_IM1RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_IMRM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IM1RM1

   FUNCTION FMDIV_RATIONAL_RM1IM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) /= SIZE(MB)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA)
             CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
             CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMIM_0(MA(J),MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1IM1

   FUNCTION FMDIV_RATIONAL_IMRM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMDIV_RATIONAL_IMRM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IMRM1

   FUNCTION FMDIV_RATIONAL_RM1IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMIM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM1IM

   FUNCTION FMDIV_RATIONAL_RMIM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB)
         CALL FMDIV_RATIONAL_RMIM_0(MA,MB(J),RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMIM1

   FUNCTION FMDIV_RATIONAL_IM1RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_IMRM_0(MA(J),MB,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IM1RM

   FUNCTION FMDIV_RATIONAL_RM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2RM2

   FUNCTION FMDIV_RATIONAL_RM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2RM

   FUNCTION FMDIV_RATIONAL_RMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMDIV_RATIONAL_RMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMRM2

   FUNCTION FMDIV_RATIONAL_I2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_IRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_I2RM2

   FUNCTION FMDIV_RATIONAL_RM2I2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMI_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2I2

   FUNCTION FMDIV_RATIONAL_IRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMDIV_RATIONAL_IRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IRM2

   FUNCTION FMDIV_RATIONAL_RM2I(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMI_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2I

   FUNCTION FMDIV_RATIONAL_RMI2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      INTEGER, DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMDIV_RATIONAL_RMI_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMI2

   FUNCTION FMDIV_RATIONAL_I2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      INTEGER, DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_IRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_I2RM

   FUNCTION FMDIV_RATIONAL_IM2RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_IMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IM2RM2

   FUNCTION FMDIV_RATIONAL_RM2IM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA,DIM=1) /= SIZE(MB,DIM=1) .OR. SIZE(MA,DIM=2) /= SIZE(MB,DIM=2)) THEN
          CALL IMST2M('UNKNOWN',MT_RM%NUMERATOR)
          CALL IMST2M('UNKNOWN',MT_RM%DENOMINATOR)
          DO J = 1, SIZE(MA,DIM=1)
             DO K = 1, SIZE(MA,DIM=2)
                CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
             ENDDO
          ENDDO
          RETURN
      ENDIF
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMIM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2IM2

   FUNCTION FMDIV_RATIONAL_IMRM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM) :: MA
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMDIV_RATIONAL_IMRM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IMRM2

   FUNCTION FMDIV_RATIONAL_RM2IM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (IM) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMIM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RM2IM

   FUNCTION FMDIV_RATIONAL_RMIM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (IM), DIMENSION(:,:) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MB,DIM=1)
         DO K = 1, SIZE(MB,DIM=2)
            CALL FMDIV_RATIONAL_RMIM_0(MA,MB(J,K),RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_RMIM2

   FUNCTION FMDIV_RATIONAL_IM2RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (IM), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL) :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_IMRM_0(MA(J,K),MB,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMDIV_RATIONAL_IM2RM


 END MODULE FM_RATIONAL_ARITHMETIC_5


 MODULE FM_RATIONAL_ARITHMETIC_6
    USE FM_RATIONAL_ARITHMETIC_1


   INTERFACE OPERATOR (==)
       MODULE PROCEDURE FMLEQ_RATIONAL_RMRM
       MODULE PROCEDURE FMLEQ_RATIONAL_RMI
       MODULE PROCEDURE FMLEQ_RATIONAL_IRM
       MODULE PROCEDURE FMLEQ_RATIONAL_RMIM
       MODULE PROCEDURE FMLEQ_RATIONAL_IMRM
   END INTERFACE

   INTERFACE OPERATOR (/=)
       MODULE PROCEDURE FMLNE_RATIONAL_RMRM
       MODULE PROCEDURE FMLNE_RATIONAL_RMI
       MODULE PROCEDURE FMLNE_RATIONAL_IRM
       MODULE PROCEDURE FMLNE_RATIONAL_RMIM
       MODULE PROCEDURE FMLNE_RATIONAL_IMRM
   END INTERFACE

   INTERFACE OPERATOR (<)
       MODULE PROCEDURE FMLLT_RATIONAL_RMRM
       MODULE PROCEDURE FMLLT_RATIONAL_RMI
       MODULE PROCEDURE FMLLT_RATIONAL_IRM
       MODULE PROCEDURE FMLLT_RATIONAL_RMIM
       MODULE PROCEDURE FMLLT_RATIONAL_IMRM
   END INTERFACE

   INTERFACE OPERATOR (<=)
       MODULE PROCEDURE FMLLE_RATIONAL_RMRM
       MODULE PROCEDURE FMLLE_RATIONAL_RMI
       MODULE PROCEDURE FMLLE_RATIONAL_IRM
       MODULE PROCEDURE FMLLE_RATIONAL_RMIM
       MODULE PROCEDURE FMLLE_RATIONAL_IMRM
   END INTERFACE

   INTERFACE OPERATOR (>)
       MODULE PROCEDURE FMLGT_RATIONAL_RMRM
       MODULE PROCEDURE FMLGT_RATIONAL_RMI
       MODULE PROCEDURE FMLGT_RATIONAL_IRM
       MODULE PROCEDURE FMLGT_RATIONAL_RMIM
       MODULE PROCEDURE FMLGT_RATIONAL_IMRM
   END INTERFACE

   INTERFACE OPERATOR (>=)
       MODULE PROCEDURE FMLGE_RATIONAL_RMRM
       MODULE PROCEDURE FMLGE_RATIONAL_RMI
       MODULE PROCEDURE FMLGE_RATIONAL_IRM
       MODULE PROCEDURE FMLGE_RATIONAL_RMIM
       MODULE PROCEDURE FMLGE_RATIONAL_IMRM
   END INTERFACE

 CONTAINS

!                                                                                        Logical ==

   FUNCTION FMLEQ_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      RETURN_VALUE = IM_COMP(R_5,'==',R_1)
   END FUNCTION FMLEQ_RATIONAL_RMRM

   FUNCTION FMLEQ_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'==',R_5)
   END FUNCTION FMLEQ_RATIONAL_RMI

   FUNCTION FMLEQ_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'==',R_5)
   END FUNCTION FMLEQ_RATIONAL_IRM

   FUNCTION FMLEQ_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'==',R_5)
   END FUNCTION FMLEQ_RATIONAL_RMIM

   FUNCTION FMLEQ_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'==',R_5)
   END FUNCTION FMLEQ_RATIONAL_IMRM


!                                                                                        Logical /=

   FUNCTION FMLNE_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      RETURN_VALUE = IM_COMP(R_5,'/=',R_1)
   END FUNCTION FMLNE_RATIONAL_RMRM

   FUNCTION FMLNE_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'/=',R_5)
   END FUNCTION FMLNE_RATIONAL_RMI

   FUNCTION FMLNE_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'/=',R_5)
   END FUNCTION FMLNE_RATIONAL_IRM

   FUNCTION FMLNE_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'/=',R_5)
   END FUNCTION FMLNE_RATIONAL_RMIM

   FUNCTION FMLNE_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      RETURN_VALUE = IM_COMP(R_1,'/=',R_5)
   END FUNCTION FMLNE_RATIONAL_IMRM


!                                                                                        Logical <


   FUNCTION FMLLT_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ENDIF
   END FUNCTION FMLLT_RATIONAL_RMRM

   FUNCTION FMLLT_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'<',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'>',R_5)
      ENDIF
   END FUNCTION FMLLT_RATIONAL_RMI

   FUNCTION FMLLT_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ENDIF
   END FUNCTION FMLLT_RATIONAL_IRM

   FUNCTION FMLLT_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'<',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'>',R_5)
      ENDIF
   END FUNCTION FMLLT_RATIONAL_RMIM

   FUNCTION FMLLT_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ENDIF
   END FUNCTION FMLLT_RATIONAL_IMRM


!                                                                                        Logical <=


   FUNCTION FMLLE_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ENDIF
   END FUNCTION FMLLE_RATIONAL_RMRM

   FUNCTION FMLLE_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'<=',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'>=',R_5)
      ENDIF
   END FUNCTION FMLLE_RATIONAL_RMI

   FUNCTION FMLLE_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ENDIF
   END FUNCTION FMLLE_RATIONAL_IRM

   FUNCTION FMLLE_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'<=',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'>=',R_5)
      ENDIF
   END FUNCTION FMLLE_RATIONAL_RMIM

   FUNCTION FMLLE_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ENDIF
   END FUNCTION FMLLE_RATIONAL_IMRM


!                                                                                        Logical >


   FUNCTION FMLGT_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ENDIF
   END FUNCTION FMLGT_RATIONAL_RMRM

   FUNCTION FMLGT_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'>',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'<',R_5)
      ENDIF
   END FUNCTION FMLGT_RATIONAL_RMI

   FUNCTION FMLGT_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ENDIF
   END FUNCTION FMLGT_RATIONAL_IRM

   FUNCTION FMLGT_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'>',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'<',R_5)
      ENDIF
   END FUNCTION FMLGT_RATIONAL_RMIM

   FUNCTION FMLGT_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<',R_1)
      ENDIF
   END FUNCTION FMLGT_RATIONAL_IMRM


!                                                                                        Logical >=


   FUNCTION FMLGE_RATIONAL_RMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA,MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ENDIF
   END FUNCTION FMLGE_RATIONAL_RMRM

   FUNCTION FMLGE_RATIONAL_RMI(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      INTEGER :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'>=',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'<=',R_5)
      ENDIF
   END FUNCTION FMLGE_RATIONAL_RMI

   FUNCTION FMLGE_RATIONAL_IRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      INTEGER :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MA,           R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ENDIF
   END FUNCTION FMLGE_RATIONAL_IRM

   FUNCTION FMLGE_RATIONAL_RMIM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MA
      TYPE(IM) :: MB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_1,'>=',R_5)
      ELSE
          RETURN_VALUE = IM_COMP(R_1,'<=',R_5)
      ENDIF
   END FUNCTION FMLGE_RATIONAL_RMIM

   FUNCTION FMLGE_RATIONAL_IMRM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      LOGICAL :: RETURN_VALUE
      TYPE (FM_RATIONAL) :: MB
      TYPE(IM) :: MA
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MB%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MA%MIM,        R_3%MIM)
      CALL IM_MPY(R_2,R_3,R_5)
      IF (R_2%MIM%MP(1) > 0) THEN
          RETURN_VALUE = IM_COMP(R_5,'>=',R_1)
      ELSE
          RETURN_VALUE = IM_COMP(R_5,'<=',R_1)
      ENDIF
   END FUNCTION FMLGE_RATIONAL_IMRM


 END MODULE FM_RATIONAL_ARITHMETIC_6


 MODULE FM_RATIONAL_ARITHMETIC_7
    USE FM_RATIONAL_ARITHMETIC_1


   INTERFACE OPERATOR (**)
       MODULE PROCEDURE FMIPWR_RATIONAL_RM
       MODULE PROCEDURE FMIPWR_RATIONAL_RM1
       MODULE PROCEDURE FMIPWR_RATIONAL_RM2
   END INTERFACE

   INTERFACE ABS
       MODULE PROCEDURE FMABS_RATIONAL_RM
       MODULE PROCEDURE FMABS_RATIONAL_RM1
       MODULE PROCEDURE FMABS_RATIONAL_RM2
   END INTERFACE

   INTERFACE CEILING
       MODULE PROCEDURE FMCEILING_RATIONAL_RM
       MODULE PROCEDURE FMCEILING_RATIONAL_RM1
       MODULE PROCEDURE FMCEILING_RATIONAL_RM2
   END INTERFACE

   INTERFACE DIM
       MODULE PROCEDURE FMDIM_RATIONAL_RM
       MODULE PROCEDURE FMDIM_RATIONAL_RM1
       MODULE PROCEDURE FMDIM_RATIONAL_RM2
   END INTERFACE

   INTERFACE FLOOR
       MODULE PROCEDURE FMFLOOR_RATIONAL_RM
       MODULE PROCEDURE FMFLOOR_RATIONAL_RM1
       MODULE PROCEDURE FMFLOOR_RATIONAL_RM2
   END INTERFACE

   INTERFACE INT
       MODULE PROCEDURE FMINT_RATIONAL_RM
       MODULE PROCEDURE FMINT_RATIONAL_RM1
       MODULE PROCEDURE FMINT_RATIONAL_RM2
   END INTERFACE

   INTERFACE IS_UNKNOWN
       MODULE PROCEDURE RM_IS_UNKNOWN
       MODULE PROCEDURE RM_IS_UNKNOWN1
       MODULE PROCEDURE RM_IS_UNKNOWN2
   END INTERFACE

   INTERFACE MAX
       MODULE PROCEDURE FMMAX_RATIONAL_RM
   END INTERFACE

   INTERFACE MIN
       MODULE PROCEDURE FMMIN_RATIONAL_RM
   END INTERFACE

   INTERFACE MOD
       MODULE PROCEDURE FMMOD_RATIONAL_RM
       MODULE PROCEDURE FMMOD_RATIONAL_RM1
       MODULE PROCEDURE FMMOD_RATIONAL_RM2
   END INTERFACE

   INTERFACE MODULO
       MODULE PROCEDURE FMMODULO_RATIONAL_RM
       MODULE PROCEDURE FMMODULO_RATIONAL_RM1
       MODULE PROCEDURE FMMODULO_RATIONAL_RM2
   END INTERFACE

   INTERFACE NINT
       MODULE PROCEDURE FMNINT_RATIONAL_RM
       MODULE PROCEDURE FMNINT_RATIONAL_RM1
       MODULE PROCEDURE FMNINT_RATIONAL_RM2
   END INTERFACE

 CONTAINS

!                                                                                        ABS

   FUNCTION FMABS_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMABS(MA%NUMERATOR,RETURN_VALUE%NUMERATOR)
      CALL IMABS(MA%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMABS_RATIONAL_RM

   FUNCTION FMABS_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMABS(MA(J)%NUMERATOR,RETURN_VALUE(J)%NUMERATOR)
         CALL IMABS(MA(J)%DENOMINATOR,RETURN_VALUE(J)%DENOMINATOR)
      ENDDO
   END FUNCTION FMABS_RATIONAL_RM1

   FUNCTION FMABS_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMABS(MA(J,K)%NUMERATOR,RETURN_VALUE(J,K)%NUMERATOR)
            CALL IMABS(MA(J,K)%DENOMINATOR,RETURN_VALUE(J,K)%DENOMINATOR)
         ENDDO
      ENDDO
   END FUNCTION FMABS_RATIONAL_RM2


!                                                                                        CEILING

   FUNCTION FMCEILING_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMDIVR(MA%NUMERATOR,MA%DENOMINATOR,R_1%MIM,R_2%MIM)
      IF (R_2%MIM%MP(3) == 0) THEN
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
      ELSE IF (R_2%MIM%MP(1) < 0) THEN
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
      ELSE
          CALL IMI2M(1,R_2%MIM)
          CALL IMADD(R_1%MIM,R_2%MIM,RETURN_VALUE%NUMERATOR)
      ENDIF
      CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMCEILING_RATIONAL_RM

   FUNCTION FMCEILING_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMDIVR(MA(J)%NUMERATOR,MA(J)%DENOMINATOR,R_1%MIM,R_2%MIM)
         IF (R_2%MIM%MP(3) == 0) THEN
             CALL IMEQ(R_1%MIM,RETURN_VALUE(J)%NUMERATOR)
         ELSE IF (R_2%MIM%MP(1) < 0) THEN
             CALL IMEQ(R_1%MIM,RETURN_VALUE(J)%NUMERATOR)
         ELSE
             CALL IMI2M(1,R_2%MIM)
             CALL IMADD(R_1%MIM,R_2%MIM,RETURN_VALUE(J)%NUMERATOR)
         ENDIF
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
      ENDDO
   END FUNCTION FMCEILING_RATIONAL_RM1

   FUNCTION FMCEILING_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMDIVR(MA(J,K)%NUMERATOR,MA(J,K)%DENOMINATOR,R_1%MIM,R_2%MIM)
            IF (R_2%MIM%MP(3) == 0) THEN
                CALL IMEQ(R_1%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ELSE IF (R_2%MIM%MP(1) < 0) THEN
                CALL IMEQ(R_1%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ELSE
                CALL IMI2M(1,R_2%MIM)
                CALL IMADD(R_1%MIM,R_2%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ENDIF
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
         ENDDO
      ENDDO
   END FUNCTION FMCEILING_RATIONAL_RM2


!                                                                                        DIM

   FUNCTION FMDIM_RATIONAL_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL FMSUB_RATIONAL_RMRM_0(MA,MB,RETURN_VALUE)
      IF (RETURN_VALUE%NUMERATOR%MP(1) < 0) THEN
          CALL IMI2M(0,RETURN_VALUE%NUMERATOR)
          CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FMDIM_RATIONAL_RM

   FUNCTION FMDIM_RATIONAL_RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMSUB_RATIONAL_RMRM_0(MA(J),MB(J),RETURN_VALUE(J))
         IF (RETURN_VALUE(J)%NUMERATOR%MP(1) < 0) THEN
             CALL IMI2M(0,RETURN_VALUE(J)%NUMERATOR)
             CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
         ENDIF
      ENDDO
   END FUNCTION FMDIM_RATIONAL_RM1

   FUNCTION FMDIM_RATIONAL_RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMSUB_RATIONAL_RMRM_0(MA(J,K),MB(J,K),RETURN_VALUE(J,K))
            IF (RETURN_VALUE(J,K)%NUMERATOR%MP(1) < 0) THEN
                CALL IMI2M(0,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMDIM_RATIONAL_RM2


!                                                                                        FLOOR

   FUNCTION FMFLOOR_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMDIVR(MA%NUMERATOR,MA%DENOMINATOR,R_1%MIM,R_2%MIM)
      IF (R_2%MIM%MP(3) == 0) THEN
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
      ELSE IF (R_2%MIM%MP(1) < 0) THEN
          CALL IMI2M(1,R_2%MIM)
          CALL IMSUB(R_1%MIM,R_2%MIM,RETURN_VALUE%NUMERATOR)
      ELSE
          CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
      ENDIF
      CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMFLOOR_RATIONAL_RM

   FUNCTION FMFLOOR_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMDIVR(MA(J)%NUMERATOR,MA(J)%DENOMINATOR,R_1%MIM,R_2%MIM)
         IF (R_2%MIM%MP(3) == 0) THEN
             CALL IMEQ(R_1%MIM,RETURN_VALUE(J)%NUMERATOR)
         ELSE IF (R_2%MIM%MP(1) < 0) THEN
             CALL IMI2M(1,R_2%MIM)
             CALL IMSUB(R_1%MIM,R_2%MIM,RETURN_VALUE(J)%NUMERATOR)
         ELSE
             CALL IMEQ(R_1%MIM,RETURN_VALUE(J)%NUMERATOR)
         ENDIF
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
      ENDDO
   END FUNCTION FMFLOOR_RATIONAL_RM1

   FUNCTION FMFLOOR_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMDIVR(MA(J,K)%NUMERATOR,MA(J,K)%DENOMINATOR,R_1%MIM,R_2%MIM)
            IF (R_2%MIM%MP(3) == 0) THEN
                CALL IMEQ(R_1%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ELSE IF (R_2%MIM%MP(1) < 0) THEN
                CALL IMI2M(1,R_2%MIM)
                CALL IMSUB(R_1%MIM,R_2%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ELSE
                CALL IMEQ(R_1%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ENDIF
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
         ENDDO
      ENDDO
   END FUNCTION FMFLOOR_RATIONAL_RM2


!                                                                                        INT

   FUNCTION FMINT_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMDIVR(MA%NUMERATOR,MA%DENOMINATOR,R_1%MIM,R_2%MIM)
      CALL IMEQ(R_1%MIM,RETURN_VALUE%NUMERATOR)
      CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMINT_RATIONAL_RM

   FUNCTION FMINT_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMDIVR(MA(J)%NUMERATOR,MA(J)%DENOMINATOR,R_1%MIM,R_2%MIM)
         CALL IMEQ(R_1%MIM,RETURN_VALUE(J)%NUMERATOR)
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
      ENDDO
   END FUNCTION FMINT_RATIONAL_RM1

   FUNCTION FMINT_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMDIVR(MA(J,K)%NUMERATOR,MA(J,K)%DENOMINATOR,R_1%MIM,R_2%MIM)
            CALL IMEQ(R_1%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
         ENDDO
      ENDDO
   END FUNCTION FMINT_RATIONAL_RM2

!                                                         IS_UNKNOWN

   FUNCTION RM_IS_UNKNOWN(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      LOGICAL :: RETURN_VALUE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      IF (MA%NUMERATOR%MP(2)   == MUNKNO) RETURN_VALUE = .TRUE.
      IF (MA%DENOMINATOR%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
   END FUNCTION RM_IS_UNKNOWN

   FUNCTION RM_IS_UNKNOWN1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,N
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      N = SIZE(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, N
         IF (MA(J)%NUMERATOR%MP(2)   == MUNKNO) RETURN_VALUE = .TRUE.
         IF (MA(J)%DENOMINATOR%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
      ENDDO
   END FUNCTION RM_IS_UNKNOWN1

   FUNCTION RM_IS_UNKNOWN2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      LOGICAL :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      RETURN_VALUE = .FALSE.
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            IF (MA(J,K)%NUMERATOR%MP(2)   == MUNKNO) RETURN_VALUE = .TRUE.
            IF (MA(J,K)%DENOMINATOR%MP(2) == MUNKNO) RETURN_VALUE = .TRUE.
         ENDDO
      ENDDO
   END FUNCTION RM_IS_UNKNOWN2

!                                                                                    MAX

   SUBROUTINE FMMAX_RATIONAL(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      LOGICAL :: MA_LT_MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          MA_LT_MB = IM_COMP(R_5,'<',R_1)
      ELSE
          MA_LT_MB = IM_COMP(R_5,'>',R_1)
      ENDIF

      IF (MA_LT_MB) THEN
          CALL IMEQ(MB%NUMERATOR,  MC%NUMERATOR)
          CALL IMEQ(MB%DENOMINATOR,MC%DENOMINATOR)
      ELSE
          CALL IMEQ(MA%NUMERATOR,  MC%NUMERATOR)
          CALL IMEQ(MA%DENOMINATOR,MC%DENOMINATOR)
      ENDIF
   END SUBROUTINE FMMAX_RATIONAL


   FUNCTION FMMAX_RATIONAL_RM(MA,MB,MC,MD,ME,MF,MG,MH,MI,MJ)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      TYPE (FM_RATIONAL), OPTIONAL :: MC,MD,ME,MF,MG,MH,MI,MJ
      INTENT (IN) :: MA,MB,MC,MD,ME,MF,MG,MH,MI,MJ
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL FMMAX_RATIONAL(MA,MB,MT_RM)
      IF (PRESENT(MC)) THEN
          CALL FM_UNDEF_INP(MC)
          CALL FMMAX_RATIONAL(MT_RM,MC,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MD)) THEN
          CALL FM_UNDEF_INP(MD)
          CALL FMMAX_RATIONAL(MT_RM,MD,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(ME)) THEN
          CALL FM_UNDEF_INP(ME)
          CALL FMMAX_RATIONAL(MT_RM,ME,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MF)) THEN
          CALL FM_UNDEF_INP(MF)
          CALL FMMAX_RATIONAL(MT_RM,MF,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MG)) THEN
          CALL FM_UNDEF_INP(MG)
          CALL FMMAX_RATIONAL(MT_RM,MG,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MH)) THEN
          CALL FM_UNDEF_INP(MH)
          CALL FMMAX_RATIONAL(MT_RM,MH,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MI)) THEN
          CALL FM_UNDEF_INP(MI)
          CALL FMMAX_RATIONAL(MT_RM,MI,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MJ)) THEN
          CALL FM_UNDEF_INP(MJ)
          CALL FMMAX_RATIONAL(MT_RM,MJ,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      CALL FMEQ_RATIONAL(MT_RM,RETURN_VALUE)
   END FUNCTION FMMAX_RATIONAL_RM


!                                                                                    MIN

   SUBROUTINE FMMIN_RATIONAL(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,MC
      LOGICAL :: MA_LT_MB
      INTENT (IN) :: MA,MB
      INTENT (INOUT) :: MC
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMEQ(MB%NUMERATOR,  R_3%MIM)
      CALL IMEQ(MB%DENOMINATOR,R_4%MIM)
      CALL IM_MPY(R_1,R_4,R_5)
      CALL IM_MPY(R_2,R_3,R_1)
      IF (R_2%MIM%MP(1) * R_4%MIM%MP(1) > 0) THEN
          MA_LT_MB = IM_COMP(R_5,'<',R_1)
      ELSE
          MA_LT_MB = IM_COMP(R_5,'>',R_1)
      ENDIF

      IF (MA_LT_MB) THEN
          CALL IMEQ(MA%NUMERATOR,  MC%NUMERATOR)
          CALL IMEQ(MA%DENOMINATOR,MC%DENOMINATOR)
      ELSE
          CALL IMEQ(MB%NUMERATOR,  MC%NUMERATOR)
          CALL IMEQ(MB%DENOMINATOR,MC%DENOMINATOR)
      ENDIF
   END SUBROUTINE FMMIN_RATIONAL


   FUNCTION FMMIN_RATIONAL_RM(MA,MB,MC,MD,ME,MF,MG,MH,MI,MJ)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      TYPE (FM_RATIONAL), OPTIONAL :: MC,MD,ME,MF,MG,MH,MI,MJ
      INTENT (IN) :: MA,MB,MC,MD,ME,MF,MG,MH,MI,MJ
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL FMMIN_RATIONAL(MA,MB,MT_RM)
      IF (PRESENT(MC)) THEN
          CALL FM_UNDEF_INP(MC)
          CALL FMMIN_RATIONAL(MT_RM,MC,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MD)) THEN
          CALL FM_UNDEF_INP(MD)
          CALL FMMIN_RATIONAL(MT_RM,MD,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(ME)) THEN
          CALL FM_UNDEF_INP(ME)
          CALL FMMIN_RATIONAL(MT_RM,ME,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MF)) THEN
          CALL FM_UNDEF_INP(MF)
          CALL FMMIN_RATIONAL(MT_RM,MF,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MG)) THEN
          CALL FM_UNDEF_INP(MG)
          CALL FMMIN_RATIONAL(MT_RM,MG,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MH)) THEN
          CALL FM_UNDEF_INP(MH)
          CALL FMMIN_RATIONAL(MT_RM,MH,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MI)) THEN
          CALL FM_UNDEF_INP(MI)
          CALL FMMIN_RATIONAL(MT_RM,MI,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      IF (PRESENT(MJ)) THEN
          CALL FM_UNDEF_INP(MJ)
          CALL FMMIN_RATIONAL(MT_RM,MJ,MU_RM)
          CALL FMEQ_RATIONAL(MU_RM,MT_RM)
      ENDIF
      CALL FMEQ_RATIONAL(MT_RM,RETURN_VALUE)
   END FUNCTION FMMIN_RATIONAL_RM


!                                                                                        MOD

   FUNCTION FMMOD_RATIONAL_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL FMDIV_RATIONAL_RMRM_0(MA,MB,MT_RM)
      CALL IMDIV(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
      CALL IMMPY(R_1%MIM,MB%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MB%DENOMINATOR,MT_RM%DENOMINATOR)
      CALL FMSUB_RATIONAL_RMRM_0(MA,MT_RM,RETURN_VALUE)
   END FUNCTION FMMOD_RATIONAL_RM

   FUNCTION FMMOD_RATIONAL_RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMRM_0(MA(J),MB(J),MT_RM)
         CALL IMDIV(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
         CALL IMMPY(R_1%MIM,MB(J)%NUMERATOR,MT_RM%NUMERATOR)
         CALL IMEQ(MB(J)%DENOMINATOR,MT_RM%DENOMINATOR)
         CALL FMSUB_RATIONAL_RMRM_0(MA(J),MT_RM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMOD_RATIONAL_RM1

   FUNCTION FMMOD_RATIONAL_RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMRM_0(MA(J,K),MB(J,K),MT_RM)
            CALL IMDIV(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
            CALL IMMPY(R_1%MIM,MB(J,K)%NUMERATOR,MT_RM%NUMERATOR)
            CALL IMEQ(MB(J,K)%DENOMINATOR,MT_RM%DENOMINATOR)
            CALL FMSUB_RATIONAL_RMRM_0(MA(J,K),MT_RM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMOD_RATIONAL_RM2


!                                                                                        MODULO

   FUNCTION FMMODULO_RATIONAL_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,MB,RETURN_VALUE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      CALL FMDIV_RATIONAL_RMRM_0(MA,MB,MT_RM)
      CALL IMDIVR(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM,R_2%MIM)
      IF (MT_RM%NUMERATOR%MP(1) < 0 .AND. R_2%MIM%MP(3) /= 0) THEN
          CALL IMI2M(1,R_3%MIM)
          CALL IMSUB(R_1%MIM,R_3%MIM,R_4%MIM)
          CALL IMEQ(R_4%MIM,R_1%MIM)
      ENDIF
      CALL IMMPY(R_1%MIM,MB%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MB%DENOMINATOR,MT_RM%DENOMINATOR)
      CALL FMSUB_RATIONAL_RMRM_0(MA,MT_RM,RETURN_VALUE)
   END FUNCTION FMMODULO_RATIONAL_RM

   FUNCTION FMMODULO_RATIONAL_RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA)
         CALL FMDIV_RATIONAL_RMRM_0(MA(J),MB(J),MT_RM)
         CALL IMDIVR(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM,R_2%MIM)
         IF (MT_RM%NUMERATOR%MP(1) < 0 .AND. R_2%MIM%MP(3) /= 0) THEN
             CALL IMI2M(1,R_3%MIM)
             CALL IMSUB(R_1%MIM,R_3%MIM,R_4%MIM)
             CALL IMEQ(R_4%MIM,R_1%MIM)
         ENDIF
         CALL IMMPY(R_1%MIM,MB(J)%NUMERATOR,MT_RM%NUMERATOR)
         CALL IMEQ(MB(J)%DENOMINATOR,MT_RM%DENOMINATOR)
         CALL FMSUB_RATIONAL_RMRM_0(MA(J),MT_RM,RETURN_VALUE(J))
      ENDDO
   END FUNCTION FMMODULO_RATIONAL_RM1

   FUNCTION FMMODULO_RATIONAL_RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA,MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL FMDIV_RATIONAL_RMRM_0(MA(J,K),MB(J,K),MT_RM)
            CALL IMDIVR(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM,R_2%MIM)
            IF (MT_RM%NUMERATOR%MP(1) < 0 .AND. R_2%MIM%MP(3) /= 0) THEN
                CALL IMI2M(1,R_3%MIM)
                CALL IMSUB(R_1%MIM,R_3%MIM,R_4%MIM)
                CALL IMEQ(R_4%MIM,R_1%MIM)
            ENDIF
            CALL IMMPY(R_1%MIM,MB(J,K)%NUMERATOR,MT_RM%NUMERATOR)
            CALL IMEQ(MB(J,K)%DENOMINATOR,MT_RM%DENOMINATOR)
            CALL FMSUB_RATIONAL_RMRM_0(MA(J,K),MT_RM,RETURN_VALUE(J,K))
         ENDDO
      ENDDO
   END FUNCTION FMMODULO_RATIONAL_RM2


!                                                                                        NINT

   FUNCTION FMNINT_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTENT (IN) :: MA
      LOGICAL, EXTERNAL :: IMCOMP
      CALL FM_UNDEF_INP(MA)
      CALL IMABS(MA%NUMERATOR,R_1%MIM)
      CALL IMABS(MA%DENOMINATOR,R_2%MIM)
      CALL IMDIVR(R_1%MIM,R_2%MIM,R_3%MIM,R_4%MIM)
      CALL IMMPYI(R_4%MIM,2,R_5%MIM)
      IF (IMCOMP(R_5%MIM,'>=',MA%DENOMINATOR)) THEN
          CALL IMI2M(1,R_4%MIM)
          CALL IMADD(R_3%MIM,R_4%MIM,RETURN_VALUE%NUMERATOR)
      ELSE
          CALL IMEQ(R_3%MIM,RETURN_VALUE%NUMERATOR)
      ENDIF
      CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
      IF (MA%NUMERATOR%MP(1) < 0) RETURN_VALUE%NUMERATOR%MP(1) = -1
   END FUNCTION FMNINT_RATIONAL_RM

   FUNCTION FMNINT_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      LOGICAL, EXTERNAL :: IMCOMP
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMABS(MA(J)%NUMERATOR,R_1%MIM)
         CALL IMABS(MA(J)%DENOMINATOR,R_2%MIM)
         CALL IMDIVR(R_1%MIM,R_2%MIM,R_3%MIM,R_4%MIM)
         CALL IMMPYI(R_4%MIM,2,R_5%MIM)
         IF (IMCOMP(R_5%MIM,'>=',MA(J)%DENOMINATOR)) THEN
             CALL IMI2M(1,R_4%MIM)
             CALL IMADD(R_3%MIM,R_4%MIM,RETURN_VALUE(J)%NUMERATOR)
         ELSE
             CALL IMEQ(R_3%MIM,RETURN_VALUE(J)%NUMERATOR)
         ENDIF
         CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
         IF (MA(J)%NUMERATOR%MP(1) < 0) RETURN_VALUE(J)%NUMERATOR%MP(1) = -1
      ENDDO
   END FUNCTION FMNINT_RATIONAL_RM1

   FUNCTION FMNINT_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      LOGICAL, EXTERNAL :: IMCOMP
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMABS(MA(J,K)%NUMERATOR,R_1%MIM)
            CALL IMABS(MA(J,K)%DENOMINATOR,R_2%MIM)
            CALL IMDIVR(R_1%MIM,R_2%MIM,R_3%MIM,R_4%MIM)
            CALL IMMPYI(R_4%MIM,2,R_5%MIM)
            IF (IMCOMP(R_5%MIM,'>=',MA(J,K)%DENOMINATOR)) THEN
                CALL IMI2M(1,R_4%MIM)
                CALL IMADD(R_3%MIM,R_4%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ELSE
                CALL IMEQ(R_3%MIM,RETURN_VALUE(J,K)%NUMERATOR)
            ENDIF
            CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
            IF (MA(J,K)%NUMERATOR%MP(1) < 0)  &
                RETURN_VALUE(J,K)%NUMERATOR%MP(1) = -1
         ENDDO
      ENDDO
   END FUNCTION FMNINT_RATIONAL_RM2


!                                                                                   ** integer power

   FUNCTION FMIPWR_RATIONAL_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA,RETURN_VALUE
      INTEGER :: MB
      INTENT (IN) :: MA, MB
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA%NUMERATOR,  R_1%MIM)
      CALL IMEQ(MA%DENOMINATOR,R_2%MIM)
      CALL IMI2M(MB,           R_3%MIM)
      IF (MB > 0) THEN
          CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE IF (MB < 0) THEN
          R_3%MIM%MP(1) = 1
          CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE%NUMERATOR)
          CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE%DENOMINATOR)
      ELSE
          CALL IMI2M(1,RETURN_VALUE%NUMERATOR)
          CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
      ENDIF
      CALL FM_MAX_EXP_RM(RETURN_VALUE)
      IF (RETURN_VALUE%DENOMINATOR%MP(1) < 0) THEN
          RETURN_VALUE%DENOMINATOR%MP(1) = 1
          RETURN_VALUE%NUMERATOR%MP(1) = -RETURN_VALUE%NUMERATOR%MP(1)
      ENDIF
   END FUNCTION FMIPWR_RATIONAL_RM

   FUNCTION FMIPWR_RATIONAL_RM1(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA, MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA)
         CALL IMEQ(MA(J)%NUMERATOR,  R_1%MIM)
         CALL IMEQ(MA(J)%DENOMINATOR,R_2%MIM)
         CALL IMI2M(MB,           R_3%MIM)
         IF (MB > 0) THEN
             CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE(J)%NUMERATOR)
             CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE(J)%DENOMINATOR)
         ELSE IF (MB < 0) THEN
             R_3%MIM%MP(1) = 1
             CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE(J)%NUMERATOR)
             CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE(J)%DENOMINATOR)
         ELSE
             CALL IMI2M(1,RETURN_VALUE(J)%NUMERATOR)
             CALL IMI2M(1,RETURN_VALUE(J)%DENOMINATOR)
         ENDIF
         CALL FM_MAX_EXP_RM(RETURN_VALUE(J))
         IF (RETURN_VALUE(J)%DENOMINATOR%MP(1) < 0) THEN
             RETURN_VALUE(J)%DENOMINATOR%MP(1) = 1
             RETURN_VALUE(J)%NUMERATOR%MP(1) =  &
             -RETURN_VALUE(J)%NUMERATOR%MP(1)
         ENDIF
      ENDDO
   END FUNCTION FMIPWR_RATIONAL_RM1

   FUNCTION FMIPWR_RATIONAL_RM2(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      INTEGER :: MB
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMEQ(MA(J,K)%NUMERATOR,  R_1%MIM)
            CALL IMEQ(MA(J,K)%DENOMINATOR,R_2%MIM)
            CALL IMI2M(MB,           R_3%MIM)
            IF (MB > 0) THEN
                CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE(J,K)%DENOMINATOR)
            ELSE IF (MB < 0) THEN
                R_3%MIM%MP(1) = 1
                CALL IMPWR(R_2%MIM,R_3%MIM,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMPWR(R_1%MIM,R_3%MIM,RETURN_VALUE(J,K)%DENOMINATOR)
            ELSE
                CALL IMI2M(1,RETURN_VALUE(J,K)%NUMERATOR)
                CALL IMI2M(1,RETURN_VALUE(J,K)%DENOMINATOR)
            ENDIF
            CALL FM_MAX_EXP_RM(RETURN_VALUE(J,K))
            IF (RETURN_VALUE(J,K)%DENOMINATOR%MP(1) < 0) THEN
                RETURN_VALUE(J,K)%DENOMINATOR%MP(1) = 1
                RETURN_VALUE(J,K)%NUMERATOR%MP(1) =  &
                -RETURN_VALUE(J,K)%NUMERATOR%MP(1)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMIPWR_RATIONAL_RM2


 END MODULE FM_RATIONAL_ARITHMETIC_7


 MODULE FM_RATIONAL_ARITHMETIC_8
    USE FM_RATIONAL_ARITHMETIC_1

   INTERFACE TO_FM
       MODULE PROCEDURE FM_RATIONAL_RM
       MODULE PROCEDURE FM_RATIONAL_RM1
       MODULE PROCEDURE FM_RATIONAL_RM2
   END INTERFACE

   INTERFACE RATIONAL_APPROX
       MODULE PROCEDURE FM_RATIONAL_APPROX_RM
   END INTERFACE

   INTERFACE DOT_PRODUCT
      MODULE PROCEDURE FMDOTPRODUCT_RM
   END INTERFACE

   INTERFACE MATMUL
      MODULE PROCEDURE FMMATMUL22_RM
      MODULE PROCEDURE FMMATMUL12_RM
      MODULE PROCEDURE FMMATMUL21_RM
   END INTERFACE

   INTERFACE MAXLOC
      MODULE PROCEDURE FMMAXLOC_RM1
      MODULE PROCEDURE FMMAXLOC_RM2
   END INTERFACE

   INTERFACE MAXVAL
      MODULE PROCEDURE FMMAXVAL_RM1
      MODULE PROCEDURE FMMAXVAL_RM2
   END INTERFACE

   INTERFACE MINLOC
      MODULE PROCEDURE FMMINLOC_RM1
      MODULE PROCEDURE FMMINLOC_RM2
   END INTERFACE

   INTERFACE MINVAL
      MODULE PROCEDURE FMMINVAL_RM1
      MODULE PROCEDURE FMMINVAL_RM2
   END INTERFACE

   INTERFACE PRODUCT
      MODULE PROCEDURE FMPRODUCT_RM1
      MODULE PROCEDURE FMPRODUCT_RM2
   END INTERFACE

   INTERFACE SUM
      MODULE PROCEDURE FMSUM_RM1
      MODULE PROCEDURE FMSUM_RM2
   END INTERFACE

   INTERFACE TRANSPOSE
      MODULE PROCEDURE FMTRANSPOSE_RM
   END INTERFACE

 CONTAINS

!                                                                                        TO_FM

   FUNCTION FM_RATIONAL_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA
      TYPE (FM) :: RETURN_VALUE
      INTENT (IN) :: MA
      INTEGER :: NDSAVE
      CALL FM_UNDEF_INP(MA)
      NDSAVE = NDIG
      NDIG = NDIG + NGRD21
      CALL IMI2FM(MA%NUMERATOR,F_1%MFM)
      CALL IMI2FM(MA%DENOMINATOR,F_2%MFM)
      CALL FMDIV(F_1%MFM,F_2%MFM,RETURN_VALUE%MFM)
      CALL FMEQU_R1(RETURN_VALUE%MFM,NDIG,NDSAVE)
      NDIG = NDSAVE
   END FUNCTION FM_RATIONAL_RM

   FUNCTION FM_RATIONAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA)) :: RETURN_VALUE
      INTEGER :: J
      INTENT (IN) :: MA
      INTEGER :: NDSAVE
      CALL FM_UNDEF_INP(MA)
      NDSAVE = NDIG
      NDIG = NDIG + NGRD21
      DO J = 1, SIZE(MA)
         CALL IMI2FM(MA(J)%NUMERATOR,F_1%MFM)
         CALL IMI2FM(MA(J)%DENOMINATOR,F_2%MFM)
         CALL FMDIV(F_1%MFM,F_2%MFM,RETURN_VALUE(J)%MFM)
         CALL FMEQU_R1(RETURN_VALUE(J)%MFM,NDIG,NDSAVE)
      ENDDO
      NDIG = NDSAVE
   END FUNCTION FM_RATIONAL_RM1

   FUNCTION FM_RATIONAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL), DIMENSION(:,:) :: MA
      TYPE (FM), DIMENSION(SIZE(MA,DIM=1),SIZE(MA,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K
      INTENT (IN) :: MA
      INTEGER :: NDSAVE
      CALL FM_UNDEF_INP(MA)
      NDSAVE = NDIG
      NDIG = NDIG + NGRD21
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            CALL IMI2FM(MA(J,K)%NUMERATOR,F_1%MFM)
            CALL IMI2FM(MA(J,K)%DENOMINATOR,F_2%MFM)
            CALL FMDIV(F_1%MFM,F_2%MFM,RETURN_VALUE(J,K)%MFM)
            CALL FMEQU_R1(RETURN_VALUE(J,K)%MFM,NDIG,NDSAVE)
         ENDDO
      ENDDO
      NDIG = NDSAVE
   END FUNCTION FM_RATIONAL_RM2


!                                                                             Rational approximation

   FUNCTION FM_RATIONAL_APPROX_RM(MA,DIGITS)     RESULT (RETURN_VALUE)

!  Return a TYPE (FM_RATIONAL) result that approximates TYPE (FM) input MA so that the
!  numerator and denominator of the result have no more than DIGITS digits in base 10.
!  Ex:  MA = pi, DIGITS = 2   gives      22 /      7
!       MA = pi, DIGITS = 3   gives     355 /    113
!       MA = pi, DIGITS = 6   gives  833719 / 265381

!  The rational result usually approximates MA to about 2*DIGITS significant digits, so
!  DIGITS should not be more than about half the precision carried for the FM result.
!  Ex:  833719 / 265381 = 3.141592653581077771204419306... agrees with pi to about 11 s.d.

      USE FMVALS
      IMPLICIT NONE
      TYPE (FM) :: MA
      INTEGER :: DIGITS,J,N
      TYPE(MULTI), SAVE :: A, B, B1, RA1, RA2, RA3, RB1, RB2, RB3, CF
      TYPE (FM_RATIONAL) :: RETURN_VALUE
      LOGICAL, EXTERNAL :: FMCOMP
      INTENT (IN) :: MA,DIGITS
      CALL FM_UNDEF_INP(MA)

      CALL FMABS(MA%MFM,A)
      N = DIGITS
      N = MAX(N,1)
      J = (ALOGMT*NDIG)/2
      N = MIN(N,J)
      CALL FMI2M(10,B1)
      CALL FMIPWR(B1,N,B)

      CALL FMINT(A,RA1)
      CALL FMI2M(1,RA2)
      CALL FMI2M(0,RA3)
      CALL FMI2M(1,RB1)
      CALL FMI2M(0,RB2)
      CALL FMI2M(1,RB3)

      CALL FMSUB_R1(A,RA1)
      DO J = 1, 1000
         IF (FMCOMP(RA1,'>=',B) .OR. FMCOMP(RB1,'>=',B) .OR. A%MP(3) == 0) THEN
             IF (J > 1 .AND. A%MP(3) /= 0) THEN
                 CALL FMEQ(RA2,RETURN_VALUE%NUMERATOR)
                 CALL FMEQ(RB2,RETURN_VALUE%DENOMINATOR)
             ELSE
                 CALL FMEQ(RA1,RETURN_VALUE%NUMERATOR)
                 CALL FMEQ(RB1,RETURN_VALUE%DENOMINATOR)
             ENDIF
             IF (MA < 0) RETURN_VALUE%NUMERATOR%MP(1) = -1
             EXIT
         ENDIF
         CALL FMI2M(1,B1)
         CALL FMDIV_R2(B1,A)
         CALL FMINT(A,CF)

         CALL FMEQ(RA2,RA3)
         CALL FMEQ(RA1,RA2)
         CALL FMMPY(CF,RA2,B1)
         CALL FMADD(B1,RA3,RA1)

         CALL FMEQ(RB2,RB3)
         CALL FMEQ(RB1,RB2)
         CALL FMMPY(CF,RB2,B1)
         CALL FMADD(B1,RB3,RB1)

         CALL FMSUB_R1(A,CF)
      ENDDO

   END FUNCTION FM_RATIONAL_APPROX_RM

!                                                         DOT_PRODUCT

   FUNCTION FMDOTPRODUCT_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),MB(:),RETURN_VALUE
      INTEGER :: J,JA,JB
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      IF (SIZE(MA) == SIZE(MB)) THEN
          CALL IMI2M(0,RETURN_VALUE%NUMERATOR)
          CALL IMI2M(1,RETURN_VALUE%DENOMINATOR)
          DO J = 1, SIZE(MA)
             JA = LBOUND(MA,DIM=1) + J - 1
             JB = LBOUND(MB,DIM=1) + J - 1
             CALL FMMPY_RATIONAL_RMRM_0(MA(JA),MB(JB),MT_RM)
             CALL FMADD_RATIONAL_RMRM_0(RETURN_VALUE,MT_RM,MU_RM)
             CALL IMEQ(MU_RM%NUMERATOR,RETURN_VALUE%NUMERATOR)
             CALL IMEQ(MU_RM%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
          ENDDO
      ELSE
          CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
          CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
      ENDIF
   END FUNCTION FMDOTPRODUCT_RM

!                                                         MATMUL

   FUNCTION FMMATMUL22_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),MB(:,:)
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1),SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: I,J,K,MXSAVE
      LOGICAL :: SKIP_GCD_SAVE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      SKIP_GCD_SAVE = SKIP_GCD
      IF (SIZE(MA,DIM=2) == SIZE(MB,DIM=1)) THEN
          MXSAVE = MXEXP
          DO I = 1, SIZE(MA,DIM=1)
             DO J = 1, SIZE(MB,DIM=2)
                MXEXP = MXEXP2
                CALL IMI2M(0,MT_RM%NUMERATOR)
                CALL IMI2M(1,MT_RM%DENOMINATOR)
                DO K = 1, SIZE(MA,DIM=2)
                   SKIP_GCD = .TRUE.
                   CALL FMMPY_RATIONAL_RMRM_0(MA(I,K),MB(K,J),MU_RM)
                   IF (K == SIZE(MA,DIM=2)) SKIP_GCD = .FALSE.
                   CALL FMADD_RATIONAL_RMRM_0(MT_RM,MU_RM,RETURN_VALUE(I,J))
                   CALL IMEQ(RETURN_VALUE(I,J)%NUMERATOR,MT_RM%NUMERATOR)
                   CALL IMEQ(RETURN_VALUE(I,J)%DENOMINATOR,MT_RM%DENOMINATOR)
                ENDDO
                MXEXP = MXSAVE
             ENDDO
          ENDDO
      ELSE
          DO I = 1, SIZE(MA,DIM=1)
             DO J = 1, SIZE(MB,DIM=2)
                CALL IMST2M('UNKNOWN',RETURN_VALUE(I,J)%NUMERATOR)
                CALL IMST2M('UNKNOWN',RETURN_VALUE(I,J)%DENOMINATOR)
             ENDDO
          ENDDO
      ENDIF
      SKIP_GCD = SKIP_GCD_SAVE
   END FUNCTION FMMATMUL22_RM

   FUNCTION FMMATMUL12_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),MB(:,:)
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MB,DIM=2)) :: RETURN_VALUE
      INTEGER :: J,K,MXSAVE
      LOGICAL :: SKIP_GCD_SAVE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      SKIP_GCD_SAVE = SKIP_GCD
      IF (SIZE(MA) == SIZE(MB,DIM=1)) THEN
          MXSAVE = MXEXP
          DO J = 1, SIZE(MB,DIM=2)
             MXEXP = MXEXP2
             CALL IMI2M(0,MT_RM%NUMERATOR)
             CALL IMI2M(1,MT_RM%DENOMINATOR)
             DO K = 1, SIZE(MA,DIM=1)
                SKIP_GCD = .TRUE.
                CALL FMMPY_RATIONAL_RMRM_0(MA(K),MB(K,J),MU_RM)
                IF (K == SIZE(MA,DIM=1)) SKIP_GCD = .FALSE.
                CALL FMADD_RATIONAL_RMRM_0(MT_RM,MU_RM,RETURN_VALUE(J))
                CALL IMEQ(RETURN_VALUE(J)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(RETURN_VALUE(J)%DENOMINATOR,MT_RM%DENOMINATOR)
             ENDDO
             MXEXP = MXSAVE
          ENDDO
      ELSE
          DO J = 1, SIZE(MB,DIM=2)
             CALL IMST2M('UNKNOWN',RETURN_VALUE(J)%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
      ENDIF
      SKIP_GCD = SKIP_GCD_SAVE
   END FUNCTION FMMATMUL12_RM

   FUNCTION FMMATMUL21_RM(MA,MB)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),MB(:)
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=1)) :: RETURN_VALUE
      INTEGER :: J,K,MXSAVE
      LOGICAL :: SKIP_GCD_SAVE
      INTENT (IN) :: MA,MB
      CALL FM_UNDEF_INP(MA)
      CALL FM_UNDEF_INP(MB)
      SKIP_GCD_SAVE = SKIP_GCD
      IF (SIZE(MB) == SIZE(MA,DIM=2)) THEN
          MXSAVE = MXEXP
          DO J = 1, SIZE(MA,DIM=1)
             MXEXP = MXEXP2
             CALL IMI2M(0,MT_RM%NUMERATOR)
             CALL IMI2M(1,MT_RM%DENOMINATOR)
             DO K = 1, SIZE(MB,DIM=1)
                SKIP_GCD = .TRUE.
                CALL FMMPY_RATIONAL_RMRM_0(MA(J,K),MB(K),MU_RM)
                IF (K == SIZE(MB,DIM=1)) SKIP_GCD = .FALSE.
                CALL FMADD_RATIONAL_RMRM_0(MT_RM,MU_RM,RETURN_VALUE(J))
                CALL IMEQ(RETURN_VALUE(J)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(RETURN_VALUE(J)%DENOMINATOR,MT_RM%DENOMINATOR)
             ENDDO
             MXEXP = MXSAVE
          ENDDO
      ELSE
          DO J = 1, SIZE(MA,DIM=1)
             CALL IMST2M('UNKNOWN',RETURN_VALUE(J)%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE(J)%DENOMINATOR)
          ENDDO
      ENDIF
      SKIP_GCD = SKIP_GCD_SAVE
   END FUNCTION FMMATMUL21_RM

!                                                              MAXLOC

   FUNCTION FMMAXLOC_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:)
      INTEGER :: RETURN_VALUE
      INTEGER :: J,JA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      RETURN_VALUE = LBOUND(MA,DIM=1)
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             RETURN_VALUE = -1
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL IMMPY(MA(JA)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
         CALL IMMPY(MA(JA)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
         IF (IM_COMP(R_1,'>',R_2)) THEN
             CALL IMEQ(MA(JA)%NUMERATOR,MT_RM%NUMERATOR)
             CALL IMEQ(MA(JA)%DENOMINATOR,MT_RM%DENOMINATOR)
             RETURN_VALUE = JA
         ENDIF
      ENDDO
   END FUNCTION FMMAXLOC_RM1

   FUNCTION FMMAXLOC_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:)
      INTEGER :: RETURN_VALUE(2)
      INTEGER :: J,K,JA,JB
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      RETURN_VALUE = (/ LBOUND(MA,DIM=1), LBOUND(MA,DIM=2) /)
      DO K = 1, SIZE(MA,DIM=2)
         DO J = 1, SIZE(MA,DIM=1)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                RETURN_VALUE = (/ -1, -1 /)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            CALL IMMPY(MA(JA,JB)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
            CALL IMMPY(MA(JA,JB)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
            IF (IM_COMP(R_1,'>',R_2)) THEN
                CALL IMEQ(MA(JA,JB)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(MA(JA,JB)%DENOMINATOR,MT_RM%DENOMINATOR)
                RETURN_VALUE = (/ JA, JB /)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMMAXLOC_RM2

!                                                              MAXVAL

   FUNCTION FMMAXVAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),RETURN_VALUE
      INTEGER :: J,JA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL IMMPY(MA(JA)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
         CALL IMMPY(MA(JA)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
         IF (IM_COMP(R_1,'>',R_2)) THEN
             CALL IMEQ(MA(JA)%NUMERATOR,MT_RM%NUMERATOR)
             CALL IMEQ(MA(JA)%DENOMINATOR,MT_RM%DENOMINATOR)
         ENDIF
      ENDDO
      CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE%NUMERATOR)
      CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMMAXVAL_RM1

   FUNCTION FMMAXVAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),RETURN_VALUE
      INTEGER :: J,K,JA,JB
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
                CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            CALL IMMPY(MA(JA,JB)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
            CALL IMMPY(MA(JA,JB)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
            IF (IM_COMP(R_1,'>',R_2)) THEN
                CALL IMEQ(MA(JA,JB)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(MA(JA,JB)%DENOMINATOR,MT_RM%DENOMINATOR)
            ENDIF
         ENDDO
      ENDDO
      CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE%NUMERATOR)
      CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMMAXVAL_RM2

!                                                              MINLOC

   FUNCTION FMMINLOC_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:)
      INTEGER :: RETURN_VALUE
      INTEGER :: J,JA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      RETURN_VALUE = LBOUND(MA,DIM=1)
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             RETURN_VALUE = -1
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL IMMPY(MA(JA)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
         CALL IMMPY(MA(JA)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
         IF (IM_COMP(R_1,'<',R_2)) THEN
             CALL IMEQ(MA(JA)%NUMERATOR,MT_RM%NUMERATOR)
             CALL IMEQ(MA(JA)%DENOMINATOR,MT_RM%DENOMINATOR)
             RETURN_VALUE = JA
         ENDIF
      ENDDO
   END FUNCTION FMMINLOC_RM1

   FUNCTION FMMINLOC_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:)
      INTEGER :: RETURN_VALUE(2)
      INTEGER :: J,K,JA,JB
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      RETURN_VALUE = (/ LBOUND(MA,DIM=1), LBOUND(MA,DIM=2) /)
      DO K = 1, SIZE(MA,DIM=2)
         DO J = 1, SIZE(MA,DIM=1)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                RETURN_VALUE = (/ -1, -1 /)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            CALL IMMPY(MA(JA,JB)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
            CALL IMMPY(MA(JA,JB)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
            IF (IM_COMP(R_1,'<',R_2)) THEN
                CALL IMEQ(MA(JA,JB)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(MA(JA,JB)%DENOMINATOR,MT_RM%DENOMINATOR)
                RETURN_VALUE = (/ JA, JB /)
            ENDIF
         ENDDO
      ENDDO
   END FUNCTION FMMINLOC_RM2

!                                                              MINVAL

   FUNCTION FMMINVAL_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),RETURN_VALUE
      INTEGER :: J,JA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL IMMPY(MA(JA)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
         CALL IMMPY(MA(JA)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
         IF (IM_COMP(R_1,'<',R_2)) THEN
             CALL IMEQ(MA(JA)%NUMERATOR,MT_RM%NUMERATOR)
             CALL IMEQ(MA(JA)%DENOMINATOR,MT_RM%DENOMINATOR)
         ENDIF
      ENDDO
      CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE%NUMERATOR)
      CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMMINVAL_RM1

   FUNCTION FMMINVAL_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),RETURN_VALUE
      INTEGER :: J,K,JA,JB
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
                CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            CALL IMMPY(MA(JA,JB)%NUMERATOR,MT_RM%DENOMINATOR,R_1%MIM)
            CALL IMMPY(MA(JA,JB)%DENOMINATOR,MT_RM%NUMERATOR,R_2%MIM)
            IF (IM_COMP(R_1,'<',R_2)) THEN
                CALL IMEQ(MA(JA,JB)%NUMERATOR,MT_RM%NUMERATOR)
                CALL IMEQ(MA(JA,JB)%DENOMINATOR,MT_RM%DENOMINATOR)
            ENDIF
         ENDDO
      ENDDO
      CALL IMEQ(MT_RM%NUMERATOR,RETURN_VALUE%NUMERATOR)
      CALL IMEQ(MT_RM%DENOMINATOR,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMMINVAL_RM2

!                                                              PRODUCT

   FUNCTION FMPRODUCT_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),RETURN_VALUE
      INTEGER :: J,JA
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL IMMPY(MA(JA)%NUMERATOR,MT_RM%NUMERATOR,R_1%MIM)
         CALL IMMPY(MA(JA)%DENOMINATOR,MT_RM%DENOMINATOR,R_2%MIM)
         CALL IMEQ(R_1%MIM,MT_RM%NUMERATOR)
         CALL IMEQ(R_2%MIM,MT_RM%DENOMINATOR)
      ENDDO
      CALL IMGCD(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_3%MIM)
      CALL IMDIV(MT_RM%NUMERATOR,R_3%MIM,RETURN_VALUE%NUMERATOR)
      CALL IMDIV(MT_RM%DENOMINATOR,R_3%MIM,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMPRODUCT_RM1

   FUNCTION FMPRODUCT_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),RETURN_VALUE
      INTEGER :: J,K,JA,JB
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
                CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            CALL IMMPY(MA(JA,JB)%NUMERATOR,MT_RM%NUMERATOR,R_1%MIM)
            CALL IMMPY(MA(JA,JB)%DENOMINATOR,MT_RM%DENOMINATOR,R_2%MIM)
            CALL IMEQ(R_1%MIM,MT_RM%NUMERATOR)
            CALL IMEQ(R_2%MIM,MT_RM%DENOMINATOR)
         ENDDO
      ENDDO
      CALL IMGCD(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_3%MIM)
      CALL IMDIV(MT_RM%NUMERATOR,R_3%MIM,RETURN_VALUE%NUMERATOR)
      CALL IMDIV(MT_RM%DENOMINATOR,R_3%MIM,RETURN_VALUE%DENOMINATOR)
   END FUNCTION FMPRODUCT_RM2

!                                                              SUM

   FUNCTION FMSUM_RM1(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:),RETURN_VALUE
      INTEGER :: J,JA
      LOGICAL :: SKIP_GCD_SAVE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      SKIP_GCD_SAVE = SKIP_GCD
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1))%DENOMINATOR,MT_RM%DENOMINATOR)
      SKIP_GCD = .TRUE.
      DO J = 1, SIZE(MA)
         JA = LBOUND(MA,DIM=1) + J - 1
         IF (MA(JA)%NUMERATOR%MP(2) == MUNKNO .OR.  &
             MA(JA)%DENOMINATOR%MP(2) == MUNKNO) THEN
             CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
             CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
             RETURN
         ENDIF
         IF (J == 1) CYCLE
         CALL FMADD_RATIONAL_RMRM_0(MT_RM,MA(JA),MU_RM)
         CALL IMEQ(MU_RM%NUMERATOR,MT_RM%NUMERATOR)
         CALL IMEQ(MU_RM%DENOMINATOR,MT_RM%DENOMINATOR)
      ENDDO
      SKIP_GCD = .FALSE.
      CALL IMGCD(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_3%MIM)
      CALL IMDIV(MT_RM%NUMERATOR,R_3%MIM,RETURN_VALUE%NUMERATOR)
      CALL IMDIV(MT_RM%DENOMINATOR,R_3%MIM,RETURN_VALUE%DENOMINATOR)
      SKIP_GCD = SKIP_GCD_SAVE
   END FUNCTION FMSUM_RM1

   FUNCTION FMSUM_RM2(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:),RETURN_VALUE
      INTEGER :: J,K,JA,JB
      LOGICAL :: SKIP_GCD_SAVE
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      SKIP_GCD_SAVE = SKIP_GCD
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%NUMERATOR,MT_RM%NUMERATOR)
      CALL IMEQ(MA(LBOUND(MA,DIM=1),LBOUND(MA,DIM=2))%DENOMINATOR,MT_RM%DENOMINATOR)
      DO J = 1, SIZE(MA,DIM=1)
         DO K = 1, SIZE(MA,DIM=2)
            JA = LBOUND(MA,DIM=1) + J - 1
            JB = LBOUND(MA,DIM=2) + K - 1
            IF (MA(JA,JB)%NUMERATOR%MP(2) == MUNKNO .OR.  &
                MA(JA,JB)%DENOMINATOR%MP(2) == MUNKNO) THEN
                CALL IMST2M('UNKNOWN',RETURN_VALUE%NUMERATOR)
                CALL IMST2M('UNKNOWN',RETURN_VALUE%DENOMINATOR)
                RETURN
            ENDIF
            IF (J == 1 .AND. K == 1) CYCLE
            SKIP_GCD = .TRUE.
            IF (K == 1) SKIP_GCD = .FALSE.
            CALL FMADD_RATIONAL_RMRM_0(MT_RM,MA(JA,JB),MU_RM)
            CALL IMEQ(MU_RM%NUMERATOR,MT_RM%NUMERATOR)
            CALL IMEQ(MU_RM%DENOMINATOR,MT_RM%DENOMINATOR)
         ENDDO
      ENDDO
      SKIP_GCD = .FALSE.
      CALL IMGCD(MT_RM%NUMERATOR,MT_RM%DENOMINATOR,R_3%MIM)
      CALL IMDIV(MT_RM%NUMERATOR,R_3%MIM,RETURN_VALUE%NUMERATOR)
      CALL IMDIV(MT_RM%DENOMINATOR,R_3%MIM,RETURN_VALUE%DENOMINATOR)
      SKIP_GCD = SKIP_GCD_SAVE
   END FUNCTION FMSUM_RM2

!                                                           TRANSPOSE

   FUNCTION FMTRANSPOSE_RM(MA)     RESULT (RETURN_VALUE)
      USE FMVALS
      IMPLICIT NONE
      TYPE (FM_RATIONAL) :: MA(:,:)
      TYPE (FM_RATIONAL), DIMENSION(SIZE(MA,DIM=2),SIZE(MA,DIM=1)) :: RETURN_VALUE
      INTEGER :: I,J
      INTENT (IN) :: MA
      CALL FM_UNDEF_INP(MA)
      DO I = 1, SIZE(MA,DIM=1)
         DO J = 1, SIZE(MA,DIM=2)
            CALL IMEQ(MA(I,J)%NUMERATOR,RETURN_VALUE(J,I)%NUMERATOR)
            CALL IMEQ(MA(I,J)%DENOMINATOR,RETURN_VALUE(J,I)%DENOMINATOR)
         ENDDO
      ENDDO
   END FUNCTION FMTRANSPOSE_RM


 END MODULE FM_RATIONAL_ARITHMETIC_8


 MODULE FM_RATIONAL_ARITHMETIC

   USE FM_RATIONAL_ARITHMETIC_1
   USE FM_RATIONAL_ARITHMETIC_2
   USE FM_RATIONAL_ARITHMETIC_3
   USE FM_RATIONAL_ARITHMETIC_4
   USE FM_RATIONAL_ARITHMETIC_5
   USE FM_RATIONAL_ARITHMETIC_6
   USE FM_RATIONAL_ARITHMETIC_7
   USE FM_RATIONAL_ARITHMETIC_8

   CONTAINS

!  Since finding exact inverse matrices and solutions to linear systems are such common
!  applications for the rational arithmetic package, they are included here in the basic
!  module file rather than in a separate file like FM_LIN_SOLVE and its related routines.

      SUBROUTINE RM_LIN_SOLVE(A,X,B,N,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Find the exact rational solution to the linear system  A X = B, where:

!  A   is the matrix of the system, containing the  N x N coefficient matrix.

!  B   is the  N x 1  right-hand-side vector.

!  X   is the returned  N x 1  solution vector.

!  DET is returned as the determinant of A.
!      Nonzero DET means a solution was found.
!      DET = 0 is returned if the system is singular.

!  A,X,B,DET are all TYPE (FM_RATIONAL) multiprecision rational variables.

!  This routine uses modular integer arithmetic to solve related integer systems A1 x1 = b1 Mod p
!  for many different primes p, then these solutions are combined using the Chinese Remainder
!  Theorem to recover the original rational solution X.

      INTEGER :: N
      TYPE (FM_RATIONAL) :: A(N,N), B(N), X(N), DET
      TYPE (IM), SAVE :: PC, ABMPY, G, T, DET_IM, LAST_DET_IM, PRIME_PRODUCT
      TYPE (IM), ALLOCATABLE :: A1(:,:), XT(:), PRIME_MK(:)
      DOUBLE PRECISION, ALLOCATABLE :: A2(:,:), MOD_SOLUTION(:,:), PRIMES(:), DET_P(:), Y(:),  &
                                       Z_MS(:,:), Z_P(:), Z_D(:)
      DOUBLE PRECISION :: P, R
      LOGICAL :: SINGULAR
      INTEGER :: I, I_EXTRA, J, JP, K, L, MAX_PRIMES, PRIMES_USED

      IF (N <= 0) THEN
          DET = 0
          RETURN
      ENDIF
      IF (N == 1) THEN
          DET = A(1,1)
          IF (A(1,1) /= 0) THEN
              X(1) = B(1) / A(1,1)
          ELSE
              X(1) = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          ENDIF
          RETURN
      ENDIF

      MAX_PRIMES = 1001
      ALLOCATE(A1(N,N+1),A2(N,N+1),XT(N),PRIME_MK(MAX_PRIMES),MOD_SOLUTION(N,MAX_PRIMES),  &
               PRIMES(MAX_PRIMES),DET_P(MAX_PRIMES),Y(MAX_PRIMES),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in RM_LIN_SOLVE.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      A1 = 0
      XT = 0
      G = 0
      T = 0

!             Copy A and B to augmented matrix A1 and convert to integer coefficients.

      DET = 1
      DO I = 1, N
         ABMPY = 1
         DO J = 1, N
            IF (A(I,J)%DENOMINATOR%MP(2) == 1 .AND.  &
                A(I,J)%DENOMINATOR%MP(3) == 1) CYCLE
            CALL IMGCD(ABMPY%MIM,A(I,J)%DENOMINATOR,G%MIM)
            CALL IMMPY(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
            IF (G%MIM%MP(2) == 1 .AND. G%MIM%MP(3) == 1) THEN
                CALL IMEQ(T%MIM,ABMPY%MIM)
            ELSE
                CALL IMDIV(T%MIM,G%MIM,ABMPY%MIM)
            ENDIF
         ENDDO
         CALL IMGCD(ABMPY%MIM,B(I)%DENOMINATOR,G%MIM)
         CALL IMMPY(ABMPY%MIM,B(I)%DENOMINATOR,T%MIM)
         CALL IMDIV(T%MIM,G%MIM,ABMPY%MIM)

         IF (ABMPY%MIM%MP(2) == 1 .AND. ABMPY%MIM%MP(3) == 1) THEN
             DO J = 1, N
                CALL IMEQ(A(I,J)%NUMERATOR,A1(I,J)%MIM)
             ENDDO
             CALL IMEQ(B(I)%NUMERATOR,A1(I,N+1)%MIM)
         ELSE
             DO J = 1, N
                CALL IMDIV(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
                CALL IMMPY(A(I,J)%NUMERATOR,T%MIM,A1(I,J)%MIM)
             ENDDO
             CALL IMDIV(ABMPY%MIM,B(I)%DENOMINATOR,T%MIM)
             CALL IMMPY(B(I)%NUMERATOR,T%MIM,A1(I,N+1)%MIM)
             DET = DET / ABMPY
         ENDIF
      ENDDO

!             Find the determinant of the integer matrix A1, by finding modular solutions using
!             a collection of prime moduli.

      I = 0
      I_EXTRA = 3
      PRIMES_USED = 0
      LAST_DET_IM = 0
      P = FLOOR(MXBASE/10)
      JP = 0
  110 JP = JP + 1
      IF (PRIMES_USED >= MAX_PRIMES) THEN
          ALLOCATE(Z_MS(N,MAX_PRIMES),Z_P(MAX_PRIMES),Z_D(MAX_PRIMES),STAT=L)
          IF (L /= 0) THEN
              WRITE (*,"(/' Error in RM_LIN_SOLVE.  Unable to allocate arrays with N = ',I8/)") N
              STOP
          ENDIF
          DO L = 1, PRIMES_USED
             DO K = 1, N
                Z_MS(K,L) = MOD_SOLUTION(K,L)
             ENDDO
             Z_P(L) = PRIMES(L)
             Z_D(L) = DET_P(L)
          ENDDO
          DEALLOCATE(PRIME_MK,MOD_SOLUTION,PRIMES,DET_P,Y)
          ALLOCATE(PRIME_MK(2*MAX_PRIMES),MOD_SOLUTION(N,2*MAX_PRIMES),PRIMES(2*MAX_PRIMES),  &
                   DET_P(2*MAX_PRIMES),Y(2*MAX_PRIMES),STAT=L)
          IF (L /= 0) THEN
              WRITE (*,"(/' Error in RM_LIN_SOLVE.  Unable to allocate arrays with N = ',I8/)") N
              STOP
          ENDIF
          DO L = 1, PRIMES_USED
             DO K = 1, N
                MOD_SOLUTION(K,L) = Z_MS(K,L)
             ENDDO
             PRIMES(L) = Z_P(L)
             DET_P(L) = Z_D(L)
          ENDDO
          DEALLOCATE(Z_MS,Z_P,Z_D)
          MAX_PRIMES = 2 * MAX_PRIMES
      ENDIF
      CALL RM_NEXT_PRIME(P)
      CALL RM_MOD_LIN_SOLVE(A1, A2, DET_P(PRIMES_USED+1), N, P, SINGULAR)
      IF (PRIMES_USED == 0 .AND. JP > 100) THEN
          DET = 0
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in RM_LIN_SOLVE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          X = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          GO TO 200
      ENDIF
      IF (SINGULAR) GO TO 110
      PRIMES_USED = PRIMES_USED + 1
      MOD_SOLUTION(1:N , PRIMES_USED) = A2(1:N , N+1)
      PRIMES(PRIMES_USED) = P
      IF (PRIMES_USED == 1) THEN
          DET_IM = DET_P(1)
          PC = 1
      ELSE
          LAST_DET_IM = DET_IM
          CALL RM_SOLVE_MOD(DET_P(PRIMES_USED), P, DET_IM, PC)
      ENDIF
      PC = PC * INT(P)
      IF (LAST_DET_IM == DET_IM) THEN
          I = I + 1
      ELSE
          I = 0
      ENDIF
      IF (I < I_EXTRA) GO TO 110

!             Solve the simultaneous congruences to get the rational solution of
!             the original system.

      PRIME_PRODUCT = INT(PRIMES(1))
      DO J = 2, PRIMES_USED
         IF (J == PRIMES_USED) PRIME_MK(J) = PRIME_PRODUCT
         PRIME_PRODUCT = PRIME_PRODUCT * INT(PRIMES(J))
      ENDDO
      DO J = 1, PRIMES_USED-1
         PRIME_MK(J) = PRIME_PRODUCT / INT(PRIMES(J))
      ENDDO
      DO J = 1, PRIMES_USED
         CALL RM_MOD_ONE(PRIME_MK(J)%MIM,PRIMES(J),R)
         CALL RM_INV_MOD_P(R,PRIMES(J),Y(J))
         IF (Y(J)*2.0D0 > PRIMES(J)) Y(J) = Y(J) - PRIMES(J)
      ENDDO
      DO J = 1, PRIMES_USED
         PRIME_MK(J) = ( PRIME_MK(J) * INT(Y(J)) ) * INT(DET_P(J))
      ENDDO
      DO I = 1, N
         XT(I) = 0
         DO J = 1, PRIMES_USED
            XT(I) = XT(I) + PRIME_MK(J) * INT(MOD_SOLUTION(I,J))
         ENDDO
         XT(I) = MODULO( XT(I), PRIME_PRODUCT )
         IF (2*XT(I) > PRIME_PRODUCT) XT(I) = XT(I) - PRIME_PRODUCT
      ENDDO

      DO J = 1, N
         X(J) = TO_FM_RATIONAL( XT(J), DET_IM )
      ENDDO
      DET = DET_IM * DET

!             This modular method can fail if PRIME_PRODUCT is too small or if
!             gcd(PRIME_PRODUCT,DET_IM) is not 1 -- see Cetin K. Koc, "A Parallel Algorithm
!             for Exact Solutions of Linear Equations via Congruence Technique",
!             Computers Math. Applic., Vol. 23, No. 12, pp. 13-24, 1992.

!             Check these conditions and also check that the solution solves the linear system.
!             If not, first try adding more primes and modular solutions, and if that doesn't
!             help, use the slower method in RM_LIN_SOLVE2.

      G = 0
      DO I = 1, N
         DO J = 1, N
            IF (ABS(A1(I,J)) > G) G = ABS(A1(I,J))
         ENDDO
      ENDDO
      IF (PRIME_PRODUCT < 2*DET_IM .OR. PRIME_PRODUCT < G .OR. GCD(PRIME_PRODUCT,DET_IM) /= 1) THEN
          CALL RM_LIN_SOLVE2(A,X,B,N,DET)
          GO TO 200
      ENDIF

      DO I = 1, N
         G = 0
         DO J = 1, N
            G = G + A1(I,J) * XT(J)
         ENDDO
         IF (G /= A1(I,N+1) * DET_IM) THEN
             I_EXTRA = I_EXTRA + 10
             IF (I_EXTRA < 30) GO TO 110
             CALL RM_LIN_SOLVE2(A,X,B,N,DET)
             GO TO 200
         ENDIF
      ENDDO

  200 DEALLOCATE(XT,PRIME_MK,A1,A2,MOD_SOLUTION,PRIMES,DET_P,Y)

      END SUBROUTINE RM_LIN_SOLVE

      SUBROUTINE RM_LIN_SOLVE2(A,X,B,N,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination with exact rational arithmetic to solve the linear system  A X = B, where:

!  A   is the matrix of the system, containing the  N x N coefficient matrix.

!  B   is the  N x 1  right-hand-side vector.

!  X   is the returned  N x 1  solution vector.

!  DET is returned as the determinant of A.
!      Nonzero DET means a solution was found.
!      DET = 0 is returned if the system is singular.

!  A,X,B,DET are all TYPE (FM_RATIONAL) multiprecision rational variables.

      INTEGER :: N
      TYPE (FM_RATIONAL) :: A(N,N), B(N), X(N), DET
      TYPE (IM), SAVE :: ABMPY, G, T
      TYPE (IM), ALLOCATABLE :: A1(:,:)
      TYPE (FM), ALLOCATABLE :: A2(:,:), B2(:), X2(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      TYPE (FM), SAVE :: DET_FM, T_FM
      INTEGER :: I, J, NDSAVE

      ALLOCATE(A1(N,N+1),A2(N,N),B2(N),X2(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in RM_LIN_SOLVE2.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      A1 = 0
      A2 = 0
      B2 = 0
      X2 = 0
      G = 0
      T = 0

!             Copy A and B to A1 and convert to integer coefficients.

      DET = 1
      DO I = 1, N
         ABMPY = 1
         DO J = 1, N
            IF (A(I,J)%DENOMINATOR%MP(2) == 1 .AND.  &
                A(I,J)%DENOMINATOR%MP(3) == 1) CYCLE
            CALL IMGCD(ABMPY%MIM,A(I,J)%DENOMINATOR,G%MIM)
            CALL IMMPY(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
            IF (G%MIM%MP(2) == 1 .AND. G%MIM%MP(3) == 1) THEN
                CALL IMEQ(T%MIM,ABMPY%MIM)
            ELSE
                CALL IMDIV(T%MIM,G%MIM,ABMPY%MIM)
            ENDIF
         ENDDO
         CALL IMGCD(ABMPY%MIM,B(I)%DENOMINATOR,G%MIM)
         CALL IMMPY(ABMPY%MIM,B(I)%DENOMINATOR,T%MIM)
         CALL IMDIV(T%MIM,G%MIM,ABMPY%MIM)

         IF (ABMPY%MIM%MP(2) == 1 .AND. ABMPY%MIM%MP(3) == 1) THEN
             DO J = 1, N
                CALL IMEQ(A(I,J)%NUMERATOR,A1(I,J)%MIM)
             ENDDO
             CALL IMEQ(B(I)%NUMERATOR,A1(I,N+1)%MIM)
         ELSE
             DO J = 1, N
                CALL IMDIV(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
                CALL IMMPY(A(I,J)%NUMERATOR,T%MIM,A1(I,J)%MIM)
             ENDDO
             CALL IMDIV(ABMPY%MIM,B(I)%DENOMINATOR,T%MIM)
             CALL IMMPY(B(I)%NUMERATOR,T%MIM,A1(I,N+1)%MIM)
             DET = DET / ABMPY
         ENDIF
      ENDDO
      CALL IMEQ(DET%DENOMINATOR,ABMPY%MIM)

!             Set precision level for type FM variables.

      NDSAVE = NDIG
      CALL FM_SET(50)
  110 DO I = 1, N
         DO J = 1, N
            A2(I,J) = A1(I,J)
         ENDDO
         B2(I) = A1(I,N+1)
      ENDDO

!             Factor the matrix.

      CALL FM_FACTOR_LU_RM(A2,N,DET_FM,KSWAP)

      DET = TO_IM(NINT(DET_FM))
      DET = DET / ABMPY
      IF (DET_FM == 0 .OR. IS_UNKNOWN(DET_FM)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in RM_LIN_SOLVE2.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          X = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          GO TO 200
      ENDIF

      IF ((NDIG-1)*DLOGMB/DLOGTN < DET_FM%MFM%MP(2)*DLOGMB/DLOGTN + 20) THEN
          NDIG = DET_FM%MFM%MP(2) + 20*DLOGTN/DLOGMB + 2
          GO TO 110
      ENDIF
      IF (ABS(DET_FM - NINT(DET_FM)) > 1.0D-10) THEN
          NDIG = 1.5D0 * NDIG
          GO TO 110
      ENDIF

!             Solve the system.

      CALL FM_SOLVE_LU_RM(A2,N,B2,X2,KSWAP)

      G = TO_IM(NINT( DET_FM ))
      DO J = 1, N
         T_FM = X2(J) * DET_FM
         IF (ABS(T_FM - NINT(T_FM)) > 1.0D-10) THEN
             NDIG = 1.5D0 * NDIG
             GO TO 110
         ENDIF
         T = TO_IM(NINT( T_FM ))
         X(J) = TO_FM_RATIONAL( T, G )
      ENDDO

  200 DEALLOCATE(A1,A2,B2,X2,KSWAP)

      NDIG = NDSAVE

      END SUBROUTINE RM_LIN_SOLVE2

      SUBROUTINE RM_MOD_LIN_SOLVE(A1, A2, DET_P, N, P, SINGULAR)

!  Solve the NxN linear system with augmented coefficient matrix A1 with modular arithmetic mod P.
!  A2 is A1 mod P, and the modular solution vector is returned in A2( 1:N, N+1 ).
!  DET_P is returned as the modular determinant.
!  SINGULAR is returned true if the system is singular.

      USE FMVALS
      USE FMZM
      IMPLICIT NONE
      INTEGER :: N, I, J, JD, JR, JC
      TYPE (IM) :: A1(N,N+1)
      DOUBLE PRECISION :: A2(N,N+1), DET_P, A2JDJD, AMULT, P, T
      LOGICAL :: SINGULAR

      SINGULAR = .FALSE.

!             Copy A1 to A2 Mod P.

      DO I = 1, N
         DO J = 1, N+1
            CALL RM_MOD_ONE(A1(I,J)%MIM,P,A2(I,J))
            IF (A1(I,J)%MIM%MP(1) < 0) A2(I,J) = -A2(I,J) + P
         ENDDO
      ENDDO

!             Gauss Elimination.

      DET_P = 1
      DO JD = 1, N-1
         IF (A2(JD,JD) == 0.0D0) THEN
             DO JR = JD+1, N
                IF (A2(JR,JD) /= 0.0D0) THEN
                    DO JC = JD, N+1
                       T = A2(JD,JC)
                       A2(JD,JC) = A2(JR,JC)
                       A2(JR,JC) = T
                    ENDDO
                    EXIT
                ENDIF
             ENDDO
             DET_P = -DET_P
         ENDIF
         IF (A2(JD,JD) == 0.0D0) THEN
             SINGULAR = .TRUE.
             RETURN
         ENDIF
         A2JDJD = A2(JD,JD)
         CALL RM_INV_MOD_P(A2(JD,JD),P,T)
         DET_P = MODULO( DET_P * A2JDJD, P )
         DO JR = JD+1, N
            AMULT = MODULO( A2(JR,JD) * T , P )
            DO JC = JD, N+1
               A2(JR,JC) = MODULO( A2(JR,JC) - AMULT * A2(JD,JC) , P )
            ENDDO
         ENDDO
      ENDDO
      DET_P = MODULO( DET_P * A2(N,N), P )
      IF (DET_P == 0) THEN
          SINGULAR = .TRUE.
          RETURN
      ENDIF

!             Back substitution.

      DO JR = N, 1, -1
         DO JC = JR+1, N
            A2(JR,N+1) = MODULO( A2(JR,N+1) - A2(JR,JC) * A2(JC,N+1) , P )
         ENDDO
         CALL RM_INV_MOD_P(A2(JR,JR),P,T)
         A2(JR,N+1) = MODULO( A2(JR,N+1) * T , P )
      ENDDO

      END SUBROUTINE RM_MOD_LIN_SOLVE

      SUBROUTINE RM_INVERSE(A,N,B,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Return B as the exact rational inverse of the N x N matrix A, and DET as the exact rational
!  determinant of A.

!  A,B,DET are all TYPE (FM_RATIONAL) multiprecision rational variables.

!  This routine uses modular integer arithmetic to find modular inverse matrices for related
!  integer matrices, B1 = inverse( A1 ) Mod p, for many different primes p. These solutions are
!  then combined using the Chinese Remainder Theorem to recover the original rational inverse B.

      INTEGER :: N
      TYPE (FM_RATIONAL) :: A(N,N), B(N,N), DET
      TYPE (IM), SAVE :: PC, ABDET, ABMPY, G, T, DET_IM, LAST_DET_IM, PRIME_PRODUCT
      TYPE (IM), ALLOCATABLE :: A1(:,:), BT(:,:), PRIME_MK(:)
      DOUBLE PRECISION, ALLOCATABLE :: A2(:,:), MOD_SOLUTION(:,:,:), PRIMES(:), DET_P(:), Y(:),  &
                                       Z_MS(:,:,:), Z_P(:), Z_D(:)
      DOUBLE PRECISION :: P, R
      LOGICAL :: SINGULAR
      INTEGER :: I, I_EXTRA, J, JP, K, L, MAX_PRIMES, PRIMES_USED


      IF (N <= 0) THEN
          DET = 0
          RETURN
      ENDIF
      IF (N == 1) THEN
          DET = A(1,1)
          IF (A(1,1) /= 0) THEN
              B(1,1) = 1 / A(1,1)
          ELSE
              B(1,1) = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          ENDIF
          RETURN
      ENDIF

      MAX_PRIMES = 101
      ALLOCATE(A1(N,2*N),A2(N,2*N),BT(N,N),PRIME_MK(MAX_PRIMES),MOD_SOLUTION(N,N,MAX_PRIMES),  &
               PRIMES(MAX_PRIMES),DET_P(MAX_PRIMES),Y(MAX_PRIMES),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in RM_INVERSE.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      A1 = 0
      BT = 0
      G = 0
      T = 0
      A2 = 0

!             Copy A to augmented matrix A1 and convert to integer coefficients.

      DET = 1
      ABDET = 1
      DO I = 1, N
         ABMPY = 1
         DO J = 1, N
            IF (A(I,J)%DENOMINATOR%MP(2) == 1 .AND.  &
                A(I,J)%DENOMINATOR%MP(3) == 1) CYCLE
            CALL IMGCD(ABMPY%MIM,A(I,J)%DENOMINATOR,G%MIM)
            CALL IMMPY(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
            IF (G%MIM%MP(2) == 1 .AND. G%MIM%MP(3) == 1) THEN
                CALL IMEQ(T%MIM,ABMPY%MIM)
            ELSE
                CALL IMDIV(T%MIM,G%MIM,ABMPY%MIM)
            ENDIF
         ENDDO

         IF (ABMPY%MIM%MP(2) == 1 .AND. ABMPY%MIM%MP(3) == 1) THEN
             DO J = 1, N
                CALL IMEQ(A(I,J)%NUMERATOR,A1(I,J)%MIM)
             ENDDO
             A1(I,I+N) = 1
         ELSE
             DO J = 1, N
                CALL IMDIV(ABMPY%MIM,A(I,J)%DENOMINATOR,T%MIM)
                CALL IMMPY(A(I,J)%NUMERATOR,T%MIM,A1(I,J)%MIM)
             ENDDO
             A1(I,I+N) = ABMPY
             DET = DET / ABMPY
         ENDIF
      ENDDO
      CALL IMEQ(DET%DENOMINATOR,ABDET%MIM)

!             Find the determinant of the integer matrix A1, by finding modular solutions using
!             a collection of prime moduli.

      I = 0
      I_EXTRA = 3
      PRIMES_USED = 0
      LAST_DET_IM = 0
      P = FLOOR(MXBASE/10)
      JP = 0
  110 JP = JP + 1
      IF (PRIMES_USED >= MAX_PRIMES) THEN
          ALLOCATE(Z_MS(N,N,MAX_PRIMES),Z_P(MAX_PRIMES),Z_D(MAX_PRIMES),STAT=L)
          IF (L /= 0) THEN
              WRITE (*,"(/' Error in RM_INVERSE.  Unable to allocate arrays with N = ',I8/)") N
              STOP
          ENDIF
          DO L = 1, PRIMES_USED
             DO K = 1, N
                Z_MS(K,1:N,L) = MOD_SOLUTION(K,1:N,L)
             ENDDO
             Z_P(L) = PRIMES(L)
             Z_D(L) = DET_P(L)
          ENDDO
          DEALLOCATE(PRIME_MK,MOD_SOLUTION,PRIMES,DET_P,Y)
          ALLOCATE(PRIME_MK(2*MAX_PRIMES),MOD_SOLUTION(N,N,2*MAX_PRIMES),PRIMES(2*MAX_PRIMES),  &
                   DET_P(2*MAX_PRIMES),Y(2*MAX_PRIMES),STAT=L)
          IF (L /= 0) THEN
              WRITE (*,"(/' Error in RM_INVERSE.  Unable to allocate arrays with N = ',I8/)") N
              STOP
          ENDIF
          DO L = 1, PRIMES_USED
             DO K = 1, N
                MOD_SOLUTION(K,1:N,L) = Z_MS(K,1:N,L)
             ENDDO
             PRIMES(L) = Z_P(L)
             DET_P(L) = Z_D(L)
          ENDDO
          DEALLOCATE(Z_MS,Z_P,Z_D)
          MAX_PRIMES = 2 * MAX_PRIMES
      ENDIF
      CALL RM_NEXT_PRIME(P)
      CALL RM_MOD_INVERSE(A1, A2, DET_P(PRIMES_USED+1), N, P, SINGULAR)
      IF (PRIMES_USED == 0 .AND. JP > 100) THEN
          DET = 0
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in RM_INVERSE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          B = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          GO TO 200
      ENDIF
      IF (SINGULAR) GO TO 110
      PRIMES_USED = PRIMES_USED + 1
      MOD_SOLUTION(1:N , 1:N , PRIMES_USED) = A2(1:N , N+1:2*N)
      PRIMES(PRIMES_USED) = P
      IF (PRIMES_USED == 1) THEN
          DET_IM = DET_P(1)
          PC = 1
      ELSE
          LAST_DET_IM = DET_IM
          CALL RM_SOLVE_MOD(DET_P(PRIMES_USED), P, DET_IM, PC)
      ENDIF
      PC = PC * INT(P)
      IF (LAST_DET_IM == DET_IM) THEN
          I = I + 1
      ELSE
          I = 0
      ENDIF
      IF (I < I_EXTRA) GO TO 110

!             Solve the simultaneous congruences to get the rational solution of
!             the original system.

      PRIME_PRODUCT = INT(PRIMES(1))
      DO J = 2, PRIMES_USED
         IF (J == PRIMES_USED) PRIME_MK(J) = PRIME_PRODUCT
         PRIME_PRODUCT = PRIME_PRODUCT * INT(PRIMES(J))
      ENDDO
      DO J = 1, PRIMES_USED-1
         PRIME_MK(J) = PRIME_PRODUCT / INT(PRIMES(J))
      ENDDO
      DO J = 1, PRIMES_USED
         CALL RM_MOD_ONE(PRIME_MK(J)%MIM,PRIMES(J),R)
         CALL RM_INV_MOD_P(R,PRIMES(J),Y(J))
         IF (Y(J)*2.0D0 > PRIMES(J)) Y(J) = Y(J) - PRIMES(J)
      ENDDO
      DO J = 1, PRIMES_USED
         PRIME_MK(J) = ( PRIME_MK(J) * INT(Y(J)) ) * INT(DET_P(J))
      ENDDO
      DO I = 1, N
         BT(I,1:N) = 0
         DO J = 1, PRIMES_USED
            BT(I,1:N) = BT(I,1:N) + PRIME_MK(J) * INT(MOD_SOLUTION(I,1:N,J))
         ENDDO
         DO K = 1, N
            BT(I,K) = MODULO( BT(I,K), PRIME_PRODUCT )
            IF (2*BT(I,K) > PRIME_PRODUCT) BT(I,K) = BT(I,K) - PRIME_PRODUCT
         ENDDO
      ENDDO

      DO J = 1, N
         DO K = 1, N
            B(J,K) = TO_FM_RATIONAL( BT(J,K), DET_IM )
         ENDDO
      ENDDO
      DET = DET_IM * DET

!             This modular method can fail if PRIME_PRODUCT is too small or if
!             gcd(PRIME_PRODUCT,DET_IM) is not 1 -- see Cetin K. Koc, "A Parallel Algorithm
!             for Exact Solutions of Linear Equations via Congruence Technique",
!             Computers Math. Applic., Vol. 23, No. 12, pp. 13-24, 1992.

!             Check these conditions and also check that the solution solves the linear system.
!             If not, first try adding more primes and modular solutions, and if that doesn't
!             help, use the slower method in RM_INVERSE2.

      G = 0
      DO I = 1, N
         DO J = 1, N
            IF (ABS(A1(I,J)) > G) G = ABS(A1(I,J))
         ENDDO
      ENDDO
      IF (PRIME_PRODUCT < 2*DET_IM .OR. PRIME_PRODUCT < G .OR. GCD(PRIME_PRODUCT,DET_IM) /= 1) THEN
          CALL RM_INVERSE2(A1,N,B,DET)
          GO TO 200
      ENDIF

      DO I = 1, N
         DO K = 1, N
            G = 0
            DO J = 1, N
               G = G + A1(I,J) * BT(J,K)
            ENDDO
            IF ( ( I == K .AND. G /= DET_IM * A1(I,I+N) ) .OR. ( I /= K .AND. G /= 0 )) THEN
                I_EXTRA = I_EXTRA + 10
                DET = TO_FM_RATIONAL( TO_IM(1), ABDET )
                IF (I_EXTRA < 30) GO TO 110
                CALL RM_INVERSE2(A1,N,B,DET)
                GO TO 200
            ENDIF
         ENDDO
      ENDDO

  200 DEALLOCATE(BT,PRIME_MK,A1,A2,MOD_SOLUTION,PRIMES,DET_P,Y)

      END SUBROUTINE RM_INVERSE

      SUBROUTINE RM_INVERSE2(A1,N,B,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination to find the exact rational inverse of A1.

!  A1  is the augmented type(im) N x 2N matrix of the system, scaled to have integer coefficients.

!  B   is the returned rational  N x N  inverse matrix.

!  DET is returned as the rational determinant of A, the A1 matrix before scaling.
!      Nonzero DET means a solution was found.
!      DET = 0 is returned if the system is singular.

!  B,DET are TYPE (FM_RATIONAL) multiprecision rational variables.

      INTEGER :: N
      TYPE (IM) :: A1(N,2*N)
      TYPE (FM_RATIONAL) :: B(N,N), DET
      TYPE (IM), SAVE :: G
      TYPE (FM), ALLOCATABLE :: A2(:,:), B2(:), X2(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      TYPE (FM), SAVE :: DET_FM
      INTEGER :: I, J, K, NDSAVE

      ALLOCATE(A2(N,N),B2(N),X2(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in RM_INVERSE2.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      A2 = 0
      B2 = 0
      X2 = 0
      G = 0

!             Set precision level for type FM variables.

      NDSAVE = NDIG
      CALL FM_SET(50)
  110 DO I = 1, N
         DO J = 1, N
            A2(I,J) = A1(I,J)
         ENDDO
      ENDDO

!             Factor the matrix.

      CALL FM_FACTOR_LU_RM(A2,N,DET_FM,KSWAP)

      DET = TO_IM(NINT(DET_FM))
      DO I = 1, N
         DET = DET / A1(I,I+N)
      ENDDO
      IF (DET_FM == 0 .OR. IS_UNKNOWN(DET_FM)) THEN
          B = TO_FM_RATIONAL(' UNKNOWN ', ' UNKNOWN ')
          GO TO 200
      ENDIF

      IF ((NDIG-1)*DLOGMB/DLOGTN < DET_FM%MFM%MP(2)*DLOGMB/DLOGTN + 20) THEN
          NDIG = DET_FM%MFM%MP(2) + 20*DLOGTN/DLOGMB + 2
          GO TO 110
      ENDIF
      IF (ABS(DET_FM - NINT(DET_FM)) > 1.0D-10) THEN
          NDIG = 1.5D0 * NDIG
          GO TO 110
      ENDIF

!             Solve for the inverse matrix.

      G = TO_IM(NINT( DET_FM ))
      DO J = 1, N
         B2 = 0
         B2(J) = A1(J,J+N)
         CALL FM_SOLVE_LU_RM(A2,N,B2,X2,KSWAP)
         DO K = 1, N
            B(K,J) = TO_FM_RATIONAL( TO_IM( NINT( X2(K) * DET_FM ) ) , G )
         ENDDO
      ENDDO

  200 DEALLOCATE(A2,B2,X2,KSWAP)

      NDIG = NDSAVE

      END SUBROUTINE RM_INVERSE2

!             The next several routines, with FM_* names, are the same as the ones in the
!             standard FM sample routine file.  Some are used in special cases by the RM
!             routines in this package, and some are used in the SampleFMrational.f95
!             program to compare the exact rational routines with their floating-point
!             counterparts.  To avoid name conflicts in a program that might use both
!             the FM rational package and the FM sample routine file, these names have
!             an extra "_RM" at the end.

      SUBROUTINE FM_INVERSE_RM(A,N,B,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Return B as the inverse of the N x N matrix A, and DET as the determinant of A.

!  A and B are type (fm) (real) multiprecision arrays.

      INTEGER :: N
      TYPE (FM) :: A(N,N), B(N,N), DET
      TYPE (FM), SAVE :: TOL
      TYPE (FM), ALLOCATABLE :: A1(:,:), A2(:,:), B1(:), R1(:), X1(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      INTEGER :: I, J, K, KWARN_SAVE, NDSAVE

      TOL = EPSILON(TO_FM(1))/MBASE/TO_FM(10)**10

      ALLOCATE(A1(N,N),A2(N,N),B1(N),R1(N),X1(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in FM_INVERSE_RM.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

!             Raise precision.

      NDSAVE = NDIG
      NDIG = 2*NDIG
      KWARN_SAVE = KWARN
      KWARN = 0

!             Copy A to A1 with higher precision.

  110 CALL FM_EQU_R1(TOL,NDSAVE,NDIG)
      DO I = 1, N
         DO J = 1, N
            CALL FM_EQU(A(I,J),A1(I,J),NDSAVE,NDIG)
         ENDDO
      ENDDO
      A2 = A1

!             Factor A into L*U form.

      CALL FM_FACTOR_LU_RM(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in FM_INVERSE_RM.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          B = TO_FM(' UNKNOWN ')
          GO TO 120
      ENDIF

!             Solve for the inverse matrix one column at a time.

      DO K = 1, N
         B1 = 0
         B1(K) = 1
         CALL FM_SOLVE_LU_RM(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

         R1 = MATMUL(A2,X1) - B1

         CALL FM_SOLVE_LU_RM(A1,N,R1,B1,KSWAP)
         X1 = X1 - B1

!             Check for accuracy at the user's precision.

         IF (SQRT( DOT_PRODUCT( B1 , B1 ) ) > TOL) THEN
             NDIG = 2*NDIG
             GO TO 110
         ENDIF

!             Round the results and store column K in the B matrix.

         DO I = 1, N
            CALL FM_EQU(X1(I),B(I,K),NDIG,NDSAVE)
         ENDDO
      ENDDO
  120 CALL FM_EQU_R1(DET,NDIG,NDSAVE)

      DEALLOCATE(A1,A2,B1,R1,X1,KSWAP)

      NDIG = NDSAVE
      KWARN = KWARN_SAVE
      END SUBROUTINE FM_INVERSE_RM

      SUBROUTINE FM_LIN_SOLVE_RM(A,X,B,N,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination to solve the linear system  A X = B, where:

!  A   is the matrix of the system, containing the  N x N coefficient matrix.

!  B   is the  N x 1  right-hand-side vector.

!  X   is the returned  N x 1  solution vector.

!  DET is returned as the determinant of A.
!      Nonzero DET means a solution was found.
!      DET = 0 is returned if the system is singular.

!  A,X,B,DET are all type (fm) multiprecision variables.

      INTEGER :: N
      TYPE (FM) :: A(N,N), B(N), X(N), DET
      TYPE (FM), SAVE :: TOL
      TYPE (FM), ALLOCATABLE :: A1(:,:), A2(:,:), B1(:), R1(:), X1(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      INTEGER :: I, J, NDSAVE

      ALLOCATE(A1(N,N),A2(N,N),B1(N),R1(N),X1(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in FM_LIN_SOLVE_RM.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      TOL = EPSILON(B(1))/MBASE/TO_FM(10)**10

      NDSAVE = NDIG
      NDIG = 2*NDIG

!             Copy A and B to A1 and B1 with higher precision.

  110 CALL FM_EQU_R1(TOL,NDSAVE,NDIG)
      DO I = 1, N
         DO J = 1, N
            CALL FM_EQU(A(I,J),A1(I,J),NDSAVE,NDIG)
            CALL FM_EQ(A1(I,J),A2(I,J))
         ENDDO
         CALL FM_EQU(B(I),B1(I),NDSAVE,NDIG)
      ENDDO

!             Solve the system.

      CALL FM_FACTOR_LU_RM(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in FM_LIN_SOLVE_RM.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          X1 = TO_FM(' UNKNOWN ')
          GO TO 120
      ENDIF
      CALL FM_SOLVE_LU_RM(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

      R1 = MATMUL(A2,X1) - B1

      CALL FM_SOLVE_LU_RM(A1,N,R1,B1,KSWAP)
      X1 = X1 - B1

!             Check for accuracy at the user's precision.

      IF (SQRT( DOT_PRODUCT( B1 , B1 ) ) > TOL) THEN
          NDIG = 2*NDIG
          GO TO 110
      ENDIF

!             Round and return X and DET.

  120 DO I = 1, N
         CALL FM_EQU(X1(I),X(I),NDIG,NDSAVE)
      ENDDO
      CALL FM_EQU_R1(DET,NDIG,NDSAVE)

      NDIG = NDSAVE

      DEALLOCATE(A1,A2,B1,R1,X1,KSWAP)

      END SUBROUTINE FM_LIN_SOLVE_RM

      SUBROUTINE FM_FACTOR_LU_RM(A,N,DET,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination to factor the NxN matrix A (LU decomposition).

!  The time is proportional to  N**3.

!  Once this factorization has been done, a linear system  A x = b
!  with the same coefficient matrix A and Nx1 vector b can be solved
!  for x using routine FM_SOLVE_LU_RM in time proportional to  N**2.

!  DET is returned as the determinant of A.
!      Nonzero DET means there is a unique solution.
!      DET = 0 is returned if the system is singular.

!  KSWAP is a list of row interchanges made by the partial pivoting strategy during the
!        elimination phase.

!  After returning, the values in matrix A have been replaced by the multipliers
!  used during elimination.  This is equivalent to factoring the A matrix into
!  a lower triangular matrix L times an upper triangular matrix U.

      INTEGER :: N
      INTEGER :: JCOL, JDIAG, JMAX, JROW, KSWAP(N)
      TYPE (FM) :: A(N,N), DET
      TYPE (FM), SAVE :: AMAX, AMULT, TEMP

      DET = 1
      KSWAP(1:N) = 1
      IF (N <= 0) THEN
          DET = 0
          RETURN
      ENDIF
      IF (N == 1) THEN
          KSWAP(1) = 1
          DET = A(1,1)
          RETURN
      ENDIF

!             Do the elimination phase.
!             JDIAG is the current diagonal element below which the elimination proceeds.

      DO JDIAG = 1, N-1

!             Pivot to put the element with the largest absolute value on the diagonal.

         AMAX = ABS(A(JDIAG,JDIAG))
         JMAX = JDIAG
         DO JROW = JDIAG+1, N
            IF (ABS(A(JROW,JDIAG)) > AMAX) THEN
                AMAX = ABS(A(JROW,JDIAG))
                JMAX = JROW
            ENDIF
         ENDDO

!             If AMAX is zero here then the system is singular.

         IF (AMAX == 0.0) THEN
             DET = 0
             RETURN
         ENDIF

!             Swap rows JDIAG and JMAX unless they are the same row.

         KSWAP(JDIAG) = JMAX
         IF (JMAX /= JDIAG) THEN
             DET = -DET
             DO JCOL = JDIAG, N
                TEMP = A(JDIAG,JCOL)
                A(JDIAG,JCOL) = A(JMAX,JCOL)
                A(JMAX,JCOL) = TEMP
             ENDDO
         ENDIF
         DET = DET * A(JDIAG,JDIAG)

!             For JROW = JDIAG+1, ..., N, eliminate A(JROW,JDIAG) by replacing row JROW by
!                 row JROW - A(JROW,JDIAG) * row JDIAG / A(JDIAG,JDIAG)

         DO JROW = JDIAG+1, N
            IF (A(JROW,JDIAG) == 0) CYCLE
            AMULT = A(JROW,JDIAG)/A(JDIAG,JDIAG)

!             Save the multiplier for use later by FM_SOLVE_LU_RM.

            A(JROW,JDIAG) = AMULT
            DO JCOL = JDIAG+1, N
               CALL FMMPY_SUB_RM(A(JROW,JCOL)%MFM,AMULT%MFM,A(JDIAG,JCOL)%MFM)
            ENDDO
         ENDDO
      ENDDO
      DET = DET * A(N,N)

      END SUBROUTINE FM_FACTOR_LU_RM

      SUBROUTINE FM_SOLVE_LU_RM(A,N,B,X,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Solve a linear system  A x = b.
!  A is the NxN coefficient matrix, after having been factored by FM_FACTOR_LU_RM.
!  B is the Nx1 right-hand-side vector.
!  X is returned with the solution of the linear system.
!  KSWAP is a list of row interchanges made by the partial pivoting strategy during the
!        elimination phase in FM_FACTOR_LU_RM.
!  Time for this call is proportional to  N**2.

      INTEGER :: N, KSWAP(N)
      TYPE (FM) :: A(N,N), B(N), X(N)
      INTEGER :: J, JDIAG, JMAX
      TYPE (FM), SAVE :: TEMP

      IF (N <= 0) THEN
          RETURN
      ENDIF
      IF (N == 1) THEN
          X(1) = B(1) / A(1,1)
          RETURN
      ENDIF
      DO J = 1, N
         X(J) = B(J)
      ENDDO

!             Do the elimination phase operations only on X.
!             JDIAG is the current diagonal element below which the elimination proceeds.

      DO JDIAG = 1, N-1

!             Pivot to put the element with the largest absolute value on the diagonal.

         JMAX = KSWAP(JDIAG)

!             Swap rows JDIAG and JMAX unless they are the same row.

         IF (JMAX /= JDIAG) THEN
             TEMP = X(JDIAG)
             X(JDIAG) = X(JMAX)
             X(JMAX) = TEMP
         ENDIF

!             For JROW = JDIAG+1, ..., N, eliminate A(JROW,JDIAG) by replacing row JROW by
!                 row JROW - A(JROW,JDIAG) * row JDIAG / A(JDIAG,JDIAG)
!             After factoring, A(JROW,JDIAG) is the original A(JROW,JDIAG) / A(JDIAG,JDIAG).

         DO J = JDIAG+1, N
            X(J) = X(J) - A(J,JDIAG) * X(JDIAG)
         ENDDO
      ENDDO

!             Do the back substitution.

      DO JDIAG = N, 1, -1

!             Divide row JDIAG by the diagonal element.

         X(JDIAG) = X(JDIAG) / A(JDIAG,JDIAG)

!             Zero above the diagonal in column JDIAG by replacing row JROW by
!                 row JROW - A(JROW,JDIAG) * row JDIAG
!             For JROW = 1, ..., JDIAG-1.

         IF (JDIAG == 1) EXIT
         DO J = 1, JDIAG-1
            X(J) = X(J) - A(J,JDIAG) * X(JDIAG)
         ENDDO
      ENDDO

      END SUBROUTINE FM_SOLVE_LU_RM

      SUBROUTINE FMMPY_SUB_RM(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE

!  Fused multiply-subtract operation.  Return  MA = MA - MB*MC
!  This is an internal FM routine used by FM_FACTOR_LU_RM.  It doesn't always return correctly
!  rounded results, since precision will have already been raised by FM_LIN_SOLVE_RM before
!  calling FM_FACTOR_LU_RM.

      TYPE(MULTI) :: MA,MB,MC
      TYPE(MULTI) :: MXY
      INTEGER :: J,K,K1,KPTA,KPTC,MA_VS_MBMC
      DOUBLE PRECISION :: A1,A2,B,B1,B2,C1,C2,DPA,DPBC
      REAL (KIND(1.0D0)) :: MBJ, MGD

!             Special cases.

      IF (MB%MP(3) == 0 .OR. MC%MP(3) == 0 .OR.  &
          MA%MP(2) - 1 > MB%MP(2) + MC%MP(2) + NDIG) THEN
          RETURN
      ENDIF
      IF (MA%MP(3) == 0 .OR.  &
          MB%MP(2) + MC%MP(2) - 1 > MA%MP(2) + NDIG) THEN
          CALL FMMPY(MB,MC,MA)
          IF (MA%MP(2) /= MUNKNO) MA%MP(1) = -MA%MP(1)
          RETURN
      ENDIF

      IF (ABS(MA%MP(2)) > MEXPAB .OR. ABS(MB%MP(2)) > MEXPAB .OR.       &
          ABS(MC%MP(2)) > MEXPAB .OR. MBASE < 1000 .OR. MBASE**2 > MAXINT .OR.  &
          NDIG > 900 .OR. NDIG > MAXINT/MBASE**2) THEN
          GO TO 110
      ENDIF

!             Determine which of abs(MA) and abs(MB*MC) is larger.

      MA_VS_MBMC = 0
      B = MBASE
      IF (MA%MP(2) <= MB%MP(2) + MC%MP(2) - 2) THEN
          MA_VS_MBMC = -1
          GO TO 120
      ENDIF
      IF (MA%MP(2) >= MB%MP(2) + MC%MP(2) + 1) THEN
          MA_VS_MBMC = 1
          GO TO 120
      ENDIF
      A1 = MA%MP(3)
      A2 = MA%MP(4)
      B1 = MB%MP(3)
      B2 = MB%MP(4)
      C1 = MC%MP(3)
      C2 = MC%MP(4)
      IF (MA%MP(2) == MB%MP(2) + MC%MP(2) - 1) THEN
          DPA = A1 * B + A2 + 1
          DPBC = B1 * C1 * B  +  B1 * C2  +  B2 * C1  +  C2 * B2 / B
          IF (DPA < DPBC) THEN
              MA_VS_MBMC = -1
              GO TO 120
          ENDIF

          DPA = A1 * B + A2
          DPBC = B1 * C1 * B  +  B1 * (C2+1)  +  (B2+1) * C1  +  (C2+1) * (B2+1) / B
          IF (DPA > DPBC) THEN
              MA_VS_MBMC = 1
              GO TO 120
          ENDIF
      ELSE IF (MA%MP(2) == MB%MP(2) + MC%MP(2)) THEN
          DPA = A1 * B + A2 + 1
          DPBC = B1 * C1  +  ( B1 * C2 + B2 * C1 ) / B  +  C2 * B2 / B**2
          IF (DPA < DPBC) THEN
              MA_VS_MBMC = -1
              GO TO 120
          ENDIF

          DPA = A1 * B + A2
          DPBC = B1 * C1  +  ( B1 * (C2+1) + (B2+1) * C1 ) / B  +  (C2+1) * (B2+1) / B**2
          IF (DPA > DPBC) THEN
              MA_VS_MBMC = 1
              GO TO 120
          ENDIF
      ENDIF

!             If it is not easy to determine which term is larger, make separate calls to
!             multiply and subtract.

  110 CALL FMMPY(MB,MC,MXY)
      CALL FMSUB_R1(MA,MXY)
      RETURN

!             Handle the operation using 4 cases, depending on which term is larger and whether
!             MA and MB*MC have opposite signs or not.

  120 IF (MA%MP(1) * MB%MP(1) * MC%MP(1) < 0) THEN
          IF (MA_VS_MBMC == 1) THEN
              MGD = 0
              K1 = 2 + MA%MP(2) - MB%MP(2) - MC%MP(2)
              DO J = K1, NDIG
                 MBJ = MB%MP(3+J-K1)
                 KPTA = 2+J
                 KPTC = 3
                 DO K = J, NDIG
                    MA%MP(KPTA) = MA%MP(KPTA) + MBJ * MC%MP(KPTC)
                    KPTA = KPTA + 1
                    KPTC = KPTC + 1
                 ENDDO
                 IF (KPTC <= 2+NDIG) MGD = MGD + MBJ * MC%MP(KPTC)
              ENDDO
              K1 = MA%MP(2)
              MA%MP(2) = 0
              KPTA = 3+NDIG
              MA%MP(KPTA-1) = MA%MP(KPTA-1) + NINT( MGD / MBASE )
              DO J = NDIG, 1, -1
                 KPTA = KPTA - 1
                 IF (MA%MP(KPTA) >= MBASE) THEN
                     K = MA%MP(KPTA) / MBASE
                     MA%MP(KPTA) = MA%MP(KPTA) - K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) + K
                 ENDIF
              ENDDO
              IF (MA%MP(2) > 0) THEN
                  DO J = NDIG, 1, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K1 = K1 + 1
              ENDIF
              MA%MP(2) = K1
              IF (MA%MP(3) >= MBASE) THEN
                  DO J = NDIG, 3, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K = MA%MP(3) / MBASE
                  MA%MP(4) = MA%MP(3) - K * MBASE
                  MA%MP(3) = K
                  MA%MP(2) = MA%MP(2) + 1
              ENDIF
          ENDIF

          IF (MA_VS_MBMC == -1) THEN
              MGD = 0
              K1 = MB%MP(2) + MC%MP(2) - 1
              K = MB%MP(2) + MC%MP(2) - MA%MP(2) - 1
              MA%MP(2) = 0
              IF (K > 0) THEN
                  IF (K <= NDIG) MGD = MA%MP(2+NDIG+1-K)
                  DO J = NDIG, K+1, -1
                     MA%MP(2+J) = MA%MP(2+J-K)
                  ENDDO
                  DO J = 1, MIN(K,NDIG)
                     MA%MP(2+J) = 0
                  ENDDO
              ELSE IF (K == -1) THEN
                  DO J = 1, NDIG
                     MA%MP(1+J) = MA%MP(1+J+1)
                  ENDDO
                  MA%MP(2+NDIG) = 0
              ENDIF

              DO J = 1, NDIG
                 MBJ = MB%MP(2+J)
                 KPTA = 2+J
                 KPTC = 3
                 DO K = J, NDIG
                    MA%MP(KPTA) = MA%MP(KPTA) + MBJ * MC%MP(KPTC)
                    KPTA = KPTA + 1
                    KPTC = KPTC + 1
                 ENDDO
                 IF (KPTC <= 2+NDIG) MGD = MGD + MBJ * MC%MP(KPTC)
              ENDDO
              KPTA = 3+NDIG
              MA%MP(KPTA-1) = MA%MP(KPTA-1) + NINT( MGD / MBASE )
              DO J = NDIG, 1, -1
                 KPTA = KPTA - 1
                 IF (MA%MP(KPTA) >= MBASE) THEN
                     K = MA%MP(KPTA) / MBASE
                     MA%MP(KPTA) = MA%MP(KPTA) - K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) + K
                 ENDIF
              ENDDO
              IF (MA%MP(2) > 0) THEN
                  DO J = NDIG, 1, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K1 = K1 + 1
              ENDIF
              MA%MP(2) = K1
              IF (MA%MP(3) >= MBASE) THEN
                  DO J = NDIG, 3, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K = MA%MP(3) / MBASE
                  MA%MP(4) = MA%MP(3) - K * MBASE
                  MA%MP(3) = K
                  MA%MP(2) = MA%MP(2) + 1
              ENDIF
          ENDIF

      ELSE
          CALL FMEQ(MA,MXY)

          IF (MA_VS_MBMC == 1) THEN
              MGD = 0
              K1 = 2 + MA%MP(2) - MB%MP(2) - MC%MP(2)
              DO J = K1, NDIG
                 MBJ = MB%MP(3+J-K1)
                 KPTA = 2+J
                 KPTC = 3
                 DO K = J, NDIG
                    MA%MP(KPTA) = MA%MP(KPTA) - MBJ * MC%MP(KPTC)
                    KPTA = KPTA + 1
                    KPTC = KPTC + 1
                 ENDDO
                 IF (KPTC <= 2+NDIG) MGD = MGD - MBJ * MC%MP(KPTC)
              ENDDO
              K1 = MA%MP(2)
              MA%MP(2) = 0
              KPTA = 3+NDIG
              MA%MP(KPTA-1) = MA%MP(KPTA-1) + NINT( MGD / MBASE )
              DO J = NDIG, 1, -1
                 KPTA = KPTA - 1
                 IF (MA%MP(KPTA) < 0) THEN
                     K = (-MA%MP(KPTA)-1) / MBASE + 1
                     MA%MP(KPTA) = MA%MP(KPTA) + K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) - K
                 ELSE IF (MA%MP(KPTA) >= MBASE) THEN
                     K = MA%MP(KPTA) / MBASE
                     MA%MP(KPTA) = MA%MP(KPTA) - K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) + K
                 ENDIF
              ENDDO
              IF (MA%MP(2) > 0) THEN
                  DO J = NDIG, 1, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K1 = K1 + 1
              ENDIF
              MA%MP(2) = K1
              IF (MA%MP(3) == 0) THEN
                  CALL FMMPY(MB,MC,MA)
                  CALL FMSUB_R2(MXY,MA)
                  RETURN
              ENDIF
          ENDIF

          IF (MA_VS_MBMC == -1) THEN
              MGD = 0
              K1 = MB%MP(2) + MC%MP(2) - 1
              K = MB%MP(2) + MC%MP(2) - MA%MP(2) - 1
              MA%MP(2) = 0
              IF (K > 0) THEN
                  IF (K <= NDIG) MGD = -MA%MP(2+NDIG+1-K)
                  DO J = NDIG, K+1, -1
                     MA%MP(2+J) = -MA%MP(2+J-K)
                  ENDDO
                  DO J = 1, MIN(K,NDIG)
                     MA%MP(2+J) = 0
                  ENDDO
              ELSE IF (K == -1) THEN
                  DO J = 1, NDIG
                     MA%MP(1+J) = -MA%MP(1+J+1)
                  ENDDO
                  MA%MP(2+NDIG) = 0
              ELSE IF (K == 0) THEN
                  DO J = 1, NDIG
                     MA%MP(2+J) = -MA%MP(2+J)
                  ENDDO
              ENDIF

              DO J = 1, NDIG
                 MBJ = MB%MP(2+J)
                 KPTA = 2+J
                 KPTC = 3
                 DO K = J, NDIG
                    MA%MP(KPTA) = MA%MP(KPTA) + MBJ * MC%MP(KPTC)
                    KPTA = KPTA + 1
                    KPTC = KPTC + 1
                 ENDDO
                 IF (KPTC <= 2+NDIG) MGD = MGD + MBJ * MC%MP(KPTC)
              ENDDO
              KPTA = 3+NDIG
              MA%MP(KPTA-1) = MA%MP(KPTA-1) + NINT( MGD / MBASE )
              DO J = NDIG, 1, -1
                 KPTA = KPTA - 1
                 IF (MA%MP(KPTA) >= MBASE) THEN
                     K = MA%MP(KPTA) / MBASE
                     MA%MP(KPTA) = MA%MP(KPTA) - K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) + K
                 ELSE IF (MA%MP(KPTA) < 0) THEN
                     K = (-MA%MP(KPTA)-1) / MBASE + 1
                     MA%MP(KPTA) = MA%MP(KPTA) + K * MBASE
                     MA%MP(KPTA-1) = MA%MP(KPTA-1) - K
                 ENDIF
              ENDDO
              IF (MA%MP(2) > 0) THEN
                  DO J = NDIG, 1, -1
                     MA%MP(2+J) = MA%MP(2+J-1)
                  ENDDO
                  K1 = K1 + 1
              ENDIF
              MA%MP(2) = K1
              MA%MP(1) = -MA%MP(1)
              IF (MA%MP(3) == 0) THEN
                  CALL FMMPY(MB,MC,MA)
                  CALL FMSUB_R2(MXY,MA)
                  RETURN
              ENDIF
          ENDIF

          RETURN
      ENDIF

      END SUBROUTINE FMMPY_SUB_RM

      SUBROUTINE RM_MOD_INVERSE(A1, A2, DET_P, N, P, SINGULAR)

!  Find the NxN inverse matrix of A1 with modular arithmetic mod P.
!  A2 is initially A1 mod P, and the modular inverse matrix is returned in A2.
!  DET_P is returned as the modular determinant.
!  SINGULAR is returned true if the system is singular.

      USE FMVALS
      USE FMZM
      IMPLICIT NONE
      INTEGER :: N, I, J, JD, JR, JC
      TYPE (IM) :: A1(N,2*N)
      DOUBLE PRECISION :: A2(N,2*N), DET_P, A2JDJD, AMULT, P, T
      LOGICAL :: SINGULAR

      SINGULAR = .FALSE.

!             Copy A1 to A2 Mod P.

      DO I = 1, N
         DO J = 1, N
            CALL RM_MOD_ONE(A1(I,J)%MIM,P,A2(I,J))
            IF (A1(I,J)%MIM%MP(1) < 0) A2(I,J) = -A2(I,J) + P
            A2(I,J+N) = 0
         ENDDO
         CALL RM_MOD_ONE(A1(I,I+N)%MIM,P,A2(I,I+N))
      ENDDO

!             Gauss Elimination.

      DET_P = 1
      DO JD = 1, N-1
         IF (A2(JD,JD) == 0.0D0) THEN
             DO JR = JD+1, N
                IF (A2(JR,JD) /= 0.0D0) THEN
                    DO JC = JD, 2*N
                       T = A2(JD,JC)
                       A2(JD,JC) = A2(JR,JC)
                       A2(JR,JC) = T
                    ENDDO
                    EXIT
                ENDIF
             ENDDO
             DET_P = -DET_P
         ENDIF
         IF (A2(JD,JD) == 0.0D0) THEN
             SINGULAR = .TRUE.
             RETURN
         ENDIF
         A2JDJD = A2(JD,JD)
         CALL RM_INV_MOD_P(A2(JD,JD),P,T)
         DET_P = MODULO( DET_P * A2JDJD, P )
         DO JR = JD+1, N
            AMULT = MODULO( A2(JR,JD) * T , P )
            DO JC = JD, 2*N
               A2(JR,JC) = MODULO( A2(JR,JC) - AMULT * A2(JD,JC) , P )
            ENDDO
         ENDDO
      ENDDO
      DET_P = MODULO( DET_P * A2(N,N), P )
      IF (DET_P == 0) THEN
          SINGULAR = .TRUE.
          RETURN
      ENDIF

!             Back substitution.

      DO JR = N, 1, -1
         DO J = 1, N
            DO JC = JR+1, N
               A2(JR,N+J) = MODULO( A2(JR,N+J) - A2(JR,JC) * A2(JC,N+J) , P )
            ENDDO
            CALL RM_INV_MOD_P(A2(JR,JR),P,T)
            A2(JR,N+J) = MODULO( A2(JR,N+J) * T , P )
         ENDDO
      ENDDO

      END SUBROUTINE RM_MOD_INVERSE

      SUBROUTINE RM_SOLVE_MOD(A2, P2, A3, P3)

!  Solve for smallest magnitude x such that  x = a2 mod p2  and  x = a3 mod p3.

!  A3 is returned as that x between  -p2*p3/2 and +p2*p3/2

!  A2 and P2 are double precision, A3 and P3 are type (im).

      USE FMVALS
      USE FMZM
      IMPLICIT NONE
      INTEGER :: I2, IR
      INTEGER, SAVE :: KPT
      TYPE (IM) :: A3, P3
      TYPE (IM), SAVE :: Q3, T(0:2)
      DOUBLE PRECISION :: A2, P2, A, B, Q, R

!             Use the Euclidean algorithm to solve the modular equations.

      KPT = 0
      I2 = P2
      T(2) = 0
      Q3 = 0
      CALL IMDVIR(P3%MIM,I2,T(2)%MIM,IR)
      B = P2
      A = IR
      T(0) = 1
      T(1) = -T(2)
      DO WHILE (A > 0)
         Q = AINT(B/A)
         R = B - Q * A
         IF (Q /= 1.0D0) THEN
             CALL IMMPYI(T(MOD(KPT+1,3))%MIM,INT(Q),Q3%MIM)
             CALL IMSUB(T(KPT)%MIM,Q3%MIM,T(MOD(KPT+2,3))%MIM)
             KPT = MOD(KPT+1,3)
         ELSE
             CALL IMSUB(T(KPT)%MIM,T(MOD(KPT+1,3))%MIM,T(MOD(KPT+2,3))%MIM)
             KPT = MOD(KPT+1,3)
         ENDIF
         B = A
         A = R
      ENDDO
      Q3 = MODULO( (A3 - INT(A2)) * T(KPT), P3 )
      A3 = INT(A2) + Q3 * INT(P2)
      Q3 = INT(P2) * P3
      IF (2*A3 > Q3) A3 = A3 - Q3

      END SUBROUTINE RM_SOLVE_MOD

      SUBROUTINE RM_INV_MOD_P(X,P,X_INVERSE)

!  Return X_INVERSE as the multiplicative inverse of x mod p.  I.e.,  x * x_inverse mod p = 1.

      IMPLICIT NONE
      DOUBLE PRECISION :: X,P,X_INVERSE, Q,R0,R1,R2,S0,S1,S2

      R0 = P
      R1 = X
      S0 = 0
      S1 = 1
      DO WHILE (R1 > 0)
         Q = AINT(R0 / R1)
         R2 = R0 - Q * R1
         S2 = S0 - Q * S1
         R0 = R1
         R1 = R2
         S0 = S1
         S1 = S2
      ENDDO
      IF (S0 < 0) S0 = S0 + P
      X_INVERSE = S0

      END SUBROUTINE RM_INV_MOD_P


      SUBROUTINE RM_MOD_ONE(NUM,P,REM)

!  Return integer REM = mod( NUM , P )

!  NUM is an IM-format multiple precision integer
!  P is a one-word integer.

      USE FMVALS
      IMPLICIT NONE

      TYPE(MULTI) :: NUM
      INTEGER :: J, KPT, N
      DOUBLE PRECISION :: P, REM
      REAL (KIND(1.0D0)) :: MDIV, MREM

      MDIV = P
      KPT = 3
      N = NUM%MP(KPT-1)

      MREM = 0.0D0
      DO J = 1, N
         MREM = MOD( MBASE*MREM + NUM%MP(KPT) , MDIV )
         KPT = KPT + 1
      ENDDO

      REM = MREM

      END SUBROUTINE RM_MOD_ONE

      SUBROUTINE RM_NEXT_PRIME(P)
      IMPLICIT NONE

      INTEGER :: J
      DOUBLE PRECISION :: P, D

!  Find the next prime after P and return it in P.

  110 P = P + 1
      IF (MOD(P,2.0D0) == 0) GO TO 110
      IF (MOD(P,3.0D0) == 0) GO TO 110
      DO J = 5, INT(SQRT(P))+7, 6
         D = J
         IF (MOD(P,D) == 0) GO TO 110
         IF (MOD(P,D+2.0D0) == 0) GO TO 110
      ENDDO

      END SUBROUTINE RM_NEXT_PRIME

 END MODULE FM_RATIONAL_ARITHMETIC
