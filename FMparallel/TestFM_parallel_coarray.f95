
!  Test version 1.4 of FM_parallel.f95, the thread-safe version of the FM package, using CoArrays.

      PROGRAM TEST
      USE FMVALS_PARALLEL
      USE FMZM_PARALLEL
      IMPLICIT NONE

    INTERFACE
      FUNCTION Z_TEST_FUNCTION(XI,NI,QX)     RESULT (RETURN_VALUE)
      USE FMVALS_PARALLEL
      IMPLICIT NONE
      TYPE (MULTI) :: XI(2)
      INTEGER :: NI
      TYPE (MULTI), DIMENSION(2) :: RETURN_VALUE
      TYPE(FM_SETTINGS) :: QX
      END FUNCTION Z_TEST_FUNCTION
    END INTERFACE
      INTEGER, PARAMETER :: N_VALS = 10, MAX_CASE = 500
      INTEGER :: L, N_ERRORS, SKIP, SKIP_CHECK(30)
      DOUBLE PRECISION :: TIME1, TIME2
      CHARACTER(8) :: FILE_NUMBER
      CHARACTER(25) :: FILE_NAME
      CHARACTER(251) :: LINE1, LINE2
      LOGICAL :: CHK_FILE_EXISTS
      TYPE(FM), EXTERNAL :: FM_FPRIME
      TYPE(ZM), EXTERNAL :: ZM_FPRIME
      TYPE(MULTI), EXTERNAL :: F_TEST_FUNCTION
      EXTERNAL :: FM_RK14_F

!             Declare the coarray variables.

      TYPE (FM) :: X1[*], X2[*], R[*], X_VALS(2*N_VALS)[*],            &
                   A(5,5)[*], B(5)[*], X(5)[*], DET[*], AINV(5,5)[*],  &
                   AX[*], BX[*], TOL[*], XVAL[*], FVAL[*], S(3)[*], S1(3)[*]
      TYPE (IM) :: I1[*]
      TYPE (ZM) :: Z1[*], Z2[*], Z3[*], ZA(5,5)[*], ZB(5)[*], ZX(5)[*], ZDET[*], ZAINV(5,5)[*],  &
                   LIST_OF_ROOTS(8)[*]
      INTEGER :: I[*], N_CASE[*], J[*], K[*], N_FOUND[*], N_ORDER[*], N_FUNCTION[*]
      DOUBLE PRECISION :: RM[*], RAN(4*N_VALS*MAX_CASE)[*]

!             The first section is not run in parallel.

if (this_image() .eq. 1) then
      CALL CLOCK_TIME(TIME1)
endif

sync all

!             Test each operation at 2*N_VALS different arguments.

      X1 = TO_FM('0.001234')
      X2 = TO_FM('987.6')
      R = (X2/X1) ** (TO_FM(1)/(N_VALS-1))
      CALL FM_FORM('ES60.50',R,LINE1)
      R = TO_FM(TRIM(LINE1))
      X_VALS(1) = X1
      DO J = 2, N_VALS
         X_VALS(J) = R * X_VALS(J-1)
         CALL FM_FORM('ES60.50',X_VALS(J),LINE1)
         X_VALS(J) = TO_FM(TRIM(LINE1))
      ENDDO
      DO J = 1, N_VALS
         X_VALS(J+N_VALS) = -X_VALS(J)
      ENDDO

      RM = 2.0D0**31 - 1
      RAN(1) = RM / 3
      DO J = 2, 4*N_VALS*MAX_CASE
         RAN(J) = MOD( 314159.0D0 * RAN(J-1) , RM )
      ENDDO
      RAN = RAN/RM
      RAN = RAN*(2*N_VALS) + 1


sync all

!             Run the test cases in parallel.

      DO I = this_image(), 2*N_VALS, num_images()

         IF (I == this_image()) THEN
             WRITE(FILE_NUMBER,"(I8)") this_image()
             FILE_NAME = TRIM( 'TestFM_parallel' // ADJUSTL(FILE_NUMBER) ) // '.out'
             OPEN(30+this_image(),FILE=TRIM(FILE_NAME))
         ENDIF

         N_CASE = 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = X_VALS(J) + X_VALS(K)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 2
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = X_VALS(J) - X_VALS(K)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 3
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = X_VALS(J) * X_VALS(K)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 4
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = X_VALS(J) / X_VALS(K)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 5
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = X_VALS(J) ** X_VALS(K)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 6
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = SQRT(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 7
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = (X_VALS(I)) ** 2
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 8
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SIN(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 9
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = COS(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 10
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = TAN(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 11
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1.0,1.0,J,K)
         X1 = ASIN(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 12
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1.0,1.0,J,K)
         X1 = ACOS(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 13
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ATAN(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 14
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SINH(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 15
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = COSH(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 16
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = TANH(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 17
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ASINH(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 18
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,1.0,1001.0,J,K)
         X1 = ACOSH(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 19
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1.0,1.0,J,K)
         X1 = ATANH(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 20
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP(X_VALS(I))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 21
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = LOG(X_VALS(J))
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 22
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP(X_VALS(I)) + TO_FM(2)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 23
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM('2.1') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 24
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM('2.2D0') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 25
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM('2.3') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 26
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM(TO_ZM('2.4+2.5i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 27
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM(TO_ZM('2.5D0+2.6D0i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 28
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM((X_VALS(I))/10) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 29
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM(TO_IM(2)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 30
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_FM(TO_ZM('2.01+2.01i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 31
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP(X_VALS(I)) + TO_IM(2)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 32
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(2.1) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 33
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(2.2D0) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 34
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM('2.3') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 35
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(TO_ZM('2.4+2.5i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 36
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(TO_ZM('2.5D0+2.6D0i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 37
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM((X_VALS(I))/10) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 38
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(TO_IM(2)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 39
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP( (X_VALS(I)) + TO_IM(TO_ZM('2.01+2.01i')) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 40
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = INT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 41
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = NINT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 42
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FLOOR( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 43
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = CEILING( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 44
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MOD( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 45
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MOD( TO_IM(X_VALS(J)) + 12345678 , TO_IM(X_VALS(K)) + 1234 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 46
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = GCD( TO_IM(X_VALS(J)) + 12345678 , TO_IM(X_VALS(K)) + 1234 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 47
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FACTORIAL( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 48
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = GAMMA( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 49
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = GAMMA( (X_VALS(I)) + 1 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 50
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = LOG_GAMMA( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 51
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BINOMIAL( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 52
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_EULER(X1)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 53
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BERNOULLI( TO_INT(ABS(X_VALS(I))) + 12 + I )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 54
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BETA( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 55
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = PSI( (X_VALS(J)) + TO_FM('0.12345') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 56
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = POLYGAMMA( I , (X_VALS(J)) + TO_FM('0.12345') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 57
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = POCHHAMMER( (X_VALS(J)) + TO_FM('0.12345') , I )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 58
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = INCOMPLETE_GAMMA1( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 59
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = INCOMPLETE_GAMMA2( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 60
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BESSEL_J( I , (X_VALS(I)) + TO_FM('0.12345') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 61
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = BESSEL_Y( I , (X_VALS(J)) + TO_FM('0.12345') )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 62
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BESSEL_J0( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 63
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = BESSEL_J1( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 64
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = BESSEL_Y0( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 65
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = BESSEL_Y1( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 66
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = COS_INTEGRAL( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 67
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = COSH_INTEGRAL( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 68
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ERF( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 69
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ERFC( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 70
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ERFC_SCALED( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 71
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXP_INTEGRAL_EI( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 72
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = EXP_INTEGRAL_EN( I , (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 73
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FRESNEL_C( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 74
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FRESNEL_S( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 75
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X2 = TO_FM(I) / (2*N_VALS+1)
         X1 = INCOMPLETE_BETA( X2, X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 76
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = LOG_ERFC( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 77
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = LOG_INTEGRAL( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 78
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SIN_INTEGRAL( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 79
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SINH_INTEGRAL( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 80
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) == X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 81
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) /= X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 82
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) < X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 83
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) <= X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 84
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) > X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 85
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (X_VALS(J) >= X_VALS(K)) THEN
             X1 = X_VALS(J)
         ELSE
             X1 = X_VALS(K)
         ENDIF
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 86
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J)) + TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 87
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J)) - TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 88
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J)) * TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 89
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J)) / TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 90
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) == TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 91
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) /= TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 92
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) <= TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 93
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) < TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 94
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) >= TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 95
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         IF (TO_IM(TO_FM('1.0D+41')*X_VALS(J)) > TO_IM(TO_FM('1.0D+43')*X_VALS(K))) THEN
             I1 = TO_IM(TO_FM('1.0D+41')*X_VALS(J))
         ELSE
             I1 = TO_IM(TO_FM('1.0D+43')*X_VALS(K))
         ENDIF
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 96
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = ABS(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 97
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = BINOMIAL(TO_IM(100),TO_IM(50))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 98
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = CEILING(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 99
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = DIGITS(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 100
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = DIM(TO_IM(TO_FM('1.0D+41')*X_VALS(J)),TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 101
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = FACTORIAL(TO_IM(50))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 102
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = FLOOR(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 103
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = GCD(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 104
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = INT(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 105
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = MAX(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 106
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = MIN(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 107
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = MOD(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 108
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = MODULO(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 109
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = MULTIPLY_MOD(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)) ,  &
              TO_IM('1234567'))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 110
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = NINT(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 111
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = POWER_MOD(ABS(TO_IM(TO_FM('1.0D+41')*X_VALS(J))) ,  &
                        ABS(TO_IM(TO_FM('1.0D+43')*X_VALS(K))) , TO_IM('1234567'))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 112
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = RADIX(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 113
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = RANGE(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 114
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = SIGN(TO_IM(TO_FM('1.0D+41')*X_VALS(J)) , TO_IM(TO_FM('1.0D+43')*X_VALS(K)))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 115
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         I1 = TINY(TO_IM(TO_FM('1.0D+41')*(X_VALS(I))))
         CALL OUTPUT_IM(I,N_CASE,I1)

         N_CASE = 116
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ABS( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 117
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = AINT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 118
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ANINT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 119
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = ATAN2( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 120
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = CEILING( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 121
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = DBLE( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 122
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = DIGITS( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 123
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = DIM( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 124
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = DINT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 125
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EPSILON( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 126
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = EXPONENT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 127
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FLOOR( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 128
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = FRACTION( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 129
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = HUGE( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 130
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = HYPOT( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 131
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = INT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 132
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,0.0,1001.0,J,K)
         X1 = LOG10( (X_VALS(J)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 133
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MAX( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 134
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MIN( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 135
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MAXEXPONENT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 136
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MINEXPONENT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 137
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = MODULO( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 138
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = NEAREST( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 139
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = NINT( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 140
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = PRECISION( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 141
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = RADIX( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 142
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = RANGE( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 143
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = RRSPACING( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 144
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SCALE( (X_VALS(I)) , 4 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 145
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SIGN( X_VALS(J) , X_VALS(K) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 146
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         X1 = SPACING( (X_VALS(I)) )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 147
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_COSH_SINH( (X_VALS(I)) , X1 , X2 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 148
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_COSH_SINH( (X_VALS(I)) , X1 , X2 )
         CALL OUTPUT(I,N_CASE,X2)

         N_CASE = 149
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_COS_SIN( (X_VALS(I)) , X1 , X2 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 150
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_COS_SIN( (X_VALS(I)) , X1 , X2 )
         CALL OUTPUT(I,N_CASE,X2)

         N_CASE = 151
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         CALL FM_ULP( (X_VALS(I)) , X1 )
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 152
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         N_CASE = N_CASE + 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z3 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = Z2 + Z3
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 154
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         N_CASE = N_CASE + 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z3 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = Z2 - Z3
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 156
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         N_CASE = N_CASE + 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z3 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = Z2 * Z3
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 158
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         N_CASE = N_CASE + 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z3 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = Z2 / Z3
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 160
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         N_CASE = N_CASE + 1
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z3 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = Z2 ** Z3
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 162
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ABS(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 164
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ACOS(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 166
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ACOSH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 168
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = AINT(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 170
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ASIN(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 172
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ASINH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 174
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ATAN(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 176
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ATANH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 178
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = CEILING(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 180
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = CONJG(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 182
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = COS(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 184
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = COSH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 186
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = DBLE(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 188
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = DIGITS(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 190
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = DINT(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 192
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ERF(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 194
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ERFC(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 196
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = ERFC_SCALED(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 198
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = EXP(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 200
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = FACTORIAL(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 202
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = FLOOR(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 204
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = FRACTION(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 206
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = GAMMA(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 208
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = HUGE(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 210
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = INT(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 212
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = LOG(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 214
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = LOG10(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 216
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = LOG_GAMMA(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 218
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = NINT(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 220
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = PRECISION(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 222
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = RADIX(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 224
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = RANGE(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 226
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = REAL(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 228
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = SCALE(Z2,I)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 230
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = SIN(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 232
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = SINH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 234
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = SQRT(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 236
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = TAN(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 238
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = TANH(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 240
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         Z1 = TINY(Z2)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 242
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         CALL ZM_COSH_SINH(Z2,Z1,Z3)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE,REAL(Z3))
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 246
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         CALL ZM_COS_SIN(Z2,Z1,Z3)
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE-1,REAL(Z1))
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE,REAL(Z3))
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE,AIMAG(Z1))

         N_CASE = 250
         CALL ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,-1001.0,1001.0,J,K)
         Z2 = CMPLX( X_VALS(J) , X_VALS(K) )
         CALL ZM_ARG(Z2,X1)
         CALL OUTPUT(I,N_CASE,X1)

         N_CASE = 251
         DO J = 1, 5
            DO K = 1, 5
               A(J,K) = TAN(TO_FM(J*K)) + EXP(TO_FM(J-K))*LOG(TO_FM('1.2')*(J+K)) +  &
                        TO_FM(I*100)/N_CASE
            ENDDO
            B(J) = J
         ENDDO
         CALL FM_LIN_SOLVE(A,X,B,5,DET)
         DO J = 1, 5
            CALL OUTPUT(I,N_CASE,X(J))
            N_CASE = N_CASE + 1
         ENDDO

         N_CASE = 256
         CALL OUTPUT(I,N_CASE,DET)

         N_CASE = 257
         CALL FM_INVERSE(A,5,AINV,DET)
         DO J = 1, 5
            DO K = 1, 5
               CALL OUTPUT(I,N_CASE,AINV(J,K))
               N_CASE = N_CASE + 1
            ENDDO
         ENDDO

         N_CASE = 282
         AX = 0
         BX = 2
         TOL = TO_FM('1.0E-25')
         CALL FM_FIND_MIN(1,AX,BX,TOL,XVAL,FVAL,F_TEST_FUNCTION,1,0,6)
         CALL OUTPUT(I,N_CASE,XVAL)
         N_CASE = 283
         CALL OUTPUT(I,N_CASE,FVAL)

         N_CASE = 284
         AX = TO_FM(I)/4
         BX = FM_FPRIME(MOD(I,6),AX,F_TEST_FUNCTION,1)
         CALL OUTPUT(I,N_CASE,BX)

         N_CASE = 285
         AX = TO_FM(I)/4
         BX = AX + 1
         TOL = TO_FM('1.0E-40')
         CALL FM_INTEGRATE(F_TEST_FUNCTION,1,AX,BX,TOL,FVAL,0,6)
         CALL OUTPUT(I,N_CASE,FVAL)

         N_CASE = 286
         N_ORDER = 2
         N_FUNCTION = 1
         AX = TO_FM(I)/4
         BX = AX + TO_FM(1)/4
         S(1) = 1
         S(2) = -1
         TOL = TO_FM(' 1.0e-30 ')
         CALL FM_RK14(AX,BX,N_ORDER,FM_RK14_F,N_FUNCTION,S,TOL,S1)
         CALL OUTPUT(I,N_CASE,S1(1))
         N_CASE = N_CASE + 1
         CALL OUTPUT(I,N_CASE,S1(2))

         N_CASE = 288
         AX = TO_FM(-3) + TO_FM(I-1)/2
         BX = AX + 1.0E-3
         CALL FM_SECANT(AX,BX,F_TEST_FUNCTION,2,XVAL,0,6)
         CALL OUTPUT(I,N_CASE,XVAL)

         N_CASE = 289
         DO J = 1, 5
            DO K = 1, 5
               ZA(J,K) = CMPLX(TAN(TO_FM(J*K)) + EXP(TO_FM(J-K))*LOG(TO_FM('1.2')*(J+K)) +  &
                                   TO_FM(I*100)/N_CASE ,                                    &
                               TAN(TO_FM(J*K)) + EXP(TO_FM(J-K))*LOG(TO_FM('2.3')*(J+K)) +  &
                                   TO_FM(I*100)/N_CASE                                      &
                              )
            ENDDO
            ZB(J) = J
         ENDDO
         CALL ZM_LIN_SOLVE(ZA,ZX,ZB,5,ZDET)
         DO J = 1, 5
            CALL OUTPUT(I,N_CASE,REAL(ZX(J)))
            N_CASE = N_CASE + 1
            CALL OUTPUT(I,N_CASE,AIMAG(ZX(J)))
            N_CASE = N_CASE + 1
         ENDDO

         N_CASE = 299
         CALL OUTPUT(I,N_CASE,REAL(ZDET))
         N_CASE = 300
         CALL OUTPUT(I,N_CASE,AIMAG(ZDET))

         N_CASE = 301
         CALL ZM_INVERSE(ZA,5,ZAINV,ZDET)
         DO J = 1, 5
            DO K = 1, 5
               CALL OUTPUT(I,N_CASE,REAL(ZAINV(J,K)))
               N_CASE = N_CASE + 1
               CALL OUTPUT(I,N_CASE,AIMAG(ZAINV(J,K)))
               N_CASE = N_CASE + 1
            ENDDO
         ENDDO

         N_CASE = 351
         CALL OUTPUT(I,N_CASE,REAL(ZDET))
         N_CASE = 352
         CALL OUTPUT(I,N_CASE,AIMAG(ZDET))

         N_CASE = 353
         Z1 = CMPLX(3*TO_FM(I)/4,6*TO_FM(I)/5)
         Z2 = ZM_FPRIME(MOD(I,4),Z1,Z_TEST_FUNCTION,1)
         CALL OUTPUT(I,N_CASE,REAL(Z2))
         N_CASE = 354
         CALL OUTPUT(I,N_CASE,AIMAG(Z2))

         N_CASE = 355
         Z1 = EXP(CMPLX(TO_FM(0),TO_FM(I))) * TO_FM('0.5') * TO_FM('1.05')**(I-1)
         Z2 = Z1 + CMPLX(TO_FM('0.001'),I*TO_FM('0.001'))
         CALL ZM_SECANT(Z1,Z2,Z_TEST_FUNCTION,1,Z3,0,6)
         CALL OUTPUT(I,N_CASE,REAL(Z3))
         N_CASE = 356
         CALL OUTPUT(I,N_CASE,AIMAG(Z3))

         N_CASE = 357
         CALL ZM_ROOTS(8,Z_TEST_FUNCTION,1,N_FOUND,LIST_OF_ROOTS,0,6)
         DO J = 1, N_FOUND-1
            DO K = 1, N_FOUND-J
               IF (ABS( ABS(LIST_OF_ROOTS(K)) - ABS(LIST_OF_ROOTS(K+1)) ) < 1.0D-50 .AND.  &
                   AIMAG(LIST_OF_ROOTS(K)) < 0) THEN
                   Z1 = LIST_OF_ROOTS(K)
                   LIST_OF_ROOTS(K) = LIST_OF_ROOTS(K+1)
                   LIST_OF_ROOTS(K+1) = Z1
               ELSE IF (ABS(LIST_OF_ROOTS(K)) < ABS(LIST_OF_ROOTS(K+1))) THEN
                   Z1 = LIST_OF_ROOTS(K)
                   LIST_OF_ROOTS(K) = LIST_OF_ROOTS(K+1)
                   LIST_OF_ROOTS(K+1) = Z1
               ENDIF
            ENDDO
         ENDDO
         DO J = 1, N_FOUND
            CALL OUTPUT(I,N_CASE,REAL(LIST_OF_ROOTS(J)))
            N_CASE = N_CASE + 1
            CALL OUTPUT(I,N_CASE,AIMAG(LIST_OF_ROOTS(J)))
            N_CASE = N_CASE + 1
         ENDDO

         N_CASE = 373
         AX = TO_FM('-2.11') + TO_FM(I-1)/3
         BX = AX + 1.0E-3
         CALL FM_SECANT(AX,BX,F_TEST_FUNCTION,3,XVAL,0,6)
         CALL OUTPUT(I,N_CASE,XVAL)

         N_CASE = 374
         Z1 = EXP(CMPLX(TO_FM(0),TO_FM(I))) * TO_FM('0.5') * TO_FM('1.05')**(I-1)
         Z2 = Z1 + CMPLX(TO_FM('0.001'),I*TO_FM('0.001'))
         CALL ZM_SECANT(Z1,Z2,Z_TEST_FUNCTION,2,Z3,0,6)
         CALL OUTPUT(I,N_CASE,REAL(Z3))
         N_CASE = 375
         CALL OUTPUT(I,N_CASE,AIMAG(Z3))

         N_CASE = 376
         CALL ZM_ROOTS(6,Z_TEST_FUNCTION,2,N_FOUND,LIST_OF_ROOTS,0,6)
         DO J = 1, N_FOUND-1
            DO K = 1, N_FOUND-J
               IF (ABS( ABS(LIST_OF_ROOTS(K)) - ABS(LIST_OF_ROOTS(K+1)) ) < 1.0D-50 .AND.  &
                   AIMAG(LIST_OF_ROOTS(K)) < 0) THEN
                   Z1 = LIST_OF_ROOTS(K)
                   LIST_OF_ROOTS(K) = LIST_OF_ROOTS(K+1)
                   LIST_OF_ROOTS(K+1) = Z1
               ELSE IF (ABS(LIST_OF_ROOTS(K)) > ABS(LIST_OF_ROOTS(K+1))) THEN
                   Z1 = LIST_OF_ROOTS(K)
                   LIST_OF_ROOTS(K) = LIST_OF_ROOTS(K+1)
                   LIST_OF_ROOTS(K+1) = Z1
               ENDIF
            ENDDO
         ENDDO
         DO J = 1, N_FOUND
            CALL OUTPUT(I,N_CASE,REAL(LIST_OF_ROOTS(J)))
            N_CASE = N_CASE + 1
            CALL OUTPUT(I,N_CASE,AIMAG(LIST_OF_ROOTS(J)))
            N_CASE = N_CASE + 1
         ENDDO
         N_CASE = N_CASE - 1

      ENDDO

      CLOSE(30+this_image())

sync all

if (this_image() .eq. 1) then
      CALL CLOCK_TIME(TIME2)
      WRITE (*,"(//F9.2,A)") TIME2-TIME1, ' seconds for the tests.'

!             Check the results against file TestFM_parallel.chk

      INQUIRE(FILE='TestFM_parallel.chk', EXIST=CHK_FILE_EXISTS)
      IF (.NOT. CHK_FILE_EXISTS) THEN
          WRITE (*,"(//A//)") ' Cannot check the results because file  TestFM_parallel.chk ' //  &
                              ' does not exist in this location.'
          STOP
      ENDIF

!             Some of the cases above test the FM functions for machine characteristics, such as
!             DIGITS, EXPONENT, FRACTION, ...
!             These might be different when this program runs on a different machine, so
!             don't call it an error if one of those cases differs from the chk file.

      SKIP_CHECK = [  99, 112, 113, 122, 125, 126, 128, 129, 135, 136, 140, 141, 142, 143, 144,  &
                     146, 151, 188, 204, 205, 208, 209, 220, 221, 222, 223, 224, 225, 228, 229 ]

      OPEN(21,FILE='TestFM_parallel.chk')
      DO I = 1, num_images()
         WRITE(FILE_NUMBER,"(I8)") I
         FILE_NAME = TRIM( 'TestFM_parallel' // ADJUSTL(FILE_NUMBER) ) // '.out'
         OPEN(30+I,FILE=TRIM(FILE_NAME))
      ENDDO

      IF (FM_SIGNIFICANT_DIGITS /= 50) THEN

      ENDIF

      N_ERRORS = 0
      I = 1
      DO K = 1, 2*N_VALS
         DO J = 1, N_CASE
            READ (21,"(A)") LINE1
            READ (30+I,"(A)") LINE2
            X1 = TO_FM(TRIM(LINE1(15:251)))
            X2 = TO_FM(TRIM(LINE2(15:251)))
            R = ABS(X1-X2)
            IF (J >= 355) THEN
                IF (ABS(X1) < 1.0D-50 .AND. ABS(X2) < 1.0D-50) R = 0
            ENDIF
            IF (R > 0) THEN
                IF (ABS(X2) > 0) THEN
                    R = R / ABS(X2)
                ELSE
                    R = R / ABS(X1)
                ENDIF
            ENDIF
            IF (R > 1.0D-50 .OR. IS_UNKNOWN(R)) THEN
                SKIP = 0
                DO L = 1, 30
                   IF (SKIP_CHECK(L) == J) THEN
                       SKIP = 1
                       EXIT
                   ENDIF
                ENDDO
                IF (SKIP == 1) CYCLE
                WRITE (*,"(/A)") ' The current result does not agree with file  TestFM_parallel.chk'
                WRITE (*,"(A,A)") ' chk:   ', TRIM(LINE1)
                WRITE (*,"(A,A)") ' out:   ', TRIM(LINE2)
                N_ERRORS = N_ERRORS + 1
            ENDIF
         ENDDO
         I = I + 1
         IF (I > num_images()) I = 1
      ENDDO
      WRITE (*,"(//I5,A,I6,A)") N_CASE*2*N_VALS, ' cases were tested using',  &
                                num_images(), ' threads.'
      IF (N_ERRORS == 0) THEN
          WRITE (*,"(//A//)") ' All results were ok.'
      ELSE
          WRITE (*,"(//I6,A//)") N_ERRORS, ' errors were found.'
      ENDIF
endif


      STOP
      END PROGRAM TEST

      SUBROUTINE OUTPUT(I,N_CASE,X1)
      USE FMVALS_PARALLEL
      USE FMZM_PARALLEL
      IMPLICIT NONE
      TYPE (FM) :: X1
      INTEGER :: I,N_CASE,JD
      CHARACTER(251)  :: ST1
      CHARACTER(20) :: FMT

      JD = 7*NDIG_USER

      WRITE (FMT,"(A,I6,'.',I6)") 'ES', JD+30, JD
      CALL FM_FORM(TRIM(FMT),X1,ST1)
      WRITE (* ,"(I4,I6,5X,A)") I,N_CASE,TRIM(ST1)
      WRITE (30+this_image(),"(I4,I6,5X,A)") I,N_CASE,TRIM(ST1)

      END SUBROUTINE OUTPUT

      SUBROUTINE OUTPUT_IM(I,N_CASE,I1)
      USE FMVALS_PARALLEL
      USE FMZM_PARALLEL
      IMPLICIT NONE
      TYPE (IM) :: I1
      INTEGER :: I,N_CASE,JD
      CHARACTER(251)  :: ST1
      CHARACTER(20) :: FMT
      IF (I1 == 0) THEN
          JD = 2
      ELSE
          JD = LOG10(ABS(TO_FM(I1))) + 2
      ENDIF
      WRITE (FMT,"(A,I6)") 'I', JD
      CALL IM_FORM(TRIM(FMT),I1,ST1)
      WRITE (* ,"(I4,I6,5X,A)") I,N_CASE,TRIM(ST1)
      WRITE (30+this_image(),"(I4,I6,5X,A)") I,N_CASE,TRIM(ST1)

      END SUBROUTINE OUTPUT_IM

      SUBROUTINE ARGS(I,N_CASE,MAX_CASE,N_VALS,X_VALS,RAN,X_MIN,X_MAX,J,K)
      USE FMZM_PARALLEL
      IMPLICIT NONE

!  Return J and K for the test program to use x_vals(j) and optionally x_vals(k)
!  as input for testing a function.

!  For functions with restricted domains, choose j,k so that x_min < x_vals(j) < x_max.

      INTEGER :: N_VALS
      TYPE (FM) :: X_VALS(2*N_VALS)
      REAL :: X_MIN,X_MAX
      INTEGER :: I, J, K, L, N_CASE, MAX_CASE
      DOUBLE PRECISION :: RAN(4*N_VALS*MAX_CASE)


      L = (I-1)*MAX_CASE + N_CASE
      J = RAN(L)
      K = RAN(L+2*N_VALS*MAX_CASE)

      DO WHILE (X_VALS(J) < X_MIN .OR. X_VALS(J) > X_MAX)
         J = J + 7
         IF (J > 2*N_VALS) J = J - 2*N_VALS
      ENDDO

      DO WHILE (X_VALS(K) < X_MIN .OR. X_VALS(K) > X_MAX)
         K = K + 7
         IF (K > 2*N_VALS) K = K - 2*N_VALS
      ENDDO

      END SUBROUTINE ARGS

      SUBROUTINE CLOCK_TIME(CLOCK_SECONDS)

!  Convert intrinsic function DATE_AND_TIME character output to a real second count.
!  It seems optimistic to worry about the century changing during the time interval
!  being measured, so that field is ignored.

!  Since not all months have the same number of days, using the average number of seconds/month
!  below may cause a wrong elapsed time to be computed if the start time and stop time calls
!  occur in different months.

!  DATE = "ccyymmdd"  for century, year, month, date

!  TIME = "hhmmss.sss"  for hour, minute, seconds

      IMPLICIT NONE
      DOUBLE PRECISION :: CLOCK_SECONDS
      CHARACTER( 8) :: DATE
      CHARACTER(10) :: TIME
      INTEGER :: FIELD(8)

      CALL DATE_AND_TIME(DATE,TIME)

      READ( DATE , "(4I2)" ) FIELD(1:4)
      READ( TIME , "(3I2,1X,I3)" ) FIELD(5:8)

      CLOCK_SECONDS = FIELD(2) * 3.15576D7 + FIELD(3) * 2.6298D6 + FIELD(4) * 8.64D4 +  &
                      FIELD(5) * 3.6D+3 + FIELD(6) * 60.0D0 + FIELD(7) + FIELD(8) / 1000.0D0

      END SUBROUTINE CLOCK_TIME


      FUNCTION F_TEST_FUNCTION(X,NF,SETTINGS)     RESULT (RETURN_VALUE)

!  Many of the routines from FM_Sample_Routines.f95 need to call a function defined in the
!  user's program.  That function defines the model to be fitted, the function to be minimized,
!  integrated, etc.

!  Because these user functions are called from a lower-level FM routine, they use FMVALS_PARALLEL
!  and not FMZM.  They need a SETTINGS variable added to their argument list and they
!  need to do their calculations using calls to the low-level FM routines.

      USE FMVALS_PARALLEL
      IMPLICIT NONE

      INTEGER :: NF
      TYPE (MULTI) :: RETURN_VALUE, X
      TYPE (MULTI) :: T1, T2
      TYPE (FM_SETTINGS) :: SETTINGS

      IF (NF == 1) THEN
          CALL FMGAM(X,RETURN_VALUE,SETTINGS)
      ELSE IF (NF == 2) THEN

!               5.3136 + 15.678 x - 7.83 x^2 - 3.2 x^3 + x^4

          T1 = X
          CALL FMST2M('3.2',T2,SETTINGS)
          CALL FMSUB_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMST2M('7.83',T2,SETTINGS)
          CALL FMSUB_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMST2M('15.678',T2,SETTINGS)
          CALL FMADD_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMST2M('5.3136',T2,SETTINGS)
          CALL FMADD(T1,T2,RETURN_VALUE,SETTINGS)
      ELSE IF (NF == 3) THEN

!             32 - 120 x + 100 x^2 + 90 x^3 - 135 x^4 + 27 x^5

!             Triple root at x = 2/3, simple roots at x = -1, 4.

          T1 = X
          CALL FMMPYI_R1(T1,27,SETTINGS)
          CALL FMI2M(-135,T2,SETTINGS)
          CALL FMADD_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMI2M(90,T2,SETTINGS)
          CALL FMADD_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMI2M(100,T2,SETTINGS)
          CALL FMADD_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMI2M(-120,T2,SETTINGS)
          CALL FMADD_R1(T1,T2,SETTINGS)
          CALL FMMPY_R1(T1,X,SETTINGS)

          CALL FMI2M(32,T2,SETTINGS)
          CALL FMADD(T1,T2,RETURN_VALUE,SETTINGS)
      ELSE
          CALL FMI2M(0,RETURN_VALUE,SETTINGS)
      ENDIF

      END FUNCTION F_TEST_FUNCTION


      FUNCTION Z_TEST_FUNCTION(X,NF,SETTINGS)     RESULT (RETURN_VALUE)

!  This function is used by the complex type(zm) routines from FM_Sample_Routines.f95.
!  Like F_TEST_FUNCTION, it uses type(multi) arguments and results, and must be coded
!  using low-level calls.

      USE FMVALS_PARALLEL
      IMPLICIT NONE

      INTEGER :: NF
      TYPE (MULTI), DIMENSION(2) :: RETURN_VALUE, X
      TYPE (MULTI), DIMENSION(2) :: T1, T2
      TYPE (FM_SETTINGS) :: SETTINGS

      IF (NF == 1) THEN

!             x**8 - 3*x**5 + 2*x - 1

          CALL ZMIPWR(X,8,T1,SETTINGS)
          CALL ZMIPWR(X,5,T2,SETTINGS)
          CALL ZMMPYI_R1(T2,3,SETTINGS)
          CALL ZMSUB_R1(T1,T2,SETTINGS)
          CALL ZMMPYI(X,2,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMI2M(1,T2,SETTINGS)
          CALL ZMSUB(T1,T2,RETURN_VALUE,SETTINGS)
      ELSE IF (NF == 2) THEN

!             x**6 - x**5 - 5*x**4 - 19*x**3 - 28*x**2 - 28*x - 16

!             This function has 2 double roots, which forces the root-finder to evaluate
!             derivatives.  That causes precision to be raised.

!             The parallel version of FM has a fixed upper limit on precision, so using
!             zmipwr as in the function above can cause the root-finder to fail.

!             Using Horner's rule below avoids the zmipwr call and can sometimes avoid
!             exceeding the precision limit.

!             Double roots at -1/2 +- sqrt(3)/2 i, simple roots at -1, 4.

          CALL ZMEQ(X,T1,SETTINGS)
          CALL ZMI2M(-1,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMMPY_R1(T1,X,SETTINGS)
          CALL ZMI2M(-5,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMMPY_R1(T1,X,SETTINGS)
          CALL ZMI2M(-19,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMMPY_R1(T1,X,SETTINGS)
          CALL ZMI2M(-28,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMMPY_R1(T1,X,SETTINGS)
          CALL ZMI2M(-28,T2,SETTINGS)
          CALL ZMADD_R1(T1,T2,SETTINGS)
          CALL ZMMPY_R1(T1,X,SETTINGS)
          CALL ZMI2M(-16,T2,SETTINGS)
          CALL ZMADD(T1,T2,RETURN_VALUE,SETTINGS)
      ELSE
          CALL ZMI2M(0,RETURN_VALUE,SETTINGS)
      ENDIF

      END FUNCTION Z_TEST_FUNCTION


      SUBROUTINE FM_RK14_F(N_ORDER, N_FUNCTION, X, S, RHS, SETTINGS)

!  Compute the right-hand-side function for the vector first-order differential equation
!  s' = f(x,s).

!  N_ORDER is the order of the differential equation.  After reducing the equation to
!          a first-order vector D.E., N_ORDER is the length of vectors S and RHS.
!          (N_ORDER is unused in this sample version)

!  RHS is returned as the right-hand-side vector function of the differential equation,
!  with S as the input vector:  RHS = F(X,S).

!  N_FUNCTION is the function to be evaluated, for cases where a program may solve
!             several different differential equations.


      USE FMVALS_PARALLEL
      IMPLICIT NONE

      INTEGER, PARAMETER :: MAXIMUM_ORDER = 3
      INTEGER :: N_ORDER, N_FUNCTION
      TYPE (MULTI) :: X, S(MAXIMUM_ORDER), RHS(MAXIMUM_ORDER)
      TYPE (MULTI) :: T1, T2, T3
      TYPE (FM_SETTINGS) :: SETTINGS

      IF (N_FUNCTION == 1) THEN

!             y'' = -y' + sin(x)*y - cos(x)

          RHS(1) = S(2)

!         RHS(2) = -S(2) + SIN(X)*S(1) - COS(X)

!             We can speed this up by computing sin(x) and cos(x) both at the same time.
!             FMCSSN is the low-level version of FM_COS_SIN.  FMCSSN needs type(multi)
!             arguments, while FM_COS_SIN needs type(fm).

          CALL FMCSSN(X,T1,T2,SETTINGS)

          CALL FMMPY(T2,S(1),T3,SETTINGS)
          CALL FMSUB(T3,T1,T2,SETTINGS)
          CALL FMSUB(T2,S(2),RHS(2),SETTINGS)
      ELSE
          RHS(1) = S(1)
          IF (N_ORDER >= 2) CALL FMI2M(0,S(2),SETTINGS)
          IF (N_ORDER >= 3) CALL FMI2M(0,S(3),SETTINGS)
      ENDIF

      END SUBROUTINE FM_RK14_F
