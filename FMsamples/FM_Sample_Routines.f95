
!  Version 1.4

!  This file collects all the various FM routines from the "More sample programs" page on the
!  FM web site.  See that page for the programs that call these routines and illustrate their
!  use.  Here is a list of the user-callable routines in this file (see the documentation at
!  the top of each routine for explanation of each of the arguments to the routines).

!  1.  Find a minimum or maximum function value of a real function of one variable.

!                   SUBROUTINE FM_FIND_MIN(MIN_OR_MAX,AX,BX,TOL,XVAL,FVAL,F,NF,KPRT,KW)

!  2.  Nth derivative of a real function of one variable.

!                   FUNCTION FM_FPRIME(N,A,F,NF)

!  3.  Nth derivative of a complex function of one variable.

!                   FUNCTION ZM_FPRIME(N,A,F,NF)

!  4.  Definite integral for a real function of one variable.

!                   SUBROUTINE FM_INTEGRATE(F,N,A,B,TOL,RESULT,KPRT,NW)

!  5.  Inverse matrix for a real NxN matrix.

!                   SUBROUTINE FM_INVERSE(A,N,B,DET)

!  6.  Inverse matrix for a complex NxN matrix.

!                   SUBROUTINE ZM_INVERSE(A,N,B,DET)

!  7.  Generate the real linear system of normal equations for a least square fit.

!                   SUBROUTINE FM_GENEQ(F,A,B,K,X,Y,N)

!  8.  Solve a real NxN linear system of equations.

!                   SUBROUTINE FM_LIN_SOLVE(A,X,B,N,DET)

!  9.  Solve a complex NxN linear system of equations.

!                   SUBROUTINE ZM_LIN_SOLVE(A,X,B,N,DET)

! 10.  Solve a real differential equation (initial value problem).

!                   SUBROUTINE FM_RK14(A,B,N_ORDER,N_FUNCTION,S,TOL,S1)

! 11.  Find a root of a real function of one variable.

!                   SUBROUTINE FM_SECANT(AX,BX,F,NF,ROOT,KPRT,KU)

! 12.  Find a root of a complex function of one variable.

!                   SUBROUTINE ZM_SECANT(AX,BX,F,NF,ROOT,KPRT,KU)

! 13.  Find NR roots of a complex function of one variable.

!                   SUBROUTINE ZM_ROOTS(NR,F,NF,N_FOUND,LIST_OF_ROOTS,KPRT,KU)



      SUBROUTINE FM_FIND_MIN(MIN_OR_MAX,AX,BX,TOL,XVAL,FVAL,F,NF,KPRT,KW)
      USE FMZM

!  MIN_OR_MAX having value 1 means minimize the function, otherwise maximize.
!  AX, BX     define the endpoints of an interval in which the search takes place.
!  TOL        is the tolerance for the minimum.  Usually TOL should be no less than
!             sqrt(epsilon(ax)), meaning the x-coordinate XVAL of the extreme point will
!             be accurate to only about half the digits carried.  The y-coordinate FVAL
!             should be accurate to nearly full precision.
!             This happens because the typical graph is nearly parabolic near the minimum,
!             and within sqrt(epsilon(ax)) of the minimum all the function values are
!             essentially identical at the current precision.
!  XVAL       is returned as the value of X that minimizes (or maximizes) function F(X,NF).
!             It is a relative extreme point, and may not be the global extreme point if the
!             function has more than one extremum on the interval.
!  FVAL       is returned as the function value at XVAL.
!  F(X,NF)    is the function to be minimized.  X is the argument and NF is the function
!             number, in case several functions are defined within F.
!  KPRT       controls printing within the routine:
!             KPRT = 0 for no output
!             KPRT = 1 for the approximation to the root and the function
!                      value to be printed once at the end of the routine.
!             KPRT = 2 for the approximation to the root and the function
!                      value to be printed each iteration.
!  KW         is the unit number for output.

!  The method used is a combination of golden section search and successive parabolic interpolation.
!  Convergence is never much slower than that for a fibonacci search.  If f has a continuous second
!  derivative which is positive at the minimum (which is not at ax or bx), then convergence is
!  superlinear, and usually of the order of about 1.324....

!  The function f is never evaluated at two points closer together than eps*abs(FVAL)+(tol/3),
!  where eps is approximately the square root of the relative machine precision.  If f is a
!  unimodal function and the computed values of f are always unimodal when separated by at least
!  eps*abs(x)+(tol/3), then FVAL approximates the abscissa of the global minimum of f on the
!  interval ax,bx with an error less than 3*eps*abs(FVAL)+tol.  If f is not unimodal, then FVAL
!  may approximate a local, but perhaps non-global, minimum to the same accuracy.

!  This routine is a slightly modified translation of function FVAL from netlib, which was adapted
!  from the algol 60 procedure localmin given by Richard Brent in Algorithms For Minimization
!  Without Derivatives, Prentice-Hall (1973).

      IMPLICIT NONE
      CHARACTER(80)  :: ST1,ST2
      INTEGER :: J,MINV,MIN_OR_MAX,NF,KPRT,KW
      TYPE (FM)           :: AX,BX,TOL,XVAL,FVAL
      TYPE (FM), EXTERNAL :: F

      TYPE (FM), SAVE :: A, B, C, D, E, EPS, XM, P, Q, R, T2, U, V, W, FU, FV, FW, FX, X, TOL1, TOL3

      MINV = 1
      IF (MIN_OR_MAX /= 1) MINV = -1

!             C is the squared inverse of the golden ratio.

      C = (3-SQRT(TO_FM('5.0D0')))/2

!             EPS is approximately the square root of the relative machine precision.

      EPS = EPSILON(AX)
      TOL1 = EPS + 1
      EPS = SQRT(EPS)

      A = MIN(AX,BX)
      B = MAX(AX,BX)
      V = A + C*(B-A)
      W = V
      X = V
      E = 0
      FX = F(X,NF)*MINV
      FV = FX
      FW = FX
      TOL3 = TOL/3
      J = 1

      IF (KPRT == 2) THEN
          WRITE (KW,*) ' '
          IF (MIN_OR_MAX == 1) THEN
              WRITE (KW,*) ' FM_FIND_MIN.  Begin trace of all iterations.'
              WRITE (KW,*) '               Search for a relative minimum on the interval'
              WRITE (KW,"(13X,ES20.10,'    to ',ES20.10/)") TO_DP(AX),TO_DP(BX)
          ELSE
              WRITE (KW,*) ' FM_FIND_MIN.  Begin trace of all iterations.'
              WRITE (KW,*) '               Search for a relative maximum on the interval'
              WRITE (KW,"(13X,ES20.10,'    to ',ES20.10/)") TO_DP(AX),TO_DP(BX)
          ENDIF
          ST1 = FM_FORMAT('ES35.25',X)
          ST2 = FM_FORMAT('ES35.25',FX*MINV)
          WRITE (KW,"('      J =',I3,4X,'  x  = ',A)") J,TRIM(ST1)
          WRITE (KW,"('         ',3X,4X,'f(x) = ',A/)") TRIM(ST2)
      ENDIF

!             The main loop starts here.

  110 XM = (A+B)/2
      TOL1 = EPS*ABS(X) + TOL3
      T2 = 2*TOL1

!             Check the stopping criterion.

      IF (ABS(X-XM) <= (T2-(B-A)/2)) GO TO 160
      P = 0
      Q = 0
      R = 0
      IF (ABS(E) > TOL1) THEN
          R = (X-W)*(FX-FV)    !     Fit a parabola.
          Q = (X-V)*(FX-FW)
          P = (X-V)*Q-(X-W)*R
          Q = 2*(Q-R)
          IF (Q > 0) THEN
              P = -P
          ELSE
              Q = -Q
          ENDIF
          R = E
          E = D
      ENDIF

      IF ((ABS(P) >= ABS(Q*R/2)) .OR. (P <= Q*(A-X)) .OR. (P >= Q*(B-X))) GO TO 120

!             Make a parabolic-interpolation step.

      D = P/Q
      U = X + D

!             f must not be evaluated too close to ax or bx.

      IF (((U-A) >= T2) .AND. ((B-U) >= T2)) GO TO 130
      D = TOL1
      IF (X >= XM) D = -D
      GO TO 130

!             Make a golden-section step.

  120 IF (X < XM) THEN
          E = B - X
      ELSE
          E = A - X
      ENDIF
      D = C*E

!             f must not be evaluated too close to x.

  130 IF (ABS(D) >= TOL1) THEN
          U = X + D
      ELSE
          IF (D > 0) THEN
              U = X + TOL1
          ELSE
              U = X - TOL1
          ENDIF
      ENDIF
      FU = F(U,NF)*MINV

      J = J + 1
      IF (KPRT == 2) THEN
          ST1 = FM_FORMAT('ES35.25',U)
          ST2 = FM_FORMAT('ES35.25',FU*MINV)
          WRITE (KW,"('      J =',I3,4X,'  x  = ',A)") J,TRIM(ST1)
          WRITE (KW,"('         ',3X,4X,'f(x) = ',A/)") TRIM(ST2)
      ENDIF

!             update  a, b, v, w, and x.

      IF (FX <= FU) THEN
          IF (U < X) THEN
              A = U
          ELSE
              B = U
          ENDIF
      ENDIF
      IF (FU > FX) GO TO 140
      IF (U < X) THEN
          B = X
      ELSE
          A = X
      ENDIF
      V = W
      FV = FW
      W = X
      FW = FX
      X = U
      FX = FU
      GO TO 110

  140 IF ((FU > FW) .AND. (W /= X)) GO TO 150
      V = W
      FV = FW
      W = U
      FW = FU
      GO TO 110

  150 IF ((FU > FV) .AND. (V /= X) .AND. (V /= W)) GO TO 110
      V = U
      FV = FU
      GO TO 110

!  end of main loop

  160 XVAL = X
      FVAL = FX*MINV

      IF (KPRT >= 1) THEN
          IF (KPRT == 1) WRITE (KW,*) ' '
          IF (MIN_OR_MAX == 1) THEN
              WRITE (KW,"('  FM_FIND_MIN.   Function ',I3,I6,' iterations.  A relative minimum"// &
                        " on interval'/13X,ES20.10,'    to ',ES20.10,'  is')")                    &
                     NF,J,TO_DP(AX),TO_DP(BX)
              ST1 = FM_FORMAT('ES35.25',XVAL)
              ST2 = FM_FORMAT('ES35.25',FVAL)
              WRITE (KW,"(15X,'   x  = ',A)") TRIM(ST1)
              WRITE (KW,"(15X,' f(x) = ',A)") TRIM(ST2)
          ELSE
              WRITE (KW,"('  FM_FIND_MIN.   Function ',I3,I6,' iterations.  A relative maximum"// &
                        " on interval'/13X,ES20.10,'    to ',ES20.10,'  is')")                    &
                     NF,J,TO_DP(AX),TO_DP(BX)
              ST1 = FM_FORMAT('ES35.25',XVAL)
              ST2 = FM_FORMAT('ES35.25',FVAL)
              WRITE (KW,"(15X,'   x  = ',A)") TRIM(ST1)
              WRITE (KW,"(15X,' f(x) = ',A)") TRIM(ST2)
          ENDIF
          WRITE (KW,*) ' '
      ENDIF

      END SUBROUTINE FM_FIND_MIN

      FUNCTION FM_FPRIME(N,A,F,NF)     RESULT (RETURN_VALUE)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This routine finds the Nth derivative of F(X,NF), evaluated at A.
!  NF is passed on to function F to indicate which function to use in cases where several
!  different functions may be defined there.

!  F must be defined in an interval containing A, so that F can be sampled on both sides of A.

!  N may be zero, so that in cases where F suffers cancellation error at A, an accurate
!  function value is returned.

!  FM_FPRIME tries to return full accuracy for the derivative, by raising precision above
!  the user's level and using difference formulas.

      TYPE (FM)           :: RETURN_VALUE, A
      TYPE (FM), EXTERNAL :: F
      INTEGER :: J, K, KWARN_SAVE, NDSAVE, N, NF
      TYPE (FM), SAVE :: D1, D2, F1, F2, H, TOL, TOL2, X1

!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      CALL FM_EQU(A,X1,NDSAVE,NDIG)
      KWARN_SAVE = KWARN
      KWARN = 0

      D2 = 0
      F1 = F(X1,NF)
      IF (F1 /= 0) THEN
          CALL FM_ULP(F1,TOL)
      ELSE
          TOL = EPSILON(TO_FM(1))
      ENDIF
      TOL = ABS(TOL)

!             Check for a legal function value.

      IF (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1) .OR. IS_UNDERFLOW(F1) .OR. N < 0) THEN
          D2 = TO_FM(' UNKNOWN ')
          GO TO 110
      ENDIF
      F2 = F1

!             Loop at increasing precision until the difference formula is accurate.

      DO J = 1, 100

         NDIG = 2*NDIG

!             Define the variables used below at the new higher precision.

         CALL FM_EQU(D2,D1,NDIG/2,NDIG)
         CALL FM_EQU(F2,F1,NDIG/2,NDIG)
         CALL FM_EQU(TOL,TOL2,NDSAVE,NDIG)
         CALL FM_EQU(A,X1,NDSAVE,NDIG)

!             Special case for N = 0.

         IF (N == 0) THEN
             F2 = F(X1,NF)
             D2 = F2
             IF (ABS(F2-F1) < TOL2) GO TO 110
             CYCLE
         ENDIF
         F2 = F1

!             Special case for N = 1.

         IF (N == 1) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(X1,H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = SQRT(ABS(H))
             D2 = ( F(X1+H,NF) - F(X1-H,NF) ) / (2*H)
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

!             General case for even N > 1.

         IF (MOD(N,2) == 0) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(X1,H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = ABS(H)**(TO_FM(1)/(N+2))
             D2 = (-1)**(N/2) * BINOMIAL(TO_FM(N),TO_FM(N/2)) * F(X1,NF)
             DO K = 0, N/2-1
                D2 = D2 + (-1)**K * BINOMIAL(TO_FM(N),TO_FM(K)) *  &
                     ( F(X1+(N/2-K)*H,NF) + F(X1-(N/2-K)*H,NF) )
             ENDDO
             D2 = D2 / H**N
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

!             General case for odd N > 1.

         IF (MOD(N,2) == 1) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(X1,H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = ABS(H)**(TO_FM(1)/(N+2))
             D2 = 0
             DO K = 0, N/2
                D2 = D2 + (-1)**K * BINOMIAL(TO_FM(N-1),TO_FM(K))    *  &
                     ( F(X1+(N/2-K+1)*H,NF) - F(X1-(N/2-K+1)*H,NF) ) *  &
                     TO_FM(N*(N+1-2*K)) / ((N-K)*(N+1-K))
             ENDDO
             D2 = D2 / (2*H**N)
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

      ENDDO

!             Round and return.

  110 CALL FM_EQU(D2,RETURN_VALUE,NDIG,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWARN_SAVE
      END FUNCTION FM_FPRIME

      SUBROUTINE FM_GENEQ(F,A,B,K,X,Y,N)
      USE FMZM
      IMPLICIT NONE

!  Generate the KxK matrix A and Kx1 vector B of normal equations for the least square
!  fit of the K-parameter model

!     Y = C(1)*F(1,X) + ... + C(K)*F(K,X)

!  to the data points (X(J),Y(J)), J = 1, 2, ..., N.

!  A and B are returned, and then the coefficients C can be found by solving the
!  linear system  A * C = B.

!  Function L in the model evaluated at X is referenced by F(L,X) in this routine,
!  and F should be supplied as an external function subprogram by the user.

      INTEGER :: K, N
      TYPE (FM), EXTERNAL :: F
      TYPE (FM) :: A(K,K), B(K), X(N), Y(N)
      TYPE (FM), ALLOCATABLE :: FXI(:)
      INTEGER :: I, J, L
      TYPE (FM) :: XI, YI, FXIL

      IF (N <= 0 .OR. K <= 0) THEN
          WRITE (*,"(/' Error in FM_GENEQ.  K,N=',2I8/)") K,N
          STOP
      ENDIF

      ALLOCATE(FXI(K),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in FM_GENEQ.  Unable to allocate FXI with size ',I8/)") K
          STOP
      ENDIF

!             Initialize the upper triangle of A.

      DO I = 1, K
         DO J = I, K
            A(I,J) = 0
         ENDDO
         B(I) = 0
      ENDDO

!             Loop over the data points.

      DO I = 1, N
         XI = X(I)
         YI = Y(I)

!             Compute the K function values at X(I).

         DO J = 1, K
            FXI(J) = F(J,XI)
         ENDDO

!             Multiply the function values and add the products to the matrix.

         DO L = 1, K
            FXIL = FXI(L)
            DO J = L, K
               A(L,J) = A(L,J) + FXIL*FXI(J)
            ENDDO

!             Sum the right-hand-side term.

            B(L) = B(L) + YI*FXIL
         ENDDO
      ENDDO

!             Fill the lower triangle of the A matrix using symmetry.

      IF (K >= 2) THEN
          DO L = 2, K
             DO J = 1, L-1
                A(L,J) = A(J,L)
             ENDDO
          ENDDO
      ENDIF

      DEALLOCATE(FXI)
      END SUBROUTINE FM_GENEQ

      RECURSIVE SUBROUTINE FM_INTEGRATE(F,N,A,B,TOL,RESULT,KPRT,NW)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  High-precision numerical integration.

!  Integrate F(X,N) from A to B.  N is passed on to function F to indicate which function to use in
!  cases where several different functions may be defined there.


!  WARNING:  If the function F being integrated or one of its derivatives does not exist at one or
!            both of the endpoints (A,B), the endpoints should be exactly representable in FM's
!            number system.  For non-exact numbers like 1/3, sqrt(2), or pi/2, when FM_INTEGRATE
!            raises precision to evaluate the integration formula the endpoints are not accurate
!            enough at the higher precision.

!  Example:  Integrate  sqrt( tan( x ) )  from  0  to  pi/2.
!            First, pi/2 is not exact as an FM number.  At some precisions it may have rounded up,
!            making tan(x) negative and causing an error in sqrt.  Using sqrt( abs( tan( x ) ) )
!            is safer.
!            Second, b = pi/2 has been computed at the user's precision, so when FM_INTEGRATE
!            raises precision, the value of b is just zero-padded on the end, which does not give
!            enough information about how f(x) behaves near the singularity at pi/2.
!
!            Make the endpoints exact by changing variables.  Change the interval to [ 0 , 1 ]:
!
!                                        u = ( 2/pi ) * x   =>   du = ( 2/pi ) * dx
!            so
!                                        x = ( pi/2 ) * u   =>   dx = ( pi/2 ) * du
!            new limits
!                                        x = 0  =>  u = 0  and  x = pi/2  =>  u = 1
!
!            New integral:   Integrate  (pi/2) * sqrt( abs( tan( pi*u/2 ) ) )  from  0  to  1.
!
!            Now the function F should declare a local saved type(fm) variable PI, and then use
!            CALL FM_PI(PI) each time F is called to make sure the value of PI is correct at the
!            higher precision being used by FM_INTEGRATE when F is called.

      TYPE (FM) :: A,B,RESULT,TOL
      TYPE (FM), EXTERNAL :: F
      INTEGER :: N,KPRT,NW
      INTENT (IN) :: N,A,B,TOL,KPRT,NW
      INTENT (INOUT) :: RESULT

!  A,B,TOL, and RESULT are all type (FM) variables, and function F returns a type (FM) result.

!  RESULT is returned as the value of the integral, with ABS((RESULT-true)/true) less than TOL
!         (i.e., TOL is a relative error tolerance).
!         For example, to get 30 significant digits correct for the integral, set TOL = 1.0E-30.

!  FM precision must be set high enough in the calling program (using FM_SET) so that 1+TOL > 1
!  at that precision.  Using TOL = EPSILON(TO_FM(1)) will usually get a full precision result,
!  but for some functions this might fail.  A better strategy is to set precision higher than
!  the accuracy required for the integral.  For example, to get the integral to 50 significant
!  digits, CALL FMSET(60) and then set TOL = TO_FM(' 1.0E-50 ') before the call to FM_INTEGRATE.

!  KPRT can be used to show intermediate results on unit NW.
!  KPRT = 0 for no output
!       = 1 prints a summary for each call to FM_INTEGRATE
!       = 2 prints a trace of all iterations.

!  NW is the unit number used for KPRT output and any error or warning messages.

!  No method for numerical integration is foolproof.  Since it samples only a finite number of
!  function values, any numerical integration algorithm can be made to fail by choosing a
!  sufficiently badly-behaved function.  Such functions often vary by many orders of magnitude
!  over relatively small fractions of the interval (A,B).

!  F should be well-behaved in the interior of the interval (A,B).
!  The routine tries to handle any singularities of F or F' at A and/or B, so cases with interior
!  singularities should be done as separate calls having the singularities as endpoints.
!  The routine will try to handle cases where F or F' has singularities inside (A,B), but then
!  the time will be much slower and the routine might fail.

!  For a function with a removable singularity in the interior of the interval, such as
!  f(x) = 1/ln(x) - 1/(x-1), define F(X,N) to check for X = 1 and return the correct limiting
!  value, 0.5 in this case, when X is 1.

!  Among functions with no singularities, examples of badly behaved functions are those with one
!  or more extremely narrow tall spikes in their graphs. If possible, identify the peaks of any
!  such spikes first, then make separate calls with the peaks as endpoints of the intervals of
!  integration.

!  If the value of the integral is zero or very close to zero, relative error may be undefined, so
!  this routine may fail to converge and then return unknown.  For these cases, try breaking the
!  integral into two pieces and calling twice to get two non-zero results.  These two results can
!  then be added, often giving the original integral with sufficiently small absolute error even
!  though small relative error could not be attained.

!  If the function values are too extreme, it can cause problems.  For example, if an exponential
!  in F underflows and then is multiplied by something bigger than one, then F will return unknown.
!  If the result of the integral is much larger than the underflow threshold (TINY(TO_FM(1))), then
!  it is safe to set the underflowed results in F to zero to avoid getting unknown.

!  If the function is nearly divergent FM_INTEGRATE may fail.  1/x from 0 to b is divergent.
!  1/x**0.99 converges, but so slowly that FM_INTEGRATE may run a long time and then might fail.
!  1/x**0.9999 converges even more slowly and FM_INTEGRATE may fail by declaring that the integral
!  seems divergent.

!  When the integrand is highly (or infinitely) oscillatory, FM_INTEGRATE may fail.
!  If F has more than about 100 oscillations on the interval (A,B), it may be necessary to break
!  the interval into smaller intervals and call FM_INTEGRATE several times.
!  For infinitely many oscillations, like sin(1/x) from 0 to 1, first turn the integral into an
!  infinite series by calling FM_INTEGRATE to integrate each separate loop between roots of
!  sin(1/x).  The function is well-behaved for each call, so FM_INTEGRATE can get high precision
!  quickly for each.  Next form a sequence of k partial sums for this series.  The series converges
!  slowly, with 50 or 100 terms giving only 3 or 4 significant digits of the sum, so an
!  extrapolation method can be used to get a more accurate value of the sum of this series from
!  its first k terms. For an alternating series like this, the extrapolation method of Cohen,
!  Villegas, and Zagier often works very well.
!  Repeated Aitken extrapolation could be used instead -- it is a more widely known method.
!  Sample program Oscillate.f95 computes this integral.


!             M  is the maximum level for the integration algorithm.  The number of function
!                evaluations roughly doubles for each successive level until the tolerance is met.
!                Using M = 12 allows up to about 5,000 digits for most integrals, but the upper
!                limit for a given M depends on the function.
!                Raising M further will approximately double the maximum precision for each
!                extra level, but will also double the memory usage for each extra level.

      INTEGER, PARAMETER :: M = 12
      INTEGER, PARAMETER :: NT = 20*2**M
      TYPE (FM), SAVE :: ST_SAVE(0:NT)
      INTEGER, SAVE :: PRECISION_OF(0:NT) = 0
      INTEGER :: ABSIGN,I,ISTEP,K,KWSAVE,NDS,NDSAVE,NRETRY
      INTEGER, SAVE :: R_LEVEL = 0, NUM_F = 0

      TYPE (FM) :: A1,AB2,B1,C1,C2,CI,CT,D,EPS,ERR1,ERR2,FMAX,FMAX2,H,HF,LAST_H,PI,PRIOR_HS,  &
                   S,S1,S2,SI,ST,T,TOL1,TOL2,X,XF,XMAX,V,W

      CHARACTER(80) :: ST1,ST2
      REAL :: TIME1,TIME2
      LOGICAL :: SPIKE_FOUND,ST_IS_SAVED

!             Iterative tanh-sinh integration is used, increasing the order until convergence
!             is obtained, or M levels have been done.

      RESULT = 0

      NCALL = NCALL + 1
      NAMEST(NCALL) = 'INTEGRATE'
      KWSAVE = KW
      KW = NW
      R_LEVEL = R_LEVEL + 1
      NRETRY = 0
      IF (KPRT >= 2) THEN
          WRITE (NW,"(A)") ' '
          WRITE (NW,"(A,I9,A)") ' Input to FM_INTEGRATE.    Function N = ',N,'.    A, B ='
          CALL FM_PRINT(A)
          CALL FM_PRINT(B)
          CALL FM_FORM('ES20.8',TOL,ST1)
          WRITE (NW,"(A,A)") ' TOL =',TRIM(ST1)
      ENDIF
      CALL CPU_TIME(TIME1)

!             Check for special cases.

      IF (A == B) THEN
          A1 = F(A,N)
          IF (R_LEVEL <= 1) THEN
              NUM_F = 1
          ELSE
              NUM_F = NUM_F + 1
          ENDIF
          IF (A1%MFM%MP(2) == MUNKNO) THEN
              CALL FM_F_FAIL(A,N,NW)
              PRIOR_HS = A1
              GO TO 120
          ELSE
              PRIOR_HS = 0
              GO TO 120
          ENDIF
      ENDIF

!             Check to make sure the user has set precision high enough for the value of TOL chosen.

      TOL1 = TOL
      IF (TOL < ABS(EPSILON(A))) THEN
          WRITE (NW,"(A)") ' '
          WRITE (NW,"(A)") ' Error in FM_INTEGRATE.  TOL is '
          CALL FM_PRINT(TOL)
          WRITE (NW,"(A)") ' This is too small for the current precision level.  Current epsilon ='
          TOL1 = ABS(EPSILON(A))
          CALL FM_PRINT(TOL1)
          WRITE (NW,"(A)") ' This larger value will be used.  TOL ='
          CALL FM_PRINT(TOL1)
          WRITE (NW,"(A)") ' Use FM_SET to set a higher precision before the call to'
          WRITE (NW,"(A)") ' FM_INTEGRATE if the smaller TOL is needed.'
      ENDIF

!             Raise the precision.
!             Check for an integrable singularity at either endpoint, and increase precision
!             if it seems that a retry would be needed at the first precision.

      NDSAVE = NDIG
      A1 = LOG10(F(A+(B-A)*TO_FM(' 1.0E-10 '),N)/F(A+(B-A)*TO_FM(' 1.0E-20 '),N))/10
      B1 = LOG10(F(B-(B-A)*TO_FM(' 1.0E-10 '),N)/F(B-(B-A)*TO_FM(' 1.0E-20 '),N))/10
      IF (R_LEVEL <= 1) THEN
          NUM_F = 4
      ELSE
          NUM_F = NUM_F + 4
      ENDIF
      IF (A1 < -0.999 .OR. B1 < -0.999) THEN
          PRIOR_HS = TO_FM(' UNKNOWN ')
          WRITE (NW,"(A)") ' '
          WRITE (NW,"(A,I9,A)") ' FM_INTEGRATE failed -- F(X,N) for N = ',N,  &
                                ' seems to have a non-integrable singularity'
          WRITE (NW,"(A)")      '                        at an endpoint.  A,B ='
          CALL FM_PRINT(A)
          CALL FM_PRINT(B)
          WRITE (NW,"(A)") ' Check the limits of integration, function number (N), and' // &
                           ' function definition.'
          WRITE (NW,"(A)") ' '
          GO TO 120
      ENDIF
      NDIG = NDIG+INT(30/ALOGMT)
      CALL FM_EQU_R1(A1,NDSAVE,NDIG)
      CALL FM_EQU_R1(B1,NDSAVE,NDIG)
      IF (A1 < -0.2 .OR. B1 < -0.2) NDIG = 2*NDIG

      CALL CPU_TIME(TIME1)

!             Start here when doing a retry.

  110 NRETRY = NRETRY + 1
      CALL FM_EQU(A,A1,NDSAVE,NDIG)
      CALL FM_EQU(B,B1,NDSAVE,NDIG)
      ABSIGN = 1
      IF (A1 > B1) THEN
          CALL FM_EQU(B,A1,NDSAVE,NDIG)
          CALL FM_EQU(A,B1,NDSAVE,NDIG)
          ABSIGN = -1
      ELSE IF (A1 == B1) THEN
          PRIOR_HS = 0
          GO TO 120
      ENDIF
      CALL FM_EQU(TOL1,TOL2,NDSAVE,NDIG)

      IF (KPRT >= 2) THEN
          WRITE (NW,"(A)") ' '
          WRITE (NW,"(A,I9,A,I5)") ' Begin FM_INTEGRATE.  NDIG = ',NDIG,'   Recursion level = ',  &
                                   R_LEVEL
      ENDIF

      S = 0
      PRIOR_HS = 0
      ERR2 = 1
      EPS = EPSILON(TOL2)
      D = ABS(B1-A1)/100
      FMAX = 0
      FMAX2 = 0
      XMAX = A1
      H = 1
      LAST_H = H/2**M
      CALL FM_PI(PI)

      HF = (B1-A1)/2
      AB2 = A1 + HF
      DO K = 1, M
         H = H/2
         ISTEP = 2**(M-K)
         IF (K > 1) THEN
             T = ISTEP*LAST_H
             CALL FM_CHSH(T,C1,S1)
             T = 2*S1*C1
             C2 = C1**2 + S1**2
             S2 = T
         ENDIF
         DO I = 0, NT, ISTEP
            IF (MOD(I,2*ISTEP) /= 0 .OR. K == 1) THEN

!                The + or -X values are the abscissas for interval (-1,1).
!                XF translates these to the interval (A,B).

                IF (I == 0) THEN
                    X = 0
                    W = PI/2
                    XF = HF*X + AB2
                    T = F(XF,N)
                    NUM_F = NUM_F + 1
                    IF (T%MFM%MP(2) == MUNKNO) THEN
                        CALL FM_F_FAIL(XF,N,NW)
                        PRIOR_HS = T
                        GO TO 120
                    ENDIF
                    FMAX2 = MAX(FMAX2,ABS(T))
                    IF (ABS(T) > FMAX .AND. XF > A1+D .AND. XF < B1-D) THEN
                        FMAX = ABS(T)
                        XMAX = XF
                    ENDIF
                    S = S + W*HF*T
                ELSE
                    IF (K == 1) THEN
                        T = I*LAST_H
                        CALL FM_CHSH(T,CI,SI)
                    ELSE

!                       Use the hyperbolic addition formulas to get the next cosh and sinh
!                       quickly when evaluated at I*LAST_H.

                        IF (I == ISTEP) THEN
                            CI = C1
                            SI = S1
                        ELSE
                            T  = SI*C2 + CI*S2
                            CI = CI*C2 + SI*S2
                            SI = T
                            C1 = CI
                            S1 = SI
                        ENDIF
                    ENDIF
                    ST_IS_SAVED = .FALSE.
                    IF (ALLOCATED(ST_SAVE(I)%MFM%MP)) THEN
                        IF (PRECISION_OF(I) >= NDIG) ST_IS_SAVED = .TRUE.
                    ENDIF
                    IF (ST_IS_SAVED) THEN
                        ST = ST_SAVE(I)
                        CT = SQRT(1+ST**2)
                    ELSE
                        T = PI*SI/2
                        CALL FM_CHSH(T,CT,ST)
                        ST_SAVE(I) = ST
                        PRECISION_OF(I) = NDIG
                    ENDIF
                    W = (PI/2)*CI/CT**2
                    IF (W < EPS) EXIT
                    X = ST/CT
                    XF = HF*(-X) + AB2
                    IF (XF > A1) THEN
                        T = F(XF,N)
                        NUM_F = NUM_F + 1
                        IF (T%MFM%MP(2) == MUNKNO) THEN
                            CALL FM_F_FAIL(XF,N,NW)
                            PRIOR_HS = T
                            GO TO 120
                        ENDIF
                        FMAX2 = MAX(FMAX2,ABS(T))
                        IF (ABS(T) > FMAX .AND. XF > A1+D .AND. XF < B1-D) THEN
                            FMAX = ABS(T)
                            XMAX = XF
                        ENDIF
                        S = S + W*HF*T
                    ENDIF
                    XF = HF*(X) + AB2
                    IF (XF < B1) THEN
                        T = F(XF,N)
                        NUM_F = NUM_F + 1
                        IF (T%MFM%MP(2) == MUNKNO) THEN
                            CALL FM_F_FAIL(XF,N,NW)
                            PRIOR_HS = T
                            GO TO 120
                        ENDIF
                        FMAX2 = MAX(FMAX2,ABS(T))
                        IF (ABS(T) > FMAX .AND. XF > A1+D .AND. XF < B1-D) THEN
                            FMAX = ABS(T)
                            XMAX = XF
                        ENDIF
                        S = S + W*HF*T
                    ENDIF
                ENDIF
            ENDIF
         ENDDO
         IF (KPRT >= 2) THEN
             WRITE (NW,"(A)") ' '
             WRITE (NW,"(A,I9,A,I9,A)") ' K = ',K,'   ',NUM_F,  &
                                        ' function calls so far.   Integral approximation ='
             V = H*S
             CALL FM_PRINT(V)
         ENDIF
         IF (K > 1) THEN
             ERR1 = ERR2
             IF (S /= 0) THEN
                 ERR2 = ABS( (PRIOR_HS - H*S)/(H*S) )
             ELSE
                 ERR2 = ABS( (PRIOR_HS - H*S) )
             ENDIF
             IF (KPRT >= 2) THEN
                 CALL FM_FORM('ES15.3',ERR2,ST1)
                 WRITE (NW,"(A,A)") '      relative error of the last two approximations = ',  &
                                    TRIM(ST1)
             ENDIF

!             Check for convergence.

             IF (K > 3 .AND. ERR2 > 0 .AND. ERR2 < TOL2/10.0) EXIT
             IF (K > 5 .AND. ERR2 == 0) EXIT

!             If the errors do not decrease fast enough, raise precision and try again.

             IF (K > 3*NRETRY .AND. ERR1 > 0 .AND. ERR2 > 0) THEN
                 IF (LOG(ERR2)/LOG(ERR1) < 1.2 .AND. ERR1 < 1.0D-6) THEN
                     NDIG = 2*NDIG
                     IF (KPRT >= 2) THEN
                         WRITE (NW,"(A,I9,A,I9)") ' FM_INTEGRATE Retry.  So far, NUM_F = ',NUM_F,  &
                                                  '   New NDIG = ',NDIG
                     ENDIF
                     IF (NRETRY <= 3) GO TO 110
                     NDIG = NDIG/2
                 ENDIF
             ENDIF
         ENDIF
         PRIOR_HS = H*S

!             No convergence in M iterations.
!             Before giving up, look for an interior singularity or tall spike.  If one is found,
!             split (A,B) into two intervals with the interior singularity as an endpoint, and try
!             again as two integrals.

         IF (K == M .OR. (K >= 9 .AND. ERR2 > 1.0D-7 .AND. ABS(TOL2) < 1.0D-16)) THEN
             IF (KPRT >= 2) THEN
                 WRITE (NW,"(A)") ' '
                 WRITE (NW,"(A,I6,A)") ' No convergence in ',M,  &
                              ' iterations.  Look for an interior singularity.'
                 CALL FM_FORM('ES25.6',XMAX,ST1)
                 CALL FM_FORM('ES25.6',FMAX,ST2)
                 WRITE (NW,"(I9,A,A,A)") NUM_F,' function calls so far.   XMAX, FMAX =',  &
                                         TRIM(ST1),TRIM(ST2)
             ENDIF
             CALL FM_SPIKE(F,N,A1,B1,XMAX,FMAX,NUM_F,SPIKE_FOUND,KPRT,NW)
             CALL FMEQU_R1(A1%MFM,NDIG,NDSAVE)
             CALL FMEQU_R1(B1%MFM,NDIG,NDSAVE)
             CALL FMEQU_R1(XMAX%MFM,NDIG,NDSAVE)
             NDS = NDIG
             NDIG = NDSAVE
             IF (SPIKE_FOUND) THEN
                 IF (MIN(ABS(A-XMAX),ABS(B-XMAX)) < 1.01*D) THEN
                     NDIG = 2*NDS
                     IF (NRETRY <= 5) GO TO 110
                     NDIG = NDSAVE
                 ENDIF
                 IF (KPRT >= 2) THEN
                     WRITE (NW,"(A)") ' '
                     WRITE (NW,"(A)") ' Split the integral.  First half:  A,B = '
                     CALL FM_PRINT(A1)
                     CALL FM_PRINT(XMAX)
                 ENDIF
                 CALL FM_INTEGRATE(F,N,A1,XMAX,TOL,C1,KPRT,NW)
                 IF (C1%MFM%MP(2) == MUNKNO) THEN
                     PRIOR_HS = C1
                     GO TO 120
                 ENDIF
                 IF (KPRT >= 2) THEN
                     WRITE (NW,"(A)") ' '
                     WRITE (NW,"(A)") ' Split the integral.  Second half:  A,B = '
                     CALL FM_PRINT(XMAX)
                     CALL FM_PRINT(B1)
                 ENDIF
                 CALL FM_INTEGRATE(F,N,XMAX,B1,TOL,C2,KPRT,NW)
                 PRIOR_HS = C1 + C2
                 GO TO 120
             ENDIF
             CALL FM_INT_FAIL(N,A,B,TOL,M,ERR2,PRIOR_HS,NW)
             PRIOR_HS = TO_FM(' UNKNOWN ')
             GO TO 120
         ENDIF
      ENDDO

      PRIOR_HS = ABSIGN*H*S

!             Round the result and return.

  120 CALL FM_EQU(PRIOR_HS,RESULT,NDIG,NDSAVE)

      NDIG = NDSAVE
      NCALL = NCALL - 1
      CALL CPU_TIME(TIME2)

      IF (KPRT >= 2 .OR. ( R_LEVEL <= 1 .AND. KPRT == 1 ) ) THEN
          WRITE (NW,"(A)") ' '
          WRITE (NW,"(A,I9,A)") ' Return from FM_INTEGRATE.    Function N = ',N,'.    A, B ='
          CALL FM_PRINT(A)
          CALL FM_PRINT(B)
          CALL FM_FORM('ES20.8',TOL,ST1)
          WRITE (NW,"(A,A)") ' TOL =',TRIM(ST1)
          IF (ABS(TIME2-TIME1) > 0.0001 .AND. ABS(TIME2-TIME1) < 1000.0) THEN
              WRITE (NW,"(1X,I9,A,F9.5,A)") NUM_F,' function calls were made in ',TIME2-TIME1,  &
                                            ' seconds.'
              WRITE (NW,"(A)") ' RESULT ='
          ELSE
              WRITE (NW,"(1X,I9,A,ES14.5,A)") NUM_F,' function calls were made in ',TIME2-TIME1,  &
                                              ' seconds.'
              WRITE (NW,"(A)") ' RESULT ='
          ENDIF
          CALL FM_PRINT(RESULT)
      ENDIF
      KW = KWSAVE
      R_LEVEL = R_LEVEL - 1

      END SUBROUTINE FM_INTEGRATE

      SUBROUTINE FM_INT_FAIL(N,A,B,TOL,M,ERR,VAL,NW)
      USE FMZM
      IMPLICIT NONE
      INTEGER :: N,M,NW
      TYPE (FM) :: A,B,TOL,ERR,VAL

      WRITE (NW,*) ' '
      WRITE (NW,*) ' FM_INTEGRATE failed -- no convergence in ',M,' iterations.'
      WRITE (NW,*) ' UNKNOWN has been returned in RESULT.'
      WRITE (NW,*) ' Possible causes:  (1) highly oscillatory integrand'
      WRITE (NW,*) '                   (2) non-convergent integral'
      WRITE (NW,*) '                   (3) integrable singularity in the interior of interval (A,B)'
      WRITE (NW,*) '                   (4) narrow spike in the interior of interval (A,B)'
      WRITE (NW,*) '                   (5) integral too close to zero'
      WRITE (NW,*) ' A possible remedy for the last 3 is to split the integral into two pieces,'
      WRITE (NW,*) ' making two calls to FM_INTEGRATE and then adding the two results.'
      WRITE (NW,*) ' Put singularities or spikes at the endpoints of the intervals of integration.'
      WRITE (NW,*) ' '
      WRITE (NW,*) ' Function N = ',N,'.    A, B ='
      CALL FM_PRINT(A)
      CALL FM_PRINT(B)
      WRITE (NW,*) ' TOL ='
      CALL FM_PRINT(TOL)
      WRITE (NW,*) ' The last integral approximation ='
      CALL FM_PRINT(VAL)
      WRITE (NW,*) ' The estimated relative error in the last integral approximation ='
      CALL FM_PRINT(ERR)
      WRITE (NW,*) ' '

      END SUBROUTINE FM_INT_FAIL

      SUBROUTINE FM_F_FAIL(X,N,NW)
      USE FMZM
      IMPLICIT NONE
      INTEGER :: N,NW
      TYPE (FM) :: X

      WRITE (NW,*) ' '
      WRITE (NW,*) ' FM_INTEGRATE failed -- F(X,N) gave UNKNOWN for N = ',N,'   and X ='
      CALL FM_PRINT(X)
      WRITE (NW,*) ' Check the limits of integration, function number (N), and' // &
                   ' function definition.'
      WRITE (NW,*) ' Be careful of rounding at an irrational endpoint producing an' // &
                   ' illegal function argument.'
      WRITE (NW,*) ' Example:  Integrate  log( cos( t ) ) from 0 to pi/2'
      WRITE (NW,*) ' At some precisions the computed value of pi/2 rounds up, giving' // &
                   ' a small negative cos(t),'
      WRITE (NW,*) ' which then causes the log function to return UNKNOWN.'
      WRITE (NW,*) ' Possible fixes:'
      WRITE (NW,*) '     Change variables to get an integral from 0 to 1'
      WRITE (NW,*) '     Change the function to log( abs( cos( t ) ) )'
      WRITE (NW,*) '     Compute pi/2 with rounding toward -infinity'
      WRITE (NW,*) ' '

      END SUBROUTINE FM_F_FAIL

      SUBROUTINE FM_SPIKE(F,N,A,B,XMAX,FMAX,NUM_F,SPIKE_FOUND,KPRT,NW)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  look for an interior singularity or tall spike in F(X,N).
!  After getting no convergence in M iterations in FM_INTEGRATE, FMAX = ABS(F(XMAX,N)) was
!  the largest magnitude found for F on the interval (A+D,B-D), where D = ABS(B-A)/100.

      TYPE (FM) :: A,B,XMAX,FMAX
      TYPE (FM), EXTERNAL :: F
      INTEGER :: N,NUM_F,KPRT,NW
      LOGICAL :: SPIKE_FOUND
      TYPE (FM), SAVE :: AB,AVERAGE,D,DX,FPMAX,EPS,F1,F2,H,T,X1,XPMAX
      INTEGER :: J

      SPIKE_FOUND = .FALSE.
      IF (KPRT >= 2) THEN
          WRITE (NW,"(A)") ' Enter FM_SPIKE'
      ENDIF

      DX = ABS(B-A)/100
      H = MIN(XMAX-A,B-XMAX)/2

      AVERAGE = 0
      DO J = 1, 99
         X1 = A + (J*(B-A))/100
         F1 = F(X1,N)
         NUM_F = NUM_F + 1
         IF (F1%MFM%MP(2) == MUNKNO) THEN
             XMAX = X1
             FMAX = ABS(F1)
             SPIKE_FOUND = .TRUE.
             RETURN
         ELSE
             AVERAGE = AVERAGE + ABS(F1)
         ENDIF
      ENDDO
      AVERAGE = AVERAGE/99

!             Search for a singularity in F.

      DO
         X1 = XMAX + H
         IF (X1 > A + DX .AND. X1 < B - DX) THEN
             F1 = F(X1,N)
             NUM_F = NUM_F + 1
             IF (F1%MFM%MP(2) == MUNKNO) THEN
                 XMAX = X1
                 FMAX = ABS(F1)
                 SPIKE_FOUND = .TRUE.
                 RETURN
             ENDIF
             IF (ABS(F1) > FMAX) THEN
                 XMAX = X1
                 FMAX = ABS(F1)
                 H = -(H*14)/10
             ENDIF
         ENDIF
         H = -(H*10)/14
         AB = MAX(ABS(A),ABS(B))
         CALL FM_ULP(AB,EPS)
         IF (ABS(H) < ABS(EPS)) THEN
             IF (FMAX > 50*AVERAGE) THEN
                 SPIKE_FOUND = .TRUE.
                 RETURN
             ELSE
                 EXIT
             ENDIF
         ENDIF
      ENDDO

!             Search for a singularity in F'.

      FPMAX = -1
      AVERAGE = 0
      AB = MAX(ABS(A),ABS(B))
      CALL FM_ULP(AB,EPS)
      EPS = SQRT(EPS)
      DO J = 1, 99
         X1 = A + (J*(B-A))/100
         IF (X1-EPS <= A) CYCLE
         F1 = F(X1-EPS,N)
         IF (X1+EPS >= B) CYCLE
         F2 = F(X1+EPS,N)
         NUM_F = NUM_F + 2
         IF (F1%MFM%MP(2) == MUNKNO .OR. F2%MFM%MP(2) == MUNKNO) THEN
             IF (F1%MFM%MP(2) == MUNKNO) THEN
                 XMAX = X1 - EPS
                 FMAX = ABS(F1)
             ELSE
                 XMAX = X1 + EPS
                 FMAX = ABS(F2)
             ENDIF
             SPIKE_FOUND = .TRUE.
             RETURN
         ELSE
             D = ABS((F2-F1)/(2*EPS))
             AVERAGE = AVERAGE + D
             IF (D > FPMAX) THEN
                 XPMAX = X1
                 FPMAX = D
             ENDIF
         ENDIF
      ENDDO
      AVERAGE = AVERAGE/99

      H = MIN(XPMAX-A,B-XPMAX)/2
      DO
         X1 = XPMAX + H
         IF (X1 > A + DX .AND. X1 < B - DX) THEN
             F1 = F(X1-EPS,N)
             NUM_F = NUM_F + 1
             IF (F1%MFM%MP(2) == MUNKNO) THEN
                 XPMAX = X1
                 FPMAX = ABS(F1)
                 SPIKE_FOUND = .TRUE.
                 RETURN
             ENDIF
             F2 = F(X1+EPS,N)
             NUM_F = NUM_F + 1
             IF (F2%MFM%MP(2) == MUNKNO) THEN
                 XPMAX = X1+EPS
                 FPMAX = ABS(F2)
                 SPIKE_FOUND = .TRUE.
                 RETURN
             ENDIF
             D = ABS((F2-F1)/(2*EPS))
             IF (D > FPMAX) THEN
                 XPMAX = X1
                 FPMAX = D
                 H = -(H*14)/10
             ENDIF
         ENDIF
         H = -(H*10)/14
         AB = MAX(ABS(A),ABS(B))
         CALL FM_ULP(AB,T)
         IF (ABS(H) < ABS(T)) THEN
             IF (FPMAX > 50*AVERAGE) THEN
                 SPIKE_FOUND = .TRUE.
                 XMAX = XPMAX
                 FMAX = FPMAX
                 RETURN
             ELSE
                 EXIT
             ENDIF
         ENDIF
      ENDDO

      END SUBROUTINE FM_SPIKE

      SUBROUTINE FM_INVERSE(A,N,B,DET)
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
          WRITE (*,"(/' Error in FM_INVERSE.  Unable to allocate arrays with N = ',I8/)") N
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

      CALL FM_FACTOR_LU(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in FM_INVERSE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          B = TO_FM(' UNKNOWN ')
          GO TO 120
      ENDIF

!             Solve for the inverse matrix one column at a time.

      DO K = 1, N
         B1 = 0
         B1(K) = 1
         CALL FM_SOLVE_LU(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

         R1 = MATMUL(A2,X1) - B1

         CALL FM_SOLVE_LU(A1,N,R1,B1,KSWAP)
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
      END SUBROUTINE FM_INVERSE

      SUBROUTINE FM_LIN_SOLVE(A,X,B,N,DET)
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
          WRITE (*,"(/' Error in FM_LIN_SOLVE.  Unable to allocate arrays with N = ',I8/)") N
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

      CALL FM_FACTOR_LU(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in FM_LIN_SOLVE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          X1 = TO_FM(' UNKNOWN ')
          GO TO 120
      ENDIF
      CALL FM_SOLVE_LU(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

      R1 = MATMUL(A2,X1) - B1

      CALL FM_SOLVE_LU(A1,N,R1,B1,KSWAP)
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

      END SUBROUTINE FM_LIN_SOLVE

      SUBROUTINE FM_FACTOR_LU(A,N,DET,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination to factor the NxN matrix A (LU decomposition).

!  The time is proportional to  N**3.

!  Once this factorization has been done, a linear system  A x = b
!  with the same coefficient matrix A and Nx1 vector b can be solved
!  for x using routine FM_SOLVE_LU in time proportional to  N**2.

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

!             Save the multiplier for use later by FM_SOLVE_LU.

            A(JROW,JDIAG) = AMULT
            DO JCOL = JDIAG+1, N
               CALL FMMPY_SUB(A(JROW,JCOL)%MFM,AMULT%MFM,A(JDIAG,JCOL)%MFM)
            ENDDO
         ENDDO
      ENDDO
      DET = DET * A(N,N)

      END SUBROUTINE FM_FACTOR_LU

      SUBROUTINE FM_SOLVE_LU(A,N,B,X,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Solve a linear system  A x = b.
!  A is the NxN coefficient matrix, after having been factored by FM_FACTOR_LU.
!  B is the Nx1 right-hand-side vector.
!  X is returned with the solution of the linear system.
!  KSWAP is a list of row interchanges made by the partial pivoting strategy during the
!        elimination phase in FM_FACTOR_LU.
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

      END SUBROUTINE FM_SOLVE_LU

      SUBROUTINE FMMPY_SUB(MA,MB,MC)
      USE FMVALS
      IMPLICIT NONE

!  Fused multiply-subtract operation.  Return  MA = MA - MB*MC
!  This is an internal FM routine used by FM_FACTOR_LU.  It doesn't always return correctly
!  rounded results, since precision will have already been raised by FM_LIN_SOLVE before
!  calling FM_FACTOR_LU.

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
      END SUBROUTINE FMMPY_SUB

      SUBROUTINE FM_RK14( A, B, N_ORDER, FM_RK14_F, N_FUNCTION, S, TOL, S1 )

!  Solve the vector first-order differential equation s' = f(x,s).

!  This routine uses 14th order Runge-Kutta with adjustable step size, starting at x = A
!  with state vector S (initial conditions), and returns state vector S1 as the solution
!  at x = B.

!  N_ORDER is the order of the differential equation (length of vectors S and S1).

!  N_FUNCTION is the function number (used in FM_RK14_F) identifying which function f(x,s)
!             defines the right-hand-side of the differential equation to be solved.

!  TOL is the absolute error tolerance.  Because the coefficients are defined with no more than
!      about 85-digit precision, TOL should not be less than 1.0e-75.

!  The FM precision level should be set to at least 10 digits more than TOL.
!  For example, set precision with CALL FM_SET(30) if FM_RK14 will be called with TOL = 1.0e-20.

!  The error estimate that is used to control step size will fail if f(x,s) is a function of
!  x only, not depending on s.  In this case, the differential equation is really just an
!  integration problem and should be done as a numerical integration.

      USE FMZM
      IMPLICIT NONE

      INTEGER, PARAMETER :: MAXIMUM_ORDER = 3
      INTEGER :: J, N_ORDER, N_FUNCTION
      TYPE (FM) :: A, B, TOL, S(MAXIMUM_ORDER), S1(MAXIMUM_ORDER)
      TYPE (FM), SAVE :: ERROR_EST, H, TOL2, X
      EXTERNAL :: FM_RK14_F
      LOGICAL, SAVE :: LAST_STEP


      IF (TOL < TO_FM(' 1.0e-75 ')) THEN
          WRITE (*,*) ' '
          WRITE (*,*) ' Error in input to FM_RK14.  TOL should not be less than 1.0e-75.  It was'
          CALL FM_PRINT(TOL)
          WRITE (*,*) ' '
          STOP
      ENDIF

!             Pick an initial step size.

      TOL2 = ABS(TOL) / 1000
      H = ABS(TOL2)**(1.0D0/14)

      X = A
      LAST_STEP = .FALSE.

      DO J = 1, 10**7
         IF (X+H >= B) THEN
             H = B - X
             LAST_STEP = .TRUE.
         ENDIF
         CALL FM_RK14_STEP( N_ORDER, FM_RK14_F, N_FUNCTION, X, S, H, ERROR_EST, S1 )

!             If the error is too big, try again with halved step size.

         IF (ERROR_EST > TOL2) THEN
             H = H / 2
             CYCLE
         ENDIF

!             Make the step.

         IF (LAST_STEP) THEN
             EXIT
         ENDIF
         S(1:N_ORDER) = S1(1:N_ORDER)
         X = X + H

!             If the error is much smaller than TOL2, try doubling the step size.
!             Otherwise, if the error is less than TOL2/10, try to fine-tune the
!             step size by increasing H slightly.

         IF (ERROR_EST < TOL2/1.0D+5) THEN
             H = 2*H
         ELSE IF (ERROR_EST < TOL2/10) THEN
             H = 1.05*H
         ENDIF
      ENDDO

      END SUBROUTINE FM_RK14

      SUBROUTINE FM_RK14_STEP( N_ORDER, FM_RK14_F, N_FUNCTION, X, S, H, ERROR_EST, S1 )

!  Do one step of 14th order Runge-Kutta, starting with state vector S(X) and returning
!  state vector S1(X+H) after a step of H.
!  N_ORDER is the order of the differential equation (length of vector S).
!  N_FUNCTION is the function number (used in FM_RK14_F) identifying which function is to be solved.

      USE FMVALS
      USE FMZM
      IMPLICIT NONE

      INTEGER, PARAMETER :: MAXIMUM_ORDER = 3
      INTEGER :: J, K, N_STAGES, N_ORDER, N_FUNCTION
      TYPE (FM) :: H, ERROR_EST, S(MAXIMUM_ORDER), S1(MAXIMUM_ORDER), X
      TYPE (FM), SAVE :: A(0:34), B(0:34,0:34), C(0:34), FI_J(MAXIMUM_ORDER,0:34),  &
                         FI(MAXIMUM_ORDER), YK(0:34,MAXIMUM_ORDER), Y(MAXIMUM_ORDER)
      EXTERNAL :: FM_RK14_F
      INTEGER, SAVE :: COEFF_BASE = 0, COEFF_PRECISION = 0

      N_STAGES = 35

      IF (COEFF_BASE /= MBASE .OR. COEFF_PRECISION < NDIG) THEN
          COEFF_BASE = MBASE
          COEFF_PRECISION = NDIG
          CALL FM_RK14_COEFFS(A, B, C)
      ENDIF

!             Do the step.

      YK(0,1:N_ORDER) = S(1:N_ORDER)
      DO K = 1, N_STAGES - 1
         YK(K,1:N_ORDER) = 0
         DO J = 0, K-1
            IF (J == K-1) THEN
                Y(1:N_ORDER) = YK(J,1:N_ORDER)
                CALL FM_RK14_F(N_ORDER, N_FUNCTION, X+A(J)*H, Y, FI)
                FI_J(1:N_ORDER,J) = FI(1:N_ORDER)
            ENDIF
            IF (B(K,J) /= 0) THEN
                YK(K,1:N_ORDER) = YK(K,1:N_ORDER) + B(K,J) * FI_J(1:N_ORDER,J)
            ENDIF
         ENDDO
         YK(K,1:N_ORDER) = YK(0,1:N_ORDER) + H * YK(K,1:N_ORDER)
      ENDDO

      S1(1:N_ORDER) = 0
      DO K = 0, N_STAGES - 1
         IF (K == N_STAGES-1) THEN
             Y(1:N_ORDER) = YK(K,1:N_ORDER)
             CALL FM_RK14_F(N_ORDER, N_FUNCTION, X+A(K)*H, Y, FI)
             S1(1:N_ORDER) = S1(1:N_ORDER) + C(K) * FI(1:N_ORDER)
         ELSE
             S1(1:N_ORDER) = S1(1:N_ORDER) + C(K) * FI_J(1:N_ORDER,K)
         ENDIF
      ENDDO

      S1(1:N_ORDER) = S(1:N_ORDER) + H * S1(1:N_ORDER)

      ERROR_EST = H * ( NORM2( FI_J(1:N_ORDER,33) - FI_J(1:N_ORDER,1) ) ) / 1000

      END SUBROUTINE FM_RK14_STEP

      SUBROUTINE FM_RK14_COEFFS(A, B, C)

!  Define the coefficients used in the RK14 formula (85 digits).

!  These came from:
!  http://www.peterstone.name/Maplepgs/Maple/nmthds/RKcoeff/Runge_Kutta_schemes/RK14/RKcoeff14a_1.pdf

      USE FMZM
      IMPLICIT NONE

      TYPE (FM) :: A(0:34), B(0:34,0:34), C(0:34)

!             Many of the coefficients are zero.  Initialize A, B, and C here, then
!             skip those definitions below.

      A = 0
      B = 0
      C = 0

      A( 1) = TO_FM('   1 ') /   9
      A( 2) = TO_FM('   5 ') /   9
      A( 3) = TO_FM('   5 ') /   6
      A( 4) = TO_FM('   1 ') /   3
      A( 5) = TO_FM('   1 ')
      A( 6) = TO_FM(' .6699869792727729217646837855059985139388452296384603532851421391683474428303956826239 ')
      A( 7) = TO_FM(' .2970683842138183573895847168082194132233320946989156873791682903324708698499266217383 ')
      A( 8) = TO_FM('   8 ') /  11
      A( 9) = TO_FM(' .1401527990421887652761874879669467176298064630825329362873230163439023340348096838456 ')
      A(10) = TO_FM(' .7007010397701507371510998548307493379414070492655464089692218490447945746638665522966 ')
      A(11) = TO_FM('   4 ') /  11
      A(12) = TO_FM('   5 ') /  19
      A(13) = TO_FM(' .392172246650270859125196642501208648863714315266128052078483e-1 ')
      A(14) = TO_FM(' .8129175029283767629833931592780365061896123726172385507744269795906758195776958783707 ')
      A(15) = TO_FM('   1 ') /   6
      A(16) = TO_FM('   9 ') /  10
      A(17) = TO_FM(' .6412992574519669233127711938966828094810966516150832254029235721305050295351572963693e-1 ')
      A(18) = TO_FM(' .2041499092834288489277446343010234050271495052413337516288702042649259099754335560687 ')
      A(19) = TO_FM(' .3953503910487605656156713698273243723522272974566594505545766538389345381768585023057 ')
      A(20) = TO_FM(' .6046496089512394343843286301726756276477727025433405494454233461610654618231414976943 ')
      A(21) = TO_FM(' .7958500907165711510722553656989765949728504947586662483711297957350740900245664439313 ')
      A(22) = TO_FM(' .9358700742548033076687228806103317190518903348384916774597076427869494970464842703631 ')
      A(23) = TO_FM('   1 ') /   6
      A(24) = TO_FM(' .8129175029283767629833931592780365061896123726172385507744269795906758195776958783707 ')
      A(25) = TO_FM(' .392172246650270859125196642501208648863714315266128052078483e-1 ')
      A(26) = TO_FM('   4 ') /  11
      A(27) = TO_FM(' .7007010397701507371510998548307493379414070492655464089692218490447945746638665522966 ')
      A(28) = TO_FM(' .1401527990421887652761874879669467176298064630825329362873230163439023340348096838456 ')
      A(29) = TO_FM(' .2970683842138183573895847168082194132233320946989156873791682903324708698499266217383 ')
      A(30) = TO_FM(' .6699869792727729217646837855059985139388452296384603532851421391683474428303956826239 ')
      A(31) = TO_FM('   1 ') /   3
      A(32) = TO_FM('   5 ') /   9
      A(33) = TO_FM('   1 ') /   9
      A(34) = TO_FM('   1 ')

      C( 0) = TO_FM('   1 ') /  56
      C( 1) = TO_FM('   3 ') / 512
      C( 2) = TO_FM('   3 ') / 256
      C( 4) = TO_FM('   9 ') / 512
      C( 6) = TO_FM('   3 ') / 128
      C( 7) = TO_FM('  15 ') / 512
      C( 9) = TO_FM('   9 ') / 256
      C(10) = TO_FM('  21 ') / 512
      C(11) = TO_FM('   3 ') /  64
      C(13) = TO_FM('  27 ') / 512
      C(14) = TO_FM('  15 ') / 256
      C(15) = TO_FM('  33 ') / 512
      C(17) = TO_FM(' .1053521135717530196914960328878781622276730830805238840416702908213176249782427570033 ')
      C(18) = TO_FM(' .1705613462417521823821203385538740858875554878027908047375010369442754416180982144816 ')
      C(19) = TO_FM(' .2062293973293519407835264857011048947419142862595424540779715293772640762608018856579 ')
      C(20) = TO_FM(' .2062293973293519407835264857011048947419142862595424540779715293772640762608018856579 ')
      C(21) = TO_FM(' .1705613462417521823821203385538740858875554878027908047375010369442754416180982144816 ')
      C(22) = TO_FM(' .1053521135717530196914960328878781622276730830805238840416702908213176249782427570033 ')
      C(23) = TO_FM(' -33 ') / 512
      C(24) = TO_FM(' -15 ') / 256
      C(25) = TO_FM(' -27 ') / 512
      C(26) = TO_FM('  -3 ') /  64
      C(27) = TO_FM(' -21 ') / 512
      C(28) = TO_FM('  -9 ') / 256
      C(29) = TO_FM(' -15 ') / 512
      C(30) = TO_FM('  -3 ') / 128
      C(31) = TO_FM('  -9 ') / 512
      C(32) = TO_FM('  -3 ') / 256
      C(33) = TO_FM('  -3 ') / 512
      C(34) = TO_FM('   1 ') /  56

      B( 1, 0) = TO_FM('   1 ') /   9
      B( 2, 0) = TO_FM('  -5 ') /   6
      B( 2, 1) = TO_FM('  25 ') /  18
      B( 3, 0) = TO_FM('   5 ') /  24
      B( 3, 2) = TO_FM('   5 ') /   8
      B( 4, 0) = TO_FM('  29 ') / 150
      B( 4, 2) = TO_FM('  11 ') /  50
      B( 4, 3) = TO_FM('  -2 ') /  25
      B( 5, 0) = TO_FM('   1 ') /  10
      B( 5, 3) = TO_FM('   2 ') /   5
      B( 5, 4) = TO_FM('   1 ') /   2
      B( 6, 0) = TO_FM(' .1034845616366797766729935465119103444997447982019713166066629728281981965079290745983 ')
      B( 6, 3) = TO_FM(' .1220688873064072225896440828689620771395927148341621347412746563709055937325311521675 ')
      B( 6, 4) = TO_FM(' .4825744903312466224751347801256881128659190238501680496794015023696413273862321544150 ')
      B( 6, 5) = TO_FM(' -.3814096000156069997308862400056202056641130724784114774219699240039767479629669855696e-1 ')
      B( 7, 0) = TO_FM(' .1243805266540944128815164208687993162684914663596714231632892354628068537117612942798 ')
      B( 7, 4) = TO_FM(' .2261202821975843014222386629792029011967523207426331439651447460281196206643404356021 ')
      B( 7, 5) = TO_FM(' .1378858876180808806076958370164778145309694174914933853635428709475288586061552782365e-1 ')
      B( 7, 6) = TO_FM(' -.6722101339966844497493995074143058569500863415253821828561997825320849038679063596730e-1 ')
      B( 8, 0) = TO_FM(' .9369190656596738155308854560830059338663496952177500856556033862893464429241815101000e-1 ')
      B( 8, 5) = TO_FM(' -.6134068434505109872294989956416647356209145071288588710070986068372475355320835997035e-2 ')
      B( 8, 6) = TO_FM(' .2160198256255030637088600976598665734909794332781173201886676706066128640340557614360 ')
      B( 8, 7) = TO_FM(' .4236950635157619373376190739609767532058674695441235326831157041055522397561196508237 ')
      B( 9, 0) = TO_FM(' .8384798124090526646169687913728140859805331392249111310693346670107922625197375034871e-1 ')
      B( 9, 5) = TO_FM(' -.1179493671009738143197550560312957753679619605907361507776128268875265788248790903515e-1 ')
      B( 9, 6) = TO_FM(' -.2472990205688126523394738387431945983259928403533401326974984247503501083158412965835 ')
      B( 9, 7) = TO_FM(' .9780808583677290122593130140812916655037406554767339407565991037499621093437371932341e-1 ')
      B( 9, 8) = TO_FM(' .2175906892434206313600086517678603183441681200247821768799893467069296630467914197921 ')
      B(10, 0) = TO_FM(' .6152553597694282279545623896143147143334239690648211074539397569215087099333654844097e-1 ')
      B(10, 5) = TO_FM(' .5922327803245033080429900057980465247383895604442571368349896773084347972825775455007e-2 ')
      B(10, 6) = TO_FM(' .4703261599638411122172243032058941134553625307461088250108483236601604516650193568134 ')
      B(10, 7) = TO_FM(' .2996888638486790008539818370961923991368311216717812791841936858888827504094204242461 ')
      B(10, 8) = TO_FM(' -.2476568775939949146899922763298108258539580692639470955481886317480090967647905771626 ')
      B(10, 9) = TO_FM(' .1108950297714376828939998518390617145224451736006787182086245987785252503880550245038 ')
      B(11, 0) = TO_FM(' .4197000733627825798617928647872777872134836565431046112459945389674655429048057710370e-1 ')
      B(11, 5) = TO_FM(' -.317987696266205093901912847692712407988609169703103952205634e-2 ')
      B(11, 6) = TO_FM(' .8063977149061920772608217115203795063935431115674197501197468839656405367779525213500 ')
      B(11, 7) = TO_FM(' .9759831264123889790935228506842888513146720480030545503571875185550549213299958241991e-1 ')
      B(11, 8) = TO_FM(' .7785755781583989090275124464529272389997634605941819649588520345133050850477185489203 ')
      B(11, 9) = TO_FM(' .2048904238315994281894992020981056033120292350814206535748293420400885242747823516625 ')
      B(11,10) = TO_FM(' -1.562615796274681883070709439505278252114628922364243608928053762634922556160297217820 ')
      B(12, 0) = TO_FM(' .4377267822337301635744652424953398116882149670716141232569729223172939742940416733395e-1 ')
      B(12, 8) = TO_FM(' .6243650275201952087943586285809336252816312169030959172012504609444028241438248581173e-2 ')
      B(12, 9) = TO_FM(' .2000430971095773149944351654696478568290662322182649696087680691197048872391143823078 ')
      B(12,10) = TO_FM(' -.8053283678049830368238571620489029119233928873370293148442058928084075077460302544840e-2 ')
      B(12,11) = TO_FM(' .2115175280673965219157119035233996013168778251575505730512208770404786743066139905871e-1 ')
      B(13, 0) = TO_FM(' .2834992503635145630950235919207173122471376548964770977684956012393009143065795513785e-1 ')
      B(13, 8) = TO_FM(' .2491632048558174075389491488059951494598846535854176800982219995075912885766744587193e-2 ')
      B(13, 9) = TO_FM(' .2301387878545931496383998463737427687720871226381422342236583655735620108657836993957e-1 ')
      B(13,10) = TO_FM(' -.3221559566929770987244760924671208781894636047606204610433085107190031098987004938258e-2 ')
      B(13,11) = TO_FM(' .9884425494476646689463354144878852560408199827860146481292993078049373245839618405001e-2 ')
      B(13,12) = TO_FM(' -.2130107713288873513843076428759273848866345654295724666320922464722154754985568313136e-1 ')
      B(14, 0) = TO_FM(' .3435118942902430010494322347351479430833531749807014262686507474123120416010457867571 ')
      B(14, 8) = TO_FM(' .2104519120236273856090970119990106557888074052256267000419050051487632641518018732685 ')
      B(14, 9) = TO_FM(' 1.034274520572304119364829268288257099386679996983247401666929134177931632176349026735 ')
      B(14,10) = TO_FM(' .6003036458644224870512404482066405749390780924061569454673075686417142117164254262878e-2 ')
      B(14,11) = TO_FM(' .8559381250996195375780121060024077289150626526164160058172684354881277648341960563008 ')
      B(14,12) = TO_FM(' -.9772350050367668108722648523725256330131076568928396776974412446349105799705851506077 ')
      B(14,13) = TO_FM(' -.6600269804792946946162250138563276937205739812199748747775581736879654453322759683463 ')
      B(15, 0) = TO_FM(' -.1435740016721680695382063999350763666577559543783998809757153672896315044426183882232e-1 ')
      B(15, 8) = TO_FM(' -.3662532700490399702936857968489747917331190817335522078657913621382824038988807796287e-1 ')
      B(15, 9) = TO_FM(' .3502549756362136819768494069798465243467890824711035742020654749717518291597210559354e-1 ')
      B(15,10) = TO_FM(' .3609460163621135089317866587583352398236899298642376718895880083960486970547825683491e-1 ')
      B(15,11) = TO_FM(' -.2652199675536811063515959468346019236496270124574642848667252606942739787160130682669e-1 ')
      B(15,12) = TO_FM(' .4456990113056981196389115375088399081043363230822267716707629092315111479614958673268e-1 ')
      B(15,13) = TO_FM(' .1243430933313582432862255957417864480389734088951067419167759990001419776217292554191 ')
      B(15,14) = TO_FM(' .4138296932394806944035124962043359604261929086744760344472227418812310333088685698274e-2 ')
      B(16, 0) = TO_FM(' .3560324044251202909756091163980891762641062223797488026536968101501275805051289760823 ')
      B(16, 8) = TO_FM(' -.450192758947562595966821779075956175110645100214763601190349 ')
      B(16, 9) = TO_FM(' .430527907083710898626656292808782917793030154094709462877146 ')
      B(16,10) = TO_FM(' .5119730290110222376685569603940716920771257870306513863906805244405042813755411512463 ')
      B(16,11) = TO_FM(' .9083036388864042603901591246381102139974962148199046305445452541870528153933236088278 ')
      B(16,12) = TO_FM(' -1.239210933719339317573724691515340288544138892486057261860887966510000755220957594942 ')
      B(16,13) = TO_FM(' -.6490486616717614651416723488790625539054028319671910976544025456235491510559878435372 ')
      B(16,14) = TO_FM(' .2517089045868192922104805299489705414048878529314474912189256354259853776829630937658 ')
      B(16,15) = TO_FM(' .7799064703455863988107567952823344760235405934115501870206452879298798513199886085571 ')
      B(17, 0) = TO_FM(' .1309356874065130664068812064188349801274704382131924878449566575565302965696195341197e-1 ')
      B(17,12) = TO_FM(' -.9320530679851139459084619627671082378586315096846671421247697017556505173897578610165e-4 ')
      B(17,13) = TO_FM(' .5053743342622993596400904431385907267709423447161223817027456630856526555478831396014e-1 ')
      B(17,14) = TO_FM(' .8044703419444879791095791096101977976413118689308653610493721999399129417586629251430e-6 ')
      B(17,15) = TO_FM(' .5917260294941711905287557427777172598443409719243215281782302034071342229921661278343e-3 ')
      B(17,16) = TO_FM(' -.4016147221545573370646916849063755877322642479500938046774565993013424294867398455789e-6 ')
      B(18, 0) = TO_FM(' .2079264844660530125419445440007656521672552061443734079797586969853055549175505457737e-1 ')
      B(18,12) = TO_FM(' .5826959188000859151019026978372841089514061030298715701031065480360641416298102920851e-3 ')
      B(18,13) = TO_FM(' -.8017007323588159390833421865258527466405584659196335246554992680506588169863285718822e-2 ')
      B(18,14) = TO_FM(' .4038476438471369403751708217435605704841172903308955066191655368223862388605213690921e-5 ')
      B(18,15) = TO_FM(' .8546099980555061442250561145675356025101146220336224918025961310211940592009621595606e-1 ')
      B(18,16) = TO_FM(' -.2044864809358042427067075696910043079044428375526774562331430989116458814609927891477e-5 ')
      B(18,17) = TO_FM(' .1053285788244318933997994029790939973542409042351728431465827473723673651882417656762 ')
      B(19, 0) = TO_FM(' 1.401534497957360214154462473557713067184864529175977331289881318884096354294079099114 ')
      B(19,12) = TO_FM(' -.2302520009842212616162724103674156212611302982744556219175010157057031125814669239016 ')
      B(19,13) = TO_FM(' -7.211068404669129056595822371068742471658564935099615697324849532576890894506619405031 ')
      B(19,14) = TO_FM(' .3729015606948363352369953278521323402177595666786623882373057096229137360164435411243e-2 ')
      B(19,15) = TO_FM(' -4.714154957271250206787781793922247570113233732218200980194845522013711035054762664884 ')
      B(19,16) = TO_FM(' -.1763676575453492420538419950327976735749038866956001340593194717236122233799126229446e-2 ')
      B(19,17) = TO_FM(' 7.641305480386987655630293108802376511851733678139370059818519661401442202665741111270 ')
      B(19,18) = TO_FM(' 3.506020436597518349898960829497447109682129498933753736341591881470708008233521976557 ')
      B(20, 0) = TO_FM(' 11.95146506941206867993723858307164016744736108265535168242754934626543968357331742096 ')
      B(20,12) = TO_FM(' 7.794809321081759687835167002317643882202842795989809549197917776161588225206322580459 ')
      B(20,13) = TO_FM(' -56.45013938673257925235609911209042814404681000613405538635967763011214022629172907669 ')
      B(20,14) = TO_FM(' .9123763069306449013445304492902766457096074504036737047499704936582270274950128398912e-1 ')
      B(20,15) = TO_FM(' -12.73362799254348862019455243091992750381627175299189605168457824373779389828110581300 ')
      B(20,16) = TO_FM(' -.3968959219047197123135428109397366747123830704331478729319411886202118671113516172493e-1 ')
      B(20,17) = TO_FM(' 54.43921418835708869962257651553077918614383784233053341001985423053366890118247056463 ')
      B(20,18) = TO_FM(' -3.644116379215692368464069903613506458067214784092667356589342345057374050114156075061 ')
      B(20,19) = TO_FM(' -.8045032499105099108990307879585794993156949132107878807481027183961246894903442258757 ')
      B(21, 0) = TO_FM(' -148.8094265071004884278388682686476255619306120821485965777899951377767737092911763254 ')
      B(21,12) = TO_FM(' -91.72952782912564843579356624023216234952287290363542836291360346578688265538801398361 ')
      B(21,13) = TO_FM(' 707.6561449715983598345757192863357161548211289666495623584804744987957677893379157809 ')
      B(21,14) = TO_FM(' -1.10563611857482440905296961311590930801338308942637769555540 ')
      B(21,15) = TO_FM(' 176.1345918838113725878598980760556604069995167623016865882869129962911416096097878945 ')
      B(21,16) = TO_FM(' .4913848242148806622688983451644545574168846314027647925019604519368994965045299923826 ')
      B(21,17) = TO_FM(' -684.2780004498149443582375356108950819560771678936002751371799726829821841834791232605 ')
      B(21,18) = TO_FM(' 27.99106049983982589842243321243804074460025184006686868209688958109916979926727384229 ')
      B(21,19) = TO_FM(' 13.19397100302823334436709643711532384350641596237449753683872220663989495376087330358 ')
      B(21,20) = TO_FM(' 1.251287812839804454501149741480560063172688300773964063605141347518040989702499199856 ')
      B(22, 0) = TO_FM(' -9.673079469481967636441261184332193958399514085718772596349277868068021458303626779169 ')
      B(22,12) = TO_FM(' -4.469901508585055314438462277019603604978306814087514357488023393670679083633020106516 ')
      B(22,13) = TO_FM(' 45.51271286909526819682419504000527511789059078173984816890412459840121969200961260987 ')
      B(22,14) = TO_FM(' -.713085086183826912791492024438246129930559805352394367050813e-1 ')
      B(22,15) = TO_FM(' 11.22736140684127415825906244799393842078268007767944830815221105133516977144595052189 ')
      B(22,16) = TO_FM(' .1262443767176227245162379129091388093617868898191054263714925416869147773104813482457 ')
      B(22,17) = TO_FM(' -43.54393395494833136058106249072421076238143044676214056937881652359375369765457150165 ')
      B(22,18) = TO_FM(' .7871743075430589783987929949965509020645460914432340378113766124779028133099797867162 ')
      B(22,19) = TO_FM(' .5322646967446842156693007086038866907853957768215038536520118921656033723449302296244 ')
      B(22,20) = TO_FM(' .4224227339963253260102251274713887725750865388096033468497941673910509540050957057177 ')
      B(22,21) = TO_FM(' .8591312495030671073084380314998594434411150562941549563989586466154235621165245563192e-1 ')
      B(23, 0) = TO_FM(' -10.06640324470547024033966069004268914722028247579687652710623604380152449409080444899 ')
      B(23, 8) = TO_FM(' -.3662532700490399702936857968489747917331190817335522078657913621382824038988807796287e-1 ')
      B(23, 9) = TO_FM(' .3502549756362136819768494069798465243467890824711035742020654749717518291597210559354e-1 ')
      B(23,10) = TO_FM(' .3609460163621135089317866587583352398236899298642376718895880083960486970547825683491e-1 ')
      B(23,11) = TO_FM(' -.2652199675536811063515959468346019236496270124574642848667252606942739787160130682669e-1 ')
      B(23,12) = TO_FM(' -6.270889721814641435905531494788716038393561229573960230194057818533161624674313994502 ')
      B(23,13) = TO_FM(' 48.20792374425629890907021030081950639234925931416361161278899187780407980462426656808 ')
      B(23,14) = TO_FM(' -.694471689136165640882395180583732834557754169149088630301342e-1 ')
      B(23,15) = TO_FM(' 12.68106902048502956983413709136098070661084838114121251454273060707937017246509534894 ')
      B(23,16) = TO_FM(' .119671168968323754838161435501011294100927813964199613229864e-1 ')
      B(23,17) = TO_FM(' -46.72497649924824080033582682426626955932013216597956070401309263301039263373634230581 ')
      B(23,18) = TO_FM(' 1.330296133266267113147100392982165913990335111912271192356479099067512051132965697343 ')
      B(23,19) = TO_FM(' 1.007667875033982983534389036199266577711627177936617199056121787956529680139072027935 ')
      B(23,20) = TO_FM(' .2095120519336650916641223884754807028927707538644872411247284065032940106679251005781e-1 ')
      B(23,21) = TO_FM(' .2101347063312641773177354243313964074244121884437574908902263894855162847478911411134e-1 ')
      B(23,22) = TO_FM(' .9521960144171217941751015424545759073763602336583562405468424451848266905185171865534e-2 ')
      B(24, 0) = TO_FM(' -409.4780816777437087725890974093703576244243416067520683455326035855162023776088699896 ')
      B(24, 8) = TO_FM(' .2104519120236273856090970119990106557888074052256267000419050051487632641518018732685 ')
      B(24, 9) = TO_FM(' 1.034274520572304119364829268288257099386679996983247401666929134177931632176349026735 ')
      B(24,10) = TO_FM(' .6003036458644224870512404482066405749390780924061569454673075686417142117164254262878e-2 ')
      B(24,11) = TO_FM(' .8559381250996195375780121060024077289150626526164160058172684354881277648341960563008 ')
      B(24,12) = TO_FM(' -250.5169985474478604927776577293161303865840504207820779326393997812026874735614210230 ')
      B(24,13) = TO_FM(' 1946.424666523884277660537503282647585958298508957614274560610260899186136259514015246 ')
      B(24,14) = TO_FM(' -3.045038821023103655061058090868608827869505440976021016842196622317831446605499698935 ')
      B(24,15) = TO_FM(' 490.6263795282817135212082652991680838415985422740616633051003594128766152337185220086 ')
      B(24,16) = TO_FM(' 1.566475895312709071154840670135974457395956152459667753199388690841173424714434871921 ')
      B(24,17) = TO_FM(' -1881.974289940111733622172673770358706192159066384530557689275696031792911993357071098 ')
      B(24,18) = TO_FM(' 75.25922247248471752788377136433031498216206189142459440229301807516615379972994062700 ')
      B(24,19) = TO_FM(' 34.57343569803310676224343447365546896967286447935510158001529990937243976348724448442 ')
      B(24,20) = TO_FM(' 3.211476794409689614354173618470737551690229667488916278855754113243135684398993410117 ')
      B(24,21) = TO_FM(' -.4604080417384143913072014042370588488672450952653828208427296561415079214017074427602 ')
      B(24,22) = TO_FM(' -.8707183398418105224318841379579862457242520473889365722145748143125162133630944128398e-1 ')
      B(24,23) = TO_FM(' -7.393518141583030675670169521955210639991857732491329543926346613193825315394087286297 ')
      B(25, 0) = TO_FM(' 3.433474758535508789210934962575967811206238910720084588712755786644583035514752699598 ')
      B(25, 8) = TO_FM(' .2491632048558174075389491488059951494598846535854176800982219995075912885766744587193e-2 ')
      B(25, 9) = TO_FM(' .2301387878545931496383998463737427687720871226381422342236583655735620108657836993957e-1 ')
      B(25,10) = TO_FM(' -.3221559566929770987244760924671208781894636047606204610433085107190031098987004938258e-2 ')
      B(25,11) = TO_FM(' .9884425494476646689463354144878852560408199827860146481292993078049373245839618405001e-2 ')
      B(25,12) = TO_FM(' 2.162527993779225077883078419047573540457592253357327094851479956564246957314476133478 ')
      B(25,13) = TO_FM(' -16.26998645464574213280656406601394890069875520402288517985775075363232756881970486667 ')
      B(25,14) = TO_FM(' -.1285345021205245528435834174709350105380290375426545062302651848844352856037884822181 ')
      B(25,15) = TO_FM(' -8.98915042666504253089307820833379330486511746063552853023189 ')
      B(25,16) = TO_FM(' -.3485953632320253333870802018510136501924017672505137649688730136175086767654181319387e-2 ')
      B(25,17) = TO_FM(' 15.79361941133398075362351873886955741358533870251397376656158275266140525531011608606 ')
      B(25,18) = TO_FM(' -.5744033309140950656281654820173358201483836631956754708231458398423255984252281047127 ')
      B(25,19) = TO_FM(' -.3456020390213932966927224966081249825352372288276553067081833889419898565070467534157 ')
      B(25,20) = TO_FM(' -.6622414902065850917316199913837577811330679927074186873906450413385445874036001388495e-2 ')
      B(25,21) = TO_FM(' -.7777881292422041640325464586073643097593472096267591120155367761150273183248441708392e-2 ')
      B(25,22) = TO_FM(' -.3560841924022749133388272326974373646752408187917065879526063406092336300493607300593e-2 ')
      B(25,23) = TO_FM(' 4.792825064499307996497977496298401894572969341393590555417712618624354747222657791607 ')
      B(25,24) = TO_FM(' .153725464873068577844576387402512082757034273069877432944621 ')
      B(26, 0) = TO_FM(' 32.30385208719854423269947344400315350913649750477846297617061421719281146058139852238 ')
      B(26, 5) = TO_FM(' -.317987696266205093901912847692712407988609169703103952205634e-2 ')
      B(26, 6) = TO_FM(' .8063977149061920772608217115203795063935431115674197501197468839656405367779525213500 ')
      B(26, 7) = TO_FM(' .9759831264123889790935228506842888513146720480030545503571875185550549213299958241991e-1 ')
      B(26, 8) = TO_FM(' .7785755781583989090275124464529272389997634605941819649588520345133050850477185489203 ')
      B(26, 9) = TO_FM(' .2048904238315994281894992020981056033120292350814206535748293420400885242747823516625 ')
      B(26,10) = TO_FM(' -1.562615796274681883070709439505278252114628922364243608928053762634922556160297217820 ')
      B(26,12) = TO_FM(' 16.34298918823105706485042439739271747087533535041545512917666902744198799725970841669 ')
      B(26,13) = TO_FM(' -154.5445552935436212307301896314710363993166836696091165017078152549564923882084122674 ')
      B(26,14) = TO_FM(' 1.569710887033348726920342834176217614662635935824970859658624964687079589089479471888 ')
      B(26,15) = TO_FM(' 3.276855450872481313214298172699007311655224049747336000450385269517693130775985884604 ')
      B(26,16) = TO_FM(' -.5034892451936531763480407271997836265340810956916323972462042700071863164675818955838e-1 ')
      B(26,17) = TO_FM(' 153.3211518580416650705937678859146940112243631025945564907021486707139114294996134941 ')
      B(26,18) = TO_FM(' 7.175681863277204958467664848147841435678263080348653386540185145833155908488128910568 ')
      B(26,19) = TO_FM(' -2.940367486753004819459176598969309892153205943807775979427615740476908865098135595635 ')
      B(26,20) = TO_FM(' -.6658459460768031444707496760226288702819204931972568878708744783028558369468497032253e-1 ')
      B(26,21) = TO_FM(' -.4623460549908436612292486685622172611769665140168592842374268449140643068786760618896e-1 ')
      B(26,22) = TO_FM(' -.2041987335856794015393882286172697788485797748215817776751235910664984352284968100100e-1 ')
      B(26,23) = TO_FM(' -53.35231064387358505159534411659981079740450904957915977996876390672711239156977103431 ')
      B(26,24) = TO_FM(' -1.355487147150786549787321867059964040175545016141913251148206738329360142936656282958 ')
      B(26,25) = TO_FM(' -1.571962758012327518829017351714592491776872191144425834618663282570958684038698495739 ')
      B(27, 0) = TO_FM(' -16.64514674863415128720312944039317587645603711308189782044257016154825923946758475845 ')
      B(27, 5) = TO_FM(' .5922327803245033080429900057980465247383895604442571368349896773084347972825775455007e-2 ')
      B(27, 6) = TO_FM(' .4703261599638411122172243032058941134553625307461088250108483236601604516650193568134 ')
      B(27, 7) = TO_FM(' .2996888638486790008539818370961923991368311216717812791841936858888827504094204242461 ')
      B(27, 8) = TO_FM(' -.2476568775939949146899922763298108258539580692639470955481886317480090967647905771626 ')
      B(27, 9) = TO_FM(' .1108950297714376828939998518390617145224451736006787182086245987785252503880550245038 ')
      B(27,11) = TO_FM(' -.4917190438462291470706666287041940976780819072106730449888664749836403474888832394921 ')
      B(27,12) = TO_FM(' -11.47431544272894969683894925643525363508424541308531757856483965863898534849416840511 ')
      B(27,13) = TO_FM(' 80.25931665762302725417024858864844001527933666235899875893849400507278534931158408231 ')
      B(27,14) = TO_FM(' -.3841323039800428476253125267590291037469268413420882192068133107492120348263618466046 ')
      B(27,15) = TO_FM(' 7.281476674681075834713269509261361157676125818628777243483988994104498714011047355205 ')
      B(27,16) = TO_FM(' -.1326993846122483795105717081760352748368273416167518843018178653526280269065470590467 ')
      B(27,17) = TO_FM(' -81.07998325257307266746792897522552400060707166336329885641562357237166810196760593013 ')
      B(27,18) = TO_FM(' -1.250374928356206395217681856561791199622537474924031863192434629401819729868852090550 ')
      B(27,19) = TO_FM(' 2.592635949695436810237763795043773249942264473592968880837586883560068434349818491911 ')
      B(27,20) = TO_FM(' -.3014402983464045398301639972605268752644315372756414953420797074457552586137488110716 ')
      B(27,21) = TO_FM(' .2213844607898323374517064515727737916952468390573184143179573617704323166985265217363 ')
      B(27,22) = TO_FM(' .8275772747718929319559898709746931529962764354298098905497078729734353980896315305691e-1 ')
      B(27,23) = TO_FM(' 18.99606620406115204646724500372432639981751614122371589366718674999943569769696943522 ')
      B(27,24) = TO_FM(' .2692319464096396856234680151283341674600519103489128451211866688910668614577677735665 ')
      B(27,25) = TO_FM(' 1.626748274470665374629893649296289339881250292841836802790201430504847697803528636395 ')
      B(27,26) = TO_FM(' .4917190438462291470706666287041940976780819072106730449888664749836403474888832394921 ')
      B(28, 0) = TO_FM(' .8384798124090526646169687913728140859805331392249111310693346670107922625197375034871e-1 ')
      B(28, 5) = TO_FM(' -.1179493671009738143197550560312957753679619605907361507776128268875265788248790903515e-1 ')
      B(28, 6) = TO_FM(' -.2472990205688126523394738387431945983259928403533401326974984247503501083158412965835 ')
      B(28, 7) = TO_FM(' .9780808583677290122593130140812916655037406554767339407565991037499621093437371932341e-1 ')
      B(28, 8) = TO_FM(' .2175906892434206313600086517678603183441681200247821768799893467069296630467914197921 ')
      B(28,10) = TO_FM(' .1375856067633252248656596321967877466474472229750848659754400903987833771639575727867 ')
      B(28,11) = TO_FM(' .4398702297150466850587900923415450260461038902942613590425808839943205635447284745074e-1 ')
      B(28,13) = TO_FM(' -.5137008137681933419570044566186303037387573636419640300869712169933398305905931343468 ')
      B(28,14) = TO_FM(' .8263556911513155086442113083991534587014231586161685769224194977471882335420141183213 ')
      B(28,15) = TO_FM(' 25.70181397198118326258738829725199395111365563419600781824702737091645129169813134401 ')
      B(28,23) = TO_FM(' -25.70181397198118326258738829725199395111365563419600781824702737091645129169813134401 ')
      B(28,24) = TO_FM(' -.8263556911513155086442113083991534587014231586161685769224194977471882335420141183213 ')
      B(28,25) = TO_FM(' .5137008137681933419570044566186303037387573636419640300869712169933398305905931343468 ')
      B(28,26) = TO_FM(' -.4398702297150466850587900923415450260461038902942613590425808839943205635447284745074e-1 ')
      B(28,27) = TO_FM(' -.1375856067633252248656596321967877466474472229750848659754400903987833771639575727867 ')
      B(29, 0) = TO_FM(' .1243805266540944128815164208687993162684914663596714231632892354628068537117612942798 ')
      B(29, 4) = TO_FM(' .2261202821975843014222386629792029011967523207426331439651447460281196206643404356021 ')
      B(29, 5) = TO_FM(' .1378858876180808806076958370164778145309694174914933853635428709475288586061552782365e-1 ')
      B(29, 6) = TO_FM(' -.6722101339966844497493995074143058569500863415253821828561997825320849038679063596730e-1 ')
      B(29, 9) = TO_FM(' -.8562389750854283547553497698795017721121215974115638028550665385850612741040225222977 ')
      B(29,10) = TO_FM(' -1.963375228668589089282628500280938139881804405182674045535756631526916950083353845169 ')
      B(29,11) = TO_FM(' -.2323328227241194012372462573089218472501081992304199949782180319905262045718872259601 ')
      B(29,13) = TO_FM(' 4.306607190864533494616689368765629477724325620534780926267640393608500758570100495873 ')
      B(29,14) = TO_FM(' -2.927229632494654826597879112023904466876873949506336126307786635262992367484998786517 ')
      B(29,15) = TO_FM(' -82.31316663978589444544923341054587077357619664281386893950601309356417181948645997040 ')
      B(29,23) = TO_FM(' 82.31316663978589444544923341054587077357619664281386893950601309356417181948645997040 ')
      B(29,24) = TO_FM(' 2.927229632494654826597879112023904466876873949506336126307786635262992367484998786517 ')
      B(29,25) = TO_FM(' -4.306607190864533494616689368765629477724325620534780926267640393608500758570100495873 ')
      B(29,26) = TO_FM(' .2323328227241194012372462573089218472501081992304199949782180319905262045718872259601 ')
      B(29,27) = TO_FM(' 1.963375228668589089282628500280938139881804405182674045535756631526916950083353845169 ')
      B(29,28) = TO_FM(' .8562389750854283547553497698795017721121215974115638028550665385850612741040225222977 ')
      B(30, 0) = TO_FM(' .1034845616366797766729935465119103444997447982019713166066629728281981965079290745983 ')
      B(30, 3) = TO_FM(' .1220688873064072225896440828689620771395927148341621347412746563709055937325311521675 ')
      B(30, 4) = TO_FM(' .4825744903312466224751347801256881128659190238501680496794015023696413273862321544150 ')
      B(30, 5) = TO_FM(' -.3814096000156069997308862400056202056641130724784114774219699240039767479629669855696e-1 ')
      B(30, 7) = TO_FM(' -.5504995253108023241383885070205081774114143110000375617128363206424473498745141065969 ')
      B(30, 9) = TO_FM(' -.7119158115851892278876482620437943875782918824067455704957652139710574799878630163853 ')
      B(30,10) = TO_FM(' -.5841296056715513404329887301584808720953353296452275957070524410065417676683463009109 ')
      B(30,13) = TO_FM(' 2.110463081258649321287173000466227503003750542789369878507182287710881470618943318741 ')
      B(30,14) = TO_FM(' -.8374947367395721355257420230010379926952601753351235177405529298334532793741463162845e-1 ')
      B(30,15) = TO_FM(' 5.100214990723209140752959690433441131075450608628042491597346388445135412965217165555 ')
      B(30,23) = TO_FM(' -5.100214990723209140752959690433441131075450608628042491597346388445135412965217165555 ')
      B(30,24) = TO_FM(' .8374947367395721355257420230010379926952601753351235177405529298334532793741463162845e-1 ')
      B(30,25) = TO_FM(' -2.110463081258649321287173000466227503003750542789369878507182287710881470618943318741 ')
      B(30,27) = TO_FM(' .5841296056715513404329887301584808720953353296452275957070524410065417676683463009109 ')
      B(30,28) = TO_FM(' .7119158115851892278876482620437943875782918824067455704957652139710574799878630163853 ')
      B(30,29) = TO_FM(' .5504995253108023241383885070205081774114143110000375617128363206424473498745141065969 ')
      B(31, 0) = TO_FM('  29 ') / 150
      B(31, 2) = TO_FM('  11 ') /  50
      B(31, 3) = TO_FM('  -2 ') /  25
      B(31, 6) = TO_FM(' .1099934255807247039194624048650683408451190582958464264636524271459687549994002654752 ')
      B(31, 7) = TO_FM(' -.2542970480762701613840685069971531221418356269767039208462421656164179875269042982442 ')
      B(31, 9) = TO_FM(' .8655707771166942543437703438210982818328474012330118593467368132762510892051242759318 ')
      B(31,10) = TO_FM(' 3.324164491140930831067995527865720183368600929369864071601998386039920635781409865040 ')
      B(31,13) = TO_FM(' -12.01022233159779338823523851486618412603019426339968151272769528462035002110216728101 ')
      B(31,14) = TO_FM(' .4766014662424932394304427768620618996029637820035802094825720242694315551196576125507 ')
      B(31,15) = TO_FM(' -29.02430112210363905258026232136540995962512213324709106915239870601916450708546744075 ')
      B(31,23) = TO_FM(' 29.02430112210363905258026232136540995962512213324709106915239870601916450708546744075 ')
      B(31,24) = TO_FM(' -.4766014662424932394304427768620618996029637820035802094825720242694315551196576125507 ')
      B(31,25) = TO_FM(' 12.01022233159779338823523851486618412603019426339968151272769528462035002110216728101 ')
      B(31,27) = TO_FM(' -3.324164491140930831067995527865720183368600929369864071601998386039920635781409865040 ')
      B(31,28) = TO_FM(' -.8655707771166942543437703438210982818328474012330118593467368132762510892051242759318 ')
      B(31,29) = TO_FM(' .2542970480762701613840685069971531221418356269767039208462421656164179875269042982442 ')
      B(31,30) = TO_FM(' -.1099934255807247039194624048650683408451190582958464264636524271459687549994002654752 ')
      B(32, 0) = TO_FM('  -5 ') /   6
      B(32, 1) = TO_FM('  25 ') /  18
      B(32, 4) = TO_FM('  -3 ') /   4
      B(32, 6) = TO_FM(' -.4925295437180263044226820491140213202002146815806577847190740839644346370048749342561 ')
      B(32,30) = TO_FM(' .4925295437180263044226820491140213202002146815806577847190740839644346370048749342561 ')
      B(32,31) = TO_FM('   3 ') /   4
      B(33, 0) = TO_FM('   1 ') /   9
      B(33, 2) = TO_FM('  -2 ') /   9
      B(33,32) = TO_FM('   2 ') /   9
      B(34, 0) = TO_FM(' .2858351403889715587960888421638364148529275378945964668924322897553490152559792262023 ')
      B(34, 1) = TO_FM('   7 ') /  24
      B(34, 2) = TO_FM('   7 ') /  32
      B(34, 4) = TO_FM('  21 ') / 128
      B(34, 6) = TO_FM(' .2181943549455566583271882415813521070932888243221879411415164327116967439531911272777 ')
      B(34, 7) = TO_FM(' .1803928984786977668636352219467754377196200536418492285624347210514163759703679527180 ')
      B(34, 9) = TO_FM(' .2057138394048450188591207551229295422775700949828089053939914789386228504942804843989 ')
      B(34,10) = TO_FM(' .2427157915817702399702829279594465157627459713866705419485763522859549196625913978401 ')
      B(34,11) = TO_FM(' .2464657808136293058336092911818914077992281038693057051370210135284213379790417930740 ')
      B(34,12) = TO_FM(' -3.449919407908908249798341546016226620603704606149316442883265523381128452524989278943 ')
      B(34,13) = TO_FM(' .2288755621600360817607290607384585842942203725527402184592948392511281334278617959957 ')
      B(34,14) = TO_FM(' .2832905997021514153215274190567333359784365954938557898314048426595070708424182066065 ')
      B(34,15) = TO_FM(' 3.210851258377666409601314905442367870055573203322387098512984999880577120008173123283 ')
      B(34,16) = TO_FM(' -.2235387773648456999202337562141625079641252300836740320899016275445898395177373582441 ')
      B(34,17) = TO_FM(' -.7071211572044190735187272862074872121300912319552061607910521928571247612111795934106 ')
      B(34,18) = TO_FM(' 3.211233451502870804081747292028565008932600344430223743249588034157195885590228893622 ')
      B(34,19) = TO_FM(' 1.409543483096697660304144743011231757690459455735489863573218752821178310978199657967 ')
      B(34,20) = TO_FM(' -.1513620534437426131216022767425181110909630262036760559494590353712667648924754181285 ')
      B(34,21) = TO_FM(' .3723505745270142764547240802146199843971210282021482987373568243836683323798121465643 ')
      B(34,22) = TO_FM(' .2529787464063613367221999077621412859157757281294143192610824780367182739421617243696 ')
      B(34,23) = TO_FM(' -3.210851258377666409601314905442367870055573203322387098512984999880577120008173123283 ')
      B(34,24) = TO_FM(' -.2832905997021514153215274190567333359784365954938557898314048426595070708424182066065 ')
      B(34,25) = TO_FM(' -.2288755621600360817607290607384585842942203725527402184592948392511281334278617959957 ')
      B(34,26) = TO_FM(' -.2464657808136293058336092911818914077992281038693057051370210135284213379790417930740 ')
      B(34,27) = TO_FM(' -.2427157915817702399702829279594465157627459713866705419485763522859549196625913978401 ')
      B(34,28) = TO_FM(' -.2057138394048450188591207551229295422775700949828089053939914789386228504942804843989 ')
      B(34,29) = TO_FM(' -.1803928984786977668636352219467754377196200536418492285624347210514163759703679527180 ')
      B(34,30) = TO_FM(' -.2181943549455566583271882415813521070932888243221879411415164327116967439531911272777 ')
      B(34,31) = TO_FM(' -21 ') / 128
      B(34,32) = TO_FM('  -7 ') /  32
      B(34,33) = TO_FM('  -7 ') /  24

      END SUBROUTINE FM_RK14_COEFFS

      SUBROUTINE FM_SECANT(AX,BX,F,NF,ROOT,KPRT,KU)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This routine finds a root of F(X,NF) = 0 using AX and BX as starting points.
!  AX and BX do not have to bracket a root in the sense that F(AX,NF) and F(BX,NF) have opposite
!  signs on input.  This means the search can fail if AX and BX are not close enough to any roots
!  or if the function has no real roots or is badly behaved.

!  When a root is found, FM_SECANT tries to return full accuracy even in the case of multiple
!  or closely-spaced roots, by raising precision above the user's level.

!  ROOT  is the value returned as the approximate root of the equation.

!  KPRT  controls printing within the routine:
!        KPRT = 0 for no output
!        KPRT = 1 for the approximation to the root and the function
!                 value to be printed once at the end of the routine.
!        KPRT = 2 for the approximation to the root and the function
!                 value to be printed each iteration.

!  KU    is the unit number for output.

      TYPE (FM)           :: AX, BX, ROOT
      TYPE (FM), EXTERNAL :: F, FM_FPRIME
      CHARACTER (80) :: STR
      INTEGER :: J, KU, KPRT, KWARN_SAVE, MAXIT, NDSAVE, NF
      LOGICAL :: USE_F_OVER_FP
      TYPE (FM), SAVE :: ERR, ERR1, F1, F2, TOL, X1, X1OLD, X2, X3

      IF (KPRT == 2) THEN
          WRITE (KU,*) ' '
          WRITE (KU,*) ' FM_SECANT.  Begin trace of all iterations.'
      ENDIF

!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      CALL FM_EQU(AX,X1,NDSAVE,NDIG)
      CALL FM_EQU(BX,X2,NDSAVE,NDIG)
      KWARN_SAVE = KWARN
      KWARN = 0

      MAXIT = 1000
      ERR = 1
      TOL = 100*EPSILON(ERR)
      USE_F_OVER_FP = .FALSE.
      F1 = F(X1,NF)
      F2 = F(X2,NF)

!             Check for legal function values.

      IF (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1)) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F1 = F(X3,NF)
             IF (.NOT. (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1))) THEN
                 X1 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1)) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F1 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F1 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(F1) .OR. IS_OVERFLOW(F1)) THEN
          WRITE (KU,*) ' '
          WRITE (KU,*) ' Invalid input for FM_SECANT.  ',  &
                       'Unknown or overflowed function value for AX ='
          CALL FM_PRINT(X1)
          WRITE (KU,*) ' '
          J = 0
          X2 = TO_FM(' UNKNOWN ')
          ERR = TO_FM(' UNKNOWN ')
          GO TO 110
      ENDIF

      IF (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2)) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F2 = F(X3,NF)
             IF (.NOT. (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2))) THEN
                 X2 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2)) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F2 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F2 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2)) THEN
          WRITE (KU,*) ' '
          WRITE (KU,*) ' Invalid input for FM_SECANT.  ',  &
                       'Unknown or overflowed function value for BX ='
          CALL FM_PRINT(X2)
          WRITE (KU,*) ' '
          J = 0
          X2 = TO_FM(' UNKNOWN ')
          ERR = TO_FM(' UNKNOWN ')
          GO TO 110
      ENDIF

      IF (KPRT == 2) THEN
          STR = FM_FORMAT('ES20.10',F1)
          WRITE (KU,"('  J =',I3,3X,'f(AX) = ',A,'    x:')") 0,STR(1:25)
          CALL FM_PRINT(X1)
          STR = FM_FORMAT('ES20.10',F2)
          WRITE (KU,"('  J =',I3,3X,'f(BX) = ',A,'    x:')") 0,STR(1:25)
          CALL FM_PRINT(X2)
      ENDIF

!             This loop does the iteration.

      DO J = 1, MAXIT

         IF (F2-F1 /= 0.0) THEN
             X3 = X2 - F2*(X2-X1)/(F2-F1)
         ELSE
             X3 = X2 + 1
         ENDIF

!             Multiple roots cause very slow convergence and loss of accuracy.
!             If the slope is very small, try to improve convergence and accuracy by using
!             the (slower) function  f(x)/f'(x)  which has no multiple roots.

         X1OLD = X1
         IF ( (ABS((F2-F1)/(X2-X1)) < 1.0D-2 .AND. ABS(F2) < 1.0D-4) .OR. USE_F_OVER_FP) THEN
             USE_F_OVER_FP = .TRUE.
             X1 = X2
             X2 = X3
             F1 = F2
             F2 = FM_FPRIME(0,X3,F,NF) / FM_FPRIME(1,X3,F,NF)
         ELSE
             X1 = X2
             X2 = X3
             F1 = F2
             F2 = F(X3,NF)
         ENDIF

!             If F2 is one of the FM non-numbers, +-underflow, +-overflow, unknown,
!             then replace it by something representable, so that the next x3 will be
!             closer to x1.  Also swap x1 and x2, making the bad x go away first.

         IF (IS_UNKNOWN(F2) .OR. IS_OVERFLOW(F2)) THEN
             F2 = -2*F1
             X3 = X1
             X1 = X2
             X2 = X3
             X3 = F1
             F1 = F2
             F2 = X3
         ENDIF

         IF (KPRT == 2) THEN
             STR = FM_FORMAT('ES20.10',F2)
             WRITE (KU,"('  J =',I3,4X,'f(x) = ' ,A,'    x:')") J,STR(1:25)
             CALL FM_PRINT(X2)
         ENDIF

         ERR1 = ERR
         IF (X2 /= 0.0) THEN
             ERR = ABS((X2-X1)/X2)
         ELSE
             ERR = ABS(X2-X1)
         ENDIF

!             If the error is less than the tolerance, double check to make sure the previous
!             error was small along with the current function value.  Some divergent iterations
!             can get err < tol without being close to a root.

         IF (ERR < TOL .OR. F2 == 0) THEN
             IF (ERR1 > SQRT(SQRT(TOL)) .AND. ABS(F2) > SQRT(EPSILON(TO_FM(1)))) THEN
                 WRITE (KU,"(/' Possible false convergence in FM_SECANT after',I5,"//  &
                           "' iterations.  ','Last two approximations =')") J
                 CALL FM_PRINT(X1)
                 CALL FM_PRINT(X2)
                 WRITE (KU,"(/' These agree to the convergence tolerance, but the previous"//  &
                           " iteration was suspiciously far away:')")
                 CALL FM_PRINT(X1OLD)
                 WRITE (KU,"(/' and the function value of the last iteration was"//  &
                           " suspiciously far from zero:')")
                 CALL FM_PRINT(F2)
             ENDIF
             GO TO 110
         ENDIF

      ENDDO

!             No convergence after maxit iterations.

      WRITE (KU,"(/' No convergence in FM_SECANT after',I5,' iterations.  ',"//  &
                "'Last two approximations =')") MAXIT
      CALL FM_PRINT(X1)
      CALL FM_PRINT(X2)
      X2 = TO_FM(' UNKNOWN ')
      WRITE (KU,"(/' Unknown has been returned.')")

!             The root was found.

  110 CALL FM_EQU(X2,ROOT,NDIG,NDSAVE)
      NDIG = NDSAVE
      IF (KPRT >= 1) THEN
          CALL FM_ULP(X2,ERR1)
          IF (.NOT.( IS_UNKNOWN(ERR1) .OR. IS_UNDERFLOW(ERR1) )) THEN
              ERR1 = ABS(ERR1/X2)/2
              IF (ERR < ERR1) ERR = ERR1
          ENDIF
          STR = FM_FORMAT('ES16.6',ERR)
          WRITE (KU,*) ' '
          WRITE (KU,"('  FM_SECANT.   Function ',I3,I7,' iterations.'/17X"// &
                    "'Estimated relative error =',A,',    Root:')") NF,J,TRIM(STR)
          CALL FM_PRINT(ROOT)
          WRITE (KU,*) ' '
      ENDIF

      KWARN = KWARN_SAVE
      END SUBROUTINE FM_SECANT

      FUNCTION ZM_FPRIME(N,A,F,NF)     RESULT (RETURN_VALUE)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This routine finds the Nth derivative of F(X,NF), evaluated at A.
!  NF is passed on to function F to indicate which function to use in cases where several
!  different functions may be defined there.

!  F must be defined in an interval containing A, so that F can be sampled on both sides of A.

!  N may be zero, so that in cases where F suffers cancellation error at A, an accurate
!  function value is returned.

!  ZM_FPRIME tries to return full accuracy for the derivative, by raising precision above
!  the user's level and using difference formulas.

      TYPE (ZM)           :: A, RETURN_VALUE
      TYPE (ZM), EXTERNAL :: F
      INTEGER :: J, K, KWARN_SAVE, NDSAVE, N, NF
      TYPE (ZM), SAVE :: D1, D2, F1, F2, X1
      TYPE (FM), SAVE :: H, TOL, TOL2

!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      CALL ZM_EQU(A,X1,NDSAVE,NDIG)
      KWARN_SAVE = KWARN
      KWARN = 0

      D2 = 0
      F1 = F(X1,NF)
      IF (F1 /= 0) THEN
          CALL FM_ULP(ABS(F1),TOL)
      ELSE
          TOL = EPSILON(TO_FM(1))
      ENDIF
      TOL = ABS(TOL)

!             Check for a legal function value.

      IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)) .OR. IS_UNDERFLOW(ABS(F1)) .OR. N < 0) THEN
          D2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
          GO TO 110
      ENDIF
      F2 = F1

!             Loop at increasing precision until the difference formula is accurate.

      DO J = 1, 100

         NDIG = 2*NDIG

!             Define the variables used below at the new higher precision.

         CALL ZM_EQU(D2,D1,NDIG/2,NDIG)
         CALL ZM_EQU(F2,F1,NDIG/2,NDIG)
         CALL FM_EQU(TOL,TOL2,NDSAVE,NDIG)
         CALL ZM_EQU(A,X1,NDSAVE,NDIG)

!             Special case for N = 0.

         IF (N == 0) THEN
             F2 = F(X1,NF)
             D2 = F2
             IF (ABS(F2-F1) < TOL2) GO TO 110
             CYCLE
         ENDIF
         F2 = F1

!             Special case for N = 1.

         IF (N == 1) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(ABS(X1),H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = SQRT(ABS(H))
             D2 = ( F(X1+H,NF) - F(X1-H,NF) ) / (2*H)
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

!             General case for even N > 1.

         IF (MOD(N,2) == 0) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(ABS(X1),H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = ABS(H)**(TO_FM(1)/(N+2))
             D2 = (-1)**(N/2) * BINOMIAL(TO_FM(N),TO_FM(N/2)) * F(X1,NF)
             DO K = 0, N/2-1
                D2 = D2 + (-1)**K * BINOMIAL(TO_FM(N),TO_FM(K)) *  &
                     ( F(X1+(N/2-K)*H,NF) + F(X1-(N/2-K)*H,NF) )
             ENDDO
             D2 = D2 / H**N
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

!             General case for odd N > 1.

         IF (MOD(N,2) == 1) THEN
             IF (X1 /= 0) THEN
                 CALL FM_ULP(ABS(X1),H)
             ELSE
                 H = EPSILON(TO_FM(1))
             ENDIF
             H = ABS(H)**(TO_FM(1)/(N+2))
             D2 = 0
             DO K = 0, N/2
                D2 = D2 + (-1)**K * BINOMIAL(TO_FM(N-1),TO_FM(K))    *  &
                     ( F(X1+(N/2-K+1)*H,NF) - F(X1-(N/2-K+1)*H,NF) ) *  &
                     TO_FM(N*(N+1-2*K)) / ((N-K)*(N+1-K))
             ENDDO
             D2 = D2 / (2*H**N)
             IF (ABS(D2-D1) < TOL2 .AND. J > 1) GO TO 110
             CYCLE
         ENDIF

      ENDDO

!             Round and return.

  110 CALL ZM_EQU(D2,RETURN_VALUE,NDIG,NDSAVE)
      NDIG = NDSAVE
      KWARN = KWARN_SAVE
      END FUNCTION ZM_FPRIME

      SUBROUTINE ZM_INVERSE(A,N,B,DET)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  Return B as the inverse of the N x N matrix A, and DET as the determinant of A.

!  A and B are type (zm) (complex) multiprecision arrays.

      INTEGER :: N
      TYPE (ZM) :: A(N,N), B(N,N), DET
      TYPE (FM), SAVE :: TOL
      TYPE (ZM), ALLOCATABLE :: A1(:,:), A2(:,:), B1(:), R1(:), X1(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      INTEGER :: I, J, K, KWARN_SAVE, NDSAVE

      TOL = EPSILON(TO_FM(1))/MBASE/TO_FM(10)**10

      ALLOCATE(A1(N,N),A2(N,N),B1(N),R1(N),X1(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in ZM_INVERSE.  Unable to allocate arrays with N = ',I8/)") N
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
            CALL ZM_EQU(A(I,J),A1(I,J),NDSAVE,NDIG)
         ENDDO
      ENDDO
      A2 = A1

!             Factor A into L*U form.

      CALL ZM_FACTOR_LU(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (*,"(/' Error in ZM_INVERSE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          B = TO_ZM(' UNKNOWN ')
          GO TO 120
      ENDIF

!             Solve for the inverse matrix one column at a time.

      DO K = 1, N
         B1 = 0
         B1(K) = 1
         CALL ZM_SOLVE_LU(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

         R1 = MATMUL(A2,X1) - B1

         CALL ZM_SOLVE_LU(A1,N,R1,B1,KSWAP)
         X1 = X1 - B1

!             Check for accuracy at the user's precision.

         IF (SQRT( ABS(DOT_PRODUCT( B1 , B1 )) ) > TOL) THEN
             NDIG = 2*NDIG
             GO TO 110
         ENDIF

!             Round the result and store column K in the B matrix.

         DO I = 1, N
            CALL ZM_EQU(X1(I),B(I,K),NDIG,NDSAVE)
         ENDDO
      ENDDO
  120 CALL ZMEQU_R1(DET%MZM,NDIG,NDSAVE)

      DEALLOCATE(A1,A2,B1,R1,X1,KSWAP)

      NDIG = NDSAVE
      KWARN = KWARN_SAVE
      END SUBROUTINE ZM_INVERSE

      SUBROUTINE ZM_LIN_SOLVE(A,X,B,N,DET)
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

!  A,X,B,DET are all type (zm) complex multiprecision variables.

      INTEGER :: N
      TYPE (ZM) :: A(N,N), B(N), X(N), DET
      TYPE (FM), SAVE :: TOL
      TYPE (ZM), ALLOCATABLE :: A1(:,:), A2(:,:), B1(:), R1(:), X1(:)
      INTEGER, ALLOCATABLE :: KSWAP(:)
      INTEGER :: I, J, NDSAVE

      ALLOCATE(A1(N,N),A2(N,N),B1(N),R1(N),X1(N),KSWAP(N),STAT=J)
      IF (J /= 0) THEN
          WRITE (*,"(/' Error in ZM_LIN_SOLVE.  Unable to allocate arrays with N = ',I8/)") N
          STOP
      ENDIF

      TOL = EPSILON(TO_FM(1))/MBASE/TO_FM(10)**10

      NDSAVE = NDIG
      NDIG = 2*NDIG

!             Copy A and B to A1 and B1 with higher precision.

  110 CALL FM_EQU_R1(TOL,NDSAVE,NDIG)
      DO I = 1, N
         DO J = 1, N
            CALL ZM_EQU(A(I,J),A1(I,J),NDSAVE,NDIG)
            CALL ZM_EQ(A1(I,J),A2(I,J))
         ENDDO
         CALL ZM_EQU(B(I),B1(I),NDSAVE,NDIG)
      ENDDO

!             Solve the system.

      CALL ZM_FACTOR_LU(A1,N,DET,KSWAP)
      IF (DET == 0 .OR. IS_UNKNOWN(DET)) THEN
          IF (KWARN > 0) THEN
              WRITE (KW,"(/' Error in ZM_LIN_SOLVE.  The matrix is singular.'/)")
          ENDIF
          IF (KWARN >= 2) STOP
          X1 = TO_ZM(' UNKNOWN ')
          GO TO 120
      ENDIF
      CALL ZM_SOLVE_LU(A1,N,B1,X1,KSWAP)

!             Do an iterative refinement.

      R1 = MATMUL(A2,X1) - B1

      CALL ZM_SOLVE_LU(A1,N,R1,B1,KSWAP)
      X1 = X1 - B1

!             Check for accuracy at the user's precision.

      IF (SQRT( ABS(DOT_PRODUCT( B1 , B1 )) ) > TOL) THEN
          NDIG = 2*NDIG
          GO TO 110
      ENDIF

!             Round and return X and DET.

  120 DO I = 1, N
         CALL ZM_EQU(X1(I),X(I),NDIG,NDSAVE)
      ENDDO
      CALL ZMEQU_R1(DET%MZM,NDIG,NDSAVE)

      NDIG = NDSAVE

      DEALLOCATE(A1,A2,B1,R1,X1,KSWAP)

      END SUBROUTINE ZM_LIN_SOLVE

      SUBROUTINE ZM_FACTOR_LU(A,N,DET,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Gauss elimination to factor the NxN matrix A (LU decomposition).

!  The time is proportional to  N**3.

!  Once this factorization has been done, a linear system  A x = b
!  with the same coefficient matrix A and Nx1 vector b can be solved
!  for x using routine ZM_SOLVE_LU in time proportional to  N**2.

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
      TYPE (ZM) :: A(N,N), DET
      TYPE (ZM), SAVE :: AMAX, AMULT, TEMP

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
            IF (ABS(A(JROW,JDIAG)) > ABS(AMAX)) THEN
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

!             Save the multiplier for use later by ZM_SOLVE_LU.

            A(JROW,JDIAG) = AMULT
            DO JCOL = JDIAG+1, N
               A(JROW,JCOL) = A(JROW,JCOL) - AMULT*A(JDIAG,JCOL)
            ENDDO
         ENDDO
      ENDDO
      DET = DET * A(N,N)

      END SUBROUTINE ZM_FACTOR_LU

      SUBROUTINE ZM_SOLVE_LU(A,N,B,X,KSWAP)
      USE FMZM
      IMPLICIT NONE

!  Solve a linear system  A x = b.
!  A is the NxN coefficient matrix, after having been factored by ZM_FACTOR_LU.
!  B is the Nx1 right-hand-side vector.
!  X is returned with the solution of the linear system.
!  KSWAP is a list of row interchanges made by the partial pivoting strategy during the
!        elimination phase in ZM_FACTOR_LU.
!  Time for this call is proportional to  N**2.

      INTEGER :: N, KSWAP(N)
      TYPE (ZM) :: A(N,N), B(N), X(N)
      INTEGER :: J, JDIAG, JMAX
      TYPE (ZM), SAVE :: TEMP

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

      END SUBROUTINE ZM_SOLVE_LU

      SUBROUTINE ZM_ROOTS(NR,F,NF,N_FOUND,LIST_OF_ROOTS,KPRT,KU)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This routine searches for NR roots of F(X,NF) = 0.
!  NF is the function number in case roots to several functions are needed.

!  N_FOUND is returned as the number of roots found.
!  LIST_OF_ROOTS is an array returned with the roots found.  They are complex type (zm) numbers,
!                even when the actual root is real.

!  KPRT  controls printing within the routine:
!        KPRT = 0 for no output
!        KPRT = 1 for the approximation to each root to be printed as they are found.

!  KU    is the unit number for output.

!  The search for roots begins with fairly small magnitude complex values, so small roots are
!  often found before larger roots, but there is no guarantee of this, and the order in which
!  the roots are found is fairly random.  The user can sort LIST_OF_ROOTS and print them after
!  all have been found.

!  The secant method often fails to converge to any root for a given pair of starting points.
!  This routine may call ZM_ROOT1 many more than NR times before NR roots are found.  It can
!  also happen that ZM_ROOTS eventually gives up and returns N_FOUND < NR roots.

!  The user's function F is divided by the product of (X - LIST_OF_ROOTS(j)) over the roots that
!  have been found so far.  This tries keep the ZM_ROOT1 routine from returning to a root that is
!  already on the list (unless it is a root of multiplicity M > 1).

      TYPE (ZM), EXTERNAL :: F
      INTEGER :: J, KU, KPRT, KWARN_SAVE, NDIG_OF_ROOTS, NDSAVE, NF, NR, N_FOUND
      DOUBLE PRECISION :: VALUE
      LOGICAL :: REMOVE_PREVIOUS_ROOTS, RETRY
      TYPE (ZM) :: LIST_OF_ROOTS(NR)
      TYPE (ZM), SAVE :: AX, BX, X1


!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      KWARN_SAVE = KWARN
      KWARN = 0
      RETRY = .FALSE.

      N_FOUND = 0
      LIST_OF_ROOTS = TO_ZM(' UNKNOWN + UNKNOWN i ')
      NDIG_OF_ROOTS = NDIG

      DO J = 1, 10*NR
         IF (RETRY) THEN
             CALL FM_RANDOM_NUMBER(VALUE)
             IF (MOD(J,4) == 0) THEN
                 AX = TO_ZM(' 1.1 + 1.2 i ')*((2+J)*VALUE+1)
             ELSE IF (MOD(J,4) == 1) THEN
                 AX = TO_ZM(' 1.1 - 0.8 i ')*((2+J)*VALUE+1)
             ELSE IF (MOD(J,4) == 2) THEN
                 AX = TO_ZM(' -0.8 - 1.2 i ')*((2+J)*VALUE+1)
             ELSE IF (MOD(J,4) == 3) THEN
                 AX = TO_ZM(' -1.1 + 0.8 i ')*((2+J)*VALUE+1)
             ENDIF
             BX = TO_ZM(' 0.87 + 0.5 i ')*AX
         ELSE
             AX = TO_ZM(' 1.1 + 1.2 i ')
             BX = TO_ZM(' 3.4 + 4.5 i ')
         ENDIF
         REMOVE_PREVIOUS_ROOTS = .TRUE.
         CALL ZM_ROOT1(AX,BX,NR,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS,  &
                       X1,-1,KU)
         IF (.NOT. (IS_UNKNOWN(ABS(X1)) .OR. IS_OVERFLOW(ABS(X1))) ) THEN
             N_FOUND = N_FOUND + 1
             LIST_OF_ROOTS(N_FOUND) = X1

!             Some roots, primarily multiple roots, may have lost some accuracy due to the
!             divisions by previously found roots.  Refine them using F without dividing.

             AX = LIST_OF_ROOTS(N_FOUND)*(1+1.0D-10)
             BX = LIST_OF_ROOTS(N_FOUND)*(1+1.0D-15)
             REMOVE_PREVIOUS_ROOTS = .FALSE.
             CALL ZM_ROOT1(AX,BX,NR,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,  &
                           NDIG_OF_ROOTS,X1,-1,KU)
             IF (ABS(REAL(X1)) < EPSILON(TO_FM(1))*ABS(X1)) X1 = CMPLX( TO_FM(0) , AIMAG(X1) )
             IF (ABS(AIMAG(X1)) < EPSILON(TO_FM(1))*ABS(X1)) X1 = CMPLX( REAL(X1) , TO_FM(0) )
             LIST_OF_ROOTS(N_FOUND) = X1

             IF (KPRT > 0) THEN
                 WRITE (*,"(A,I9,A,I6,A)") ' ZM_ROOTS.  Function ',NF,' Root ',N_FOUND,' ='
                 CALL ZM_PRINT(X1)
             ENDIF
             IF (N_FOUND == NR) EXIT

!             Check to see if the conjugate of this root is also a root.

             IF (ABS(AIMAG(X1)) < 100*EPSILON(TO_FM(1))) CYCLE
             AX = CONJG(AX)
             BX = CONJG(BX)
             REMOVE_PREVIOUS_ROOTS = .TRUE.
             CALL ZM_ROOT1(AX,BX,NR,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,  &
                           NDIG_OF_ROOTS,X1,-1,KU)
             IF (.NOT. (IS_UNKNOWN(ABS(X1)) .OR. IS_OVERFLOW(ABS(X1))) ) THEN
                 N_FOUND = N_FOUND + 1
                 LIST_OF_ROOTS(N_FOUND) = X1
                 AX = LIST_OF_ROOTS(N_FOUND)*(1+1.0D-10)
                 BX = LIST_OF_ROOTS(N_FOUND)*(1+1.0D-15)
                 REMOVE_PREVIOUS_ROOTS = .FALSE.
                 CALL ZM_ROOT1(AX,BX,NR,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,  &
                               NDIG_OF_ROOTS,X1,-1,KU)
                 IF (ABS(REAL(X1)) < EPSILON(TO_FM(1))*ABS(X1)) X1 = CMPLX( TO_FM(0) , AIMAG(X1) )
                 IF (ABS(AIMAG(X1)) < EPSILON(TO_FM(1))*ABS(X1)) X1 = CMPLX( REAL(X1) , TO_FM(0) )
                 LIST_OF_ROOTS(N_FOUND) = X1
                 IF (KPRT > 0) THEN
                     WRITE (*,"(A,I9,A,I6,A)") ' ZM_ROOTS.  Function ',NF,' Root ',N_FOUND,' ='
                     CALL ZM_PRINT(X1)
                 ENDIF
                 IF (N_FOUND == NR) EXIT
             ENDIF
             RETRY = .FALSE.
         ELSE
             RETRY = .TRUE.
         ENDIF
      ENDDO

!             Round the roots to the user's precision.

      DO J = 1, N_FOUND
         X1 = LIST_OF_ROOTS(J)
         CALL ZM_EQU(X1,LIST_OF_ROOTS(J),NDIG,NDSAVE)
      ENDDO

      NDIG = NDSAVE
      KWARN = KWARN_SAVE
      END SUBROUTINE ZM_ROOTS

      SUBROUTINE ZM_ROOT1(AX,BX,NR,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,  &
                          NDIG_OF_ROOTS,ROOT,KPRT,KU)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This is a special version of ZM_SECANT, modified to work with ZM_ROOTS so that some calls
!  will use F and others will use F divided by all the (x - r) terms of the roots found so far.

!  REMOVE_PREVIOUS_ROOTS is a logical input variable telling this routine whether or not to
!  divide F by the product of (X - LIST_OF_ROOTS(j)) over the roots that have been found so far.
!  This tries keep the ZM_ROOT1 routine from returning to a root that is already on the list
!  (unless it is a root of multiplicity M > 1).

!  This routine searches for a root of F(X,NF) = 0 using AX and BX as starting points.
!  AX and BX are complex, and the search can fail if AX and BX are not close enough to any roots
!  or if the function is badly behaved.

!  When a root is found, ZM_ROOT1 tries to return full accuracy even in the case of multiple
!  or closely-spaced roots, by raising precision above the user's level.

!  ROOT  is the value returned as the approximate root of the equation.

!  KPRT  controls printing within the routine:
!        KPRT = -1 for no output
!        KPRT =  0 for no output except warning and error messages.
!        KPRT =  1 for the approximation to the root and the function
!                  value to be printed once at the end of the routine.
!        KPRT =  2 for the approximation to the root and the function
!                  value to be printed each iteration.

!  KU    is the unit number for output.

      TYPE (ZM)           :: AX, BX, ROOT
      TYPE (ZM), EXTERNAL :: F, ZM_FPRIME, ZM_ROOT_F
      CHARACTER (80) :: STR
      DOUBLE PRECISION :: VALUE
      INTEGER :: J, JSET, K, KU, KPRT, KWARN_SAVE, MAXIT, N_FOUND, NDIG_OF_ROOTS, NDSAVE, NF, NR
      LOGICAL :: REMOVE_PREVIOUS_ROOTS, USE_F_OVER_FP
      TYPE (ZM) :: LIST_OF_ROOTS(NR)
      TYPE (ZM), SAVE :: F1, F1OLD, F2, FP0, FP1, FS, S, X1, X1OLD, X2, X3
      TYPE (FM), SAVE :: ERR, ERR1, TOL

      IF (KPRT == 2) THEN
          WRITE (KU,"(A)") ' '
          WRITE (KU,"(A)") ' ZM_ROOT1.  Begin trace of all iterations.'
      ENDIF

!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      CALL ZM_EQU(AX,X1,NDSAVE,NDIG)
      CALL ZM_EQU(BX,X2,NDSAVE,NDIG)
      KWARN_SAVE = KWARN
      KWARN = 0

      MAXIT = 1000
      JSET = 50
      ERR = 1
      TOL = 100*EPSILON(ERR)
      USE_F_OVER_FP = .FALSE.
      F1 = ZM_ROOT_F(X1,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
      F2 = ZM_ROOT_F(X2,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)

!             Check for legal function values.

      IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F1 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
             IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                 X1 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F1 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
                 IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F1 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
                 IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
          IF (KPRT >= 0) THEN
              WRITE (KU,"(A)") ' '
              WRITE (KU,"(A,A)") ' Invalid input for ZM_ROOT1. ',  &
                                 ' Unknown or overflowed function value for AX ='
              CALL ZM_PRINT(X1)
              WRITE (KU,"(A)") ' '
          ENDIF
          J = 0
          X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
          ERR = TO_ZM(' UNKNOWN + UNKNOWN i ')
          GO TO 110
      ENDIF

      IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F2 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
             IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                 X2 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F2 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
                 IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F2 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
                 IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
          IF (KPRT >= 0) THEN
              WRITE (KU,"(A)") ' '
              WRITE (KU,"(A,A)") ' Invalid input for ZM_ROOT1. ',  &
                                 ' Unknown or overflowed function value for BX ='
              CALL ZM_PRINT(X2)
              WRITE (KU,"(A)") ' '
          ENDIF
          J = 0
          X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
          ERR = TO_ZM(' UNKNOWN + UNKNOWN i ')
          GO TO 110
      ENDIF

!             Secant does not do well if the magnitudes of the two starting function values differ
!             by too much.  Adjust if necessary.

      DO J = 1, 10
         IF (ABS(F2/F1) > 10) THEN
             X2 = (X1 + X2)/2
             F2 = ZM_ROOT_F(X2,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
         ELSE IF (ABS(F1/F2) > 10) THEN
             X1 = (X1 + X2)/2
             F1 = ZM_ROOT_F(X1,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
         ELSE
             EXIT
         ENDIF
      ENDDO

      IF (KPRT == 2) THEN
          STR = ZM_FORMAT('ES20.10','ES20.10',F1)
          WRITE (KU,"('  J =',I3,3X,'f(AX) = ',A,'    x:')") 0,TRIM(STR)
          CALL ZM_PRINT(X1)
          STR = ZM_FORMAT('ES20.10','ES20.10',F2)
          WRITE (KU,"('  J =',I3,3X,'f(BX) = ',A,'    x:')") 0,TRIM(STR)
          CALL ZM_PRINT(X2)
      ENDIF

!             This loop does the iteration.

      DO J = 1, MAXIT

         IF (F2-F1 /= 0.0) THEN
             X3 = X2 - F2*(X2-X1)/(F2-F1)
         ELSE
             X3 = X2 + 1
         ENDIF

!             Multiple roots cause very slow convergence and loss of accuracy.
!             If the slope is very small, try to improve convergence and accuracy by using
!             the (slower) function  f(x)/f'(x)  which has no multiple roots.

         X1OLD = X1
         F1OLD = F1
         IF ( (ABS((F2-F1)/(X2-X1)) < 1.0D-2 .AND. ABS(F2) < 1.0D-4 .AND.  &
               ABS(F2*(X2-X1)/(F2-F1)) < 1.0D-4*ABS(X2)) .OR. USE_F_OVER_FP) THEN
             USE_F_OVER_FP = .TRUE.
             X1 = X2
             X2 = X3
             F1 = F2
             IF (REMOVE_PREVIOUS_ROOTS) THEN
                 FP0 = ZM_FPRIME(0,X3,F,NF)
                 FP1 = ZM_FPRIME(1,X3,F,NF)
                 S = 0
                 DO K = 1, N_FOUND
                    S = S + 1/(X3-LIST_OF_ROOTS(K))
                 ENDDO
                 F2 = FP0 / ( FP1 - FP0*S )
             ELSE
                 F2 = ZM_FPRIME(0,X3,F,NF) / ZM_FPRIME(1,X3,F,NF)
             ENDIF
         ELSE
             X1 = X2
             X2 = X3
             F1 = F2
             F2 = ZM_ROOT_F(X3,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)

!             If the function has a large number of roots, like a high-degree polynomial, then
!             from a distance the function looks like it has multiple roots even though once we
!             get closer the roots appear distinct.  This can slow the rate of convergence in
!             the early iterations.  Try an Aitken extrapolation once every few steps to try to
!             speed up this initial phase of convergence.

             IF (MOD(J,5) == 0 .AND. X2-2*X1+X1OLD /= 0) THEN
                 S = X2 - (X2-X1)/(X2-2*X1+X1OLD)
                 FS = ZM_ROOT_F(S,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
                 IF (ABS(FS) < MAX(ABS(F1),ABS(F2))) THEN
                     X1 = X2
                     F1 = F2
                     X2 = S
                     F2 = FS
                 ENDIF
             ENDIF
         ENDIF

!             If F2 is one of the FM non-numbers, +-underflow, +-overflow, unknown,
!             then replace it by something representable, so that the next x3 will be
!             closer to x1.  Also swap x1 and x2, making the bad x go away first.

         IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
             F2 = -2*F1
             X3 = X1
             X1 = X2
             X2 = X3
             X3 = F1
             F1 = F2
             F2 = X3
         ENDIF

!             A common failure mode for secant is to get into a pattern that repeats x1 and x2
!             close together with nearly equal function values and x3 farther away with much
!             larger function value.  Check for this, and re-start the iteration by choosing
!             a different x3.

         IF (ABS(F2) > 100*MAX(ABS(F1OLD),ABS(F1)) .AND. J >= JSET) THEN
             IF (JSET >= 200) THEN
                 MAXIT = J
                 EXIT
             ENDIF
             JSET = JSET + 50
             CALL FM_RANDOM_NUMBER(VALUE)
             VALUE = 9*VALUE - 4
             X2 = VALUE*X1 + (1-VALUE)*X2
             F2 = ZM_ROOT_F(X2,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)
         ENDIF

         IF (KPRT == 2) THEN
             STR = ZM_FORMAT('ES20.10','ES20.10',F2)
             WRITE (KU,"('  J =',I3,4X,'f(x) = ' ,A,'    x:')") J,TRIM(STR)
             CALL ZM_PRINT(X2)
         ENDIF

         ERR1 = ERR
         IF (X2 /= 0.0) THEN
             ERR = ABS((X2-X1)/X2)
         ELSE
             ERR = ABS(X2-X1)
         ENDIF

!             If the error is less than the tolerance, double check to make sure the previous
!             error was small along with the current function value.  Some divergent iterations
!             can get err < tol without being close to a root.

         IF (ERR < TOL .OR. F2 == 0) THEN
             IF (ERR1 > SQRT(SQRT(TOL)) .AND. ABS(F2) > SQRT(EPSILON(TO_FM(1)))) THEN
                 IF (KPRT >= 0) THEN
                     WRITE (KU,"(/' Possible false convergence in ZM_ROOT1 after',I5,"//  &
                               "' iterations.  ','Last two approximations =')") J
                     CALL ZM_PRINT(X1)
                     CALL ZM_PRINT(X2)
                     WRITE (KU,"(/' These agree to the convergence tolerance, but the previous"//  &
                               " iteration was suspiciously far away:')")
                     CALL ZM_PRINT(X1OLD)
                     WRITE (KU,"(/' and the function value of the last iteration was"//  &
                               " suspiciously far from zero:')")
                     CALL ZM_PRINT(F2)
                     WRITE (KU,"(/' Unknown has been returned.')")
                 ENDIF
                 X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
             ENDIF
             GO TO 110
         ENDIF

      ENDDO

!             No convergence after maxit iterations.

      IF (KPRT >= 0) THEN
          WRITE (KU,"(/' No convergence in ZM_ROOT1 after',I5,' iterations.  ',"//  &
                    "'Last two approximations =')") MAXIT
          CALL ZM_PRINT(X1)
          CALL ZM_PRINT(X2)
          WRITE (KU,"(/' Unknown has been returned.')")
      ENDIF
      X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')

!             The root was found.

  110 CALL ZM_EQU(X2,ROOT,NDIG,NDSAVE)
      NDIG = NDSAVE
      IF (KPRT >= 1) THEN
          CALL FM_ULP(ABS(X2),ERR1)
          IF (.NOT.( IS_UNKNOWN(ERR1) .OR. IS_UNDERFLOW(ERR1) )) THEN
              ERR1 = ABS(ERR1/X2)/2
              IF (ERR < ERR1) ERR = ERR1
          ENDIF
          STR = FM_FORMAT('ES16.6',ERR)
          WRITE (KU,"(A)") ' '
          WRITE (KU,"('  ZM_ROOT1.   Function ',I3,I7,' iterations.'/17X"// &
                    "'Estimated relative error =',A,',    Root:')") NF,J,TRIM(STR)
          CALL ZM_PRINT(ROOT)
          WRITE (KU,"(A)") ' '
      ENDIF

      KWARN = KWARN_SAVE
      END SUBROUTINE ZM_ROOT1

      FUNCTION ZM_ROOT_F(X,F,NF,REMOVE_PREVIOUS_ROOTS,N_FOUND,LIST_OF_ROOTS,NDIG_OF_ROOTS)  &
                         RESULT (RETURN_VALUE)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  ZM_ROOT_F is used here to evaluate the user's function F and divide F by the product of
!  (X - LIST_OF_ROOTS(j)) over the roots that have been found so far.  This should keep the
!  ZM_ROOT1 routine from returning to a root that is already on the list (unless it is a
!  root of multiplicity M > 1).

!  When REMOVE_PREVIOUS_ROOTS is false, just evaluate F without doing the division.

!  X  is the argument to the function.
!  NF is the function number.

      INTEGER :: J, NDIG_OF_ROOTS, NF, N_FOUND
      LOGICAL :: REMOVE_PREVIOUS_ROOTS
      TYPE (ZM) :: LIST_OF_ROOTS(N_FOUND)
      TYPE (ZM) :: RETURN_VALUE, X
      TYPE (ZM), EXTERNAL :: F
      TYPE (ZM), SAVE :: D

      IF (REMOVE_PREVIOUS_ROOTS) THEN
          RETURN_VALUE = F(X,NF)
          DO J = 1, N_FOUND
             CALL ZM_EQU(LIST_OF_ROOTS(J),D,NDIG_OF_ROOTS,NDIG)
             IF (X /= D) RETURN_VALUE = RETURN_VALUE / (X - D)
          ENDDO
      ELSE
          RETURN_VALUE = F(X,NF)
      ENDIF

      END FUNCTION ZM_ROOT_F

      SUBROUTINE ZM_SECANT(AX,BX,F,NF,ROOT,KPRT,KU)
      USE FMVALS
      USE FMZM
      IMPLICIT NONE

!  This routine searches for a root of F(X,NF) = 0 using AX and BX as starting points.
!  AX and BX are complex, and the search can fail if AX and BX are not close enough to any roots
!  or if the function is badly behaved.

!  When a root is found, ZM_SECANT tries to return full accuracy even in the case of multiple
!  or closely-spaced roots, by raising precision above the user's level.

!  ROOT  is the value returned as the approximate root of the equation.

!  KPRT  controls printing within the routine:
!        KPRT = -1 for no output
!        KPRT =  0 for no output except warning and error messages.
!        KPRT =  1 for the approximation to the root and the function
!                  value to be printed once at the end of the routine.
!        KPRT =  2 for the approximation to the root and the function
!                  value to be printed each iteration.

!  KU    is the unit number for output.

      TYPE (ZM)           :: AX, BX, ROOT
      TYPE (ZM), EXTERNAL :: F, ZM_FPRIME
      CHARACTER (80) :: STR
      DOUBLE PRECISION :: VALUE
      INTEGER :: J, JSET, KPRT, KU, KWARN_SAVE, MAXIT, NDSAVE, NF
      LOGICAL :: USE_F_OVER_FP
      TYPE (ZM), SAVE :: F1, F1OLD, F2, X1, X1OLD, X2, X3
      TYPE (FM), SAVE :: ERR, ERR1, TOL

      IF (KPRT == 2) THEN
          WRITE (KU,*) ' '
          WRITE (KU,*) ' ZM_SECANT.  Begin trace of all iterations.'
      ENDIF

!             Raise precision slightly.

      NDSAVE = NDIG
      NDIG = NDIG + NGRD52
      CALL ZM_EQU(AX,X1,NDSAVE,NDIG)
      CALL ZM_EQU(BX,X2,NDSAVE,NDIG)
      KWARN_SAVE = KWARN
      KWARN = 0

      MAXIT = 1000
      JSET = 50
      ERR = 1
      TOL = 100*EPSILON(ERR)
      USE_F_OVER_FP = .FALSE.
      F1 = F(X1,NF)
      F2 = F(X2,NF)

!             Check for legal function values.

      IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F1 = F(X3,NF)
             IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                 X1 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F1 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F1 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1)))) THEN
                     X1 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(ABS(F1)) .OR. IS_OVERFLOW(ABS(F1))) THEN
          IF (KPRT >= 0) THEN
              WRITE (KU,*) ' '
              WRITE (KU,*) ' Invalid input for ZM_SECANT. ',  &
                           ' Unknown or overflowed function value for AX ='
              CALL ZM_PRINT(X1)
              WRITE (KU,*) ' '
          ENDIF
          J = 0
          X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
          ERR = TO_ZM(' UNKNOWN + UNKNOWN i ')
          GO TO 110
      ENDIF

      IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
          DO J = 1, 3
             X3 = ((TO_FM(4) - J)/4)*X1 + (1-(TO_FM(4) - J)/4)*X2
             F2 = F(X3,NF)
             IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                 X2 = X3
                 EXIT
             ENDIF
          ENDDO
          IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
              DO J = 1, 3
                 X3 = ((TO_FM(4) + J)/4)*X1 + (1-(TO_FM(4) + J)/4)*X2
                 F2 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
                 X3 = (1-(TO_FM(4) + J)/4)*X1 + ((TO_FM(4) + J)/4)*X2
                 F2 = F(X3,NF)
                 IF (.NOT. (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2)))) THEN
                     X2 = X3
                     EXIT
                 ENDIF
              ENDDO
          ENDIF
      ENDIF
      IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
          IF (KPRT >= 0) THEN
              WRITE (KU,*) ' '
              WRITE (KU,*) ' Invalid input for ZM_SECANT. ',  &
                           ' Unknown or overflowed function value for BX ='
              CALL ZM_PRINT(X2)
              WRITE (KU,*) ' '
          ENDIF
          J = 0
          X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
          ERR = TO_ZM(' UNKNOWN + UNKNOWN i ')
          GO TO 110
      ENDIF

!             Secant does not do well if the magnitude of the two starting function values differ
!             by too much.  Adjust if necessary.

      DO J = 1, 10
         IF (ABS(F2/F1) > 10) THEN
             X2 = (X1 + X2)/2
             F2 = F(X2,NF)
         ELSE IF (ABS(F1/F2) > 10) THEN
             X1 = (X1 + X2)/2
             F1 = F(X1,NF)
         ELSE
             EXIT
         ENDIF
      ENDDO

      IF (KPRT == 2) THEN
          STR = ZM_FORMAT('ES20.10','ES20.10',F1)
          WRITE (KU,"('  J =',I3,3X,'f(AX) = ',A,'    x:')") 0,TRIM(STR)
          CALL ZM_PRINT(X1)
          STR = ZM_FORMAT('ES20.10','ES20.10',F2)
          WRITE (KU,"('  J =',I3,3X,'f(BX) = ',A,'    x:')") 0,TRIM(STR)
          CALL ZM_PRINT(X2)
      ENDIF

!             This loop does the iteration.

      DO J = 1, MAXIT

         IF (F2-F1 /= 0.0) THEN
             X3 = X2 - F2*(X2-X1)/(F2-F1)
         ELSE
             X3 = X2 + 1
         ENDIF

!             Multiple roots cause very slow convergence and loss of accuracy.
!             If the slope is very small, try to improve convergence and accuracy by using
!             the (slower) function  f(x)/f'(x)  which has no multiple roots.

         X1OLD = X1
         F1OLD = F1
         IF ( (ABS((F2-F1)/(X2-X1)) < 1.0D-2 .AND. ABS(F2) < 1.0D-4 .AND.  &
               ABS(F2*(X2-X1)/(F2-F1)) < 1.0D-4*ABS(X2)) .OR. USE_F_OVER_FP) THEN
             USE_F_OVER_FP = .TRUE.
             X1 = X2
             X2 = X3
             F1 = F2
             F2 = ZM_FPRIME(0,X3,F,NF) / ZM_FPRIME(1,X3,F,NF)
         ELSE
             X1 = X2
             X2 = X3
             F1 = F2
             F2 = F(X3,NF)
         ENDIF

!             If F2 is one of the FM non-numbers, +-underflow, +-overflow, unknown,
!             then replace it by something representable, so that the next x3 will be
!             closer to x1.  Also swap x1 and x2, making the bad x go away first.

         IF (IS_UNKNOWN(ABS(F2)) .OR. IS_OVERFLOW(ABS(F2))) THEN
             F2 = -2*F1
             X3 = X1
             X1 = X2
             X2 = X3
             X3 = F1
             F1 = F2
             F2 = X3
         ENDIF

!             A common failure mode for secant is to get into a pattern that repeats x1 and x2
!             close together with nearly equal function values and x3 farther away with much
!             larger function value.  Check for this, and re-start the iteration by choosing
!             a different x3.

         IF (ABS(F2) > 100*MAX(ABS(F1OLD),ABS(F1)) .AND. J >= JSET) THEN
             IF (JSET >= 200) THEN
                 MAXIT = J
                 EXIT
             ENDIF
             JSET = JSET + 50
             CALL FM_RANDOM_NUMBER(VALUE)
             VALUE = 9*VALUE - 4
             X2 = VALUE*X1 + (1-VALUE)*X2
             F2 = F(X2,NF)
         ENDIF

         IF (KPRT == 2) THEN
             STR = ZM_FORMAT('ES20.10','ES20.10',F2)
             WRITE (KU,"('  J =',I3,4X,'f(x) = ' ,A,'    x:')") J,TRIM(STR)
             CALL ZM_PRINT(X2)
         ENDIF

         ERR1 = ERR
         IF (X2 /= 0.0) THEN
             ERR = ABS((X2-X1)/X2)
         ELSE
             ERR = ABS(X2-X1)
         ENDIF

!             If the error is less than the tolerance, double check to make sure the previous
!             error was small along with the current function value.  Some divergent iterations
!             can get err < tol without being close to a root.

         IF (ERR < TOL .OR. F2 == 0) THEN
             IF (ERR1 > SQRT(SQRT(TOL)) .AND. ABS(F2) > SQRT(EPSILON(TO_FM(1)))) THEN
                 IF (KPRT >= 0) THEN
                     WRITE (KU,"(/' Possible false convergence in ZM_SECANT after',I5,"//  &
                               "' iterations.  ','Last two approximations =')") J
                     CALL ZM_PRINT(X1)
                     CALL ZM_PRINT(X2)
                     WRITE (KU,"(/' These agree to the convergence tolerance, but the previous"//  &
                               " iteration was suspiciously far away:')")
                     CALL ZM_PRINT(X1OLD)
                     WRITE (KU,"(/' and the function value of the last iteration was"//  &
                               " suspiciously far from zero:')")
                     CALL ZM_PRINT(F2)
                     WRITE (KU,"(/' Unknown has been returned.')")
                 ENDIF
                 X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')
             ENDIF
             GO TO 110
         ENDIF

      ENDDO

!             No convergence after maxit iterations.

      IF (KPRT >= 0) THEN
          WRITE (KU,"(/' No convergence in ZM_SECANT after',I5,' iterations.  ',"//  &
                    "'Last two approximations =')") MAXIT
          CALL ZM_PRINT(X1)
          CALL ZM_PRINT(X2)
          WRITE (KU,"(/' Unknown has been returned.')")
      ENDIF
      X2 = TO_ZM(' UNKNOWN + UNKNOWN i ')

!             The root was found.

  110 CALL ZM_EQU(X2,ROOT,NDIG,NDSAVE)
      NDIG = NDSAVE
      IF (KPRT >= 1) THEN
          CALL FM_ULP(ABS(X2),ERR1)
          IF (.NOT.( IS_UNKNOWN(ERR1) .OR. IS_UNDERFLOW(ERR1) )) THEN
              ERR1 = ABS(ERR1/X2)/2
              IF (ERR < ERR1) ERR = ERR1
          ENDIF
          STR = FM_FORMAT('ES16.6',ERR)
          WRITE (KU,*) ' '
          WRITE (KU,"('  ZM_SECANT.   Function ',I3,I7,' iterations.'/17X"// &
                    "'Estimated relative error =',A,',    Root:')") NF,J,TRIM(STR)
          CALL ZM_PRINT(ROOT)
          WRITE (KU,*) ' '
      ENDIF

      KWARN = KWARN_SAVE
      END SUBROUTINE ZM_SECANT
