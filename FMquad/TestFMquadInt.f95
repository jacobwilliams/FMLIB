
!  This is a test program for version 1.4 of module FM_QUAD_INT, which contains the interface
!  routines allowing quadruple-length integer variables in the user's program to be used in
!  assignments, arithmetic, and comparisons involving type (fm), (im), and (zm) variables.
!  The same operations are provided as those in the basic module FMZM for single or double
!  precision variables.

!  All of the routines in module FM_QUAD_INT are tested, and if all tests are completed
!  successfully, this line is printed:

!  280 cases tested.  No errors were found.


      MODULE TEST_VARS

      USE FMVALS
      USE FMZM
      USE FM_QUAD_INT

      TYPE (FM), SAVE ::  M_A, MFM1, MFM2, MFM3, MFM4, MFM6,  &
                          MFMV1(3),  MFMV2(3),                &
                          MFMA(3,3), MFMB(3,3)

      TYPE (IM), SAVE :: M_J,  MIM1,  MIM2,  MIM3, MIM4, MIM5
      TYPE (IM), SAVE, DIMENSION(3)   :: MIMV1, MIMV2
      TYPE (IM), SAVE, DIMENSION(3,3) :: MIMA2, MIMB2

      TYPE (ZM), SAVE :: M_Z, MZM1, MZM2, MZM3, MZM4, MZM5,  &
                         MZMV1(3),   MZMV2(3),               &
                         MZMA2(3,3), MZMB2(3,3)

      INTEGER (QUAD_INT), SAVE :: QI1, QI2, QI3, QI4, QI5, QIV(3), QIV2(3,3)
      REAL, SAVE :: RV(3), RV2(3,3)
      DOUBLE PRECISION, SAVE :: DV(3), DV2(3,3)
      COMPLEX, SAVE :: CV(3), CV2(3,3)
      COMPLEX (KIND(0.0D0)), SAVE :: CDV(3), CDV2(3,3)

      INTEGER, SAVE :: KLOG, KWSAVE, NCASE, NERROR
      REAL, SAVE :: TIME1, TIME2

      END MODULE TEST_VARS

      MODULE TEST_A
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST1

!             Test the = assignment interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type = interface.')")

      NCASE = 1
      QI4 = MFM1
      IF (QI4 /= 581) CALL PRTERR(KW)

      NCASE = 2
      QI4 = MIM1
      IF (QI4 /= 661) CALL PRTERR(KW)

      NCASE = 3
      QI4 = MZM1
      IF (QI4 /= 731) CALL PRTERR(KW)

      NCASE = 4
      MFM3 = QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_SUB(MFM3,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      CALL FM_ABS(MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMPARE(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 5
      MFM3 = QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_SUB(MFM3,MFM4,MFM6)
      CALL FM_EQU(MFM6,MFM4,NDIG,NDIG)
      CALL FM_ABS(MFM4,MFM6)
      CALL FM_EQU(MFM6,MFM4,NDIG,NDIG)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMPARE(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 6
      MFM3 = QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_SUB_R2(MFM3,MFM4)
      CALL FM_ABS(MFM4,MFM6)
      CALL FM_EQU(MFM6,MFM4,NDIG,NDIG)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMPARE(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 7
      MFM3 = QI2
      MFM4 = TO_QUAD_INT(MFM3)
      CALL FM_SUB_R2(MFM3,MFM4)
      CALL FM_ABS(MFM4,MFM6)
      CALL FM_EQU(MFM6,MFM4,NDIG,NDIG)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMPARE(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 8
      MFM3 = QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_EQU(MFM3,MFM6,NDIG,NDIG)
      CALL FM_SUB_R1(MFM6,MFM4)
      CALL FM_EQU(MFM6,MFM4,NDIG,NDIG)
      CALL FM_ABS(MFM4,MFM6)
      CALL FM_EQU_R1(MFM6,NDIG,NDIG)
      CALL FM_EQ(MFM6,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMPARE(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 9
      MIM3 = QI2
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_SUB(MIM3,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      CALL IM_ST2M('0',MIM3)
      IF (IM_COMPARE(MIM4,'GT',MIM3)) CALL PRTERR(KW)

      NCASE = 10
      MIM3 = TO_IM(QI2)
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_SUB(MIM3,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      CALL IM_ST2M('0',MIM3)
      IF (IM_COMPARE(MIM4,'GT',MIM3)) CALL PRTERR(KW)

      NCASE = 11
      MIM3 = TO_IM(QI2)
      MIM4 = TO_QUAD_INT(MIM3)
      CALL IM_SUB(MIM3,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      CALL IM_ST2M('0',MIM3)
      IF (IM_COMPARE(MIM4,'GT',MIM3)) CALL PRTERR(KW)

      NCASE = 12
      MZM3 = QI2
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_SUB(MZM3,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      CALL ZM_ABS(MZM4,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMP(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 13
      MZM3 = TO_ZM(QI2)
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_SUB(MZM3,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      CALL ZM_ABS(MZM4,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMP(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 14
      MZM3 = TO_ZM(QI2)
      MZM4 = TO_QUAD_INT(MZM3)
      CALL ZM_SUB(MZM3,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      CALL ZM_ABS(MZM4,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMP(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      NCASE = 15
      QI1 = 123
      MZM3 = TO_ZM(QI1,QI2)
      CALL ZM_ST2M('123 + 123456789012345678901234567 i',MZM4)
      CALL ZM_SUB(MZM3,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      CALL ZM_ABS(MZM4,MFM4)
      CALL FM_ST2M('0',MFM3)
      IF (FM_COMP(MFM4,'GT',MFM3)) CALL PRTERR(KW)

      END SUBROUTINE TEST1

      SUBROUTINE TEST2

!  Test the derived type == interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type == interface.')")

      NCASE = 16
      QI1 = 123
      M_A = QI1
      IF (.NOT.(M_A == QI1)) THEN
          CALL ERRPRT_FM('  ==  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 17
      QI1 = 123
      M_A = QI1
      IF (.NOT.(QI1 == M_A)) THEN
          CALL ERRPRT_FM('  ==  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 18
      QI1 = 123
      M_J = QI1
      IF (.NOT.(M_J == QI1)) THEN
          CALL ERRPRT_IM('  ==  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 19
      QI1 = 123
      M_J = QI1
      IF (.NOT.(QI1 == M_J)) THEN
          CALL ERRPRT_IM('  ==  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 20
      QI1 = 123
      M_Z = QI1
      IF (.NOT.(M_Z == QI1)) THEN
          CALL ERRPRT_ZM('  ==  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 21
      QI1 = 123
      M_Z = ( 123.0 , 34.5 )
      IF (M_Z == QI1) THEN
          CALL ERRPRT_ZM('  ==  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 22
      QI1 = 123
      M_Z = QI1
      IF (.NOT.(QI1 == M_Z)) THEN
          CALL ERRPRT_ZM('  ==  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 23
      QI1 = 123
      M_Z = ( 123.0 , 34.5 )
      IF (QI1 == M_Z) THEN
          CALL ERRPRT_ZM('  ==  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      RETURN
      END SUBROUTINE TEST2

      SUBROUTINE TEST3

!  Test the derived type /= interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type /= interface.')")

      NCASE = 24
      QI1 = 123
      M_A = 1 + QI1
      IF (.NOT.(M_A /= QI1)) THEN
          CALL ERRPRT_FM('  /=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 25
      QI1 = 123
      M_A = 1 + QI1
      IF (.NOT.(QI1 /= M_A)) THEN
          CALL ERRPRT_FM('  /=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 26
      QI1 = 123
      M_J = 1 + QI1
      IF (.NOT.(M_J /= QI1)) THEN
          CALL ERRPRT_IM('  /=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 27
      QI1 = 123
      M_J = 1 + QI1
      IF (.NOT.(QI1 /= M_J)) THEN
          CALL ERRPRT_IM('  /=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 28
      QI1 = 123
      M_Z = 1 + QI1
      IF (.NOT.(M_Z /= QI1)) THEN
          CALL ERRPRT_ZM('  /=  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 29
      QI1 = 123
      M_Z = ( 123.0 , 34.5 )
      IF (.NOT.(M_Z /= QI1)) THEN
          CALL ERRPRT_ZM('  /=  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 30
      QI1 = 123
      M_Z = 1 + QI1
      IF (.NOT.(QI1 /= M_Z)) THEN
          CALL ERRPRT_ZM('  /=  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      NCASE = 31
      QI1 = 123
      M_Z = ( 123.0 , 34.5 )
      IF (.NOT.(QI1 /= M_Z)) THEN
          CALL ERRPRT_ZM('  /=  ',M_Z,'M_Z',M_Z,'M_Z',M_Z,'M_Z')
      ENDIF

      RETURN
      END SUBROUTINE TEST3

      SUBROUTINE TEST4

!  Test the derived type > interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type > interface.')")

      NCASE = 32
      QI1 = 123
      M_A = QI1 + 1
      IF (.NOT.(M_A > QI1)) THEN
          CALL ERRPRT_FM('   >  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 33
      QI1 = 123
      M_A = QI1 - 1
      IF (.NOT.(QI1 > M_A)) THEN
          CALL ERRPRT_FM('   >  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 34
      QI1 = 123
      M_J = QI1 + 1
      IF (.NOT.(M_J > QI1)) THEN
          CALL ERRPRT_IM('   >  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 35
      QI1 = 123
      M_J = QI1 - 1
      IF (.NOT.(QI1 > M_J)) THEN
          CALL ERRPRT_IM('   >  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      RETURN
      END SUBROUTINE TEST4

      SUBROUTINE TEST5

!  Test the derived type >= interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type >= interface.')")

      NCASE = 36
      QI1 = 123
      M_A = QI1 + 1
      IF (.NOT.(M_A >= QI1)) THEN
          CALL ERRPRT_FM('  >=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 37
      QI1 = 123
      M_A = QI1 - 1
      IF (.NOT.(QI1 >= M_A)) THEN
          CALL ERRPRT_FM('  >=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 38
      QI1 = 123
      M_J = QI1 + 1
      IF (.NOT.(M_J >= QI1)) THEN
          CALL ERRPRT_IM('  >=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 39
      QI1 = 123
      M_J = QI1 - 1
      IF (.NOT.(QI1 >= M_J)) THEN
          CALL ERRPRT_IM('  >=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      RETURN
      END SUBROUTINE TEST5

      SUBROUTINE TEST6

!  Test the derived type < interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type < interface.')")

      NCASE = 40
      QI1 = 123
      M_A = QI1 - 2
      IF (.NOT.(M_A < QI1)) THEN
          CALL ERRPRT_FM('   <  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 41
      QI1 = 123
      M_A = QI1 + 2
      IF (.NOT.(QI1 < M_A)) THEN
          CALL ERRPRT_FM('   <  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 42
      QI1 = 123
      M_J = QI1 - 2
      IF (.NOT.(M_J < QI1)) THEN
          CALL ERRPRT_IM('   <  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 43
      QI1 = 123
      M_J = QI1 + 2
      IF (.NOT.(QI1 < M_J)) THEN
          CALL ERRPRT_IM('   <  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      RETURN
      END SUBROUTINE TEST6

      SUBROUTINE TEST7

!  Test the derived type <= interface.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type <= interface.')")

      NCASE = 44
      QI1 = 123
      M_A = QI1 - 2
      IF (.NOT.(M_A <= QI1)) THEN
          CALL ERRPRT_FM('  <=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 45
      QI1 = 123
      M_A = QI1 + 2
      IF (.NOT.(QI1 <= M_A)) THEN
          CALL ERRPRT_FM('  <=  ',M_A,'M_A',M_A,'M_A',M_A,'M_A')
      ENDIF

      NCASE = 46
      QI1 = 123
      M_J = QI1 - 2
      IF (.NOT.(M_J <= QI1)) THEN
          CALL ERRPRT_IM('  <=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      NCASE = 47
      QI1 = 123
      M_J = QI1 + 2
      IF (.NOT.(QI1 <= M_J)) THEN
          CALL ERRPRT_IM('  <=  ',M_J,'M_J',M_J,'M_J')
      ENDIF

      RETURN
      END SUBROUTINE TEST7

      SUBROUTINE TEST8

!             Test the '+' arithmetic operator.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type + interface.')")

      NCASE = 48
      MFM3 = QI2 + MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_ADD(MFM4,MFM1,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 49
      MFM3 = QI2 + MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_ADD_R1(MFM4,MFM1)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 50
      MFM3 = QI2 + MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_ADD_R2(MFM1,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 51
      MIM3 = QI2 + MIM1
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_ADD(MIM4,MIM1,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 52
      MZM3 = QI2 + MZM1
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_ADD(MZM4,MZM1,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      NCASE = 53
      MFM3 = MFM1 + QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_ADD(MFM1,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 54
      MIM3 = MIM1 + QI2
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_ADD(MIM1,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 55
      MZM3 = MZM1 + QI2
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_ADD(MZM1,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      END SUBROUTINE TEST8

      SUBROUTINE TEST9

!             Test the '-' arithmetic operator.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type - interface.')")

      NCASE = 56
      MFM3 = QI2 - MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_SUB(MFM4,MFM1,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 57
      MIM3 = QI2 - MIM1
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_SUB(MIM4,MIM1,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 58
      MZM3 = QI2 - MZM1
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_SUB(MZM4,MZM1,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      NCASE = 59
      MFM3 = MFM1 - QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_SUB(MFM1,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 60
      MIM3 = MIM1 - QI2
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_SUB(MIM1,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 61
      MZM3 = MZM1 - QI2
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_SUB(MZM1,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      END SUBROUTINE TEST9

      END MODULE TEST_A

      MODULE TEST_B
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST10

!             Test the '*' arithmetic operator.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type * interface.')")

      NCASE = 62
      MFM3 = QI2 * MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_MPY(MFM4,MFM1,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 63
      MFM3 = QI2 * MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_MPY_R1(MFM4,MFM1)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 64
      MFM3 = QI2 * MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_MPY_R2(MFM1,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 65
      MFM3 = QI2 * MFM1
      MFM4 = MFM1 * TO_FM('123456789012345678901234567')
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 66
      MIM3 = QI2 * MIM1
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_MPY(MIM4,MIM1,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 67
      MZM3 = QI2 * MZM1
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_MPY(MZM4,MZM1,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      NCASE = 68
      MFM3 = MFM1 * QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_MPY(MFM1,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 69
      MIM3 = MIM1 * QI2
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_MPY(MIM1,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 70
      MZM3 = MZM1 * QI2
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_MPY(MZM1,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      END SUBROUTINE TEST10

      SUBROUTINE TEST11

!             Test the '/' arithmetic operator.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type / interface.')")

      NCASE = 71
      MFM3 = QI2 / MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_DIV(MFM4,MFM1,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 72
      MFM3 = QI2 / MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_EQ(MFM1,MFM6)
      CALL FM_DIV_R2(MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 73
      MIM3 = QI2 / MIM1
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_DIV(MIM4,MIM1,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 74
      MZM3 = QI2 / MZM1
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_DIV(MZM4,MZM1,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      NCASE = 75
      MFM3 = MFM1 / QI2
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_DIV(MFM1,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 76
      MIM3 = MIM1 / QI2
      CALL IM_ST2M('123456789012345678901234567',MIM4)
      CALL IM_DIV(MIM1,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 77
      MZM3 = MZM1 / QI2
      CALL ZM_ST2M('123456789012345678901234567',MZM4)
      CALL ZM_DIV(MZM1,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      END SUBROUTINE TEST11

      SUBROUTINE TEST12

!             Test the '**' arithmetic operator.

      IMPLICIT NONE

      WRITE (KW,"(/' Testing the derived type ** interface.')")

      NCASE = 78
      MFM3 = QI2 ** MFM1
      CALL FM_ST2M('123456789012345678901234567',MFM4)
      CALL FM_POWER(MFM4,MFM1,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 79
      QI4 = 2
      MIM3 = QI4 ** MIM1
      CALL IM_ST2M('2',MIM4)
      CALL IM_POWER(MIM4,MIM1,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 80
      QI4 = 23
      MZM3 = QI4 ** MZM1
      MZM4 = QI4
      CALL ZM_POWER(MZM4,MZM1,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      NCASE = 81
      QI4 = 2345
      MFM3 = MFM1 ** QI4
      MFM4 = QI4
      CALL FM_POWER(MFM1,MFM4,MFM6)
      CALL FM_EQ(MFM6,MFM4)
      IF (.NOT.(MFM3 == MFM4)) CALL PRTERR(KW)

      NCASE = 82
      QI4 = 17
      MIM3 = MIM1 ** QI4
      CALL IM_ST2M('17',MIM4)
      CALL IM_POWER(MIM1,MIM4,MIM5)
      CALL IM_EQ(MIM5,MIM4)
      IF (.NOT.(MIM3 == MIM4)) CALL PRTERR(KW)

      NCASE = 83
      QI4 = 179
      MZM3 = MZM1 ** QI4
      MZM4 = QI4
      CALL ZM_POWER(MZM1,MZM4,MZM5)
      CALL ZM_EQ(MZM5,MZM4)
      IF (.NOT.(MZM3 == MZM4)) CALL PRTERR(KW)

      END SUBROUTINE TEST12

      SUBROUTINE TEST13

!             Test functions TO_FM, TO_IM, TO_ZM, ..., TO_QUAD_INT.

      IMPLICIT NONE
      INTEGER :: J, K

      WRITE (KW,"(/' Testing the derived type TO_FM,  ..., TO_QUAD_INT interfaces.')")

      NCASE = 84
      QIV = (/  123,  -432,  567  /)
      MFMV1 = TO_FM(QIV)
      MFMV2 = QIV
      DO J = 1, 3
         IF (.NOT.(MFMV1(J) == MFMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 85
      QI3 = 123
      MFMV1 = QI3
      MFMV2 = 123
      DO J = 1, 3
         IF (.NOT.(MFMV1(J) == MFMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 86
      QIV = (/  123,  -432,  567  /)
      MFMV1 = TO_FM(QIV)
      QIV = TO_QUAD_INT(MFMV1)
      MFMV2 = QIV
      DO J = 1, 3
         IF (.NOT.(MFMV1(J) == MFMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 87
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFMA = TO_FM(QIV2)
      MFMB = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MFMA(J,K) == MFMB(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 88
      QI3 = 1234
      MFMA = QI3
      MFMB = 1234
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MFMA(J,K) == MFMB(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 89
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFMA = TO_FM(QIV2)
      QIV2 = TO_QUAD_INT(MFMA)
      MFMB = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MFMA(J,K) == MFMB(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 90
      QIV = (/  123,  -432,  567  /)
      MIMV1 = TO_IM(QIV)
      MIMV2 = QIV
      DO J = 1, 3
         IF (.NOT.(MIMV1(J) == MIMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 91
      QI3 = 1234
      MIMV1 = QI3
      MIMV2 = 1234
      DO J = 1, 3
         IF (.NOT.(MIMV1(J) == MIMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 92
      QIV = (/  123,  -432,  567  /)
      MIMV1 = TO_IM(QIV)
      QIV = TO_QUAD_INT(MIMV1)
      MIMV2 = QIV
      DO J = 1, 3
         IF (.NOT.(MIMV1(J) == MIMV2(J))) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 93
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = TO_IM(QIV2)
      MIMB2 = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MIMA2(J,K) == MIMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 94
      QI3 = 1234
      MIMA2 = QI3
      MIMB2 = 1234
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MIMA2(J,K) == MIMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 95
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = TO_IM(QIV2)
      QIV2 = TO_QUAD_INT(MIMA2)
      MIMB2 = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MIMA2(J,K) == MIMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 96
      QIV = (/  123,  -432,  567  /)
      MZMV1 = TO_ZM(QIV)
      MZMV2 = QIV
      DO J = 1, 3
         IF (ABS(MZMV1(J)-MZMV2(J)) /= 0) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 97
      QI3 = 1234
      MZMV1 = QI3
      MZMV2 = 1234
      DO J = 1, 3
         IF (ABS(MZMV1(J)-MZMV2(J)) /= 0) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 98
      QIV = (/  123,  -432,  567  /)
      MZMV1 = TO_ZM(QIV)
      QIV = TO_QUAD_INT(MZMV1)
      MZMV2 = QIV
      DO J = 1, 3
         IF (ABS(MZMV1(J)-MZMV2(J)) /= 0) THEN
             CALL PRTERR(KW)
             EXIT
         ENDIF
      ENDDO

      NCASE = 99
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZMA2 = TO_ZM(QIV2)
      MZMB2 = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MZMA2(J,K) == MZMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 100
      QI3 = 1234
      MZMA2 = QI3
      MZMB2 = 1234
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MZMA2(J,K) == MZMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 101
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZMA2 = TO_ZM(QIV2)
      QIV2 = TO_QUAD_INT(MZMA2)
      MZMB2 = QIV2
      DO J = 1, 3
         DO K = 1, 3
            IF (.NOT.(MZMA2(J,K) == MZMB2(J,K))) THEN
                CALL PRTERR(KW)
                EXIT
            ENDIF
         ENDDO
      ENDDO

      NCASE = 102
      CALL FMM2QI(MFM1%MFM,QI3)
      IF (TO_INT(MFM1) /= QI3) CALL PRTERR(KW)

      NCASE = 103
      CALL IMM2QI(MIM1%MIM,QI3)
      IF (TO_INT(MIM1) /= QI3) CALL PRTERR(KW)

      NCASE = 104
      CALL FMM2QI(MZM1%MZM(1),QI3)
      IF (TO_INT(MZM1) /= QI3) CALL PRTERR(KW)

      END SUBROUTINE TEST13

      SUBROUTINE TEST14

!             Test the derived-type interface routines that are not used elsewhere in this program.

      IMPLICIT NONE

      NCASE = 105
      QI4 = MFM1
      CALL FMM2QI(MFM1%MFM,QI5)
      IF (QI4 /= QI5) CALL PRTERR(KW)

      NCASE = 106
      MIM3 = MIM1 / 13
      QI5 = MIM1
      QI5 = QI5 / 13
      IF (.NOT.(MIM3 == QI5)) CALL PRTERR(KW)

      NCASE = 107
      QI4 = MOD(MIM1,TO_IM(13))
      QI5 = MOD(TO_QUAD_INT(MIM1),13_QUAD_INT)
      IF (QI4 /= QI5) CALL PRTERR(KW)

      NCASE = 108
      QI4 = MIM1
      CALL IMM2QI(MIM1%MIM,QI5)
      IF (QI4 /= QI5) CALL PRTERR(KW)

      NCASE = 109
      QI4 = MZM1
      CALL FMM2QI(MZM1%MZM(1),QI5)
      IF (QI4 /= QI5) CALL PRTERR(KW)

      END SUBROUTINE TEST14

      END MODULE TEST_B

      MODULE TEST_C
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST15

!  Test type (FM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J

      WRITE (KW,"(/' Testing derived-type array operations.')")

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')
      CALL FMSETVAR(' NTRACE = 0 ')

      NCASE = 110
      MFM3 = TO_FM('234.56')
      QIV = MFM3
      QI5 = 0
      DO J = 1, 3
         QI5 = QI5 + ABS(QIV(J) - 234)
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 111
      QIV = (/ 12, -34, 56 /)
      MFMV1 = QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV1(J) - QIV(J))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 112
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      QIV = MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(QIV(J) - INT(MFMV1(J)))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST15

      SUBROUTINE TEST16

!  Test type (IM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 113
      MIM1 = TO_FM('234.56')
      QIV = MIM1
      QI5 = 0
      DO J = 1, 3
         QI5 = QI5 + ABS(QIV(J) - 234)
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 114
      QIV = (/ 12, -34, 56 /)
      MIMV1 = QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV1(J) - QIV(J))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 115
      RV = (/ 12.1, -34.2, 56.3 /)
      MIMV1 = RV
      MFM3 = 0
      QIV = RV
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV1(J) - QIV(J))
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 116
      DV = (/ 12.1123456789D0, -34.2123456789D0, 56.3123456789D0 /)
      MIMV1 = DV
      MFM3 = 0
      QIV = DV
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV1(J) - QIV(J))
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 117
      CV = (/ (12.1,65.4) , (-34.2,54.3) , (56.3,-84.5) /)
      MIMV1 = CV
      MFM3 = 0
      QIV = CV
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV1(J) - QIV(J))
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 118
      CDV = (/ (12.1123456789D0,34.57D0) , (-34.2123456789D0,987.43D0) ,  &
               (56.3123456789D0,-465.84D0) /)
      MIMV1 = CDV
      MFM3 = 0
      QIV = CDV
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV1(J) - QIV(J))
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 119
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      QIV = MIMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(QIV(J) - INT(MIMV1(J)))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 120
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MIMV1 = MZMV1
      MFM3 = 0
      QIV = (/ 12, -34, 56 /)
      DO J = 1, 3
         MFM3 = MFM3 + ABS(QIV(J) - MIMV1(J))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST16

      SUBROUTINE TEST17

!  Test type (ZM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 121
      MZM1 = TO_ZM('234.56 - 65.32 i')
      QIV = MZM1
      QI5 = 0
      DO J = 1, 3
         QI5 = QI5 + ABS(QIV(J) - 234)
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 122
      QIV = (/ 12, -34, 56 /)
      MZMV1 = QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV1(J) - QIV(J))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 123
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      QIV = MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(QIV(J) - REAL(INT(MZMV1(J))))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST17

      SUBROUTINE TEST18

!  Test type (FM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')
      CALL FMSETVAR(' NTRACE = 0 ')

      NCASE = 124
      MFM3 = TO_FM('234.56')
      QIV2 = MFM3
      QI5 = 0
      DO J = 1, 3
         DO K = 1, 3
            QI5 = QI5 + ABS(QIV2(J,K) - 234)
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 125
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFMA = QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMA(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST18

      END MODULE TEST_C

      MODULE TEST_D
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST19

!  Test type (IM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 126
      MIM1 = TO_FM('234.56')
      QIV2 = MIM1
      QI5 = 0
      DO J = 1, 3
         DO K = 1, 3
            QI5 = QI5 + ABS(QIV2(J,K) - 234)
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 127
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 128
      RV2 = RESHAPE( (/(11.345+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = RV2
      MFM3 = 0
      QIV2 = RV2
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 129
      DV2 = RESHAPE( (/(12.3456789D0+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = DV2
      MFM3 = 0
      QIV2 = DV2
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 130
      DO J = 1, 3
         DO K = 1, 3
            CV2(J,K) = CMPLX(12.3+3*(J+3*(K-1)),-32.4+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MIMA2 = CV2
      MFM3 = 0
      QIV2 = CV2
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 131
      CDV2 = RESHAPE( (/(CMPLX(13.3D0+3*J,-22.4D0+7*J,KIND(1.0D0)),J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIMA2 = CDV2
      MFM3 = 0
      QIV2 = CDV2
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      IF (.NOT.(MFM3 == 0)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 132
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      QIV2 = MIMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(QIV2(J,K) - INT(MIMA2(J,K)))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 133
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MIMA2 = MZMA2
      MFM3 = 0
      QIV2 = RESHAPE( (/(62.3+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(QIV2(J,K) - MIMA2(J,K))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST19

      END MODULE TEST_D

      MODULE TEST_E
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST20

!  Test type (ZM) array equal assignments.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 134
      MZM1 = TO_ZM('234.56 - 65.32 i')
      QIV2 = MZM1
      QI5 = 0
      DO J = 1, 3
         DO K = 1, 3
            QI5 = QI5 + ABS(QIV2(J,K) - 234)
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(TO_FM(QI5) <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 135
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZMA2 = QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMA2(J,K) - QIV2(J,K))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 136
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      QIV2 = MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(QIV2(J,K) - REAL(INT(MZMA2(J,K))))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST20

      SUBROUTINE TEST21

!  Test type (FM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 137
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = MFM4 + QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFM4 + QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 138
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = QIV + MFM4
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) + MFM4 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST21

      SUBROUTINE TEST22

!  Test type (FM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 139
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QIV + MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) + MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 140
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QI3 + MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QI3 + MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 141
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 + QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) + QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 142
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 + QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) + QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST22

      SUBROUTINE TEST23

!  Test type (IM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 143
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = MIM2 + QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( MIM2 + QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 144
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = QIV + MIM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( QIV(J) + MIM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST23

      SUBROUTINE TEST24

!  Test type (IM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 145
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = QIV + MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QIV(J) + MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 146
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = QI3 + MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QI3 + MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 147
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = MIMV1 + QI3
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) + QI3 ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 148
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = MIMV1 + QIV
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) + QIV(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST24

      SUBROUTINE TEST25

!  Test type (ZM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 149
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = MZM2 + QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZM2 + QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 150
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = QIV + MZM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) + MZM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST25

      SUBROUTINE TEST26

!  Test type (ZM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 151
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QIV + MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) + MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 152
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QI3 + MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QI3 + MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 153
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 + QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) + QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 154
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 + QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) + QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST26

      SUBROUTINE TEST27

!  Test type (FM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 155
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = MFM4 + QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFM4 + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 156
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = QIV2 + MFM4
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) + MFM4 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST27

      SUBROUTINE TEST28

!  Test type (FM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 157
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QIV2 + MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) + MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 158
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QI3 + MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QI3 + MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 159
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA + QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) + QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 160
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA + QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST28

      SUBROUTINE TEST29

!  Test type (IM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 161
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = MIM2 + QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( MIM2 + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 162
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = QIV2 + MIM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( QIV2(J,K) + MIM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST29

      SUBROUTINE TEST30

!  Test type (IM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 163
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QIV2 + MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QIV2(J,K) + MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 164
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 + QIV2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 165
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QI3 + MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QI3 + MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 166
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 + QI3
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) + QI3 ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST30

      SUBROUTINE TEST31

!  Test type (ZM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 167
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = MZM2 + QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZM2 + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 168
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = QIV2 + MZM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) + MZM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST31

      END MODULE TEST_E

      MODULE TEST_F
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST32

!  Test type (ZM) array addition operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 169
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QIV2 + MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) + MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 170
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 + QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) + QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 171
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QI3 + MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QI3 + MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 172
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 + QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) + QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST32

      SUBROUTINE TEST33

!  Test type (FM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 173
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = MFM4 - QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFM4 - QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 174
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = QIV - MFM4
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) - MFM4 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST33

      SUBROUTINE TEST34

!  Test type (FM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 175
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QIV - MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) - MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 176
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 - QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) - QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 177
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QI3 - MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QI3 - MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 178
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 - QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) - QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 179
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QI3 - MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QI3 - MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 180
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA - QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) - QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST34

      SUBROUTINE TEST35

!  Test type (IM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 181
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = MIM2 - QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( MIM2 - QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 182
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = QIV - MIM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( QIV(J) - MIM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST35

      SUBROUTINE TEST36

!  Test type (IM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 183
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = QIV - MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QIV(J) - MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 184
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = MIMV1 - QIV
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) - QIV(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 185
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = QI3 - MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QI3 - MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 186
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = MIMV1 - QI3
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) - QI3 ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 187
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QI3 - MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QI3 - MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 188
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 - QI3
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) - QI3 ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST36

      SUBROUTINE TEST37

!  Test type (ZM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 189
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = MZM2 - QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZM2 - QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 190
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = QIV - MZM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) - MZM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST37

      SUBROUTINE TEST38

!  Test type (ZM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 191
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QIV - MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) - MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 192
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 - QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) - QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 193
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QI3 - MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QI3 - MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 194
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 - QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) - QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 195
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QI3 - MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QI3 - MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 196
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 - QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) - QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST38

      SUBROUTINE TEST39

!  Test type (FM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 197
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = MFM4 - QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFM4 - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 198
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = QIV2 - MFM4
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) - MFM4 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST39

      SUBROUTINE TEST40

!  Test type (FM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 199
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QIV2 - MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) - MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 200
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA - QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST40

      SUBROUTINE TEST41

!  Test type (IM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 201
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = MIM2 - QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( MIM2 - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 202
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = QIV2 - MIM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( QIV2(J,K) - MIM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST41

      SUBROUTINE TEST42

!  Test type (IM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 203
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QIV2 - MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QIV2(J,K) - MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 204
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 - QIV2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST42

      SUBROUTINE TEST43

!  Test type (ZM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 205
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = MZM2 - QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZM2 - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 206
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = QIV2 - MZM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) - MZM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST43

      END MODULE TEST_F

      MODULE TEST_G
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST44

!  Test type (ZM) array subtraction operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 207
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QIV2 - MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) - MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 208
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 - QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) - QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST44

      SUBROUTINE TEST45

!  Test type (FM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 209
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = MFM4 * QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFM4 * QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 210
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = QIV * MFM4
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) * MFM4 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST45

      SUBROUTINE TEST46

!  Test type (FM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 211
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QIV * MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) * MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 212
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 * QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) * QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 213
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QI3 * MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QI3 * MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 214
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 * QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) * QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 215
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QI3 * MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QI3 * MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 216
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA * QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) * QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST46

      SUBROUTINE TEST47

!  Test type (IM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 217
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = MIM2 * QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( MIM2 * QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 218
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = QIV * MIM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( QIV(J) * MIM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST47

      SUBROUTINE TEST48

!  Test type (IM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 219
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = QIV * MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QIV(J) * MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 220
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = MIMV1 * QIV
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) * QIV(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 221
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = QI3 * MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QI3 * MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 222
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = MIMV1 * QI3
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) * QI3 ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 223
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QI3 * MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QI3 * MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 224
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 * QI3
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) * QI3 ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST48

      SUBROUTINE TEST49

!  Test type (ZM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 225
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = MZM2 * QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZM2 * QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 226
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = QIV * MZM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) * MZM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST49

      SUBROUTINE TEST50

!  Test type (ZM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 227
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QIV * MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) * MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 228
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 * QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) * QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 229
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QI3 * MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QI3 * MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 230
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 * QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) * QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 231
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QI3 * MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QI3 * MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 232
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 * QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) * QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST50

      SUBROUTINE TEST51

!  Test type (FM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 233
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = MFM4 * QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFM4 * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 234
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = QIV2 * MFM4
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) * MFM4 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST51

      SUBROUTINE TEST52

!  Test type (FM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 235
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QIV2 * MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) * MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 236
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA * QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST52

      SUBROUTINE TEST53

!  Test type (IM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 237
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = MIM2 * QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( MIM2 * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 238
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = QIV2 * MIM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( QIV2(J,K) * MIM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST53

      SUBROUTINE TEST54

!  Test type (IM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 239
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QIV2 * MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QIV2(J,K) * MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 240
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 * QIV2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST54

      SUBROUTINE TEST55

!  Test type (ZM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 241
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = MZM2 * QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZM2 * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 242
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = QIV2 * MZM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) * MZM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST55

      SUBROUTINE TEST56

!  Test type (ZM) array multiplication operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 243
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QIV2 * MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) * MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 244
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 * QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) * QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST56

      END MODULE TEST_G

      MODULE TEST_H
      USE TEST_VARS

      CONTAINS

      SUBROUTINE TEST57

!  Test type (FM) array division operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 245
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = MFM4 / QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFM4 / QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 246
      QIV = (/ 12, -34, 56 /)
      MFM4 = TO_FM('12.1123456789')
      MFMV2 = QIV / MFM4
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) / MFM4 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST57

      SUBROUTINE TEST58

!  Test type (FM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 247
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QIV / MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QIV(J) / MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 248
      QIV = (/ 12, -34, 56 /)
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 / QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) / QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 249
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = QI3 / MFMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( QI3 / MFMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 250
      QI3 = 1234
      MFMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MFMV2 = MFMV1 / QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MFMV2(J) - ( MFMV1(J) / QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 251
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QI3 / MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QI3 / MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 252
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA / QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) / QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST58

      SUBROUTINE TEST59

!  Test type (IM) array division operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 253
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = MIM2 / QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( MIM2 / QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 254
      QIV = (/ 12, -34, 56 /)
      MIM2 = TO_FM('12.1123456789')
      MIMV2 = QIV / MIM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MIMV2(J) - ( QIV(J) / MIM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST59

      SUBROUTINE TEST60

!  Test type (IM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 255
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = QIV / MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QIV(J) / MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 256
      QIV = (/ 12, -34, 56 /)
      MIMV1 = (/ TO_FM('12.1123456789') , TO_FM('-34.2123456789') , TO_FM('56.3123456789') /)
      MIMV2 = MIMV1 / QIV
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) / QIV(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 257
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = QI3 / MIMV1
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( QI3 / MIMV1(J) ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 258
      QI3 = 1234
      MIMV1 = (/ TO_IM('121123456789') , TO_IM('-342123456789') , TO_IM('563123456789') /)
      MIMV2 = MIMV1 / QI3
      MIM1 = 0
      DO J = 1, 3
         MIM1 = MIM1 + ABS(MIMV2(J) - ( MIMV1(J) / QI3 ))
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 259
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QI3 / MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QI3 / MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 260
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 / QI3
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) / QI3 ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST60

      SUBROUTINE TEST61

!  Test type (ZM) array division operations.

      IMPLICIT NONE
      INTEGER :: J

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 261
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = MZM2 / QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZM2 / QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 262
      QIV = (/ 12, -34, 56 /)
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMV2 = QIV / MZM2
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) / MZM2 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST61

      SUBROUTINE TEST62

!  Test type (ZM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 263
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QIV / MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QIV(J) / MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 264
      QIV = (/ 12, -34, 56 /)
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 / QIV
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) / QIV(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 265
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = QI3 / MZMV1
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( QI3 / MZMV1(J) ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 266
      QI3 = 1234
      MZMV1 = (/ TO_ZM('12.1123456789 + 9.574635 i') , TO_ZM('-34.2123456789 - 5.4 i') ,  &
                 TO_ZM('56.3123456789 + 0.000345 i') /)
      MZMV2 = MZMV1 / QI3
      MFM3 = 0
      DO J = 1, 3
         MFM3 = MFM3 + ABS(MZMV2(J) - ( MZMV1(J) / QI3 ))
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 267
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QI3 / MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QI3 / MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 268
      QI3 = 1234
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 / QI3
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) / QI3 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST62

      SUBROUTINE TEST63

!  Test type (FM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 269
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = MFM4 / QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFM4 / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 270
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MFM4 = TO_FM('12.1123456789')
      MFMB = QIV2 / MFM4
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) / MFM4 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST63

      SUBROUTINE TEST64

!  Test type (FM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 271
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = QIV2 / MFMA
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( QIV2(J,K) / MFMA(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 272
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MFMA(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MFMB = MFMA / QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MFMB(J,K) - ( MFMA(J,K) / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST64

      SUBROUTINE TEST65

!  Test type (IM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 273
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = MIM2 / QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( MIM2 / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 274
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MIM2 = TO_FM('12.1123456789')
      MIMB2 = QIV2 / MIM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MIMB2(J,K) - ( QIV2(J,K) / MIM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST65

      SUBROUTINE TEST66

!  Test type (IM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 275
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = QIV2 / MIMA2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( QIV2(J,K) / MIMA2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 276
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MIMA2(J,K) = TO_FM(25+3*(J+3*(K-1)))/3
         ENDDO
      ENDDO
      MIMB2 = MIMA2 / QIV2
      MIM1 = 0
      DO J = 1, 3
         DO K = 1, 3
            MIM1 = MIM1 + ABS(MIMB2(J,K) - ( MIMA2(J,K) / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL IM_ST2M(' 0 ',MIM2)
      IF (.NOT.(MIM1 <= MIM2)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST66

      SUBROUTINE TEST67

!  Test type (ZM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 277
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = MZM2 / QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZM2 / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 278
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      MZM2 = TO_ZM('12.1123456789 - 53.837465 i')
      MZMB2 = QIV2 / MZM2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) / MZM2 ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST67

      SUBROUTINE TEST68

!  Test type (ZM) array division operations.

      IMPLICIT NONE
      INTEGER :: J, K

      KWSAVE = KW
      CALL FMSETVAR(' KW = 22 ')

      NCASE = 279
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = QIV2 / MZMA2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( QIV2(J,K) / MZMA2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      NCASE = 280
      QIV2 = RESHAPE( (/(11+3*J,J=1,9)/) , SHAPE = (/ 3,3 /) )
      DO J = 1, 3
         DO K = 1, 3
            MZMA2(J,K) = CMPLX(TO_FM('62.3')+3*(J+3*(K-1)), TO_FM('-72.4')+7*(J+3*(K-1)))
         ENDDO
      ENDDO
      MZMB2 = MZMA2 / QIV2
      MFM3 = 0
      DO J = 1, 3
         DO K = 1, 3
            MFM3 = MFM3 + ABS(MZMB2(J,K) - ( MZMA2(J,K) / QIV2(J,K) ))
         ENDDO
      ENDDO
      CALL FM_ST2M(' 1.0E-45 ',MFM4)
      IF (.NOT.(MFM3 <= MFM4)) THEN
          CALL PRTERR(KWSAVE)
      ENDIF

      KW = KWSAVE
      RETURN
      END SUBROUTINE TEST68

      END MODULE TEST_H

      PROGRAM TEST

      USE TEST_VARS
      USE TEST_A
      USE TEST_B
      USE TEST_C
      USE TEST_D
      USE TEST_E
      USE TEST_F
      USE TEST_G
      USE TEST_H
      IMPLICIT NONE

!             Write output to the standard FM output (unit KW, defined in subroutine FMSET),
!             and also to the file TestFMquadInt.out.

      KLOG = 18
      OPEN (KLOG,FILE='TestFMquadInt.out')
      KWSAVE = KW
      KW = KLOG

!             Set precision to give at least 50 significant digits and initialize the FM package.
!             This call also checks many of the initialization values used in module FMVALS
!             (file FMSAVE.f95).  Set KW = KLOG for this call so that any messages concerning these
!             values will appear in file TestFMquadInt.out.

      CALL FM_SET(50)
      KW = KWSAVE

!             Write output for testing error messages to the file FMerrmsgQI.OUT.

      OPEN (22,FILE='FMerrmsgQI.OUT')
      WRITE (22,*) ' '
      WRITE (22,*) ' This file is produced by the TestFMquadInt program while testing'
      WRITE (22,*) ' error messages and trace output options.'
      WRITE (22,*) ' '
      KW = 22
      CALL FMVARS
      WRITE (KW,*) ' RADIX(1_QUAD_INT) = ',RADIX(1_QUAD_INT),  &
                   '    DIGITS(1_QUAD_INT) = ',DIGITS(1_QUAD_INT)
      WRITE (KW,*) '    HUGE(1_QUAD_INT) = ',HUGE(1_QUAD_INT)
      KW = KWSAVE

      CALL CPU_TIME(TIME1)

!             Initialize some of the test variables.

      QI2 = 123456789012345678901234567_QUAD_INT
      CALL FM_ST2M('581.21',MFM1)
      CALL FM_ST2M('-572.42',MFM2)
      CALL IM_ST2M('661',MIM1)
      CALL IM_ST2M('-602',MIM2)
      CALL ZM_ST2M('731.51 + 711.41 i',MZM1)
      CALL ZM_ST2M('-762.12 - 792.42 i',MZM2)

!             NERROR is the number of errors found.

      NERROR = 0

!             Test the derived type = interface.

      CALL TEST1

!             Test the derived type == interface.

      CALL TEST2

!             Test the derived type /= interface.

      CALL TEST3

!             Test the derived type > interface.

      CALL TEST4

!             Test the derived type >= interface.

      CALL TEST5

!             Test the derived type < interface.

      CALL TEST6

!             Test the derived type <= interface.

      CALL TEST7

!             Test the derived type + interface.

      CALL TEST8

!             Test the derived type - interface.

      CALL TEST9

!             Test the derived type * interface.

      CALL TEST10

!             Test the derived type / interface.

      CALL TEST11

!             Test the derived type ** interface.

      CALL TEST12

!             Test the derived type functions TO_FM, TO_IM, TO_ZM, ..., TO_DPZ interface.

      CALL TEST13

!             Test the derived type functions ADDI, ..., Z2M interface.

      CALL TEST14

!             Test derived-type array equal assignments.

      CALL TEST15
      CALL TEST16
      CALL TEST17
      CALL TEST18
      CALL TEST19
      CALL TEST20

!             Test derived-type array addition operations.

      CALL TEST21
      CALL TEST22
      CALL TEST23
      CALL TEST24
      CALL TEST25
      CALL TEST26
      CALL TEST27
      CALL TEST28
      CALL TEST29
      CALL TEST30
      CALL TEST31
      CALL TEST32

!             Test derived-type array subtraction operations.

      CALL TEST33
      CALL TEST34
      CALL TEST35
      CALL TEST36
      CALL TEST37
      CALL TEST38
      CALL TEST39
      CALL TEST40
      CALL TEST41
      CALL TEST42
      CALL TEST43
      CALL TEST44

!             Test derived-type array multiplication operations.

      CALL TEST45
      CALL TEST46
      CALL TEST47
      CALL TEST48
      CALL TEST49
      CALL TEST50
      CALL TEST51
      CALL TEST52
      CALL TEST53
      CALL TEST54
      CALL TEST55
      CALL TEST56

!             Test derived-type array division operations.

      CALL TEST57
      CALL TEST58
      CALL TEST59
      CALL TEST60
      CALL TEST61
      CALL TEST62
      CALL TEST63
      CALL TEST64
      CALL TEST65
      CALL TEST66
      CALL TEST67
      CALL TEST68

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

          CALL FPVARS
          KW = KWSAVE
      ENDIF

      WRITE (KW,*) ' '
      WRITE (KW,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMquadInt.'
      WRITE (KW,*) ' '
      WRITE (KLOG,*) ' '
      WRITE (KLOG,"(F10.2,A)") TIME2-TIME1,' Seconds for TestFMquadInt.'
      WRITE (KLOG,*) ' '

      WRITE (KW,*)' End of run.'

      STOP
      END PROGRAM TEST

      SUBROUTINE ERRPRTFM(NROUT,M1,NAME1,M2,NAME2,M3,NAME3)

!  Print error messages for testing of real (FM) routines.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using FMST2M.
!  M3 is ABS(M1-M2), and ERRPRT is called if this is too big.
!  NAME1,NAME2,NAME3 are strings identifying which variables in the calling routine
!  correspond to M1,M2,M3.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      INTEGER :: M1,M2,M3

      CHARACTER(2) :: NAME1,NAME2,NAME3
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT
      WRITE (KLOG,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT

!             Temporarily change KW to KLOG so FMPRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL FMPRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL FMPRINT(M2)
      WRITE (KLOG,"(1X,A,' =')") NAME3
      CALL FMPRINT(M3)
      KW = KWSAVE
      RETURN
      END SUBROUTINE ERRPRTFM

      SUBROUTINE ERRPRTIM(NROUT,M1,NAME1,M2,NAME2)

!  Print error messages for testing of integer (IM) routines.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using IMST2M.
!  NAME1,NAME2 are strings identifying which variables in the calling routine correspond to M1,M2.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      INTEGER :: M1,M2

      CHARACTER(2) :: NAME1,NAME2
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT
      WRITE (KLOG,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT

!             Temporarily change KW to KLOG so IMPRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL IMPRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL IMPRINT(M2)
      KW = KWSAVE
      END SUBROUTINE ERRPRTIM

      SUBROUTINE ERRPRTZM(NROUT,M1,NAME1,M2,NAME2,M3,NAME3)

!  Print error messages.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using ZMST2M.
!  M3 is ABS(M1-M2), and ERRPRTZM is called if this is too big.
!  NAME1,NAME2,NAME3 are strings identifying which variables in the calling routine correspond
!  to M1,M2,M3.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      INTEGER :: M1(2),M2(2),M3(2)

      CHARACTER(2) :: NAME1,NAME2,NAME3
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT
      WRITE (KLOG,  &
          "(//' Error in case',I5,'.  The routine',' being tested was ',A6)"  &
          ) NCASE,NROUT

!             Temporarily change KW to KLOG so ZMPRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL ZMPRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL ZMPRINT(M2)
      WRITE (KLOG,"(1X,A,' =')") NAME3
      CALL ZMPRINT(M3)
      KW = KWSAVE
      END SUBROUTINE ERRPRTZM

      SUBROUTINE ERRPRT_FM(NROUT,M1,NAME1,M2,NAME2,M3,NAME3)

!  Print error messages for testing of TYPE (FM) interface routines.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using FMST2M.
!  M3 is ABS(M1-M2), and ERRPRT_FM is called if this is too big.
!  NAME1,NAME2,NAME3 are strings identifying which variables in the calling routine correspond
!  to M1,M2,M3.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      TYPE (FM) :: M1, M2, M3

      CHARACTER(3) :: NAME1,NAME2,NAME3
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
         "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
         ) NCASE,NROUT
      WRITE (KLOG,  &
         "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
         ) NCASE,NROUT

!             Temporarily change KW to KLOG so FM_PRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL FM_PRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL FM_PRINT(M2)
      WRITE (KLOG,"(1X,A,' =')") NAME3
      CALL FM_PRINT(M3)
      KW = KWSAVE
      END SUBROUTINE ERRPRT_FM

      SUBROUTINE ERRPRT_IM(NROUT,M1,NAME1,M2,NAME2)

!  Print error messages for testing of TYPE (IM) interface routines.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using IMST2M.
!  NAME1,NAME2 are strings identifying which variables in the calling routine correspond to M1,M2.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      TYPE (IM) :: M1, M2

      CHARACTER(3) :: NAME1,NAME2
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
        "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
        ) NCASE,NROUT
      WRITE (KLOG,  &
        "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
        ) NCASE,NROUT

!             Temporarily change KW to KLOG so IM_PRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL IM_PRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL IM_PRINT(M2)
      KW = KWSAVE
      END SUBROUTINE ERRPRT_IM

      SUBROUTINE ERRPRT_ZM(NROUT,M1,NAME1,M2,NAME2,M3,NAME3)

!  Print error messages for testing of TYPE (ZM) interface routines.

!  M1 is the value to be tested, as computed by the routine named NROUT.
!  M2 is the reference value, usually converted using ZMST2M.
!  M3 is ABS(M1-M2), and ERRPRT_ZM is called if this is too big.
!  NAME1,NAME2,NAME3 are strings identifying which variables in the calling routine correspond
!  to M1,M2,M3.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE

      TYPE (ZM) :: M1, M2, M3

      CHARACTER(3) :: NAME1,NAME2,NAME3
      CHARACTER(6) :: NROUT

      NERROR = NERROR + 1
      WRITE (KW,  &
        "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
        ) NCASE,NROUT
      WRITE (KLOG,  &
        "(//' Error in case',I5,'.  The interface',' being tested was ',A6)"  &
        ) NCASE,NROUT

!             Temporarily change KW to KLOG so ZM_PRINT will write to the log file.

      KWSAVE = KW
      KW = KLOG
      WRITE (KLOG,"(1X,A,' =')") NAME1
      CALL ZM_PRINT(M1)
      WRITE (KLOG,"(1X,A,' =')") NAME2
      CALL ZM_PRINT(M2)
      WRITE (KLOG,"(1X,A,' =')") NAME3
      CALL ZM_PRINT(M3)
      KW = KWSAVE
      END SUBROUTINE ERRPRT_ZM

      SUBROUTINE ERRPRT_STR(STRING1,STRING2)

!  Print error messages for testing of output formatting routines.

!  STRING1 is the output string, formatted by FM_FORM
!  STRING2 is the reference string to be compared to STRING1.

      USE FMVALS
      USE FMZM
      USE TEST_VARS
      IMPLICIT NONE
      CHARACTER(80) :: STRING1, STRING2

      NERROR = NERROR + 1
      WRITE (KW,*)   ' Error in case ',NCASE,' during input/output testing.'
      WRITE (KLOG,*) ' Error in case ',NCASE,' during input/output testing.'
      WRITE (KLOG,*) ' STRING1 = ',TRIM(STRING1)
      WRITE (KLOG,*) ' STRING2 = ',TRIM(STRING2)

      END SUBROUTINE ERRPRT_STR

      SUBROUTINE PRTERR(KW2)
      USE TEST_VARS
      IMPLICIT NONE
      INTEGER :: KW2

      WRITE (KW2,*) ' Error in case ',NCASE
      WRITE (KLOG,*) ' '
      WRITE (KLOG,*) ' Error in case ',NCASE
      NERROR = NERROR + 1
      END SUBROUTINE PRTERR
