*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
      SUBROUTINE ALPDR8 (DALP,ALP,LALP,LM,SINLAT,OLDE,KHEM)
      use alpnrt_comlrlm
      implicit none
***   ****   OCT 1975  -  JOHN D. HENDERSON  ****
***    DEC 23/93 - D.Dugas, RPN: Version Real*8.

***    CALCULATES N-S DERIVATIVES OF EACH ASSOCIATED LEGENDRE POLYNOMIAL

***    DALP WILL CONTAIN THE RESULT.
***    ALP CONTAINS ALL LEGENDRE POLYNOMIALS FOR ONE LATITUDE. 
***    EPSI IS NOW CALCULATED ON DEMAND BY ROUTINE EPSIL8. 
***    OLDE IS USED TO PRESERVE THE OLD CALLING SEQUENCE.

****   got rid of common blocks and Cray pointers  M.Valin 2022
****   introduced module alpnrt_comlrlm

      INTEGER   LM,LALP(LM),KHEM
      REAL*8    DALP(*),ALP(*),SINLAT
      REAL      OLDE

c       REAL*8    FNS,EPSI,ALPILM,AC,AD,AE
      REAL*8    FNS,ALPILM
c       INTEGER   MULT,MR,M,N,MN,LRLM,LR
      INTEGER   MULT,MR,M,N,MN,LR

c       COMMON   /COMLRLM/ LRLM
c       COMMON   /ALPNRT/ IC,ID,IE,IP


c       POINTER ( IC, AC(1) )
c       POINTER ( ID, AD(1) )
c       POINTER ( IE, AE(1) )
c       POINTER ( IP, EPSI(1) )

      EXTERNAL  EPSIL8
*-----------------------------------------------------------------------
      IF ((LALP(1)*1000+LM) .NE. LRLM)
     +    CALL EPSIL8( LALP,LM,KHEM )

                    MULT=1
      IF(KHEM.NE.0) MULT=2

      MR = 0

      DO 30 M=1,LM

         LR     = LALP(M)*MULT 
         ALPILM = 0.0
         DO 20 N=1,LR
            MN       = MR+N 
            FNS      = M+N-2
            DALP(MN) = (FNS+FNS+1.)*EPSI(MN)*ALPILM-FNS*SINLAT*ALP(MN)
            ALPILM   = ALP(MN)
   20    CONTINUE

         MR = MR+LR
   30 CONTINUE

      RETURN
      END 
