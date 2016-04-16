/*****************************************************************************/
/*                 G E N E R A T E D       C    C O D E                      */
/*****************************************************************************/
/* KIELER - Kiel Integrated Environment for Layout Eclipse RichClient        */
/*                                                                           */
/* http://www.informatik.uni-kiel.de/rtsys/kieler/                           */
/* Copyright 2014 by                                                         */
/* + Kiel University                                  */
/*   + Department of Computer Science                                        */
/*     + Real-Time and Embedded Systems Group                                */
/*                                                                           */
/* This code is provided under the terms of the Eclipse Public License (EPL).*/
/*****************************************************************************/

/* Header file for Timing program points */
#include "tpp.h"

/* Stub implementations of called functions for stand alone testing of this file */
int dummy;
void __attribute__ ((noinline)) errorLog(){dummy = 0;}

void __attribute__ ((noinline)) writeLog(){dummy = 0;}

void __attribute__ ((noinline)) getImage(){dummy = 0;}

int bumper;
int accelerator;
int motor;
int _GO;
int g0;
int g1;
int PRE_g1;
int g2;
int g3;
int g4;
int PRE_g4;
int g5;
int g6;
int g7;
void reset(){
   _GO = 1;
   PRE_g1 = 0;
   PRE_g4 = 0;
   return;
}
void tick(){
   {
      g0 = _GO;
      TPP(1);
      g1 =(_GO&&bumper);
      g2 =(PRE_g1);
      if(g2){
         errorLog();
         motor = 0;
      }
      TPP(2);
      g5 =(PRE_g4);
      g6 =(g5&&accelerator);
      if(g6){
         motor = 1;
      }
      g7 =(g5&&(!(accelerator)));
      if(g7){
         writeLog();
         motor = 0;
      }
      TPP(3);
      g3 =(g6||(g2||g7));
      TPP(4);
      g4 =(_GO&&(!(bumper)));
      if(g4){
         getImage();
      }
      TPP(5);
   }
   PRE_g1 = g1;
   PRE_g4 = g4;
   _GO = 0;
   return;
}
