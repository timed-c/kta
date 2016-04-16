/*****************************************************************************/
/*                 G E N E R A T E D       C    C O D E                      */
/*****************************************************************************/
/* KIELER - Kiel Integrated Environment for Layout Eclipse RichClient        */
/*                                                                           */
/* http://www.informatik.uni-kiel.de/rtsys/kieler/                           */
/* Copyright 2014 by                                                         */
/* + Christian-Albrechts-University of Kiel                                  */
/*   + Department of Computer Science                                        */
/*     + Real-Time and Embedded Systems Group                                */
/*                                                                           */
/* This code is provided under the terms of the Eclipse Public License (EPL).*/
/*****************************************************************************/
int bumper;
int accelerator;
int motor;
int stop;
int _GO;
int g0;
int g1;
int g2;
int PRE_g2;
int g3;
int g3b;
int g4;
int g5;
int g6;
int g7;
int g8;
int PRE_g8;
int g9;
int g10;
int g11;
int _condg5;
int _condg3;
int _condg9;
int g4_e1;
int g10_e2;
void reset(){
   _GO = 1;
   PRE_g2 = 0;
   PRE_g8 = 0;
   return;
}
void tick(){
   {
      TPP(1);
      g0 = _GO;
      if(g0){
         motor = 0;
         stop = 0;
      }
      TPP(2);
      g1 = g0;
      g3 =(PRE_g2);
      TPP(3);
      g9 =(PRE_g8);
      _condg9 = bumper;
      g10 =(g9&&_condg9);
      if(g10){
         stop = 1;
      }
      TPP(4);
      g3b = g3;
      TPP(5);
      _condg3 = stop;
      TPP(6);
      g5 =(g3b&&(!(_condg3)));
      _condg5 = accelerator;
      g6 =(g5&&_condg5);
      if(g6){
         getImage();
         motor = 1;
      }
      g2 =(g1||((g5&&(!(_condg5)))||g6));
      g4 =(g3b&&_condg3);
      if(g4){
         writeLog();
      }
      TPP(7);
      g7 = g0;
      g8 =((g9&&(!(_condg9)))||g7);
      TPP(8);
      g10_e2 =(!(g9));
      g4_e1 =(!(g3));
      g11 =((g4_e1||g4)&&((g10_e2||g10)&&(g4||g10)));
      if(g11){
         motor = 0;
      }
      TPP(9);
   }
   PRE_g2 = g2;
   PRE_g8 = g8;
   _GO = 0;
   return;
}
