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
int g2b;
int g3;
int g4;
int PRE_g4;
int g5;
int g5b;
int g6;
int g7;
int g8;
int g9;
int PRE_g9;
int g10;
int g11;
int g12;
int _condg2;
int _condg5;
int _condg6;
int _condg10;
int g3_e1;
int g11_e2;
void reset(){
   _GO = 1;
   PRE_g4 = 0;
   PRE_g9 = 0;
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
      g5 =(PRE_g4);
      TPP(3);
      g10 =(PRE_g9);
      _condg10 = bumper;
      g11 =(g10&&_condg10);
      if(g11){
         stop = 1;
      }
      TPP(4);
      g5b = g5;
      TPP(5);
      _condg5 = stop;
      TPP(6);
      g6 =(g5b&&(!(_condg5)));
      _condg6 = accelerator;
      g7 =(g6&&_condg6);
      if(g7){
         motor = 1;
      }
      g2 =(g7||g1);
      if(g2){
         getImage();
      }
      g2b = g2;
      TPP(7);
      _condg2 = stop;
      TPP(8);
      g3 =((g5b&&_condg5)||(g2b&&_condg2));
      if(g3){
         writeLog();
      }
      g4 =((g6&&(!(_condg6)))||(g2b&&(!(_condg2))));
      TPP(9);
      g8 = g0;
      g9 =((g10&&(!(_condg10)))||g8);
      TPP(10);
      g11_e2 =(!(g10));
      g3_e1 =(!(g5));
      g12 =((g3_e1||g3)&&((g11_e2||g11)&&(g3||g11)));
      if(g12){
         motor = 0;
      }
      TPP(11);
   }
   PRE_g4 = g4;
   PRE_g9 = g9;
   _GO = 0;
   return;
}
