------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                    Copyright (C) 2010-2011, AdaCore                      --
--                                                                          --
-- This is free software; you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. This is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  Root of the NXT hierarchy, the primary interface to the NXT and AVR

package NXT is
   pragma Pure (NXT);

   type Button_Id is
     (No_Button, Power_Button, Left_Button, Middle_Button, Right_Button);

   type Sensor_Id is (Sensor_1, Sensor_2, Sensor_3, Sensor_4);

   type Motor_Id is (Motor_A, Motor_B, Motor_C);

   type PWM_Value is range -100 .. 100;
   for PWM_Value'Size use 8;
   --  bounded pulse-width modulation values

   type Sensor_Power is (Standard_Power, RCX_9V, NXT_9V);

   type Color is (No_Color, Black, Blue, Green, Yellow, Red, White);

private

   Clock_Frequency : constant := 48_054_850;

   pragma Discard_Names (Button_Id);
   pragma Discard_Names (Sensor_Id);
   pragma Discard_Names (Motor_Id);
   pragma Discard_Names (Sensor_Power);
   pragma Discard_Names (Color);

end NXT;
