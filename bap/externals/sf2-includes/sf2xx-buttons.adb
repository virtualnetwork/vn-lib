------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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

with Interfaces;  use Interfaces;
with NXT.AVR;

package body NXT.Buttons is

   function Decoded_Button (From : Unsigned_16) return Button_Id;
   --  Returns the currently active button indicated by From.
   --  We arbitrarily assume only one button is pushed, i.e., no chording.

   --------------------
   -- Current_Button --
   --------------------

   function Current_Button return Button_Id is
   begin
      return Decoded_Button (NXT.AVR.Raw_Buttons);
   end Current_Button;

   --------------------
   -- Decoded_Button --
   --------------------

   function Decoded_Button (From : Unsigned_16) return Button_Id is
   begin
      if From >= 16#7FF# then
         return Power_Button;
      end if;
      if From > 16#300# then
         return Middle_Button;
      elsif From > 16#100# then
         return Right_Button;
      elsif From > 16#60# then
         return Left_Button;
      end if;
      return No_Button;
   end Decoded_Button;

end NXT.Buttons;
