------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                        Copyright (C) 2011, AdaCore                       --
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

with NXT.AVR;

package body NXT.Battery is

   ----------------
   -- Millivolts --
   ----------------

   function Millivolts return Unsigned_32 is
      Result : Unsigned_32;
   begin
      Result := Unsigned_32 (NXT.AVR.Raw_Battery);
      Result := Result and 16#3FF#;
      --  The units are 13.848 mV per bit.
      --  We substitute 14180/1024 for 13.848
      Result := Result * 14180;
      Result := Shift_Right (Result, 10);
      return Result;
   end Millivolts;

   -------------
   -- Volts --
   -------------

   function Volts return Float is
   begin
      return Float (Millivolts) * 0.001;
   end Volts;

   ------------------
   -- Rechargeable --
   ------------------

   function Rechargeable return Boolean is
   begin
      return (NXT.AVR.Raw_Battery and 16#8000#) /= 0;
   end Rechargeable;

end NXT.Battery;
