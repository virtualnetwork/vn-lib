------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

--  Interface for Analog/Digital Converter.

with Interfaces; use Interfaces;

package NXT.ADC is

   subtype ADC_Channel is Natural range 0 .. 7;

   procedure Enable_Channel  (N : ADC_Channel);
   procedure Disable_Channel (N : ADC_Channel);

   pragma Inline (Enable_Channel);
   pragma Inline (Disable_Channel);

   procedure Convert;
   pragma Inline (Convert);
   --  Start conversion.  All values must be read before the next conversion.

   function Read_Channel (N : ADC_Channel) return Unsigned_32;
   pragma Inline (Read_Channel);

   procedure Initialize;

end NXT.ADC;
