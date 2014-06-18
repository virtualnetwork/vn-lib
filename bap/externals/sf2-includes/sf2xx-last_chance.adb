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

with NXT.Display;              use NXT.Display;
with System.Storage_Elements;  use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with NXT.Audio;

with GNAT.IO;

package body NXT.Last_Chance is

   use System;

   type Char_Pointer is access all Character;
   for Char_Pointer'Storage_Size use 0;

   function As_Char_Pointer is
      new Ada.Unchecked_Conversion (Integer_Address, Char_Pointer);

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler
     (Source_Location : System.Address;
      Line            : Integer)
   is
      K : Integer_Address;
   begin
      Newline;
      --Put_Line ("Exception!");
      GNAT.IO.Put_Line ("Exception!");
      NXT.Audio.Play_Tone (Frequency => 100, Interval => 500, Volume => 50);
      if Source_Location /= Null_Address then
         K := To_Integer (Source_Location);
         loop
            --Put (As_Char_Pointer (K).all);
            GNAT.IO.Put (As_Char_Pointer (K).all);
            K := K + 1;
            exit when As_Char_Pointer (K).all = ASCII.Nul;
         end loop;
         Newline;
         if Line /= 0 then
            --Put_Noupdate ("Line: "); Put_Noupdate (Line); Newline;
            GNAT.IO.Put("Line: "); 
            GNAT.IO.Put(Line); 
            GNAT.IO.Newline;
         end if;
      end if;

      pragma Warnings (Off);
      <<Spin>> goto Spin;
      pragma Warnings (On);
   end Last_Chance_Handler;

end NXT.Last_Chance;
