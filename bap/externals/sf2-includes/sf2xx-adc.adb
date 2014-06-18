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

with NXT.Registers; use NXT.Registers;

package body NXT.ADC is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Power-on.
      PMC_PCER := 2 ** AT91C_ID_ADC;

      --  Reset
      ADC_CR := ADC_SWRST;

      --  Mode register
      --  Set LOWRES, PRESCAL=10,
      ADC_MR := 2 ** 4 or 10 * 2 ** 8 or 20 * 2 ** 24;
   end Initialize;

   --------------------
   -- Enable_Channel --
   --------------------

   procedure Enable_Channel (N : ADC_Channel) is
   begin
      ADC_CHER := 2 ** N;
   end Enable_Channel;

   ---------------------
   -- Disable_Channel --
   ---------------------

   procedure Disable_Channel (N : ADC_Channel) is
   begin
      ADC_CHDR := 2 ** N;
   end Disable_Channel;

   -------------
   -- Convert --
   -------------

   procedure Convert is
      En : constant Unsigned_32 := ADC_CHSR;
   begin
      ADC_CR := ADC_START;
      loop
         exit when (ADC_SR and En) = En;
      end loop;
   end Convert;

   ------------------
   -- Read_Channel --
   ------------------

   function Read_Channel (N : ADC_Channel) return Unsigned_32 is
   begin
      return Unsigned_32 (ADC_CDR (N));
   end Read_Channel;

end NXT.ADC;
