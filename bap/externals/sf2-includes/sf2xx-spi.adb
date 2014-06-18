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

with Interfaces;     use Interfaces;
with NXT.Registers;  use NXT.Registers;

pragma Warnings (Off);
with System.BB.Interrupts; use System.BB.Interrupts;
pragma Warnings (On);

with Ada.Unchecked_Conversion;

package body NXT.SPI is

   Pin_Sck : constant := 2 ** 14;       --  SPCK
   Pin_Si  : constant := 2 ** 13;       --  MOSI
   Pin_Cd  : constant := 2 ** 12;       --  SOBT / MISO
   Pin_Cs  : constant := 2 ** 10;       --  CSDIS / NPCS2

   protected SPI is

      procedure Send (Is_Data : Boolean; Buf : Address; Len : Natural);
      --  Send a message.
      --  The message musn't change until it has been completly sent.

      entry Wait;

      procedure Initialize;
      --  Initialize the SPI.
   private
      pragma Interrupt_Priority (System.Max_Interrupt_Priority);

      procedure Isr;
      pragma Attach_Handler (Isr, AT91C_ID_SPI);
      --  Interrupt handler.

      Ready : Boolean := False;
      --  True if ready to receive or send data.
   end SPI;

   ---------
   -- SPI --
   ---------

   protected body SPI is

      ----------
      -- Wait --
      ----------

      entry Wait when Ready is
      begin
         null;
      end Wait;

      ----------
      -- Send --
      ----------

      procedure Send (Is_Data : Boolean;
                      Buf : Address;
                      Len : Natural)
      is
         function To_Unsigned_32 is new Ada.Unchecked_Conversion
           (Address, Unsigned_32);
      begin
         if Is_Data then
            PIOA_SODR := Pin_Cd;
         else
            PIOA_CODR := Pin_Cd;
         end if;

         Ready := False;

         SPI_TNPR := To_Unsigned_32 (Buf);
         SPI_TNCR := Unsigned_32 (Len);
         SPI_IER := SPI_ENDTX;
      end Send;

      ---------
      -- Isr --
      ---------

      procedure Isr is
         Status : constant Unsigned_32 := SPI_SR;
      begin
         if (Status and SPI_TXEMPTY) /= 0 then
            Ready := True;
            SPI_IDR := SPI_ENDTX or SPI_TXEMPTY;
         elsif (Status and SPI_ENDTX) /= 0 then
            SPI_IER := SPI_TXEMPTY;
            SPI_IDR := SPI_ENDTX;
         end if;
      end Isr;

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         --  Power on PIOA and SPI.
         PMC_PCER := 2 ** AT91C_ID_PIOA + 2 ** AT91C_ID_SPI;

         --  Route MOSI and clk to SPI.
         PIOA_PDR := Pin_Sck or Pin_Si;
         PIOA_ASR := Pin_Sck or Pin_Si;

         --  Route CS and CD to PIO.
         PIOA_PER  := Pin_Cs or Pin_Cd; --  PIO
         PIOA_OER  := Pin_Cs or Pin_Cd; --  Open-drain ?
         PIOA_SODR := Pin_Cs;
         PIOA_CODR := Pin_Cd;
         PIOA_MDDR := Pin_Cs or Pin_Cd; --  Not multi-driven

         SPI_CR := SPI_SWRST;
         SPI_CR := SPI_SPIEN;
         SPI_MR := 6 * SPI_DLYBCS or SPI_MSTR;  --  Master
         SPI_CSR0 := 16#18181801#; --  8 bits.
         SPI_CSR1 := 16#18181801#; --  8 bits.
         SPI_CSR2 := 16#18181801#; --  8 bits.
         SPI_CSR3 := 16#18181801#; --  8 bits.

         --  Select uc1601.
         PIOA_CODR := Pin_Cs;

         SPI_PTCR := SPI_TXTEN;
      end Init;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Disable_Interrupt (AT91C_Id_Spi);
         SPI.Init;
         Enable_Interrupt (AT91C_Id_Spi);
      end Initialize;

   end SPI;

   ----------
   -- Send --
   ----------

   procedure Send (Is_Data : Boolean;
                   Buf : Address;
                   Len : Natural)
   is
   begin
      SPI.Send (Is_Data, Buf, Len);
      SPI.Wait;
   end Send;

begin
   SPI.Initialize;
end NXT.SPI;
