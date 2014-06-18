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

with Interfaces;     use Interfaces;
with NXT.Registers;  use NXT.Registers;
with NXT.Display;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;

pragma Warnings (Off);
with System.BB.Interrupts; use System.BB.Interrupts;
pragma Warnings (On);

package body NXT.TWI is

   protected TWI is

      procedure Send_Recv
        (Dev     : Device_Id;
         Adrlen  : Address_Length;
         Msg     : Address;
         Msg_Len : Natural;
         Is_Recv : Boolean);
      --  Send or receive a message to device DEV.
      --  The message musn't change until it has been completly sent or
      --  received.

      entry Wait;

      procedure Initialize;
      --  Initialize the twi.
   private
      pragma Interrupt_Priority (System.Max_Interrupt_Priority);

      procedure Isr;
      pragma Attach_Handler (Isr, AT91C_ID_TWI);
      --  Interrupt handler.

      Buf : Address;
      Buf_Len : Natural;
      --  Current buffer.

      Ready : Boolean := False;
      --  True if ready to receive or send data.
   end TWI;

   ---------
   -- TWI --
   ---------

   protected body TWI is

      ----------
      -- Wait --
      ----------

      entry Wait when Ready is
      begin
         null;
      end Wait;

      ---------------
      -- Send_Recv --
      ---------------

      procedure Send_Recv
        (Dev     : Device_Id;
         Adrlen  : Address_Length;
         Msg     : Address;
         Msg_Len : Natural;
         Is_Recv : Boolean)
      is
         Mode : Unsigned_32;
      begin
         Ready := False;

         Buf := Msg;
         Buf_Len := Msg_Len;

         --  Program master mode.
         Mode := Unsigned_32 (Dev * 2 ** 16 + Adrlen * 2 ** 8);
         if Is_Recv then
            Mode := Mode + TWI_MREAD;
         end if;
         TWI_MMR := Mode;

         --  Start.  Will set TXRDY.
         TWI_CR := TWI_MSEN or TWI_START;

         --  Enable interrupts.
         if Is_Recv then
            TWI_IER := TWI_NACK or TWI_RXRDY;
         else
            TWI_IER := TWI_NACK or TWI_TXRDY;
         end if;
      end Send_Recv;

      ---------
      -- Isr --
      ---------

      procedure Isr is
         use System.Storage_Elements;

         type Byte_Acc is access Unsigned_8;
         function To_Byte_Acc is new Ada.Unchecked_Conversion
           (Address, Byte_Acc);

         --  Read the status but filter only interesting bits.
         --  In particular, do not care about TX while reading.
         Status : constant Unsigned_32 := TWI_SR and TWI_IMR;
      begin
         if (Status and TWI_RXRDY) /= 0 then
            --  Byte received.
            if Buf_Len /= 0 then
               To_Byte_Acc (Buf).all := Unsigned_8 (TWI_RHR and 16#FF#);
               Buf := Buf + 1;
               Buf_Len := Buf_Len - 1;
               if Buf_Len = 1 then
                  --  Second last byte -- issue a stop on the next byte
                  TWI_CR := TWI_STOP;
               elsif Buf_Len = 0 then
                  Ready := True;
                  TWI_IDR := TWI_RXRDY;
               end if;
            end if;
         end if;

         if (Status and TWI_TXRDY) /= 0 then
            --  Can send a byte.
            if Buf_Len /= 0 then
               if Buf_Len = 1 then
                  TWI_CR := TWI_STOP;
               end if;
               TWI_THR := Unsigned_32 (To_Byte_Acc (Buf).all);
               Buf := Buf + 1;
               Buf_Len := Buf_Len - 1;
            else
               Ready := True;
               TWI_IDR := TWI_TXCOMP or TWI_TXRDY;
            end if;
         end if;

         if (Status and TWI_NACK) /= 0 then
            TWI_IDR := TWI_TXCOMP or TWI_TXRDY or TWI_TXRDY;
            Ready := True;
         end if;
      end Isr;

      -----------
      -- Reset --
      -----------

      procedure Reset is
         Pin_CL : constant Unsigned_32 := 2 ** 4;
         Pin_DA : constant Unsigned_32 := 2 ** 3;
      begin
         --  Disable all interrupts.
         TWI_IDR := not 0;

         --  Power on PIOA and TWI.
         PMC_PCER := 2 ** AT91C_ID_PIOA + 2 ** AT91C_ID_TWI;

         --  Disable PIO, select periph A, multi-driven enabled (open-drain).
         PIOA_MDER := Pin_CL or Pin_DA;
         PIOA_PDR := Pin_CL or Pin_DA;  --  Peripherical
         PIOA_ASR := Pin_CL or Pin_DA;  --  Periph A.

         --  Disable transfer and reset
         TWI_CR := TWI_SWRST or TWI_MSDIS;

         --  Set for 380kHz.
         --  MCK = 48Mhz, CHDIV = CLDIV = 15.
         --  Therefore, freq = MCK / 2 * ((15 * 2 ** 2) + 3) = 380.95Khz
         TWI_CWGR := 16#020F0F#;

         --  Master.
         TWI_CR := TWI_MSEN;

         Ready := True;
      end Reset;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Disable_Interrupt (AT91C_Id_TWI);
         TWI.Reset;
         Enable_Interrupt (AT91C_Id_TWI);
      end Initialize;

   end TWI;

   ----------
   -- Send --
   ----------

   procedure Send
     (Dev     : Device_Id;
      Adrlen  : Address_Length;
      Msg     : Address;
      Msg_Len : Natural)
   is
   begin
      TWI.Send_Recv (Dev, Adrlen, Msg, Msg_Len, False);
      TWI.Wait;
   end Send;

   ----------
   -- Recv --
   ----------

   procedure Recv
     (Dev     : Device_Id;
      Adrlen  : Address_Length;
      Msg     : Address;
      Msg_Len : Natural)
   is
   begin
      TWI.Send_Recv (Dev, Adrlen, Msg, Msg_Len, True);
      TWI.Wait;
   end Recv;

begin
   TWI.Initialize;
end NXT.TWI;
