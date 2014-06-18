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

with NXT.Sensor_Ports;
with NXT.Registers;     use NXT.Registers;
with NXT.Buffers;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;  use System.Storage_Elements;

pragma Warnings (Off);
with System.BB.Interrupts;
pragma Warnings (On);

package body NXT.I2C_Ports is

   type Port_States is
     (I2C_Disabled,        -- 0
      I2C_Idle,            -- 1
      I2C_Active_Idle,     -- 2
      I2C_Complete,        -- 3
      I2C_Release,         -- 4
      I2C_Begin,           -- 5
      I2C_New_Start,       -- 6
      I2C_New_Restart,     -- 7
      I2C_New_Read,        -- 8
      I2C_New_Write,       -- 9
      I2C_Restart1,        -- 10
      I2C_Start1,          -- 11
      I2C_Start2,          -- 12
      I2C_Delay,           -- 13
      I2C_Rx_Data1,        -- 14
      I2C_Rx_Data2,        -- 15
      I2C_Rx_End_Ack1,     -- 16
      I2C_Rx_End_Ack2,     -- 17
      I2C_Rx_End_Ack3,     -- 18
      I2C_Rx_Ack1,         -- 19
      I2C_Rx_Ack2,         -- 20
      I2C_Tx_Data1,        -- 21
      I2C_Tx_Data2,        -- 22
      I2C_Tx_Ack1,         -- 23
      I2C_Tx_Ack2,         -- 24
      I2C_Lego_End,        -- 25
      I2C_End_No_Release,  -- 26
      I2C_End_Release1,    -- 27
      I2C_End_Release2,    -- 28
      I2C_End_Release3,    -- 29
      I2C_Lego_Stop1,      -- 30
      I2C_Lego_Stop2,      -- 31
      I2C_Lego_Stop3);     -- 32
   --  relative order of the enumerals is critical (see function Busy)

   Device_Address_Size   : constant := 4; -- bytes
   Register_Address_Size : constant := 1; -- bytes

   subtype Full_Address_Index is
     Unsigned_16 range 1 .. Device_Address_Size + Register_Address_Size;

   type Full_Address is array (Full_Address_Index) of Unsigned_8;
   --  the combination of the device address byte(s) and the internal register
   --  address byte(s)

   Buffer_Size : constant := 32;

   subtype Buffer_Index is Integer range 1 .. Buffer_Size;

   type Byte_Buffer is array (Buffer_Index) of Unsigned_8;
   for Byte_Buffer'Component_Size use Unsigned_8'Size;  -- just to be sure

   Max_Partial_Transactions : constant := 5;

   subtype Transactions_Index is
     Integer range 1 .. Max_Partial_Transactions;

   type Partial_Transaction is
      record
         State            : Port_States;
         Bits_To_Transfer : Unsigned_16;
         Data_Ptr         : System.Address;
      end record;

   type Partial_Transactions_List is
     array (Transactions_Index) of Partial_Transaction;

   type I2C_Port is
      record
         Enabled           : Boolean := False;
         SCL_Pin           : Unsigned_32;  --  serial clock
         SDA_Pin           : Unsigned_32;  --  serial data
         Combined_Address  : Full_Address;
         Device_Address    : Unsigned_8;
         Buffer            : Byte_Buffer;
         Partials          : Partial_Transactions_List;
         Current_PT        : Transactions_Index := Transactions_Index'First;
         State             : Port_States;
         Data_Ptr          : System.Address; -- a byte-pointer
         Num_Bits          : Unsigned_16;
         Delay_Interval    : Integer;
         Bits              : Unsigned_8;
         Fault             : Boolean;
         Lego_Mode         : Boolean;
         Always_Active     : Boolean;
         No_Release        : Boolean;
         Bytes_To_Transfer : Positive;
      end record;

   type Known_Ports is array (Sensor_Id) of aliased I2C_Port;

   type Port_Reference is access all I2C_Port;
   for Port_Reference'Storage_Size use 0;

   package I2C_Ports is new NXT.Buffers (Port_Reference);

   subtype Port_Buffer is I2C_Ports.Buffer
     (Capacity => Sensor_Id'Pos (Sensor_Id'Last) + 1);

   type Double_Buffer_Index is new Unsigned_8 range 0 .. 1;

   type Double_Buffer is array (Double_Buffer_Index) of aliased Port_Buffer;

   All_Ports     : Known_Ports;
   Active_Ports  : Double_Buffer;
   Active_Buffer : Double_Buffer_Index := 0;

   type Byte_Reference is access all Unsigned_8;
   for Byte_Reference'Storage_Size use 0;

   type Buffer_Reference is access all Byte_Buffer;
   for Buffer_Reference'Storage_Size use 0;

   function As_Byte_Ref is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Byte_Reference);

   function As_Buffer_Ref is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Buffer_Reference);

   procedure Build_Active_List;
   --  Scans All_Ports looking for those that are active. Those that are
   --  found are inserted (by reference) into the next buffer in
   --  Active_Ports and then the Active_Buffer index is updated to refer to
   --  this next buffer in the double buffer.

   procedure Process_Active_Port (This : Port_Reference);
   --  process This active port so that bits are sent or received per the
   --  partial transactions it contains

   procedure Bang_Bits is new I2C_Ports.Iteration (Process_Active_Port);
   --  Call procedure Process_Active_Port for each active port, if any, in the
   --  buffer passed as argument. This I2C interface is a "bit-banging"
   --  implementation that sends and receives individual bits without dedicated
   --  hardware.

   ----------------
   -- Controller --
   ----------------

   protected Controller is

      procedure Initialize;

   private

      pragma Interrupt_Priority (System.Max_Interrupt_Priority);

      --  The I2C state machine is pumped by a timer interrupt running at 2x
      --  the bit speed
      procedure Timer_Interrupt_Handler;
      pragma Attach_Handler (Timer_Interrupt_Handler, AT91C_ID_TC0);

   end Controller;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Controller.Initialize;
   end Initialize;

   -----------------------
   -- Disable_All_Ports --
   -----------------------

   procedure Disable_All_Ports is
   begin
      for Port in Sensor_Id loop
         Disable (Port);
      end loop;
   end Disable_All_Ports;

   ------------------------
   -- Configure_I2C_Port --
   ------------------------

   procedure Configure_I2C_Port (Port : Sensor_Id; Mode : IO_Modes) is
      Pin_Mask : Unsigned_32;
      This     : I2C_Port renames All_Ports (Port);
      use NXT.Sensor_Ports;
   begin
      This.Enabled := True;
      This.SCL_Pin := Sensor_Pins (Port).Pins (Digital_0);
      This.SDA_Pin := Sensor_Pins (Port).Pins (Digital_1);
      Pin_Mask := This.SDA_Pin or This.SCL_Pin;
      This.State := I2C_Idle;
      --  Set clock pin for output, open collector driver, with pullups
      --  enabled. Set data to be enabled for output with pullups disabled.
      PIOA_SODR := Pin_Mask;
      PIOA_OER := Pin_Mask;
      PIOA_MDER := This.SCL_Pin;
      PIOA_PUDR := This.SDA_Pin;
      PIOA_PUER := This.SCL_Pin;
      --  If we are always active, we never drop below the ACTIVEIDLE state
      This.Lego_Mode := (Mode and Lego_Mode) /= 0;
      This.No_Release := (Mode and No_Release) /= 0;
      This.Always_Active := (Mode and Always_Active) /= 0;
      if This.Always_Active then
         This.State := I2C_Active_Idle;
         Build_Active_List;
      end if;
   end Configure_I2C_Port;

   -------------
   -- Disable --
   -------------

   procedure Disable (Port : Sensor_Id) is
      This     : I2C_Port renames All_Ports (Port);
      Pin_Mask : Unsigned_32;
      use NXT.Sensor_Ports;
   begin
      Pin_Mask := This.SDA_Pin or This.SCL_Pin;
      PIOA_ODR := Pin_Mask;
      This.Enabled := False;
      This.Current_PT := Transactions_Index'First;
      Build_Active_List;
      Reset (Port);
   end Disable;

   ----------
   -- Busy --
   ----------

   function Busy (Port : Sensor_Id) return Boolean is
      This : I2C_Port renames All_Ports (Port);
   begin
      if This.Enabled then
         return This.State > I2C_Complete;
      else
         return False;
      end if;
   end Busy;

   -------------
   -- Enabled --
   -------------

   function Enabled (Port : Sensor_Id) return Boolean is
      This : I2C_Port renames All_Ports (Port);
   begin
      return This.Enabled;
   end Enabled;

   -----------------------
   -- Start_Transaction --
   -----------------------

   procedure Start_Transaction
     (Port                   : Sensor_Id;
      Device_Address         : Unsigned_32;
      Register_Address       : Unsigned_32;
      Register_Address_Bytes : Unsigned_16;
      Write_Buffer           : System.Address;
      Bytes_To_Transfer      : Positive;
      Operation              : Transfer_Type;
      Result                 : out Integer)
   is
      PT   : Transactions_Index;
      This : I2C_Port renames All_Ports (Port);
   begin
      Result := 0;  -- success
      if not This.Enabled then
         Result := -1;
      elsif Busy (Port) then
         Result := -2;
      elsif Bytes_To_Transfer > Buffer_Size then
         Result := -4;
      elsif Register_Address_Bytes > Device_Address_Size then
         Result := -5;
      end if;
      if Result /= 0 then
         return;
      end if;

      PT := Transactions_Index'First;
      This.Current_PT := PT;

      if Register_Address_Bytes > 0 then
         declare
            Internal_Address : Unsigned_32 := Register_Address;
         begin
            This.Partials (PT).Bits_To_Transfer :=
              (Register_Address_Bytes + 1) * 8;
            --  copy internal address bytes, high bytes first, starting with
            --  the device address and then the internal register address
            --  byte(s)
            This.Combined_Address (1) := Unsigned_8 (Device_Address);
            for K in reverse 1 .. Register_Address_Bytes loop
               This.Combined_Address (K + 1) := Unsigned_8 (Internal_Address);
               Internal_Address := Shift_Right (Internal_Address, 8);
            end loop;
         end;

         This.Partials (PT).Data_Ptr := This.Combined_Address'Address;
         This.Partials (PT).State := I2C_New_Start;
         --  We add an extra stop for the odd Lego i2c sensor, but only when
         --  performing a read
         if Operation = Read and This.Lego_Mode then
            PT := PT + 1;
            This.Partials (PT).State := I2C_Lego_Stop1;
         end if;

         PT := PT + 1;
      end if;

      if (Register_Address_Bytes = 0) or (Operation = Read) then
         if Register_Address_Bytes > 0 then
            This.Partials (PT).State := I2C_New_Restart;
         else
            This.Partials (PT).State := I2C_New_Start;
         end if;
         if Operation = Read then
            This.Device_Address := Unsigned_8 (Device_Address) or 1;
         end if;
         This.Partials (PT).Data_Ptr := This.Device_Address'Address;
         This.Partials (PT).Bits_To_Transfer := 8;

         PT := PT + 1;
      end if;

      --  Set up the data transfer partial transaction
      if Operation = Write then
         This.Partials (PT).State := I2C_New_Write;
         for K in 1 .. Bytes_To_Transfer loop
            This.Buffer (K) := As_Buffer_Ref (Write_Buffer) (K);
         end loop;
      else
         This.Partials (PT).State := I2C_New_Read;
      end if;
      This.Partials (PT).Data_Ptr := This.Buffer'Address;
      This.Partials (PT).Bits_To_Transfer :=
         Unsigned_16 (Bytes_To_Transfer * 8);

      --  Sort out the final end state transaction
      PT := PT + 1;
      if This.Lego_Mode then
         if Operation = Write then
            This.Partials (PT).State := I2C_End_Release1;
         else
            This.Partials (PT).State := I2C_Lego_End;
         end if;
      else -- not in lego mode
         if This.No_Release then
            This.Partials (PT).State := I2C_End_No_Release;
         else
            This.Partials (PT).State := I2C_End_Release1;
         end if;
      end if;

      --  save the number of bytes for use for complete
      This.Bytes_To_Transfer := Bytes_To_Transfer;
      --  start the actual transaction
      This.State := I2C_Begin;
      if not This.Always_Active then
         Build_Active_List;
      end if;

      Result := 0;  -- success
   end Start_Transaction;

   --------------------------
   -- Complete_Transaction --
   --------------------------

   procedure Complete_Transaction
     (Port            : Sensor_Id;
      Incoming_Buffer : System.Address;
      Bytes_To_Read   : Positive;
      Result          : out Integer)
   is
      Data : constant Buffer_Reference := As_Buffer_Ref (Incoming_Buffer);

      Actual_Bytes_To_Read : Positive;
      This                 : I2C_Port renames All_Ports (Port);

      use type System.Address;
   begin
      if not This.Enabled then
         Result := -1;
         return;
      end if;
      if Busy (Port) then
         Result := -2;
         return;
      end if;
      if This.Fault then
         Result := -3;
         return;
      end if;
      if Bytes_To_Read > Buffer_Size then
         Result := -4;
         return;
      end if;
      if Incoming_Buffer = System.Null_Address then
         Result := -5;
         return;
      end if;

      if Bytes_To_Read > This.Bytes_To_Transfer then
         Actual_Bytes_To_Read := This.Bytes_To_Transfer;
      else
         Actual_Bytes_To_Read := Bytes_To_Read;
      end if;
      for K in 1 .. Actual_Bytes_To_Read loop
         Data (K) := This.Buffer (K);
      end loop;

      if not This.Always_Active then
         This.State := I2C_Idle;
         Build_Active_List;
      else
         This.State := I2C_Active_Idle;
      end if;

      Result := Actual_Bytes_To_Read;
   end Complete_Transaction;

   -----------------------
   -- Build_Active_List --
   -----------------------

   procedure Build_Active_List is
      Next_Buffer : Double_Buffer_Index;
      use I2C_Ports;
   begin
      Next_Buffer := Active_Buffer xor 1;

      Clear (Active_Ports (Next_Buffer));

      for Id in Sensor_Id loop
         declare
            Port : I2C_Port renames All_Ports (Id);
         begin
            if Port.Enabled and Port.State > I2C_Idle then
               Insert (Port'Access, Into => Active_Ports (Next_Buffer));
            end if;
         end;
      end loop;

      if Empty (Active_Ports (Next_Buffer)) then
         TC0_CCR := TC_CLKDIS;
      else  -- there are new active ports to process
         if Empty (Active_Ports (Active_Buffer)) then
            --  we didn't already have things set up to enable processing
            --  since there were no active ports previously, so do that now
            TC0_CCR := TC_CLKEN;  -- enable
            TC0_CCR := TC_SWTRG;  -- software trigger
         end if;
      end if;

      Active_Buffer := Next_Buffer;  -- atomic
   end Build_Active_List;

   ----------------
   -- Controller --
   ----------------

   protected body Controller is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
         Dummy : Unsigned_32;
         pragma Unreferenced (Dummy);
         Clock_Rate : constant := 9600;
         use System.BB.Interrupts;
      begin
         for Port in Sensor_Id loop
            All_Ports (Port).Enabled := False;
         end loop;

         PMC_PCER := 2 ** AT91C_ID_TC0;
         TC0_CCR := TC_CLKDIS;
         TC0_IDR := not 0;
         Dummy := TC0_SR;
         TC0_CMR := TC_CLKS_TIMER_DIV1_CLOCK or TC_CPCTRG;
         TC0_RC := (Clock_Frequency / 2) / (2 * Clock_Rate);
         TC0_IER := TC_CPCS;

         Enable_Interrupt (AT91C_ID_TC0);
      end Initialize;

      -----------------------------
      -- Timer_Interrupt_Handler --
      -----------------------------

      procedure Timer_Interrupt_Handler is
         Unused : Unsigned_32;
         pragma Unreferenced (Unused);
      begin
         Unused := TC0_SR;
         Bang_Bits (Active_Ports (Active_Buffer));
      end Timer_Interrupt_Handler;

   end Controller;

   -------------------------
   -- Process_Active_Port --
   -------------------------

   procedure Process_Active_Port (This : Port_Reference) is
      Lego_Delay : constant := 5;
   begin
      case This.State is
         when I2C_Disabled | I2C_Idle | I2C_Active_Idle | I2C_Complete =>
            null;

         when I2C_Release => -- Release the bus completely
            PIOA_ODR  := This.SDA_Pin or This.SCL_Pin;
            PIOA_SODR := This.SDA_Pin or This.SCL_Pin;
            This.State := I2C_Complete;

         when I2C_Begin => -- Start new transaction
            PIOA_OER := PIOA_OER or This.SDA_Pin or This.SCL_Pin;
            This.Fault := False;
            This.State := This.Partials (This.Current_PT).State;

         when I2C_New_Start =>  -- Start the current partial transaction
            declare
               This_Partial : Partial_Transaction
               renames This.Partials (This.Current_PT);
            begin
               This.Data_Ptr := This_Partial.Data_Ptr;
               This.Num_Bits := This_Partial.Bits_To_Transfer;
               This.Bits := As_Byte_Ref (This.Data_Ptr).all;
               PIOA_SODR := This.SDA_Pin;
               PIOA_OER := This.SDA_Pin;
               This.State := I2C_Start1;
            end;

         when I2C_New_Restart => -- Restart a new partial transaction
            declare
               This_Partial : Partial_Transaction
               renames This.Partials (This.Current_PT);
            begin
               PIOA_CODR := This.SCL_Pin;
               This.Data_Ptr := This_Partial.Data_Ptr;
               This.Num_Bits := This_Partial.Bits_To_Transfer;
               This.Bits := As_Byte_Ref (This.Data_Ptr).all;
               PIOA_SODR := This.SDA_Pin;
               PIOA_OER := This.SDA_Pin;
               This.State := I2C_Start1;
            end;

         when I2C_New_Read => -- Start the read partial transaction
            declare
               This_Partial : Partial_Transaction
               renames This.Partials (This.Current_PT);
            begin
               This.Data_Ptr := This_Partial.Data_Ptr;
               This.Num_Bits := This_Partial.Bits_To_Transfer;
               This.Bits := 0;
               if This.Lego_Mode then
                  PIOA_CODR := This.SCL_Pin;
                  --  get ready to read
                  PIOA_ODR := This.SDA_Pin;
                  This.State := I2C_Delay;
                  This.Delay_Interval := Lego_Delay;
               else
                  This.State := I2C_Rx_DATA1;
               end if;
               --  fall through to case I2C_RXDATA1
               if (PIOA_PDSR and This.SCL_Pin) /= 0 then
                  PIOA_CODR := This.SCL_Pin;
                  --  get ready to read
                  PIOA_ODR := This.SDA_Pin;
                  This.State := I2C_Rx_Data2;
               end if;
            end;

         when I2C_New_Write => -- Start the write partial transaction
            declare
               This_Partial : Partial_Transaction
               renames This.Partials (This.Current_PT);
            begin
               --  start the write partial transaction
               This.Data_Ptr := This_Partial.Data_Ptr;
               This.Num_Bits := This_Partial.Bits_To_Transfer;
               This.Bits := As_Byte_Ref (This.Data_Ptr).all;
               PIOA_OER := This.SDA_Pin;
               This.State := I2C_Tx_Data1;
               --  fall through to case I2C_TXDATA1
               if (PIOA_PDSR and This.SCL_Pin) /= 0 then
                  PIOA_CODR := This.SCL_Pin;
                  This.Num_Bits := This.Num_Bits - 1;
                  if (This.Bits and 16#80#) /= 0 then
                     PIOA_SODR := This.SDA_Pin;
                  else
                     PIOA_CODR := This.SDA_Pin;
                  end if;
                  PIOA_OER := This.SDA_Pin;
                  This.Bits := Shift_Left (This.Bits, 1);
                  This.State := I2C_Tx_Data2;
               end if;
            end;

         when I2C_Restart1 =>
            null;  -- by design

         when I2C_Start1 =>
            PIOA_SODR := This.SCL_Pin;
            This.State := I2C_Start2;

         when I2C_Start2 =>
            PIOA_CODR := This.SDA_Pin;
            This.State := I2C_Tx_Data1;

         when I2C_Delay =>
            if This.Delay_Interval = 0 then
               This.State := I2C_Rx_Data2;
            else
               This.Delay_Interval := This.Delay_Interval - 1;
            end if;

         when I2C_Rx_Data1 =>
            if (PIOA_PDSR and This.SCL_Pin) /= 0 then
               PIOA_CODR := This.SCL_Pin;
               --  get ready to read
               PIOA_ODR := This.SDA_Pin;
               This.State := I2C_Rx_Data2;
            end if;

         when I2C_Rx_Data2 =>
            PIOA_SODR := This.SCL_Pin;
            --  receive a bit
            This.Bits := Shift_Left (This.Bits, 1);
            if (PIOA_PDSR and This.SDA_Pin) /= 0 then
               This.Bits := This.Bits or 1;
            end if;
            This.Num_Bits := This.Num_Bits - 1;
            if (This.Num_Bits and 7) = 0 then
               As_Byte_Ref (This.Data_Ptr).all := This.Bits;
               if This.Num_Bits /= 0 then
                  This.State := I2C_Rx_Ack1;
               else
                  This.State := I2C_Rx_End_Ack1;
               end if;
            else
               This.State := I2C_Rx_Data1;
            end if;

         when I2C_Rx_End_Ack1 =>
            if (PIOA_PDSR and This.SCL_Pin) /= 0 then
               PIOA_CODR := This.SCL_Pin;
               PIOA_SODR := This.SDA_Pin;
               PIOA_OER := This.SDA_Pin;
               This.State := I2C_Rx_End_Ack2;
            end if;

         when I2C_Rx_End_Ack2 =>
            PIOA_SODR := This.SCL_Pin;
            This.Current_PT := This.Current_PT + 1;
            This.State := This.Partials (This.Current_PT).State;

         when I2C_Rx_End_Ack3 =>
            null; -- by design

         when I2C_Rx_Ack1 =>
            if (PIOA_PDSR and This.SCL_Pin) /= 0 then
               PIOA_CODR := This.SCL_Pin;
               PIOA_CODR := This.SDA_Pin;
               PIOA_OER := This.SDA_Pin;
               This.State := I2C_Rx_Ack2;
            end if;

         when I2C_Rx_Ack2 =>
            PIOA_SODR := This.SCL_Pin;
            --  Move on to next byte
            This.Data_Ptr := This.Data_Ptr + 1;
            This.Bits := 0;
            This.State := I2C_Rx_Data1;

         when I2C_Tx_Data1 =>
            if (PIOA_PDSR and This.SCL_Pin) /= 0 then
               PIOA_CODR := This.SCL_Pin;
               This.Num_Bits := This.Num_Bits - 1;
               --  drive the data line high or low corresponding to the value
               --  of the bit to send. this is the essential act of the
               --  bit-banging approach
               if (This.Bits and 16#80#) /= 0 then
                  PIOA_SODR := This.SDA_Pin;
               else
                  PIOA_CODR := This.SDA_Pin;
               end if;
               PIOA_OER := This.SDA_Pin;
               This.Bits := Shift_Left (This.Bits, 1);
               This.State := I2C_Tx_Data2;
            end if;

         when I2C_Tx_Data2 =>
            PIOA_SODR := This.SCL_Pin;
            if (This.Num_Bits and 7) = 0 then
               This.State := I2C_Tx_Ack1;
            else
               This.State := I2C_Tx_Data1;
            end if;

         when I2C_Tx_Ack1 =>
            --  Wait for high pulse width. If someone else is not holding
            --  the pin down, then advance
            if (PIOA_PDSR and This.SCL_Pin) /= 0 then
               --  Take SCL low
               PIOA_CODR := This.SCL_Pin;
               --  release the data line
               PIOA_ODR := This.SDA_Pin;
               This.State := I2C_Tx_Ack2;
            end if;

         when I2C_Tx_Ack2 =>
            --  Take SCL high
            PIOA_SODR := This.SCL_Pin;
            if (PIOA_PDSR and This.SDA_Pin) /= 0 then
               This.Fault := True;
               This.State := I2C_End_Release1;
            elsif This.Num_Bits = 0 then
               This.Current_PT := This.Current_PT + 1;
               This.State := This.Partials (This.Current_PT).State;
            else
               This.Data_Ptr := This.Data_Ptr + 1;
               This.Bits := As_Byte_Ref (This.Data_Ptr).all;
               This.State := I2C_Tx_Data1;
            end if;

         when I2C_Lego_End =>
            PIOA_CODR := This.SCL_Pin;
            This.State := I2C_End_Release2;

         when I2C_End_No_Release =>
            --  End the transaction but hold on to the bus, keeping the clock
            --  low.
            PIOA_CODR := This.SCL_Pin;
            PIOA_ODR := This.SDA_Pin;
            PIOA_SODR := This.SDA_Pin;
            This.State := I2C_COMPLETE;

         when I2C_End_Release1 =>
            --  Standard end case. issue stop and release the bus
            --  SCL is high, take it low
            PIOA_CODR := This.SCL_Pin;
            PIOA_CODR := This.SDA_Pin;
            PIOA_OER := This.SDA_Pin;
            This.State := I2C_End_Release2;

         when I2C_End_Release2 =>
            PIOA_SODR := This.SCL_Pin;
            This.State := I2C_End_Release3;

         when I2C_End_Release3 =>
            --  Take SDA pin high
            PIOA_SODR := This.SDA_Pin;
            This.State := I2C_Release;

         when I2C_Lego_Stop1 =>
            --  Special case stop used mid read in lego mode
            --  SCL is high, take it low
            PIOA_CODR := This.SCL_Pin;
            PIOA_CODR := This.SDA_Pin;
            PIOA_OER := This.SDA_Pin;
            This.State := I2C_LEGO_STOP2;

         when I2C_Lego_Stop2 =>
            --  Take SCL high
            PIOA_SODR := This.SCL_Pin;
            This.State := I2C_LEGO_STOP3;

         when I2C_Lego_Stop3 =>
            --  Take SDA pin high
            PIOA_SODR := This.SDA_Pin;
            This.Current_PT := This.Current_PT + 1;
            This.State := This.Partials (This.Current_PT).State;

      end case;
   end Process_Active_Port;

   ----------------------------------------------------
   --  automatically initialize this package's state --
   ----------------------------------------------------

begin
   Initialize;
end NXT.I2C_Ports;
