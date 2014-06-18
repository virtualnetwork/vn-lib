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

with NXT.I2C_Ports;  use NXT.I2C_Ports;
with Ada.Real_Time;  use Ada.Real_Time;

package body NXT.I2C_Sensors is

   ----------
   -- Port --
   ----------

   function Port (This : I2C_Sensor) return Sensor_Id is
   begin
      return This.Port_Id;
   end Port;

   ------------
   -- Device --
   ------------

   function Device (This : I2C_Sensor) return Unsigned_32 is
   begin
      return This.Device_Address;
   end Device;

   -----------------
   -- Set_Port_Id --
   -----------------

   procedure Set_Port_Id (This : in out I2C_Sensor;  To : Sensor_Id) is
   begin
      This.Port_Id := To;
   end Set_Port_Id;

   ------------------------
   -- Set_Device_Address --
   ------------------------

   procedure Set_Device_Address (This : in out I2C_Sensor; To : Unsigned_32) is
   begin
      This.Device_Address := To;
   end Set_Device_Address;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data
     (This     : in out I2C_Sensor;
      Register : Unsigned_32;
      Buffer   : System.Address;
      Length   : Positive;
      Result   : out Integer)
   is
      Transaction_Reply : array (1 .. 32) of Unsigned_8;
   begin
      Start_Transaction
        (Port                   => This.Port,
         Device_Address         => This.Device,
         Register_Address       => Register,
         Register_Address_Bytes => 1,
         Write_Buffer           => Buffer,
         Bytes_To_Transfer      => Length,
         Operation              => Write,
         Result                 => Result);

      if Result /= 0 then
         return;
      end if;

      while Busy (This.Port) loop
         delay until Clock;
      end loop;

      Complete_Transaction
        (Port            => This.Port,
         Incoming_Buffer => Transaction_Reply'Address,
         Bytes_To_Read   => Length,
         Result          => Result);

      if Result >= 0 then
         if Result = Length then
            Result := 0;
         else
            Result := -1;
         end if;
      end if;
   end Send_Data;

   --------------
   -- Get_Data --
   --------------

   procedure Get_Data
     (This     : in out I2C_Sensor;
      Register : Unsigned_32;
      Buffer   : System.Address;
      Length   : Positive;
      Result   : out Integer)
   is
   begin
      Start_Transaction
        (Port                   => This.Port,
         Device_Address         => This.Device,
         Register_Address       => Register,
         Register_Address_Bytes => 1,
         Write_Buffer           => System.Null_Address,
         Bytes_To_Transfer      => Length,
         Operation              => Read,
         Result                 => Result);

      if Result /= 0 then
         return;
      end if;

      while Busy (This.Port) loop
         delay until Clock;
      end loop;

      Complete_Transaction
        (Port            => This.Port,
         Incoming_Buffer => Buffer,
         Bytes_To_Read   => Length,
         Result          => Result);

      if Result >= 0 then
         if Result = Length then
            Result := 0;
         else
            Result := -1;
         end if;
      end if;
   end Get_Data;

   ---------------------
   -- Get_Sensor_Type --
   ---------------------

   procedure Get_Sensor_Type
     (This    : in out I2C_Sensor;
      Result  : out Query_Reply;
      Success : out Boolean)
   is
   begin
      Fetch_String
        (This,
         Register => Sensor_Type_Register,
         Length   => Query_Reply'Length,
         Fetched  => Result,
         Success  => Success);
   end Get_Sensor_Type;

   --------------------
   -- Get_Product_Id --
   --------------------

   procedure Get_Product_Id
     (This    : in out I2C_Sensor;
      Result  : out Query_Reply;
      Success : out Boolean)
   is
   begin
      Fetch_String
        (This,
         Register => Product_Id_Register,
         Length   => Query_Reply'Length,
         Fetched  => Result,
         Success  => Success);
   end Get_Product_Id;

   -----------------
   -- Get_Version --
   -----------------

   procedure Get_Version
     (This    : in out I2C_Sensor;
      Result  : out Query_Reply;
      Success : out Boolean)
   is
   begin
      Fetch_String
        (This,
         Register => Version_Register,
         Length   => Query_Reply'Length,
         Fetched  => Result,
         Success  => Success);
   end Get_Version;

   ------------------
   -- Fetch_String --
   ------------------

   procedure Fetch_String
     (This     : in out I2C_Sensor;
      Register : Unsigned_32;
      Length   : Positive;
      Fetched  : out Query_Reply;
      Success  : out Boolean)
   is
      Buffer : aliased Query_Reply;
      Result : Integer;
   begin
      Get_Data
        (I2C_Sensor'Class (This),  -- redispatch if necessary
         Register,
         Buffer'Address,
         Length,
         Result);
      if Result /= 0 then
         Success := False;
         return;
      end if;
      for K in Fetched'Range loop
         if Buffer (K) = Character'Val (0) then
            Fetched (K) := ' ';
         else
            Fetched (K) := Buffer (K);
         end if;
      end loop;
      Success := True;
   end Fetch_String;

   --------------------------
   -- Fetch_Multiple_Bytes --
   --------------------------

   procedure Fetch_Multiple_Bytes
     (This          : in out I2C_Sensor;
      Base_Register : Unsigned_32;
      Output        : out Multiple_Bytes;
      Success       : out Boolean)
   is
      Buffer : aliased Unsigned_8;
      Result : Integer;
      Index  : Integer := Output'First;
   begin
      for Register_Offset in Unsigned_32 range 0 .. Output'Length - 1 loop
         Get_Data
           (I2C_Sensor'Class (This),  -- redispatch if necessary
            Base_Register + Register_Offset,
            Buffer'Address,
            Length => 1,
            Result => Result);
         if Result /= 0 then
            Success := False;
            return;
         else
            Output (Index) := Buffer;
            Index := Index + 1;
         end if;
      end loop;
      Success := True;
   end Fetch_Multiple_Bytes;

   ------------------------
   -- Set_Multiple_Bytes --
   ------------------------

   procedure Set_Multiple_Bytes
     (This          : in out I2C_Sensor;
      Base_Register : Unsigned_32;
      Input         : Multiple_Bytes;
      Success       : out Boolean)
   is
      Buffer : aliased Unsigned_8;
      Result : Integer;
      Index  : Integer := Input'First;
   begin
      for Register_Offset in Unsigned_32 range 0 .. Input'Length - 1 loop
         Buffer := Input (Index);
         Send_Data
           (I2C_Sensor'Class (This),  -- redispatch if necessary
            Base_Register + Register_Offset,
            Buffer'Address,
            Length => 1,
            Result => Result);
         if Result /= 0 then
            Success := False;
            return;
         end if;
         Index := Index + 1;
      end loop;
      Success := True;
   end Set_Multiple_Bytes;

end NXT.I2C_Sensors;
