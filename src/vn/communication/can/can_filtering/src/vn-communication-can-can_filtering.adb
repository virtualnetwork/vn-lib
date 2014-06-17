------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- CAN_Filtering keeps track of what the hardware filters of the CAN controller should be.
-- The purpose of this is to filter out all CAN messages that are not needed.

-- Please note: The use of the hasChanged variable assumes that the
-- Get_Filter procedure is only read by one thread.

-- ToDo: CAN_Filtering has not yet been tested on acctual hardware due to problems with CAN drivers.

with Interfaces;
use Interfaces;

package body VN.Communication.CAN.CAN_Filtering is

   procedure Create_Filter(this : in out CAN_Filter_Type;
                           filterID : out Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID) is
      CREATE_FILTER_ERROR : exception;
   begin
      for i in this.myFilters'Range loop  -- ToDo: This search could be optimized
         if not this.myFilters(i).isUsed then
            this.myFilters(i).isUsed := true;
            this.myFilters(i).hasChanged := true;

            this.myFilters(i).template := template;
            this.myFilters(i).mask := mask;
            filterID := i;
            return;
         end if;
      end loop;
      raise CREATE_FILTER_ERROR; --ToDo, we should have a better way of handling when we run out of space...
   end Create_Filter;

   procedure Change_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID) is
   begin
      -- This if statement prevents error if the filterID was not initialized.
      -- I.e. read before written to.
    if filterID >= Filter_ID_Type'First and filterID <= Filter_ID_Type'Last then
         this.myFilters(filterID).template := template;
         this.myFilters(filterID).mask := mask;
         this.myFilters(filterID).isUsed := true;
         this.myFilters(filterID).hasChanged := true;
    end if;
   end Change_Filter;

   procedure Remove_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type) is
   begin
      -- This if statement prevents error if the filterID was not initialized.
      -- I.e. read before written to.
    if filterID >= Filter_ID_Type'First and filterID <= Filter_ID_Type'Last then
         this.myFilters(filterID).isUsed := false;
         this.myFilters(filterID).hasChanged := false;
    end if;
   end Remove_Filter;

   procedure Get_Filter(this : in CAN_Filter_Type;
                        filterID : Filter_ID_Type;
                        template : out CAN_message_ID;
                        mask 	 : out CAN_message_ID;
                        isUsed 	 : out Boolean;
                        hasChanged  : out Boolean) is
   begin
      template 	:= this.myFilters(filterID).template;
      mask 	:= this.myFilters(filterID).mask;
      isUsed 	:= this.myFilters(filterID).isUsed;
      hasChanged   := this.myFilters(filterID).hasChanged;
   end Get_Filter;

   procedure Create_Transmission_Filter(this : in out CAN_Filter_Type;
                                        filterID   : out Filter_ID_Type;
                                        CANaddress : VN.Communication.CAN.CAN_Address_Receiver) is
      template, mask   : VN.Communication.CAN.CAN_message_ID;
      POWER28 : constant Interfaces.Unsigned_32 := 2 ** 28;
   begin
      template := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(CANaddress),
                                                      VN.Communication.CAN.OFFSET_CAN_RECEIVER));

      mask := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(CAN_Address_Receiver'Last),
                                                  VN.Communication.CAN.OFFSET_CAN_RECEIVER) + POWER28);

      this.Create_Filter(filterID, template, mask);
   end Create_Transmission_Filter;

   procedure Change_To_Transmission_Filter(this : in out CAN_Filter_Type;
                                           filterID   : Filter_ID_Type;
                                           CANaddress : VN.Communication.CAN.CAN_Address_Receiver) is
      template, mask   : VN.Communication.CAN.CAN_message_ID;
      POWER28 : constant Interfaces.Unsigned_32 := 2 ** 28;
   begin
      template := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(CANaddress),
                                                      VN.Communication.CAN.OFFSET_CAN_RECEIVER));

      mask := VN.Communication.CAN.CAN_message_ID(Interfaces.Shift_Left(Interfaces.Unsigned_32(CAN_Address_Receiver'Last),
                                                  VN.Communication.CAN.OFFSET_CAN_RECEIVER) + POWER28);

      this.Change_Filter(filterID, template, mask);
   end Change_To_Transmission_Filter;

end VN.Communication.CAN.CAN_Filtering;
