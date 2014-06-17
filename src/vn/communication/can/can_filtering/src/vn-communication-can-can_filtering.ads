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

with VN;
with VN.Communication;
with VN.Communication.CAN;

package VN.Communication.CAN.CAN_Filtering is

   MAX_NUM_FILTERS : constant Integer := 6; -- ToDo: Check this number

   type CAN_Filter_Type is tagged limited private;
   type CAN_Filter_Access is access all CAN_Filter_Type;

   type Filter_ID_Type is range 0 .. MAX_NUM_FILTERS - 1;

   procedure Create_Filter(this : in out CAN_Filter_Type;
                           filterID : out Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID);

   procedure Change_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID);

   procedure Remove_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type);

   procedure Get_Filter(this : in CAN_Filter_Type;
                        filterID : Filter_ID_Type;
                        template : out CAN_message_ID;
                        mask 	 : out CAN_message_ID;
                        isUsed 	 : out Boolean;
                        hasChanged  : out Boolean);

   procedure Create_Transmission_Filter(this : in out CAN_Filter_Type;
                                        filterID   : out Filter_ID_Type;
                                        CANaddress : VN.Communication.CAN.CAN_Address_Receiver);

   procedure Change_To_Transmission_Filter(this : in out CAN_Filter_Type;
                                           filterID   : Filter_ID_Type;
                                           CANaddress : VN.Communication.CAN.CAN_Address_Receiver);
private

   type Filter_Type is
      record
         template : CAN_message_ID;
         mask 	  : CAN_message_ID;
         isUsed   : Boolean := false;
         hasChanged   : Boolean := false;
      end record;

   type Filter_Array_Type is array(Filter_ID_Type) of Filter_Type;

   type CAN_Filter_Type is tagged limited
      record
         myFilters 	 : Filter_Array_Type;
         overallTemplate : CAN_message_ID := 0;
         overallMask     : CAN_message_ID := 0;
         hasChanged	 : boolean := false;
      end record;

end VN.Communication.CAN.CAN_Filtering;
