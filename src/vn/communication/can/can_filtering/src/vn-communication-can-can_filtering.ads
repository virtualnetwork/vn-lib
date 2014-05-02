-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Filtering keeps track of what the hardware filters of the CAN controller should be.
-- The purpose of this is to filter out all CAN messages that are not needed.

with VN;
with VN.Communication;
with VN.Communication.CAN;

package VN.Communication.CAN.CAN_Filtering is

   MAX_NUM_FILTERS : constant Integer := 16; -- ToDo: Check this number

   type CAN_Filter_Type is tagged limited private;
   type CAN_Filter_Access is access all CAN_Filter_Type;

   type Filter_ID_Type is range 1 .. MAX_NUM_FILTERS;

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
                        isUsed 	 : out Boolean);

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
