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

-- Summary: Logical_Address_Handler keeps track of which logical addresses 
-- that exist above the CAN subnet (i.e. messages sent to these logical
-- addresses can be routed via this unit).

with VN.Communication.Routing_Table;
with VN.Communication.CAN.Logic;

with Ada.Real_Time;

package VN.Communication.CAN.Logic.Logical_Address_Handler is

   type Logical_Address_Handler is
     new VN.Communication.CAN.Logic.Duty with private;

   type Logical_Address_Handler_ptr is access all Logical_Address_Handler'Class;

   overriding procedure Update(this : in out Logical_Address_Handler; 
                               msgIn : VN.Communication.CAN.CAN_Message_Logical;      bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   procedure Received_From_Address(this : in out Logical_Address_Handler; Address : VN.VN_Logical_Address);

   procedure Sent_From_Address(this : in out Logical_Address_Handler; Address : VN.VN_Logical_Address);

   procedure Activate(this : in out Logical_Address_Handler; 
                      theCANAddress : VN.Communication.CAN.CAN_Address_Sender);

private

   package Bool_Routing is new VN.Communication.Routing_Table(Boolean);
   use Bool_Routing;

   package Address_Buffers is new Buffers(VN.VN_Logical_Address);
   use Address_Buffers;

   -- ToDo: This should be put in a config file of some sort:
   BOOL_ROUTING_TABLE_SIZE     : constant VN.VN_Logical_Address := 500;
   LOGICAL_ARRAY_SIZE 	       : constant Integer := 500;
   ADDRESS_DISTRIBUTION_PERIOD : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Seconds(2);

   type Logical_Array is array(1 .. LOGICAL_ARRAY_SIZE) of VN.VN_Logical_Address; 

   type Logical_Address_Handler_State is (Unactivated, Activated, Distributing);

   type Logical_Address_Handler is
     new VN.Communication.CAN.Logic.Duty with
      record         
         -- Keeps track of addresses we have received from, we cannot route messages to these
         -- (Routing table (a hash table) used for performance reasons.)
         hasReceivedFrom : Bool_Routing.Table_Type(BOOL_ROUTING_TABLE_SIZE);
         
         --Keeps track of addresses we have sent to but not received from, 
         -- we can route messages to these
         hasSentTo 	 : Bool_Routing.Table_Type(BOOL_ROUTING_TABLE_SIZE); 

         -- The addresses we can route to (implemented as array since 
         -- we can't iterate the has tables above)
         list		 : Logical_Array;
         numAddresses    : Integer := 0;
         distIndex    	 : Integer := Logical_Array'First;

         sendBuffer 	 : Address_Buffers.Buffer(20);

         myCANAddress 	 : VN.Communication.CAN.CAN_Address_Sender;
         currentState    : Logical_Address_Handler_State := Unactivated;

         timer 		 : Ada.Real_Time.Time;
      end record;

end VN.Communication.CAN.Logic.Logical_Address_Handler;
