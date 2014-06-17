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
-- Protocol_Routing_Test is a test for the Protocol_Routing package.

with System;
with Ada.Real_Time;

with Interfaces;
use Interfaces;

with VN;
with VN.Message;

with VN.Communication;
with VN.Communication.CAN;
use VN.Communication.CAN;
with VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.CAN.CAN_Filtering;

with VN.Communication.Protocol_Routing;

with VN.Communication.PO;
with VN.Communication.PO_Wrapper;
with Protocol_Routing_Second_Task;

package Protocol_Routing_Test is

   pragma Elaborate_Body(Protocol_Routing_Test);

   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;

   CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(10);

   identifier : constant Integer := 2;

   U1 : aliased VN.Communication.CAN.UCID := VN.Communication.CAN.UCID(identifier * 10);
   C1 : aliased VN.VN_CUUID := (Interfaces.Unsigned_8(1 + identifier * 10), others => 5);

   C2 : aliased VN.VN_CUUID := (Interfaces.Unsigned_8(2 + identifier * 10), others => 5);
   C3 : aliased VN.VN_CUUID := (Interfaces.Unsigned_8(3 + identifier * 10), others => 5);

   CANInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access,
      theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (CANInterface'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

   myInterface : VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   PO_1_2, PO_1_3 : aliased VN.Communication.PO.VN_PO;

   PO_Wrapper_1_2 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_2'Access, C1'Access, VN.Message.SM_L, true);
   PO_Wrapper_1_3 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_3'Access, C1'Access, VN.Message.SM_L, true);

   PO_Wrapper_2_1 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_2'Access, C2'Access, VN.Message.Other, false);
   PO_Wrapper_3_1 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_3'Access, C3'Access, VN.Message.Other, false);


-- **** Code for adding the application threads ***
--     first_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;
--     second_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;
--     third_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;
--
--     secondTask : Protocol_Routing_Second_Task.Second_Task_Type(C2'Access, second_PO_Router'Access, System.Priority'Last, CANPeriod'Access);
--     thirdTask  : Protocol_Routing_Second_Task.Second_Task_Type(C3'Access, third_PO_Router'Access, System.Priority'Last, CANPeriod'Access);
-- ***** ******************************
private

   procedure Init;

end Protocol_Routing_Test;
