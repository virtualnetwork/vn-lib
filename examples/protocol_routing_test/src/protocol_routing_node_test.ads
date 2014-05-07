-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Node_Test is testing the functionality of a node

-- ToDo: The VN.Communication.CAN.Logic package is as of now only implemented as a Subnet manager.
-- Only high level code will be run as a node as opposed to Subnet Manager.

with System;
with Ada.Real_Time;

with Interfaces;
use Interfaces;

with VN;

with VN.Communication;
with VN.Communication.CAN;
use VN.Communication.CAN;
with VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.CAN.CAN_Filtering;

with VN.Communication.PO;
with VN.Communication.PO_Wrapper;

with VN.Communication.Protocol_Routing;

with Protocol_Routing_Second_Task;

package Protocol_Routing_Node_Test is

   pragma Elaborate_Body(Protocol_Routing_Test);

   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;

   CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);
   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);

   CANInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access,
      theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);

   myCANTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (CANInterface'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

   mainTask : Protocol_Routing_Second_Task.Second_Task_Type(C1'Access, CANInterface'Access, System.Priority'Last, CANPeriod'Access);

private

   procedure Init;

end Protocol_Routing_Node_Test;
