-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
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

with VN.Communication.PO;
with VN.Communication.PO_Wrapper;

with VN.Communication.Protocol_Routing;

package Protocol_Routing_Test is

   pragma Elaborate_Body(Protocol_Routing_Test);

   task type Second_Task_Type(myCUUID  : access VN.VN_CUUID;
                              myAccess : VN.Communication.Com_Access;
                              Pri : System.Priority;
                              thePeriod : access Ada.Real_Time.Time_Span) is
      pragma Priority(Pri);
   end Second_Task_Type;

   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;

   CANPeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);
   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);
   C2 : aliased VN.VN_CUUID := (2, others => 5);
   C3 : aliased VN.VN_CUUID := (3, others => 5);

   CANInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access,
      theFilter'Unchecked_Access, VN.Communication.CAN.CAN_Interface.SM_CAN);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (CANInterface'Access, System.Priority'Last, CANPeriod'Access, theFilter'Unchecked_Access);

   myInterface : VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   PO_1_2, PO_1_3 : aliased VN.Communication.PO.VN_PO;

   PO_Wrapper_1_2 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_2'Access, C1'Access, VN.Message.SM_L, true);
   PO_Wrapper_1_3 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_3'Access, C1'Access, VN.Message.SM_L, true);
   first_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   PO_Wrapper_2_1 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_2'Access, C2'Access, VN.Message.SM_x, false);
   second_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   PO_Wrapper_3_1 : aliased VN.Communication.PO_Wrapper.VN_PO_Wrapper(PO_1_3'Access, C3'Access, VN.Message.SM_x, false);
   third_PO_Router : aliased VN.Communication.Protocol_Routing.Protocol_Routing_Type;

   secondTask : Second_Task_Type(C2'Access, second_PO_Router'Access, System.Priority'Last, CANPeriod'Access);
   thirdTask : Second_Task_Type(C3'Access, third_PO_Router'Access, System.Priority'Last, CANPeriod'Access);

private

   procedure Init;

end Protocol_Routing_Test;
