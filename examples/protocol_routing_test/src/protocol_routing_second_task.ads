
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Protocol_Routing_Second_Task implements a task that tests the Protocol_Routing package.

with Ada.Real_Time;
with System;
with VN;
with VN.Communication;

package Protocol_Routing_Second_Task is

   pragma Elaborate_Body(Protocol_Routing_Second_Task);

   task type Second_Task_Type(myCUUID  : access VN.VN_CUUID;
                              myAccess : VN.Communication.Com_Access;
                              Pri : System.Priority;
                              thePeriod : access Ada.Real_Time.Time_Span) is
      pragma Priority(Pri);
   end Second_Task_Type;

end Protocol_Routing_Second_Task;
