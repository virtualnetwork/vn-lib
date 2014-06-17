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
