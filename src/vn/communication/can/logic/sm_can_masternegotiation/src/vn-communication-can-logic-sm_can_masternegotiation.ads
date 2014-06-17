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
-- Implementation of the state machine for the SM-CAN master negotioation process
-- of the VN-CAN protocol.
-- Once the SM-CAN master negotioation process is completed, the process of assigning
-- CAN addresses can begin.


with Ada.Real_Time;
with VN.Communication.CAN.Logic;

package VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation is

   type SM_CAN_Mode is (UNDETERMINED, MASTER, SLAVE);


   type SM_CAN_MN_Duty(theUCID : access VN.Communication.CAN.UCID) is
     new VN.Communication.CAN.Logic.Duty with private;

   type SM_CAN_MN_Duty_ptr is access SM_CAN_MN_Duty'Class;


   overriding procedure Update(this : in out SM_CAN_MN_Duty; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean);

   function CurrentMode(this : in SM_CAN_MN_Duty) return SM_CAN_Mode;

private

   WAIT_TIME : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(Integer(500));

   type SM_CAN_MN_State is (Start, Started, Slave, Master);

   type SM_CAN_MN_Duty(theUCID : access VN.Communication.CAN.UCID) is
     new VN.Communication.CAN.Logic.Duty with
      record
         currentState : SM_CAN_MN_State := Start;
         timer    : Ada.Real_Time.Time;
         myUCID   : VN.Communication.CAN.UCID := theUCID.all;
      end record;

end VN.Communication.CAN.Logic.SM_CAN_MasterNegotiation;

