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
-- CAN_Address_Assignment will be activated by an SM-CAN if it is assigned as
-- master. CAN_Address_Assignment will remain unactivated if the SM-CAN
-- master negotiation process is lost.
-- CAN_Address_Assignment will assign CAN addresses to all other units on the
-- CAN network.

with VN.Communication.CAN.Logic.Message_Utils;

package body VN.Communication.CAN.Logic.CAN_Address_Assignment is

   overriding procedure Update(this : in out CAN_Assignment_Master; msgIn : VN.Communication.CAN.CAN_Message_Logical; bMsgReceived : boolean;
                               msgOut : out VN.Communication.CAN.CAN_Message_Logical; bWillSend : out boolean) is

      receivedUCID : VN.Communication.CAN.UCID;
      bWasSMCAN    : boolean;
   begin

      case this.currentState is
         when Unactivated =>
            bWillSend := false;

         when Started =>

            if bMsgReceived then
               if not msgIn.isNormal and msgIn.SenderUCID /= this.myUCID then
                  VN.Communication.CAN.Logic.Message_Utils.RequestCANAddressFromMessage(msgIn, receivedUCID, bWasSMCAN);

                  declare
                     temp : VN.Communication.CAN.CAN_Address_Sender;
                  begin

                     this.AssignCANAddress(receivedUCID, temp);

                     VN.Communication.CAN.Logic.Message_Utils.AssignCANAddressToMessage(msgOut, receivedUCID, temp);
                     VN.Communication.CAN.Logic.DebugOutput("Assigned CAN address " & temp'Img & " to UCID " & receivedUCID'Img, 4);
                  end;

                  bWillSend := true;
                  return;
               end if;
            end if;
            bWillSend := false;
      end case;
   end Update;

   procedure Activate(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID) is
   begin
      if this.currentState = Unactivated then
         DebugOutput("CAN address assigner started with UCID=" & theUCID'Img, 5);
         this.currentState := Started;
         this.myUCID := theUCID;
      end if;
   end Activate;

   procedure AssignCANAddress(this : in out CAN_Assignment_Master; theUCID : VN.Communication.CAN.UCID;
                              address : out VN.Communication.CAN.CAN_Address_Sender) is
      i : VN.Communication.CAN.CAN_Address_Sender := 0;
      GetCANAddress_ERROR : exception;
   begin
      for i in this.addresses'First + 1 .. this.addresses'Last loop
         if this.addresses(i).isUsed and then this.addresses(i).unitUCID = theUCID then
            address := i;
            return;

         elsif not this.addresses(i).isUsed then
            this.numUnitsFound := this.numUnitsFound + 1;
            this.addresses(i).unitUCID := theUCID;
            this.addresses(i).isUsed := true;
            address := i;
            return;
         end if;
      end loop;

      raise GetCANAddress_ERROR;
   end AssignCANAddress;
end VN.Communication.CAN.Logic.CAN_Address_Assignment;

