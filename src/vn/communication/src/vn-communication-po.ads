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
--  Copyright 2014 Christoffer Holmstedt (christoffer.holmstedt@gmail.com)
------------------------------------------------------------------------------

with VN.Message;

package VN.Communication.PO is

   protected type VN_PO is

      procedure Receive_from_SM_L(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);
      procedure Send_to_SM_L(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Send_Status);

      procedure Receive_from_Other(Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);
      procedure Send_to_Other(Message: in VN.Message.VN_Message_Basic;
                      Status: out VN.Send_Status);

      function Get_Buffer_Length_To_SM_L return Integer;
      function Get_Buffer_Length_To_Other return Integer;

   private
      Buffer_Length_To_SM_L: Integer := 0;
      Buffer_Length_To_Other: Integer := 0;

      Buffer_To_SM_L: VN_Message_Buffer.Buffer(10);
      Buffer_To_Other: VN_Message_Buffer.Buffer(10);
   end VN_PO;

   type VN_PO_Access is access all VN_PO;

end VN.Communication.PO;
