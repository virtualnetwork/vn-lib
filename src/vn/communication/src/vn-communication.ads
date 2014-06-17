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
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com) and
--  Christoffer Holmstedt (christoffer.holmstedt@gmail.com).
------------------------------------------------------------------------------

-- Summary: The VN.Message encapsulate all functionalities regarding communication.

with VN.Message;
with Buffers;

package VN.Communication is

   package VN_Message_Buffer is
         new Buffers(VN.Message.VN_Message_Basic);

   -- The Com interface is inherited by all subnet protocols and by the Protocol routing layer.
   type Com is limited interface;

   type Com_Access is access all Com'Class;

   procedure Send(This: in out Com;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status)
                        is abstract;

   procedure Receive(This: in out Com;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status)
                        is abstract;

end VN.Communication;
