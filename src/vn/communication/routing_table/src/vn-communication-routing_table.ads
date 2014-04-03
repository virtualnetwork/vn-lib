-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Simple implementation of routing table. A better implementation is recommended in the future.
-- ToDo: Only started.

generic
   type Abstract_Address_Type is private; -- e.g. logical address
   type Concrete_Address_Type is private; -- e.g. CAN address, UDP-port, etc.
package VN.Communication.Routing_Table is
   pragma Elaborate_Body;

   type Table_Type (Capacity : Positive) is private;

   procedure Insert(this : in out Table_Type;
                    Abstract_Address : Abstract_Address_Type;
                    Concrete_Address : Concrete_Address_Type);

   procedure Search(this : in out Table_Type;
                    Abstract_Address : Abstract_Address_Type;
                    Concrete_Address : out Concrete_Address_Type;
                    found : out Boolean);

private

   type Element_Type is
      record
         null;
      end record;

   type Content is array (Positive range <>) of Element_Type;

   type Table_Type (Capacity : Positive) is
      record
         Values : Content (1 .. Capacity);
      end record;

end VN.Communication.Routing_Table;
