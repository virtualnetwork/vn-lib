-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- Simple implementation of routing table. A better implementation is recommended in the future.
-- ToDo: Only started.

package body VN.Communication.Routing_Table is

   procedure Insert(this : in out Table_Type;
                    Abstract_Address : Abstract_Address_Type;
                    Concrete_Address : Concrete_Address_Type) is
   begin
      null;
   end Insert;

   procedure Search(this : in out Table_Type;
                    Abstract_Address : Abstract_Address_Type;
                    Concrete_Address : out Concrete_Address_Type;
                    found : out Boolean) is
   begin
      null;
   end Search;

end VN.Communication.Routing_Table;
