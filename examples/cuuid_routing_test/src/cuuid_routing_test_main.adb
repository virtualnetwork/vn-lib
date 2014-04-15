
pragma Profile (Ravenscar);

with GNAT.IO;

with VN;
use VN;

with VN.Communication.CAN;
use VN.Communication.CAN;

with Interfaces;
with VN.Communication.CUUID_Routing;

procedure CUUID_Routing_Test_Main is

  package CAN_CUUID_Routing is new VN.Communication.CUUID_Routing(VN.Communication.CAN.CAN_Address_Sender);

   c : array(1..127) of VN.VN_CUUID;
   a : array(c'Range) of VN.Communication.CAN.CAN_Address_Sender;
   b : array(c'Range) of VN.Communication.CAN.CAN_Address_Sender;

   d : array(1..127) of VN.VN_CUUID;

   found : boolean;
begin

   for i in c'Range loop
      c(i) := (others => Interfaces.Unsigned_8(i));
      d(i) := (others => Interfaces.Unsigned_8(i + 127));

      a(i) := VN.Communication.CAN.CAN_Address_Sender(i);
      b(i) := VN.Communication.CAN.CAN_Address_Sender(0);
      CAN_CUUID_Routing.Insert(c(i), a(i));

      if CAN_CUUID_Routing.Number_Of_Entries /= i then
          GNAT.IO.Put_Line("Number_Of_Entries was incorrect");
      end if;

   end loop;

   for i in c'Range loop
      CAN_CUUID_Routing.Search(c(i), b(i), found);

      if a(i) /= b(i) and found then
         GNAT.IO.Put_Line("Index " & i'Img & " is incorrect");
      end if;

      CAN_CUUID_Routing.Search(d(i), b(i), found);
      if found then
         GNAT.IO.Put_Line("Index " & i'Img & " was incorrectly found");
      end if;
   end loop;

   GNAT.IO.Put_Line("Test done");

end CUUID_Routing_Test_Main;
