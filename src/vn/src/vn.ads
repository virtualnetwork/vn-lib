with Interfaces;

package VN is

   type VN_CUUID is array(1..16) of Interfaces.Unsigned_8;
   subtype VN_Logical_Address is Interfaces.Unsigned_32;

end VN;
