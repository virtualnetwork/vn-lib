
-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary: 
-- Protocol_Routing is a middle layer between the subnets and the 
-- application layer. The Protocol_Routing package will decide on 
-- which subnet to send a VN message sent by the application layer
-- and whether a VN message received via a subnet shall be 
-- delivered to the application layer or sent to another
-- subnet, and if so, which one.

package VN.Communication.Protocol_Routing is
end VN.Communication.Protocol_Routing;
