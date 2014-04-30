VN CAN Subnet Protocol
==========================

### High level

#### Identifiers and addresses


**There exists two forms of identifiers in this protocol:** 
  * **The Component Universally Unique Identifier (CUUID)**  is a high level identifier that is to be unique world world for the particular component. The length of the CUUID is 128 bits (16 bytes). Any component on a VN network needs a CUUID.
  * **The Universally Unique CAN Identifier (UCID)** is a low level identifier that is to be unique world world for the particular component. The length of the UCID is 28 bits. Any component on the VN-CAN network will need a UCID.

**There exists two forms of addresses in this protocol:**
  * **The logical addresses.**  Defined in the VN protocol.
  * **CAN addresses.** The CAN address. Any component on the VN-CAN network will be assigned a CAN address. The CAN addresses are only used 	on the CAN bus on the low level part of the VN-CAN protocol. The CAN addresses are managed by the SM-CAN master.


#### Stored data
**The following data is stored in this protocol:**
  * Send buffer, for VN-messages.
  * Receive buffer, for VN-messages.
  * A primary routing table, connecting a given logical address to a low level
    address.
  * A CUUID routing table, connecting a given CUUID to a subnet address. This
    table is used in some special cases of VN messages (e.g. **AssignAddr**)
    that cannot rely on logical addresses for routing.


#### Interface
**There exists two public functions stored in this protocol:**
  * Send. Will take a VN message as argument, an out-variable will tell the
    outcome of the call, i.e. “Transmission OK” or “Buffer full”. The message
    is written to the send buffer if it is not full.
  * Receive. Will give a VN message as an out-variable, another out-variable
    will tell the outcome of the call, i.e. “No message received”, “Message
    received, buffer empty” and “Message received, more available”.

### Route discovery process
##### For routes to overlying units
See Section *For routes to overlying units* in *VN generic subnet protocol*.

##### For routes to underlying units
See Section *For routes to underlying units* in *VN generic subnet protocol*.

##### Transmission of VN messages
See Section *Transmission of VN messages* in *VN generic subnet protocol*.

##### Reception of VN messages
When a VN message is received on the subnet it shall be pushed to the receive buffer. 
Actions according to sections *Route discovery process* and *Unit discovery process* shall also be performed.
*Unit discovery process ?????????????????* 

##### Logical addresses
Assignment of logical addresses is handled by higher level protocols and is not described in this protocol.




### Low level

##### Division of message IDs
Bits are numbered 28-0 where bit 28 is most significant and is sent first.
There are two groups of message types: *RequestCANAddress* message (one message type) and *Normal* CAN Messages. These are defined by the first bit of the message ID.
The message ID is divided according to:

1. msgID[28] = 1: RequestCANAddress message
	msgID[27 .. 0] = Universally Unique CAN Identifier (UCID) for the node
2. msgID[28] = 0: Normal CAN messages_ 
	msgID[27 .. 22] = Message priority_ 
	msgID[21 .. 15] = Message type_ 
	msgID[14 .. 7] = Receiver address_ 
	msgID[6 .. 0] = Sender address

*Please note:* Even though CAN addresses are 8 bit, addresses over 127 will never be used as a sender address since the maximum number of allowed CAN nodes on a CAN network is 128 one only needs addresses 0 through 127. This means that only 7 bits are needed for the Sender address.














