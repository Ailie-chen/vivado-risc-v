XILINX_PART ?= xc7k325tffg676-2
CFG_DEVICE  ?= SPIx4 -size 32
CFG_PART    ?= s25fl256sxxxxxx0-spi-x1_x2_x4
MEMORY_SIZE ?= 0x40000000
ETHER_MAC   ?= 00 0a 35 00 00 00
ETHER_PHY   ?= gmii-rxid