#
# IPWorks VoIP 2022 .NET Edition - Sample Project
#
# This sample project demonstrates the usage of IPWorks VoIP in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.nsoftware.com/ipworksvoip
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lipworksvoip.22.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o ivr ivr.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/ipworksvoip.o
	g++ $(OS_CFLAGS) -o ivr ../../src/ipworksvoip.o ivr.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/ipworksvoip.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/ipworksvoip.o ../../src/ipworksvoip.cpp

else # LINUX
ifeq ($(shell uname -m), x86_64)
  LIB=lib64
else
  LIB=lib
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lipworksvoip  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX -o ivr ivr.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH) 

src: ../../src/ipworksvoip.o
	g++ -D UNIX -o ivr ../../src/ipworksvoip.o ivr.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/ipworksvoip.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/ipworksvoip.o ../../src/ipworksvoip.cpp

clean:
	rm -f ../../src/*.o
	rm -f ivr *.o
endif