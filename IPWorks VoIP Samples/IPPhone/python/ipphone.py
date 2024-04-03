# 
# IPWorks VoIP 2022 Python Edition - Sample Project
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

import sys
import string
from ipworksvoip import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)


def DisplayMenu():
  print("IPPhone Commands:")
  print("c            Make a Call")
  print("?            Display Options")
  print("q            Quit")

print("*********************************************************************")
print("* This demo shows how to call yourself using the IPPhone component. *")
print("*********************************************************************")

if len(sys.argv) < 4:
  print("\nusage: ivr sip_server sip_port sip_username [sip_password]\n")
  print("sip_server:       SIP Server")
  print("sip_port:         SIP Port")
  print("sip_username:     SIP Server username")
  print("sip_password:     SIP Server password")
  print("\nExample: ipphone server 5060 user test")
else:
  ipphone = IPPhone()
  
  connected = False

  def fireCallTerminated(e):
    global connected
    connected = False
  
  def fireCallReady(e):
    global connected
    connected = True
  
  def fireLog(e):
    print(e.log_type + ": " + e.message + "\r\n")

  ipphone.on_call_terminated = fireCallTerminated
  ipphone.on_call_ready = fireCallReady
  ipphone.on_log = fireLog

  ipphone.set_server(sys.argv[1])
  ipphone.set_port(int(sys.argv[2]))
  ipphone.set_user(sys.argv[3])
  if len(sys.argv) == 5:
    ipphone.set_password(sys.argv[4])
  
  ipphone.activate()

  try:
    while (True):
      DisplayMenu()
      command = input("> ")
      if (command == "c"):
        ipphone.list_speakers()
        for i in range(0, ipphone.get_speaker_count()):
          print(i, ": ", ipphone.get_speaker_name(i))
        
        idx = int(input("\r\nChoose speaker # from list above (e.g., 0): "))
        ipphone.set_speaker(ipphone.get_speaker_name(idx))

        ipphone.list_microphones()
        for i in range(0, ipphone.get_microphone_count()):
          print(i, ": ", ipphone.get_microphone_name(i))
        
        idx = int(input("\r\nChoose microphone # from list above (e.g., 0): "))
        ipphone.set_microphone(ipphone.get_microphone_name(idx))

        ipphone.dial(input("\r\nNumber to Call: "), "", True)

        while connected:
          ipphone.do_events()
      elif (command == "?"):
        DisplayMenu()
      elif (command == "q"):
        print("\r\nExiting program")
        break
      else:
        print("\r\nCommand Not Recognized")
  except IPWorksVoIPError as e:
    print("ERROR %s" %e.message)



