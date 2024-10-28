# 
# IPWorks VoIP 2024 Python Edition - Sample Project
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
  elif args[index] is None:
    args[index] = input(prompt)



digitList = { }

print("**************************************************************************************")
print("* This is a demo to show how to set up a sample IVR menu using the IVR component.    *")
print("* Once activation is complete, the component will be able to receive incoming calls. *")
print("**************************************************************************************")

if len(sys.argv) < 4:
  print("\nusage: ivr sip_server sip_port sip_username [sip_password]\n")
  print("sip_server:       SIP Server")
  print("sip_port:         SIP Port")
  print("sip_username:     SIP Server username")
  print("sip_password:     SIP Server password")
  print("\nExample: ivr server 5060 user test")
else:
  ivr = IVR()

  def fireActivated(e):
    print("\nActivated\n")

  def fireCallReady(e):
    ivr.play_text(e.call_id, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.")
    
  def fireCallTerminated(e):
    digitList.pop(e.call_id)

  def fireDigit(e):
    digitList[e.call_id] += e.digit
    currentDigits = digitList[e.call_id]
    if currentDigits[0] == "1":
      if len(currentDigits) == 1:
        ivr.play_text(e.call_id, "Please input your account number followed by #")
      elif e.digit == "#":
        digits = ""
        for i in range(1, len(currentDigits) - 1):
          digits += currentDigits[i] + " "
        ivr.play_text(e.call_id, "Your account number is {}. This account is currently active.".format(digits))
        digitList[e.call_id] = ""
    elif currentDigits[0] == "2":
      ivr.hangup(e.call_id)
    else:
      digitList[e.call_id] = ""

  def fireIncomingCall(e):
    ivr.answer(e.call_id)
    digitList[e.call_id] = ""

  def fireLog(e):
    print("{}: {}\n".format(e.log_type, e.message))
  
  def firePlayed(e):
    currentDigits = digitList[e.call_id]
    if len(currentDigits) == 0:
      ivr.play_text(e.call_id, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.")
  
  ivr.on_activated = fireActivated
  ivr.on_call_terminated = fireCallTerminated
  ivr.on_call_ready = fireCallReady
  ivr.on_digit = fireDigit
  ivr.on_incoming_call = fireIncomingCall
  ivr.on_log = fireLog
  ivr.on_played = firePlayed

  ivr.config("LogLevel=1")
  ivr.set_server(sys.argv[1])
  ivr.set_port(int(sys.argv[2]))
  ivr.set_user(sys.argv[3])
  if len(sys.argv) == 5:
    ivr.set_password(sys.argv[4])
  
  ivr.activate()

  try:
    while True:
      ivr.do_events()
  except IPWorksVoIPError as e:
    print("ERROR {}".format(e.message))


