/*
 * IPWorks VoIP 2022 .NET Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks VoIP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksvoip
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 * 
 */

using System.Collections.Generic;

﻿using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using nsoftware.async.IPWorksVoIP;

class ivrDemo
{
  private static Ivr ivr1;
  private static Dictionary<string, string> digitList = new Dictionary<string, string>();
  private static void ivr1_OnActivated(object sender, IvrActivatedEventArgs e)
  {
    Console.WriteLine("Activated\n");
  }

  private static void ivr1_OnCallReady(object sender, IvrCallReadyEventArgs e)
  {
    ivr1.PlayText(e.CallId, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.");
  }

  private static void ivr1_OnCallTerminated(object sender, IvrCallTerminatedEventArgs e)
  {
    digitList.Remove(e.CallId);
  }

  private static void ivr1_OnDigit(object sender, IvrDigitEventArgs e)
  {
    digitList[e.CallId] += e.Digit;

    switch (digitList[e.CallId][0])
    {
      case '1':
        if (digitList[e.CallId].Length == 1)
        {
          ivr1.PlayText(e.CallId, "Please input your account number followed by #");
        }
        else if (e.Digit == "#")
        {
          string digits = "";
          for (int i = 1; i < digitList[e.CallId].Length - 1; i++)
          {
            digits += digitList[e.CallId][i] + " ";
          }
          ivr1.PlayText(e.CallId, "Your account number is " + digits + ". This account is currently active.");
          digitList[e.CallId] = "";
        }
        break;
      case '2':
        ivr1.Hangup(e.CallId);
        break;
      default:
        digitList[e.CallId] = "";
        break;
    }
  }

  private static void ivr1_OnIncomingCall(object sender, IvrIncomingCallEventArgs e)
  {
    ivr1.Answer(e.CallId);
    digitList.Add(e.CallId, "");
  }

  private static void ivr1_OnLog(object sender, IvrLogEventArgs e)
  {
    Console.WriteLine(e.LogType + ": " + e.Message);
  }

  private static void ivr1_OnPlayed(object sender, IvrPlayedEventArgs e)
  {
    // Replay menu options if no option was selected, or OnDigit branch is completed
    if (digitList[e.CallId].Length == 0)
    {
      ivr1.PlayText(e.CallId, "Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.");
    }
  }

  static async Task Main(string[] args)
  {
    ivr1 = new nsoftware.async.IPWorksVoIP.Ivr();
    Console.WriteLine("**************************************************************************************");
    Console.WriteLine("* This is a demo to show how to set up a sample IVR menu using the IVR component.    *");
    Console.WriteLine("* Once activation is complete, the component will be able to receive incoming calls. *");
    Console.WriteLine("**************************************************************************************\n");

    if (args.Length < 4)
    {
      Console.WriteLine("\nusage: ivr sip_server sip_port sip_username [sip_password]\n");
      Console.WriteLine("sip_server:       SIP Server");
      Console.WriteLine("sip_port:         SIP Port");
      Console.WriteLine("sip_username:     SIP Server username");
      Console.WriteLine("sip_password:     SIP Server password");
      Console.WriteLine("\nExample: ivr server 5060 user test\n");
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }
    else
    {
      await ivr1.Config("LogLevel=1");
      ivr1.OnActivated += ivr1_OnActivated;
      ivr1.OnCallReady += ivr1_OnCallReady;
      ivr1.OnCallTerminated += ivr1_OnCallTerminated;
      ivr1.OnDigit += ivr1_OnDigit;
      ivr1.OnIncomingCall += ivr1_OnIncomingCall;
      ivr1.OnLog += ivr1_OnLog;
      ivr1.OnPlayed += ivr1_OnPlayed;

      try
      {
        ivr1.Server = args[1];
        ivr1.Port = int.Parse(args[2]);
        ivr1.User = args[3];
        if (args.Length == 5)
        {
          ivr1.Password = args[4];
        }

        await ivr1.Activate();

        while (true)
        {
          await ivr1.DoEvents();
        }
      } catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }
  }
}







class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}