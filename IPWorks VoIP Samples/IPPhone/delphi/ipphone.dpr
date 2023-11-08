(*
 * IPWorks VoIP 2022 Delphi Edition - Sample Project
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
 *)

program ipphone;

uses
  Forms,
  ipphonef in 'ipphonef.pas' {FormIpphone};

begin
  Application.Initialize;

  Application.CreateForm(TFormIpphone, FormIpphone);
  Application.Run;
end.


         
