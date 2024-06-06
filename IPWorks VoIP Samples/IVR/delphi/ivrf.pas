(*
 * IPWorks VoIP 2024 Delphi Edition - Sample Project
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
unit ivrf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ipvcore, ipvtypes, ipvipphone, System.Generics.Collections,
  Vcl.StdCtrls, ipvivr, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFormIvr = class(TForm)
    lblHeader: TLabel;
    groupBoxSipInfo: TGroupBox;
    lblServer: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    txtPassword: TEdit;
    txtUser: TEdit;
    txtServer: TEdit;
    txtPort: TEdit;
    groupBoxLogInfo: TGroupBox;
    ipvIVR1: TipvIVR;
    btnActivate: TButton;
    memoLog: TMemo;
    groupBoxCallList: TGroupBox;
    lvwCalls: TListView;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure btnActivateClick(Sender: TObject);
    procedure ipvIVR1Log(Sender: TObject; LogLevel: Integer; const Message,
      LogType: string);
    procedure ipvIVR1IncomingCall(Sender: TObject; const CallId, RemoteUser,
      RequestURI, ToURI: string);
    procedure ipvIVR1CallReady(Sender: TObject; const CallId: string);
    procedure ipvIVR1CallTerminated(Sender: TObject; const CallId: string);
    procedure ipvIVR1Played(Sender: TObject; const CallId: string;
      const Completed: Boolean);
    procedure ipvIVR1Digit(Sender: TObject; const CallId, Digit: string);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    digitList: TDictionary<String, String>;
  end;

var
  FormIvr: TFormIvr;

implementation

{$R *.dfm}

procedure TFormIvr.btnActivateClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if not ipvIVR1.Active then
    begin
      ipvIVR1.User := txtUser.Text;
      ipvIVR1.Password := txtPassword.Text;
      ipvIVR1.Server := txtServer.Text;
      ipvIVR1.Port := StrToInt(txtPort.Text);
      ipvIVR1.Activate();
    end;
  except on ex: EIPWorksVoIP do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormIvr.FormShow(Sender: TObject);
begin
  ipvIVR1.Config('LogLevel=2');
  digitList := TDictionary<String, String>.Create;
end;

procedure TFormIvr.ipvIVR1CallReady(Sender: TObject; const CallId: string);
var
  welcomeText: String;
begin
  welcomeText := 'Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.';
  ipvIVR1.PlayText(callId, welcomeText);
end;

procedure TFormIvr.ipvIVR1CallTerminated(Sender: TObject; const CallId: string);
var
  i: Integer;
begin
  digitList.ExtractPair(CallId);
  for i := 0 to lvwCalls.Items.Count - 1 do
  begin
    if CompareText(CallId, lvwCalls.Items.Item[i].SubItems.Strings[0]) = 0 then
    begin
      lvwCalls.Items.Delete(i);
      break;
    end;
  end;

  for i := 0 to lvwCalls.Items.Count - 1 do
  begin
    lvwCalls.Items.Item[i].Caption := IntToStr(i);
  end;
end;

procedure TFormIvr.ipvIVR1Digit(Sender: TObject; const CallId, Digit: string);
var
  currentDigits: String;
  accountDigits: String;
  i: Integer;
begin
  currentDigits := '';
  digitList.TryGetValue(CallId, currentDigits);
  digitList.AddOrSetValue(CallId, currentDigits + Digit);
  digitList.TryGetValue(CallId, currentDigits);
  if currentDigits[1] = '1' then
  begin
    if currentDigits.Length = 1 then
      ipvIVR1.PlayText(CallId, 'Please input your account number followed by #')
    else if Digit = '#' then
    begin
      accountDigits := '';
      for i := 2 to currentDigits.Length - 1 do
        accountDigits := accountDigits + currentDigits[i] + ' ';
      ipvIVR1.PlayText(CallId, 'Your account number is ' + accountDigits + '. This account is currently active.');
      digitList.AddOrSetValue(CallId, '');
    end;
  end
  else if currentDigits[1] = '2' then
    ipvIVR1.Hangup(CallId)
  else
    digitList.AddOrSetValue(CallId, '');
end;

procedure TFormIvr.ipvIVR1IncomingCall(Sender: TObject; const CallId,
  RemoteUser, RequestURI, ToURI: string);
var
  item: TListItem;
begin
  ipvIVR1.Answer(CallId);
  digitList.Add(CallId, '');
  item := lvwCalls.Items.Add;
  item.Caption := IntToStr(ipvIVR1.CallCount - 1);
  item.SubItems.Add(CallId);
end;

procedure TFormIvr.ipvIVR1Log(Sender: TObject; LogLevel: Integer; const Message,
  LogType: string);
begin
  memoLog.Lines.Add(LogType + ': ' + Message) ;
end;

procedure TFormIvr.ipvIVR1Played(Sender: TObject; const CallId: string;
  const Completed: Boolean);
var
  currentDigits: String;
begin
  currentDigits := '';
  digitList.TryGetValue(CallId, currentDigits);
  if currentDigits.Length = 0 then
    ipvIVR1.PlayText(CallId, 'Thank you for calling. Please press 1 to find the status of your account. Press 2 to hangup.');
end;

procedure TFormIvr.Timer1Timer(Sender: TObject);
begin
  ipvIVR1.DoEvents();
end;

end.


