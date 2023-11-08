object FormIvr: TFormIvr
  Left = 0
  Top = 0
  Caption = 'IVR Demo'
  ClientHeight = 550
  ClientWidth = 431
  Color = clBtnFace
  Constraints.MinHeight = 589
  Constraints.MinWidth = 447
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  DesignSize = (
    431
    550)
  TextHeight = 15
  object lblHeader: TLabel
    Left = 8
    Top = 8
    Width = 410
    Height = 52
    Caption = 
      'This demo shows how to use the IVR component to create a simple ' +
      'IVR menu. To start'#13#10'enter the below information, and click '#39'Acti' +
      'vate'#39'. After this, you will be able to call your'#13#10'SIP provider a' +
      'nd traverse the IVR menu. A callers digit input is recognized th' +
      'rough the'#13#10'Digit event, where you can either input your '#39'account' +
      #39' number or hang up.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object groupBoxSipInfo: TGroupBox
    Left = 8
    Top = 66
    Width = 410
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = 'SIP Server Information'
    TabOrder = 0
    DesignSize = (
      410
      154)
    object lblServer: TLabel
      Left = 16
      Top = 35
      Width = 54
      Height = 15
      Caption = 'SIP Server:'
    end
    object lblPort: TLabel
      Left = 271
      Top = 35
      Width = 44
      Height = 15
      Anchors = [akLeft, akTop, akRight]
      Caption = 'SIP Port:'
    end
    object lblUser: TLabel
      Left = 16
      Top = 64
      Width = 45
      Height = 15
      Caption = 'SIP User:'
    end
    object lblPassword: TLabel
      Left = 16
      Top = 93
      Width = 72
      Height = 15
      Caption = 'SIP Password:'
    end
    object txtPassword: TEdit
      Left = 94
      Top = 90
      Width = 295
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      PasswordChar = '*'
      TabOrder = 3
    end
    object txtUser: TEdit
      Left = 94
      Top = 61
      Width = 295
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object txtServer: TEdit
      Left = 94
      Top = 32
      Width = 171
      Height = 23
      TabOrder = 0
    end
    object txtPort: TEdit
      Left = 321
      Top = 32
      Width = 68
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '5060'
    end
    object btnActivate: TButton
      Left = 312
      Top = 119
      Width = 77
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Activate'
      TabOrder = 4
      OnClick = btnActivateClick
    end
  end
  object groupBoxLogInfo: TGroupBox
    Left = 8
    Top = 386
    Width = 410
    Height = 154
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Log Information'
    TabOrder = 2
    DesignSize = (
      410
      154)
    object memoLog: TMemo
      Left = 14
      Top = 24
      Width = 375
      Height = 113
      Anchors = [akLeft, akTop, akRight, akBottom]
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object groupBoxCallList: TGroupBox
    Left = 8
    Top = 226
    Width = 410
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Call List'
    TabOrder = 1
    DesignSize = (
      410
      154)
    object lvwCalls: TListView
      Left = 14
      Top = 20
      Width = 375
      Height = 117
      Anchors = [akLeft, akTop, akRight]
      Columns = <
        item
          Caption = 'Index'
          Width = 60
        end
        item
          AutoSize = True
          Caption = 'Call Id'
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object ipvIVR1: TipvIVR
    OnCallReady = ipvIVR1CallReady
    OnCallTerminated = ipvIVR1CallTerminated
    OnDigit = ipvIVR1Digit
    OnIncomingCall = ipvIVR1IncomingCall
    OnLog = ipvIVR1Log
    OnPlayed = ipvIVR1Played
    Left = 256
    Top = 32
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 208
    Top = 280
  end
end


