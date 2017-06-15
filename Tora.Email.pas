{
  Criador: Bruno Deuner
  Contato
  Email: bruno_deuner@hotmail.com

  Informações
  Autorização de contas:
  Gmail: https://myaccount.google.com/lesssecureapps
  Hotmail: Não precisa
}

unit Tora.Email;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.RegularExpressions,
  Inifiles,
  IdSMTP,
  IdSSLOpenSSL,
  IdMessage,
  IdText,
  IdAttachmentFile,
  IdExplicitTLSClientServerBase,
  IdEMailAddress,
  IdPOP3,
  IdAssignedNumbers;

type

  { Exceptions }
  EEmailError = class;
  EConnectError = class;
  EAuthentError = class;
  ESendError = class;
  EReceiveError = class;

  EEmailError = class(Exception)
  private type
    TEKind = (eUnknown, eConnect, eAuthent, eSend, eReceive);
  private
    FKind: TEKind;
    procedure SetKind(const Value: TEKind);
  public
    property Kind: TEKind read FKind write SetKind default eUnknown;
    constructor Create; overload;
    constructor Create(Msg: String; Kind: TEKind); overload; virtual;
  end;

  EConnectError = class(EEmailError)
    constructor Create(const Msg: string);
  end;

  EAuthentError = class(EEmailError)
    constructor Create(const Msg: string);
  end;

  ESendError = class(EEmailError)
    constructor Create(const Msg: string);
  end;

  EReceiveError = class(EEmailError)
    constructor Create(const Msg: string);
  end;

  { CallBacks }

  TAfterOperation = procedure(Sender: TObject; Status: Boolean) of object;
  TReadMessage = procedure(Sender: TObject; Mensagem: TIdMessage) of object;
  TErroEmail = procedure(Sender: TObject; Error: Exception) of object;

  { Records }
  TSMTP = (smNenhum, smHotmail, smGmail);
  TOrder = (toCrescente, toDecrecente);

  TAccount = class(TPersistent)
  private
    FPassword: String;
    FUserName: String;
    FName: String;
    procedure SetPassword(const Value: String);
    procedure SetUserName(const Value: String); virtual;
    procedure SetName(const Value: String);
  public
    property Name: String read FName write SetName;
    property Password: String read FPassword write SetPassword;
    property UserName: String read FUserName write SetUserName;
    procedure Assign(Source: TPersistent); override;
    function IsValid: Boolean; virtual;
  end;

  TEmailAccount = class(TAccount)
  private
    FSMTP: TSMTP;
    procedure DiscoverySMTP;
    function GetSMTP: TSMTP;
    procedure SetUserName(const Value: String); override;
  protected
    property SMTP: TSMTP read GetSMTP;
  public
    function IsValid: Boolean; override;
  end;

  IEmail = interface
    // End thread
    procedure TerminateThread(Sender: TObject);
    // Getter and Setters
    procedure SetOnAfterConnect(const Value: TAfterOperation);
    procedure SetOnBeforeConnect(const Value: TNotifyEvent);
    procedure SetOnBeforeExecute(const Value: TNotifyEvent);
    procedure SetOnAfterExecute(const Value: TNotifyEvent);
    procedure SetOnError(const Value: TErroEmail);
    function GetOnBeforeConnect: TNotifyEvent;
    function GetOnAfterConnect: TAfterOperation;
    function GetOnBeforeExecute: TNotifyEvent;
    function GetOnAfterExecute: TNotifyEvent;
    function GetOnError: TErroEmail;
    // Metodos
    procedure Connect;
    procedure Execute;
    procedure Cancel;
    // Callbacks
    property OnBeforeConnect: TNotifyEvent read GetOnBeforeConnect
      write SetOnBeforeConnect;
    property OnAfterConnect: TAfterOperation read GetOnAfterConnect
      write SetOnAfterConnect;
    property OnBeforeExecute: TNotifyEvent read GetOnBeforeExecute
      write SetOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read GetOnAfterExecute
      write SetOnAfterExecute;
    property OnError: TErroEmail read GetOnError write SetOnError;
  end;

  TEmail = class(TInterfacedObject, IEmail)
  private
    // Fields
    FAccount: TEmailAccount;
    FThread: TThread;
    // Eventos
    FOnAfterConnect: TAfterOperation;
    FOnBeforeConnect: TNotifyEvent;
    FOnError: TErroEmail;
    FOnAfterExecute: TNotifyEvent;
    FOnBeforeExecute: TNotifyEvent;
    // Getter and Setters
    procedure SetOnAfterConnect(const Value: TAfterOperation);
    procedure SetOnBeforeConnect(const Value: TNotifyEvent);
    procedure SetAccount(const Value: TEmailAccount);
    procedure SetOnError(const Value: TErroEmail);
    procedure SetOnAfterExecute(const Value: TNotifyEvent);
    procedure SetOnBeforeExecute(const Value: TNotifyEvent);
    function GetOnBeforeConnect: TNotifyEvent;
    function GetOnAfterConnect: TAfterOperation;
    function GetOnAfterExecute: TNotifyEvent;
    function GetOnBeforeExecute: TNotifyEvent;
    function GetOnError: TErroEmail;
    // End thread
    procedure TerminateThread(Sender: TObject);
  protected
    // Fields
    SSL: TIdSSLIOHandlerSocketOpenSSL;
    // Methods
    procedure BeforeConnect;
    function DoConnect: Boolean; virtual; abstract;
    procedure AfterConnect(Connected: Boolean);
    procedure DoExecute; virtual; abstract;
  public
    // Construtor/Destrutor
    constructor Create; virtual;
    destructor Destroy; override;
    // Metodos
    procedure Connect;
    procedure Execute; virtual;
    procedure Cancel; virtual;
    // Propriedades
    property Account: TEmailAccount read FAccount write SetAccount;
    // Callbacks
    property OnBeforeConnect: TNotifyEvent read GetOnBeforeConnect
      write SetOnBeforeConnect;
    property OnAfterConnect: TAfterOperation read GetOnAfterConnect
      write SetOnAfterConnect;
    property OnBeforeExecute: TNotifyEvent read GetOnBeforeExecute
      write SetOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read GetOnAfterExecute
      write SetOnAfterExecute;
    property OnError: TErroEmail read GetOnError write SetOnError;
  end;

  TEmailSend = class(TEmail)
  private
    IdSMTP: TIdSMTP;
    Enviou: Boolean;
    // Callbacks
    FOnAfterSend: TAfterOperation;
    FOnBeforeSend: TNotifyEvent;
    // Fields
    FAssunto: string;
    FAnexos: TStringList;
    FTexto: TStringList;
    FDestino: TStringList;
    FRemetenteNome: string;
    FRemetenteEmail: string;
    // Method
    procedure ConfigConnection;
    procedure CreateAndConfigMessage(var IdMessage: TIdMessage);
    procedure AjustaRemetente;
    // Getter and Setters
    procedure SetAfterSend(const Value: TAfterOperation);
    procedure SetAnexos(const Value: TStringList);
    procedure SetAssunto(const Value: string);
    procedure SetBeforeSend(const Value: TNotifyEvent);
    procedure SetDestino(const Value: TStringList);
    procedure SetRemetenteEmail(const Value: string);
    procedure SetRemetenteNome(const Value: string);
    procedure SetTexto(const Value: TStringList);
  protected
    // Metodos
    procedure DoExecute; override;
    function DoConnect: Boolean; override;
  public
    // Construtor
    constructor Create; override;
    destructor Destroy; override;
    // Propriedades
    property RemetenteEmail: string read FRemetenteEmail
      write SetRemetenteEmail;
    property RemetenteNome: string read FRemetenteNome write SetRemetenteNome;
    property Assunto: string read FAssunto write SetAssunto;
    property Anexos: TStringList read FAnexos write SetAnexos;
    property Texto: TStringList read FTexto write SetTexto;
    property Destino: TStringList read FDestino write SetDestino;
    // Callbacks
    property OnBeforeSend: TNotifyEvent read FOnBeforeSend write SetBeforeSend;
    property OnAfterSend: TAfterOperation read FOnAfterSend write SetAfterSend;
  end;

  TEmailReceive = class(TEmail)
  private
    IdPOP: TIdPop3;
    // Callbacks
    FOnReadMessage: TReadMessage;
    // Fields
    FEmails: TList<TIdMessage>;
    FInicial: Cardinal;
    FQuantidade: Cardinal;
    FOrdem: TOrder;
    MsgProcess: Cardinal;
    MsgPos: Cardinal;
    MsgCount: Cardinal;
    // Method
    procedure AdicionaEmail
      (Lista: System.Generics.Collections.TList<TIdMessage>; i: Integer);
    // Getters and Setters
    procedure SetInicial(const Value: Cardinal);
    procedure SetOrdem(const Value: TOrder);
    procedure SetQuantidade(const Value: Cardinal);
    procedure SetReadMessage(const Value: TReadMessage);
    function GetFinished: Boolean;
  protected
    // Metodos
    procedure DoExecute; override;
    function DoConnect: Boolean; override;
  public
    // Construtor/Destructor
    destructor Destroy; override;
    // Propriedades
    property Emails: TList<TIdMessage> read FEmails;
    property Inicial: Cardinal read FInicial write SetInicial default 0;
    property Quantidade: Cardinal read FQuantidade write SetQuantidade
      default 0;
    property Ordem: TOrder read FOrdem write SetOrdem default toDecrecente;
    property Finished: Boolean read GetFinished;
    // CallBack
    property OnReadMessage: TReadMessage read FOnReadMessage
      write SetReadMessage;
  end;

  TEmailFactory = class
  private type
    TEmailOperation = (emSend, emReceive);
  public
    class function GetTEmail(EmailOperation: TEmailOperation): TEmail;
  end;

implementation

{ TEmail }

procedure TEmail.BeforeConnect;
begin
  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnBeforeConnect) then
    FOnBeforeConnect(Self);
  if FThread.CheckTerminated then
    abort;
  // Instancia objetos
  if not Assigned(SSL) then
    SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
end;

procedure TEmail.AfterConnect(Connected: Boolean);
begin
  if FThread.CheckTerminated then
    abort;
  if Assigned(FOnAfterConnect) then
    FOnAfterConnect(Self, Connected);
  if FThread.CheckTerminated then
    abort;
end;

procedure TEmail.Cancel;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    if not FThread.Finished then
      FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TEmail.Connect;
begin
  try
    BeforeConnect;
    AfterConnect(DoConnect);
  except
    on E: EEmailError do
      if Assigned(FOnError) then
        FOnError(Self, E);
  end;
end;

procedure TEmail.TerminateThread(Sender: TObject);
begin
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

constructor TEmail.Create;
begin
  FAccount := TEmailAccount.Create;
end;

destructor TEmail.Destroy;
begin
  FreeAndNil(FAccount);
  if Assigned(SSL) then
    FreeAndNil(SSL);
  UnLoadOpenSSLLibrary;
end;

procedure TEmail.Execute;
begin
  if not FAccount.IsValid then
    raise Exception.Create('Email ' + FAccount.UserName + ' inválido!');
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        DoExecute;
      except
        on E: EEmailError do
          if Assigned(FOnError) then
            FOnError(Self, E);
      end;
    end);
  with FThread do
  begin
    FreeOnTerminate := False;
    OnTerminate := TerminateThread;
    Start;
  end;
end;

function TEmail.GetOnAfterConnect: TAfterOperation;
begin
  result := FOnAfterConnect;
end;

function TEmail.GetOnAfterExecute: TNotifyEvent;
begin
  result := FOnAfterExecute;
end;

function TEmail.GetOnBeforeConnect: TNotifyEvent;
begin
  result := FOnBeforeConnect;
end;

function TEmail.GetOnBeforeExecute: TNotifyEvent;
begin
  result := FOnBeforeExecute;
end;

function TEmail.GetOnError: TErroEmail;
begin
  result := FOnError;
end;

procedure TEmail.SetAccount(const Value: TEmailAccount);
begin
  FAccount := Value;
end;

procedure TEmail.SetOnAfterConnect(const Value: TAfterOperation);
begin
  FOnAfterConnect := Value;
end;

procedure TEmail.SetOnAfterExecute(const Value: TNotifyEvent);
begin
  FOnAfterExecute := Value;
end;

procedure TEmail.SetOnBeforeConnect(const Value: TNotifyEvent);
begin
  FOnBeforeConnect := Value;
end;

procedure TEmail.SetOnBeforeExecute(const Value: TNotifyEvent);
begin
  FOnBeforeExecute := Value;
end;

procedure TEmail.SetOnError(const Value: TErroEmail);
begin
  FOnError := Value;
end;

{ TSend }

function TEmailSend.DoConnect: Boolean;
var
  MessageError: string;
begin
  result := False;
  // Instancia SMTP caso ainda não esteja instanciado
  if not Assigned(IdSMTP) then
    IdSMTP := TIdSMTP.Create(nil);
  // Configura conexão
  ConfigConnection;
  // Conexão e autenticação
  try
    IdSMTP.Connect;
  except
    raise EConnectError.Create
      ('Ocorreu um erro durante a conexão com o servidor de e-mail.');
  end;
  if FThread.CheckTerminated then
    abort;
  try
    IdSMTP.Authenticate;
    result := True;
  except
    raise EAuthentError.Create
      ('Ocorreu um erro durante a autenticação da conta de e-mail, verifique a conta!');
  end;
end;

constructor TEmailSend.Create;
begin
  inherited;
  FAnexos := TStringList.Create;
  FTexto := TStringList.Create;
  FDestino := TStringList.Create;
end;

destructor TEmailSend.Destroy;
begin
  // Cancela thread caso esteja executando
  Cancel;
  // Desconecta e libera SMTP
  if Assigned(IdSMTP) then
  begin
    IdSMTP.Disconnect;
    FreeAndNil(IdSMTP);
  end;
  // Elimina listas
  FreeAndNil(FAnexos);
  FreeAndNil(FTexto);
  FreeAndNil(FDestino);
  FreeAndNil(FAccount);
  inherited;
end;

procedure TEmailSend.AjustaRemetente;
begin
  // Assume remetente com os dados de origem caso não informado
  if RemetenteEmail = EmptyStr then
    RemetenteEmail := FAccount.UserName;
  if RemetenteNome = EmptyStr then
    if FAccount.Name <> EmptyStr then
      RemetenteNome := FAccount.Name
    else
      RemetenteNome := FAccount.UserName;
end;

procedure TEmailSend.CreateAndConfigMessage(var IdMessage: TIdMessage);
var
  i: Integer;
  IdText: TIdText;
begin
  if FThread.CheckTerminated then
    abort;
  // Cria mensagem
  IdMessage := TIdMessage.Create;
  // Configuração da mensagem
  IdMessage.From.Address := RemetenteEmail;
  IdMessage.From.Name := RemetenteNome;
  IdMessage.Subject := FAssunto;
  IdMessage.Encoding := meMIME;
  // Adiciona destinatários
  IdMessage.ReplyTo.EMailAddresses := IdMessage.From.Address;
  for i := 0 to FDestino.Count - 1 do
    IdMessage.Recipients.Add.Text := FDestino[i];
  // Configuração do corpo do email
  IdText := TIdText.Create(IdMessage.MessageParts);
  IdText.Body := FTexto;
  // Anexa anexos
  for i := 0 to FAnexos.Count - 1 do
    if FileExists(FAnexos[i]) then
      TIdAttachmentFile.Create(IdMessage.MessageParts, FAnexos[i]);
  if FThread.CheckTerminated then
    abort;
end;

procedure TEmailSend.ConfigConnection;
begin
  // Configuração do protocolo SSL (TIdSSLIOHandlerSocketOpenSSL)
  SSL.SSLOptions.Method := sslvSSLv23;
  SSL.SSLOptions.Mode := sslmClient;
  // Configuração do servidor SMTP (TIdSMTP)
  with IdSMTP do
  begin
    IOHandler := SSL;
    AuthType := satDefault;
    UserName := FAccount.UserName;
    Password := FAccount.Password;
    if Self.FAccount.SMTP = smGmail then
    begin
      UseTLS := utUseImplicitTLS;
      Port := IdPORT_ssmtp;
      Host := 'smtp.gmail.com';
    end
    else
    begin
      UseTLS := utUseExplicitTLS;
      Port := Id_PORT_submission;
      Host := 'smtp.live.com';
    end;
  end;
end;

procedure TEmailSend.DoExecute;
var
  IdMessage: TIdMessage;
begin
  if Assigned(FOnBeforeSend) then
    FOnBeforeSend(Self);
  if FThread.CheckTerminated then
    abort;
  Enviou := False;
  try
    // Conecta se ainda não estiver conectado
    if not Assigned(IdSMTP) or not IdSMTP.Connected then
      Connect;
    // Ajusta remetente caso não informado
    AjustaRemetente;
    // Cria e configura mensagem
    CreateAndConfigMessage(IdMessage);
    try
      // Envia e-mail
      IdSMTP.Send(IdMessage);
      Enviou := True;
    except
      raise ESendError.Create
        ('Erro ao enviar e-mail, este erro pode ocorrer devido ao destinário inválido ou serviço de envio de email indisponível!');
    end;

  finally
    FreeAndNil(IdMessage);
    FreeAndNil(FDestino);
    FreeAndNil(FAnexos);
    FreeAndNil(FTexto);
  end;
end;

procedure TEmailSend.SetAfterSend(const Value: TAfterOperation);
begin
  FOnAfterSend := Value;
end;

procedure TEmailSend.SetAnexos(const Value: TStringList);
begin
  FAnexos := Value;
end;

procedure TEmailSend.SetAssunto(const Value: string);
begin
  FAssunto := Value;
end;

procedure TEmailSend.SetBeforeSend(const Value: TNotifyEvent);
begin
  FOnBeforeSend := Value;
end;

procedure TEmailSend.SetDestino(const Value: TStringList);
begin
  FDestino := Value;
end;

procedure TEmailSend.SetRemetenteEmail(const Value: string);
begin
  FRemetenteEmail := Value;
end;

procedure TEmailSend.SetRemetenteNome(const Value: string);
begin
  FRemetenteNome := Value;
end;

procedure TEmailSend.SetTexto(const Value: TStringList);
begin
  FTexto := Value;
end;

{ TReceive }

procedure TEmailReceive.AdicionaEmail
  (Lista: System.Generics.Collections.TList<TIdMessage>; i: Integer);
var
  IdMessage: TIdMessage;
begin
  if FThread.CheckTerminated then
    abort;
  IdMessage := TIdMessage.Create(nil);
  IdPOP.Retrieve(i, IdMessage);
  Lista.Add(IdMessage);
  MsgPos := i;
  if Assigned(FOnReadMessage) then
    FOnReadMessage(Self, IdMessage);
  if FThread.CheckTerminated then
    abort;
end;

function TEmailReceive.DoConnect: Boolean;
var
  MessageError: string;
begin
  result := False;
  if not Assigned(IdPOP) then
    IdPOP := TIdPop3.Create(nil);
  if Self.FAccount.SMTP = smGmail then
  begin
    IdPOP.Host := 'pop.gmail.com';
    IdPOP.Port := 995;
  end
  else
  begin
    raise Exception.Create('Função não disponivel para Hotmail');
  end;
  IdPOP.UserName := Self.FAccount.UserName;
  IdPOP.Password := Self.FAccount.Password;
  SSL.Host := IdPOP.Host;
  SSL.Port := IdPOP.Port;
  SSL.Destination := SSL.Host + ':' + IntToStr(SSL.Port);
  IdPOP.IOHandler := SSL;
  IdPOP.UseTLS := utUseImplicitTLS;
  if FThread.CheckTerminated then
    abort;
  try
    IdPOP.Connect;
    result := True;
  except
    raise EConnectError.Create('Erro durante a conexão com o servidor de email!'
      + #13#10 +
      'Verifique a conta e se o servidor de e-mail está funcionando.');
  end;
end;

destructor TEmailReceive.Destroy;
var
  i: Integer;
begin
  // Cancela thread caso esteja executando
  Cancel;
  if Assigned(FEmails) then
  begin
    for i := 0 to FEmails.Count - 1 do
      FEmails[i].Free;
    FreeAndNil(FEmails);
  end;
  if Assigned(IdPOP) then
    FreeAndNil(IdPOP);
  inherited;
end;

procedure TEmailReceive.DoExecute;
var
  i: Cardinal;
  MessageError: string;
begin
  if not Assigned(IdPOP) or not IdPOP.Connected then
    Connect;
  MsgCount := 0;
  FEmails := TList<TIdMessage>.Create;
  try
    MsgCount := IdPOP.CheckMessages;
  except
    raise EReceiveError.Create
      ('Conta foi conectada, porem, ao tentar obter mensagem ocorreu um erro inesperado!');
  end;
  if (Quantidade = 0) or (Quantidade > MsgCount) then
    MsgProcess := MsgCount
  else
    MsgProcess := Quantidade;
  if Ordem = toCrescente then
    for i := Inicial to MsgProcess - 1 Do
      AdicionaEmail(FEmails, i)
  else
    for i := MsgCount - Inicial downto MsgCount - Inicial - MsgProcess - 1 do
      AdicionaEmail(FEmails, i);
end;

procedure TEmailReceive.SetInicial(const Value: Cardinal);
begin
  FInicial := Value;
end;

procedure TEmailReceive.SetOrdem(const Value: TOrder);
begin
  FOrdem := Value;
end;

procedure TEmailReceive.SetQuantidade(const Value: Cardinal);
begin
  FQuantidade := Value;
end;

procedure TEmailReceive.SetReadMessage(const Value: TReadMessage);
begin
  FOnReadMessage := Value;
end;

function TEmailReceive.GetFinished: Boolean;
begin
  if (MsgCount = 0) or (MsgProcess < MsgCount) then
    result := False
  else if MsgProcess = MsgPos then
    result := True
  else
    result := False;
end;

{ TAccount }

procedure TAccount.Assign(Source: TPersistent);
begin
  if Source is TAccount then
    with TAccount(Source) do
    begin
      Self.Name := Name;
      Self.Password := Password;
      Self.UserName := UserName;
    end
  else
    inherited Assign(Source);
end;

function TAccount.IsValid: Boolean;
begin
  result := FUserName = EmptyStr;
end;

procedure TAccount.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TAccount.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAccount.SetUserName(const Value: String);
begin
  FUserName := Value;
end;

{ TEmailAccount }

procedure TEmailAccount.DiscoverySMTP;
begin
  if Pos('@HOTMAIL', UpperCase(Self.FUserName)) > 0 then
    FSMTP := smHotmail
  else if Pos('@GMAIL', UpperCase(Self.FUserName)) > 0 then
    FSMTP := smGmail
  else
    FSMTP := smNenhum;
end;

function TEmailAccount.GetSMTP: TSMTP;
begin
  result := FSMTP;
end;

function TEmailAccount.IsValid: Boolean;
var
  RegEx: TRegEx;
begin
  result := inherited;
  if (FSMTP <> smNenhum) and (Password <> EmptyStr) then
    RegEx := TRegEx.Create('(.)+@+.+[.]+(.)+');
  if RegEx.IsMatch(FUserName) then
    result := True;
end;

procedure TEmailAccount.SetUserName(const Value: String);
begin
  inherited;
  DiscoverySMTP;
end;

{ TEmailFactory }

class function TEmailFactory.GetTEmail(EmailOperation: TEmailOperation): TEmail;
begin
  case EmailOperation of
    emSend:
      result := TEmailSend.Create;
    emReceive:
      result := TEmailReceive.Create;
  else
    result := nil;
  end;
end;

{ EEmailError }

constructor EEmailError.Create;
begin
  Create('Ocorreu um erro desconhecido ao utilizar e-mail', eUnknown);
end;

constructor EEmailError.Create(Msg: String; Kind: TEKind);
begin
  FKind := Kind;
  Self.Message := Msg;
end;

procedure EEmailError.SetKind(const Value: TEKind);
begin
  FKind := Value;
end;

{ EConnectError }

constructor EConnectError.Create(const Msg: string);
begin
  inherited Create(Msg, eConnect);
end;

{ EAuthentError }

constructor EAuthentError.Create(const Msg: string);
begin
  inherited Create(Msg, eAuthent);
end;

{ ESendError }

constructor ESendError.Create(const Msg: string);
begin
  inherited Create(Msg, eSend);
end;

{ EReceiveError }

constructor EReceiveError.Create(const Msg: string);
begin
  inherited Create(Msg, eReceive);
end;

end.
