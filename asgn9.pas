program ExprCInterpreter;

{$mode objfpc}
{$M+}

uses
    SysUtils, TypInfo;

type
   RealList = array of Real;
   ExprC = Class
end;

type
   NumC = class(ExprC)
   private
      value: Real;
   public
      constructor Create(n: Real);
      function getVal() : Real;
end;

type
   AppC = class(ExprC)
   private
      id: ExprC;
      args: RealList;
   public
      constructor Create(i: ExprC; a: RealList);
end;

type
   IdC = class(ExprC)
   private
      id: string;
   public
      constructor Create(i: string);
end;

type
   IfC = class(ExprC)
   private
      test, thenVal, elseVal: ExprC;
   public
      constructor Create(tt, tV, eV: ExprC);
end;

type
   LamC = class(ExprC)
   private
      arg: RealList;
      body: ExprC;
   public
      constructor Create(a: RealList; b: ExprC);
end;

type
   StringC = class(ExprC)
   private
      str: string;
   public
      constructor Create(s: string);
      function getString() : string;
end;

type
   Value = class
end;

type
   Bind = class
   private
      name: string;
      value: Value;
   public
      constructor Create(n: string; v: Value);
end;

BindArray = array of Bind;

type
   Env = class
   private
      bindings: array of Bind;
   public
      constructor Create(b: BindArray);
end;

type
   NumV = class(Value)
   private
      value: Real;
   public
      constructor Create(n: Real);
      function getValue() : Real;
end;

type
   BoolV = class(Value)
   private
      bool: Boolean;
   public
      constructor Create(b: Boolean);
      function getBool() : Boolean;
end;

type
   CloV = class(Value)
   private
      args: RealList;
      body: ExprC;
      env: Env;
   public
      constructor Create(a: RealList; b: ExprC; e: Env);
end;

type
   PrimV = class(Value)
   private
      op: string;
      args: Real;
   public
      constructor Create(o: string; a: Real);
end;

type
   StringV = class(Value)
   private
      str: string;
   public
      constructor Create(s: string);
      function getStr() : string;
end;

{ Constructors }
constructor NumC.Create(n: Real);
begin
   value := n;
end;

function NumC.getVal() : Real;
begin
   getVal := value;
end;

constructor AppC.Create(i: ExprC; a: RealList);
begin
   id := i;
   args := a;
end;

constructor IdC.Create(i: string);
begin
   id := i;
end;

constructor IfC.Create(tt, tV, eV: ExprC);
begin
   test := tt;
   thenVal := tV;
   elseVal := eV;
end;

constructor LamC.Create(a: RealList; b: ExprC);
begin
   arg := a;
   body := b;
end;

constructor StringC.Create(s: string);
begin
   str := s;
end;

function StringC.getString() : string;
begin
   getString := str;
end;

constructor Bind.Create(n: string; v: Value);
begin
   name := n;
   value := v;
end;

constructor Env.Create(b: BindArray);
begin
   bindings := b;
end;

constructor NumV.Create(n: Real);
begin
   value := n;
end;

function NumV.getValue() : Real;
begin
   getValue := value;
end;

constructor BoolV.Create(b: Boolean);
begin
   bool := b;
end;

function BoolV.getBool() : Boolean;
begin
   getBool := bool;
end;

constructor CloV.Create(a: RealList; b: ExprC; e: Env);
begin
   args := a;
   body := b;
   env := e;
end;

constructor PrimV.Create(o: string; a: Real);
begin
   op := o;
   args := a;
end;

constructor StringV.Create(s: string);
begin
   str := s;
end;

function StringV.getStr() : string;
begin
   getStr := str;
end;

function interp(e: ExprC; env: env): Value;
begin
    if e is NumC then
        Result := NumV.Create(NumC(e).getVal())
    else if e is StringC then
        Result := StringV.Create(StringC(e).getString())
    else if e is AppC then
        WriteLn('AppC Case')
    else if e is LamC then
        WriteLn('LamC Case')
    else if e is IfC then
        WriteLn('IfC Case')
    else if e is IdC then
        WriteLn('IdC Case')
    else
        WriteLn('Error')
end;

function serialize(v: Value): Real;
begin
    if v is NumV then
        begin
            WriteLn(NumV(v).getValue());
            Result := 0;
        end
    else if v is StringV then
        begin
            WriteLn(StringV(v).getStr());
            Result := 0;
        end
    else if v is BoolV then
        begin
            WriteLn(BoolV(v).getBool());
            Result := 0;
        end
    else if v is CloV then
        begin
            WriteLn('<procedure>');
            Result := 0;
        end
    else if v is PrimV then
        begin
            WriteLn('<primop>');
            Result := 0;
        end
    else
        Result := -1;
end;

function topinterp(e: ExprC): Real;
var
    topEnv: Env;
begin
    topEnv := Env.Create(
        BindArray.Create(
        Bind.Create('true', BoolV.Create(True)),
        Bind.Create('false', BoolV.Create(False)),
        Bind.Create('+', PrimV.Create('+', 2)),
        Bind.Create('-', PrimV.Create('-', 2)),
        Bind.Create('*', PrimV.Create('*', 2)),
        Bind.Create('/', PrimV.Create('/', 2)),
        Bind.Create('<=', PrimV.Create('<=', 2)),
        Bind.Create('equal?', PrimV.Create('equal?', 2)),
        Bind.Create('error', PrimV.Create('error', 1))
        )
    );
    serialize(interp(e, topEnv));
    Result := 0;
end;

var
    num: NumC;
    stringy: StringC;
begin
    num := NumC.Create(5);
    if topinterp(num) = 1 then
        raise Exception.Create('uh oh');
    
    stringy := StringC.Create('aaaah');
    if topinterp(stringy) = 1 then
        raise Exception.Create('uh oh 2');
end.
