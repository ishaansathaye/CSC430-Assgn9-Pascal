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
end;

type
   BoolV = class(Value)
   private
      bool: Boolean;
   public
      constructor Create(b: Boolean);
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

constructor BoolV.Create(b: Boolean);
begin
   bool := b;
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

function interp(e: ExprC): Value;
begin
    if TypeInfo(e) = TypeInfo(NumC) then
        Result := NumV.Create(2)
    else
        Result := StringV.Create('hi');
end;


var
    num: NumC;
    num2: Value;

begin
    num := NumC.Create(5);
    num2 := interp(num);
    WriteLn('Num = ', num.getVal);
end.
