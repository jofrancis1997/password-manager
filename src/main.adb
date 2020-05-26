pragma SPARK_Mode (On);

with PasswordDatabase;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
procedure Main is
   DB : PasswordDatabase.Database;
   U1 : PasswordDatabase.URL := PasswordDatabase.From_String("google.com");
   P1 : PasswordDatabase.Password := PasswordDatabase.From_String("test_password");
   PIN1  : PIN.PIN := PIN.From_String("1234");
   PIN2  : PIN.PIN := PIN.From_String("1234");
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   Locked : Boolean := true;
   Masterpin : PIN.PIN;
begin

   if MyCommandLine.Argument_Count /= 1 then
      return;
   end if;

   Masterpin := PIN.From_String(MyCommandLine.Argument(1));
   PasswordDatabase.Init(DB);

   loop 
      if Locked = true then
         Put("locked>   ");
      else
         Put("unlocked> ");
      end if;
      Lines.Get_Line(S);
      declare
         T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
         NumTokens : Natural;
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
         declare
            Command : String := Lines.To_String(Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1));
         begin
            if Command = "get" then
               if NumTokens > 2 then
                  return;
               elsif NumTokens = 2 then
                  declare
                     U : PasswordDatabase.URL := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  begin
                     if not Locked then
                        Put_Line(PasswordDatabase.To_String(PasswordDatabase.Get(DB,U)));
                     end if;
                  end;
               end if;
            elsif Command = "rem" then
               if NumTokens > 2 then
                  return;
               elsif NumTokens = 2 then
                  declare
                     U : PasswordDatabase.URL := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  begin
                     if not Locked then
                        PasswordDatabase.Remove(DB,U);
                     end if;
                  end;
               end if;
            elsif Command = "put" then
               if NumTokens > 3 then
                  return;
               elsif NumTokens = 3 then
                  declare
                     U : PasswordDatabase.URL := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                     P : PasswordDatabase.Password := PasswordDatabase.From_String(Lines.To_String(Lines.Substring(S,T(3).Start,T(3).Start+T(3).Length-1)));
                  begin
                     if not Locked then
                        PasswordDatabase.Put(DB,U,P);
                     end if;
                  end;
               end if;
            elsif Command = "unlock" then
               if NumTokens > 2 then
                  return;
               elsif NumTokens = 2 then
                  declare
                     Number : PIN.PIN := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  begin
                     if Locked and PIN."="(Masterpin,Number) then
                        Locked := false;
                     end if;
                  end;
               end if;
            elsif Command = "lock" then
               if NumTokens > 2 then
                  return;
               elsif NumTokens = 2 then
                  declare
                     Number : PIN.PIN := PIN.From_String(Lines.To_String(Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1)));
                  begin
                     if not Locked and PIN."="(Masterpin,Number) then
                        Locked := true;
                     end if;
                  end;
               end if;
            else
               return;
            end if;
         end;
      end;
   end loop;

   Put_Line("Adding an entry to the database");
   PasswordDatabase.Put(DB,U1,P1);

   Put_Line("Reading the entry:");
   Put_Line(PasswordDatabase.To_String(PasswordDatabase.Get(DB,U1)));

   Put_Line("Removing the entry");
   PasswordDatabase.Remove(DB,U1);
   If PasswordDatabase.Has_Password_For(DB,U1) then
      Put_Line("Entry still present! It is: ");
      Put_Line(PasswordDatabase.To_String(PasswordDatabase.Get(DB,U1)));
   else
      Put_Line("Entry successfully removed");
   end if;

   Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   Lines.Get_Line(S);

   Put_Line("Splitting the text into at most 4 tokens");
   declare
      T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      NumTokens : Natural;
   begin
      MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
      Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
      if NumTokens > 3 then
         Put_Line("Too many tokens!");
      else
         for I in 1..NumTokens loop
            declare
               TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
            begin
               Put("Token "); Put(I); Put(" is: """);
               Put(TokStr); Put_Line("""");
            end;
         end loop;
      end if;
   end;

   If PIN."="(PIN1,PIN2) then
      Put_Line("The two PINs are equal, as expected.");
   end if;
end Main;
