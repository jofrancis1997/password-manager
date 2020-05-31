with PasswordDatabase;
with PIN;

package body PasswordManager with Spark_Mode is
   procedure Init(M : out Manager; P : in PIN.PIN) is
   begin
      M.locked := true;
      M.masterpin := P;
      PasswordDatabase.Init(M.database);
   end Init;

   procedure Lock(M : in out Manager; P : in PIN.PIN) is
   begin
      if not M.locked then
         M.masterpin := P;
         M.locked := true;
      end if;
   end Lock;

   procedure Unlock(M : in out Manager; P : in PIN.PIN) is
   begin
      if M.locked and PIN."="(M.masterpin,P) then
         M.locked := false;
      end if;
   end Unlock;

   function Get(M : in Manager; U : in PasswordDatabase.URL) return PasswordDatabase.Password is
   begin
      return PasswordDatabase.Get(M.database, U);
   end Get;

   procedure Put(M : in out Manager; U : in PasswordDatabase.URL; P : in PasswordDatabase.Password) is
   begin
      if not M.locked then
         PasswordDatabase.Put(M.database,U,P);
      end if;
   end Put;

   procedure Remove(M : in out Manager; U : in PasswordDatabase.URL) is
   begin
      if not M.locked and PasswordDatabase.Has_Password_For(M.database, U) then
         PasswordDatabase.Remove(M.database,U);
      end if;
   end Remove;

   function Is_PIN(S : in String) return Boolean is
   begin
      if S'Length /= 4 then
         return false;
      end if;
      for I in S'Range loop
         declare
            Ch : Character := S(I);
         begin
            if Ch < '0' or Ch > '9' then
               return false;
            end if;
            pragma Loop_Invariant (for all J in S'First..I => S(J) >= '0' and S(J) <= '9');
         end;
      end loop;
      return true;
   end Is_PIN;

   function Is_URL(S : in String) return Boolean is
   begin
      return S'Length <= PasswordDatabase.Max_URL_Length;
   end Is_URL;

   function Is_Password(S : in String) return Boolean is
   begin
      return S'Length <= PasswordDatabase.Max_Password_Length;
   end Is_Password;
end PasswordManager;
