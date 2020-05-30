with PasswordDatabase;

package body PasswordManager with Spark_Mode is
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
