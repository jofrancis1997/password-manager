with Ada.Containers;
use Ada.Containers;
with PasswordDatabase;
use PasswordDatabase;
with PIN;

package PasswordManager with Spark_Mode is
   type Manager is private;

   procedure Init(M : out Manager; P : in PIN.PIN) with
      Post => Locked(M) = True and PIN_Is_Master(M,P) = True;

   procedure Lock(M : in out Manager; P : in PIN.PIN) with
      Post => (if not Locked(M'Old) then
         Locked(M) = True and
         PIN_Is_Master(M,P) = True);

   procedure Unlock(M : in out Manager; P : in PIN.PIN) with
      Post => Length(M) = Length(M'Old) and
         (if Locked(M'Old) and PIN_Is_Master(M'Old,P) then
            Locked(M) = False);

   function Get(M : in Manager; U : in URL) return Password with
      Pre => not Locked(M) and Has_Password_For(M,U);

   procedure Put(M : in out Manager; U : in URL; P : in Password) with
      Pre => Length(M) < Max_Entries or Has_Password_For(M,U),
      Post => Locked(M) = Locked(M'Old);

   procedure Remove(M : in out Manager; U : in URL) with
      Post => Locked(M) = Locked(M'Old);

   function Locked(M : in Manager) return Boolean;

   function Has_Password_For(M : in Manager; U : in URL) return Boolean;

   function Length(M : in Manager) return Ada.Containers.Count_Type;

   function PIN_Is_Master(M : in Manager; P : in PIN.PIN) return Boolean;

   function Is_PIN(S : in String) return Boolean with
      Post => (if Is_PIN'Result then
         S'Length = 4 and
         (for all I in S'Range => S(I) >= '0' and S(I) <= '9'));

   function Is_URL(S : in String) return Boolean with
      Post => (if Is_URL'Result then
         S'Length <= Max_URL_Length);

   function Is_Password(S : in String) return Boolean with
      Post => (if Is_Password'Result then
         S'Length <= Max_Password_Length);
private
   type Manager is record
      locked : Boolean;
      masterpin : PIN.PIN;
      database : PasswordDatabase.Database;
   end record;

   function Locked(M : in Manager) return Boolean is
      (M.locked);

   function Has_Password_For(M : in Manager; U : in URL) return Boolean is 
      (PasswordDatabase.Has_Password_For(m.database,U));

   function Length(M : in Manager) return Ada.Containers.Count_Type is
      (PasswordDatabase.Length(m.database));

   function PIN_Is_Master(M : in Manager; P : in PIN.PIN) return Boolean is
      (PIN."="(M.masterpin,P));
end PasswordManager;
