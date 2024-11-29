pragma Profile (No_Implementation_Extensions);

with Ada.Finalization;
package Flex_Args is

   type Option is
     abstract new Ada.Finalization.Limited_Controlled with private;
   type General_Option_Ptr is access Option'Class;
   subtype Option_Ptr is not null General_Option_Ptr;

   function Set (O : Option) return Boolean;
   function Image (O : Option) return String is abstract;

   type Boolean_Option_Kind is (Set, Clear, Toggle);
   function Make_Boolean_Option
     (Kind : Boolean_Option_Kind := Set; Default : in Boolean := False)
      return Option_Ptr;
   function Make_Repeated_Option (Default : Natural := 0) return Option_Ptr;
   function Make_Integer_Option
     (Default : Integer := 0; Min : in Integer := Integer'First;
      Max     : Integer := Integer'Last) return Option_Ptr;
   function Make_Natural_Option
     (Default : Natural := 0) return Option_Ptr is
     (Make_Integer_Option (Default => Default, Min => 0, Max => Integer'Last));
   function Make_Positive_Option
     (Default : Positive := 1) return Option_Ptr is
     (Make_Integer_Option (Default => Default, Min => 1, Max => Integer'Last));
   function Make_String_Option (Default : String := "") return Option_Ptr;
   --  Make_Enumeration_Option -- this needs to be generic, obviously -- parse_args discrete_options!
   --  Arrays! -- parse_args discrete_array_options!
   
   type Boolean_Option is limited interface;
   function Value (A : Boolean_Option) return Boolean is abstract;
   
   type Integer_Option is limited interface;
   function Value (A: Integer_Option) return Integer is abstract;
   
   type String_Ooption is limited interface;
   function Value (A: String_Option) return String is abstract;

end Flex_Args;
