pragma Profile (No_Implementation_Extensions);

with Ada.Command_Line;
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

private with Ada.Strings.Unbounded;
private with Ada.Containers;
private with Ada.Containers.Indefinite_Hashed_Maps, Ada.Strings.Hash;
private with Ada.Containers.Doubly_Linked_Lists;

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
   
   type String_Callback is access procedure (Argument: String);
   type Boolean_Callback is access procedure (Argument: Boolean);
   type Integer_Callback is access procedure (Argument: Integer);
   type Natural_Callback is access procedure (Argument: Natural);
   type Positive_Callback is access procedure (Argument: Positive);



   function Make_Anonymous_Option (Callback: String_Callback) return Option_Ptr;
   function Make_Boolean_Callback_Option (Callback: Boolean_Callback) return Option_Ptr;
   function Make_Integer_Callback_Option (Callback: Integer_Callback) return Option_Ptr;
   function Make_Natural_Callback_Option (Callback: Natural_Callback) return Option_Ptr;
   function Make_Positive_Callback_Option (Callback: Positive_Callback) return Option_Ptr;
   
   type Boolean_Option is limited interface;
   function Value (A : Boolean_Option) return Boolean is abstract;
   
   type Integer_Option is limited interface;
   function Value (A: Integer_Option) return Integer is abstract;
   
   type String_Option is limited interface;
   function Value (A: String_Option) return String is abstract;

   type Argument_Parser is new Ada.Finalization.Limited_Controlled with private
   with Constant_Indexing => Constant_Reference,
     Default_Iterator => Iterate,
     Iterator_Element => Option;

   -- These functions shadow the standard library, but if they are used in a
   -- dispatching way they can be over-ridden in derived types, which is useful
   -- for tests

   function Command_Name(A : in Argument_Parser) return String
   with Pre'Class => not A.Ready;
   function Argument_Count(A : in Argument_Parser) return Natural
   with Pre'Class => not A.Ready;
   function Argument(A : in Argument_Parser; Number : in Positive)
                    return String
   with Pre'Class => not A.Ready;

   function Ready(A : in Argument_Parser) return Boolean;

   -- Initialising the Argument_Parser

   procedure Add_Option(A : in out Argument_Parser;
                        O : in Option_Ptr;
                        Name : in String;
                        Short_Option : in Character := '-';
                        Long_Option : in String := "";
                        Usage : in String := "";
                        Prepend_Usage : in Boolean := False
                       )
   with Pre'Class => A.Ready;

   procedure Set_Prologue(A: in out Argument_Parser;
                          Prologue : in String)
     with Pre'Class => A.Ready;

   -- Processing the command line

   procedure Parse_Command_Line(A : in out Argument_Parser)
     with Pre'Class => Ready(Argument_Parser'Class(A)),
       Post'Class => not Ready(Argument_Parser'Class(A));

   -- Retrieving the results

   function Parse_Success(A : in Argument_Parser) return Boolean
     with Pre'Class => not Ready(Argument_Parser'Class(A));
   function Parse_Message(A : in Argument_Parser) return String
     with Pre'Class => not Ready(Argument_Parser'Class(A));

   function Boolean_Value(A : in Argument_Parser; Name : in String)
                          return Boolean
     with Pre'Class => not Ready(Argument_Parser'Class(A));
   function Integer_Value(A : in Argument_Parser; Name : in String)
                          return Integer
     with Pre'Class => not Ready(Argument_Parser'Class(A));
   function String_Value(A : in Argument_Parser; Name : in String)
                         return String
     with Pre'Class => not Ready(Argument_Parser'Class(A));

   package String_Doubly_Linked_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists(Element_Type => String);
   function Tail(A: in Argument_Parser) return String_Doubly_Linked_Lists.List
     with Pre'Class => not Ready(Argument_Parser'Class(A));

   -- Convenience procedure for printing usage text

   procedure Usage(A : in Argument_Parser);

   -- The following definitions are to support the indexing, dereferencing and
   -- iteration over the Argument_Parser type

   type Cursor is private;
   function Has_Element(Position : Cursor) return Boolean;
   function Option_Name(Position : Cursor) return String;

   package Argument_Parser_Iterators is new Ada.Iterator_Interfaces(Cursor => Cursor,
                                                                    Has_Element => Has_Element);
   function Iterate (Container : in Argument_Parser)
      return Argument_Parser_Iterators.Forward_Iterator'Class;

   type Option_Constant_Ref(Element : not null access constant Option'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Reference(C : aliased in Argument_Parser;
                               Position : in Cursor) return Option_Constant_Ref;

   function Constant_Reference(C : aliased in Argument_Parser;
                               Name : in String) return Option_Constant_Ref;
   function Get(C : aliased in Argument_Parser;
                Name : in String) return Option_Constant_Ref
     renames Constant_Reference;

   
private 
   use Ada.Strings.Unbounded;

   type Option is abstract new Ada.Finalization.Limited_Controlled with
      record
         Set : Boolean := False;
      end record;

   overriding procedure Finalize(Object : in out Option) is null;

   procedure Set_Option(O : in out Option; A : in out Argument_Parser'Class) is null;
   procedure Set_Option_Argument(O : in out Option;
                                 Arg : in String;
                                 A : in out Argument_Parser'Class) is null;

   function Set(O : in Option) return Boolean is (O.Set);

   -- The following instantiations of the standard containers are used as the
   -- basic data structures for storing information on options

   package Option_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => String,
                                                                    Element_Type => Option_Ptr,
                                                                    Hash => Ada.Strings.Hash,
                                                                    Equivalent_Keys => "=");

   function Char_Hash(C : in Character) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Character'Pos(C)));

   package Option_Char_Maps is new Ada.Containers.Indefinite_Hashed_Maps(Key_Type => Character,
                                                                    Element_Type => Option_Ptr,
                                                                    Hash => Char_Hash,
                                                                    Equivalent_Keys => "=");

   type Argument_Parser_State is (Init,
                                  Ready,
                                  Required_Argument,
                                  Positional_Only,
                                  Finish_Success,
                                  Finish_Erroneous);

   type Argument_Parser is new Ada.Finalization.Limited_Controlled with
      record
         State : Argument_Parser_State := Init;
         Last_Option : access Option'Class := null;
         Arguments : Option_Maps.Map := Option_Maps.Empty_Map;
         Long_Options : Option_Maps.Map := Option_Maps.Empty_Map;
         Short_Options : Option_Char_Maps.Map := Option_Char_Maps.Empty_Map;
         Current_Positional : Positional_Lists.Cursor := Positional_Lists.No_Element;
         Positional : Positional_Lists.List := Positional_Lists.Empty_List;
         Allow_Tail : Boolean := False;
         Tail_Usage : Unbounded_String := Null_Unbounded_String;
         Tail : String_Doubly_Linked_Lists.List := String_Doubly_Linked_Lists.Empty_List;
         Message : Unbounded_String := Null_Unbounded_String;
         Prologue : Unbounded_String := Null_Unbounded_String;
         Option_Help_Details : Option_Help_Lists.List := Option_Help_Lists.Empty_List;
      end record;

   overriding procedure Finalize(Object : in out Argument_Parser);

   function Argument_Count(A : in Argument_Parser) return Natural is
     (Ada.Command_Line.Argument_Count);
   function Argument(A : in Argument_Parser; Number : in Positive) return String is
     (Ada.Command_Line.Argument(Number));

   function Ready(A : in Argument_Parser) return Boolean is
     (A.State = Init);

   function Command_Name(A : in Argument_Parser) return String is
     (Ada.Command_Line.Command_Name);

   type Option_Constant_Ref(Element : not null access constant Option'Class) is null record;

   type Cursor is new Option_Maps.Cursor;

   type Argument_Parser_Iterator is new Argument_Parser_Iterators.Forward_Iterator with
      record
         Start : Option_Maps.Cursor;
      end record;
   function First (Object : Argument_Parser_Iterator) return Cursor;
   function Next (Object : Argument_Parser_Iterator; Position : Cursor)
                  return Cursor;
end Flex_Args;
