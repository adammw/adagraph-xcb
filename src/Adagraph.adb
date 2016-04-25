-----------------------------------------------------------------------
--
--  File:        adagraph.adb
--  Description: basic X11 graphics using XCB
--  Rev:         0.1
--  Date:        April 2016
--  Prel:        xcb_library_thin_ada_bindings
--  Author:      Adam Malcontenti-Wilson
--
--  Copyright (c) Jerry van Dijk, 1997, 1998, 1999
--  Billie Hollidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int +31 (0)71 531 4365
--  for the win32 spec
--
--  Copyright (c) Adam Malcontenti-Wilson, 2016
--  for the XCB body
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Task_Identification;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with XCB;
with System;

package body Adagraph is
  use type XCB.Gcontext_Id_Type;
  use type XCB.CW_Type;
  use type XCB.Gc_Type;
  use type XCB.Event_Mask_Type;
  use type XCB.Generic_Event_Access_Type;
  use type XCB.Generic_Error_Access_Type;
  use type Interfaces.C.unsigned;
  use type Interfaces.C.int;
  use type Interfaces.Unsigned_8;

  package C renames Interfaces.C;

  type RGB_Color_Type is record
    Red, Green, Blue : Integer;
  end record;

  Adagraph_XCB_Version : constant Integer := 1;
  X11_Font_Name : constant String := "fixed";
  X11_Color_Map : constant array (Color_Type'Range) of RGB_Color_Type := (
    Black         => (     0,     0,     0),
    Blue          => (     0,     0, 32768),
    Green         => (     0, 32768,     0),
    Cyan          => (     0, 32768, 32768),
    Red           => ( 32768,     0,     0),
    Magenta       => ( 32768,     0, 32768),
    Brown         => ( 32768, 32768,     0),
    Light_Gray    => ( 44032, 44032, 44032),
    Dark_Gray     => ( 20224, 20224, 20224),
    Light_Blue    => (     0,     0,   255),
    Light_Green   => (     0, 65280,     0),
    Light_Cyan    => (     0, 65280, 65280),
    Light_Red     => ( 65280,     0,     0),
    Light_Magenta => ( 65280,     0, 65280),
    Yellow        => ( 65280, 65280,     0),
    White         => ( 65280, 65280, 65280)
  );
  X_Char_Size : constant Integer := 8;
  Y_Char_Size : constant Integer := 12;

  Pixel_Color_Map : array (Color_Type'Range) of aliased Interfaces.Unsigned_32;

  Connection : XCB.Connection_Access_Type;
  Screen : XCB.Screen_Access_Type;
  Window : XCB.Window_Id_Type;
  Graphics : XCB.Gcontext_Id_Type;
  ColorMap : XCB.Colormap_Id_Type;
  Font : XCB.Font_Id_Type;
  pragma Warnings (Off, Font);

  HasKeyboardEvent : Boolean := False;
  KeyPressed : Character;

  Unused_Cookie : XCB.Void_Cookie_Type;
  pragma Unreferenced (Unused_Cookie);

  -------------------------
  -- Internal procedures --
  -------------------------

  procedure Test_Cookie(Cookie : XCB.Void_Cookie_Type; Error_Message : String) is
    Error : XCB.Generic_Error_Access_Type;
  begin
    Error := XCB.Request_Check (Connection, Cookie);

      if (Error /= null) then
         Ada.Text_IO.Put_Line ("ERROR: " & Error_Message & " : " & Error.Error_Code'Img);
         raise Unknown_Adagraph_Error;
      end if;
  end Test_Cookie;

  procedure Initialize is
    Font_Cookie : XCB.Void_Cookie_Type;
  begin
    -- open connection with the server
    Connection := XCB.Connect(Interfaces.C.Strings.Null_Ptr, null);

    if XCB.Connection_Has_Error (Connection) /= 0 then
      Ada.Text_IO.Put_Line ("Failed to connect to the x-server");
      return;
    end if;

    -- Get the first screen
    Screen := XCB.Setup_Roots_Iterator ( XCB.Get_Setup (Connection) ).Data;

    -- Get the font
    Font_Cookie := XCB.Open_Font(Connection, Font, X11_Font_Name'Length, C.Strings.New_String(X11_Font_Name));
    Test_Cookie(Font_Cookie, "Can't open font");
  end Initialize;

  procedure Flush is
    Flush_Number : Interfaces.C.int;
  begin
    Flush_Number := XCB.Flush (Connection);
    if Flush_Number <= 0 then
       Ada.Text_IO.Put_Line ("Failed to flush");
    end if;
  end Flush;

  procedure Change_Gc_Color(Color : Color_Type) is
    Graphics_Value_Mask : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(XCB.XCB_GC_FOREGROUND);
    Graphics_Value_List : aliased XCB.Value_List_Array (0..0) := (0 => Pixel_Color_Map(Color));
  begin
    Unused_Cookie := XCB.Change_Gc(Connection, Graphics, Graphics_Value_Mask, Graphics_Value_List);
  end Change_Gc_Color;

  procedure Change_Wnd_Bg_Color(Color : Color_Type) is
    Window_Value_Mask : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(XCB.XCB_CW_BACK_PIXEL);
    Window_Value_List : aliased XCB.Value_List_Array (0..0) := (0 => Pixel_Color_Map(Color));
  begin
    Unused_Cookie := XCB.Change_Window_Attributes(Connection, Window, Window_Value_Mask, Window_Value_List);
  end Change_Wnd_Bg_Color;

  procedure Poll_And_Handle_Events is
    Event : XCB.Generic_Event_Access_Type;

    type Key_Symbols_Type is limited null record;
    type Key_Symbols_Type_Access_Type is access all Key_Symbols_Type;

    type Keysym_Type is new Interfaces.Unsigned_32;

    function Key_Symbols_Alloc (C : XCB.Connection_Access_Type) return Key_Symbols_Type_Access_Type;
    pragma Import (C, Key_Symbols_Alloc, "xcb_key_symbols_alloc");

    function Key_Press_Lookup_Keysym (
      S : Key_Symbols_Type_Access_Type;
      E : XCB.Key_Press_Event_Access_Type;
      State : Interfaces.Unsigned_16
    ) return Keysym_Type;
    pragma Import (C, Key_Press_Lookup_Keysym, "xcb_key_press_lookup_keysym");

    KeyboardEvent : XCB.Key_Press_Event_Access_Type;
    Key_Symbols : Key_Symbols_Type_Access_Type;
    Keysym : Keysym_Type;
  begin
    if Event /= null then
      XCB.Free(Event);
    end if;

    HasKeyboardEvent := False;
    Event := XCB.Poll_For_Event(Connection);
    Key_Symbols := Key_Symbols_Alloc(Connection);

    if Event /= null then
      case (Event.Response_Kind mod 128) is
        when XCB.XCB_EXPOSE =>
          -- Draw or redraw the window
          Flush;
        when XCB.XCB_KEY_PRESS =>
          KeyboardEvent := XCB.To_Key_Press_Event(Event);
          Keysym := Key_Press_Lookup_Keysym(Key_Symbols, KeyboardEvent, KeyboardEvent.State);
          if Keysym < 255 then
            HasKeyboardEvent := True;
            KeyPressed := Character'Val(Keysym);
          end if;
        when XCB.XCB_CLIENT_MESSAGE =>
          Ada.Text_IO.Put_Line ("Clicked on the X-button");
          Ada.Task_Identification.Abort_Task(Ada.Task_Identification.Current_Task);
        when others =>
          null;
      end case;
    end if;
  end Poll_And_Handle_Events;

  ----------------------------------
  -- System information functions --
  ----------------------------------

  function Get_Dll_Version return Integer is
  begin
    return Adagraph_XCB_Version;
  end Get_Dll_Version;

  -----------------------
  -- Window management --
  -----------------------

  procedure Create_Sized_Graph_Window (X_Size, Y_Size : in     Integer;
     X_Max,  Y_Max  :    out Integer;
     X_Char, Y_Char :    out Integer) is
    Graphics_Value_Mask : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(XCB.XCB_GC_FOREGROUND or XCB.XCB_GC_GRAPHICS_EXPOSURES );
    Graphics_Value_List : aliased XCB.Value_List_Array (0..1) := (Screen.Black_Pixel, 0);
    Window_Value_Mask : Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (XCB.XCB_CW_BACK_PIXEL or XCB.XCB_CW_EVENT_MASK);
    Window_Value_List : aliased XCB.Value_List_Array (0..1) := (Screen.White_Pixel, Interfaces.Unsigned_32(XCB.XCB_EVENT_MASK_EXPOSURE or XCB.XCB_EVENT_MASK_KEY_PRESS));
  begin
    -- Create window
    Window := XCB.Generate_Id (Connection);
    Unused_Cookie := XCB.Create_Window (C           => Connection,
                                       Depth        => Screen.Root_Depth,
                                       Wid          => Window,
                                       Parent       => Screen.Root,
                                       X            => 10,
                                       Y            => 10,
                                       Width        => Interfaces.Unsigned_16(X_Size),
                                       Height       => Interfaces.Unsigned_16(Y_Size),
                                       Border_Width => 1,
                                       Class        => Interfaces.Unsigned_16 (XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT),
                                       Visual       => Screen.Root_Visual,
                                       Value_Mask   => Window_Value_Mask,
                                       Value_List   => WIndow_Value_List);

    -- Create graphics context
    Graphics := XCB.Generate_Id(Connection);
    Unused_Cookie := XCB.Create_Gc(Connection, Graphics, Window, Graphics_Value_Mask, Graphics_Value_List);

    -- Create the colormap
    ColorMap := XCB.Generate_Id(Connection);
    Unused_Cookie := XCB.Create_Colormap(Connection, Interfaces.Unsigned_8(XCB.XCB_COLORMAP_ALLOC_NONE), ColorMap, Window, Screen.Root_Visual);

    for Color in Color_Type range X11_Color_Map'Range loop
      declare
        RGB_Color : RGB_Color_Type := X11_Color_Map(Color);
        Alloc_Color_Cookie : XCB.Alloc_Color_Cookie_Type;
        Alloc_Color_Reply : XCB.Alloc_Color_Reply_Type;
      begin
        Alloc_Color_Cookie := XCB.Alloc_Color(Connection, ColorMap, Interfaces.Unsigned_16(RGB_Color.Red), Interfaces.Unsigned_16(RGB_Color.Green), Interfaces.Unsigned_16(RGB_Color.Blue));
        Alloc_Color_Reply := XCB.Alloc_Color_Reply(Connection, Alloc_Color_Cookie, System.Null_Address).all;
        Pixel_Color_Map(Color) := Alloc_Color_Reply.Pixel;
      end;
    end loop;

    -- Change window background to gray
    Change_Wnd_Bg_Color(Light_Gray);

    -- Map (show) the window
    Unused_Cookie := XCB.Map_Window (Connection, Window);
    Flush;

    -- without this, some early text is missing
    delay 0.1;

    X_Max := X_Size;
    Y_Max := Y_Size;
    X_Char := 8;
    Y_Char := 12;
  end Create_Sized_Graph_Window;

  procedure Set_Window_Title (Title : in String) is
    C_Str : C.char_array := C.To_C(Title);
  begin
    Unused_Cookie := XCB.Change_Property(Connection,
                        XCB.XCB_PROP_MODE_REPLACE,
                        Window,
                        XCB.XCB_ATOM_WM_NAME,
                        XCB.XCB_ATOM_STRING,
                        Interfaces.Unsigned_8(8),
                        C_Str'Length,
                        C_Str'address);
    Flush;
  end Set_Window_Title;

  ---------------------
  -- Input functions --
  ---------------------

  function Key_Hit return Boolean is
  begin
    Poll_And_Handle_Events;
    return HasKeyboardEvent;
  end Key_Hit;

  function Get_Key return Character is
  begin
    return KeyPressed;
  end Get_Key;

  -----------------------
  -- Graphic functions --
  -----------------------

  procedure Draw_Line (X1, Y1, X2, Y2 : in Integer;
     Hue            : in Color_Type := White) is
    Points : XCB.Point_Array_Type := (
      (Interfaces.Integer_16(X1), Interfaces.Integer_16(Y1)),
      (Interfaces.Integer_16(X2), Interfaces.Integer_16(Y2))
    );
  begin
    Change_Gc_Color(Hue);
    Unused_Cookie := XCB.Poly_Line(Connection, Interfaces.Unsigned_8(XCB.XCB_COORD_MODE_ORIGIN), Window, Graphics, Points'Length, Points);
    Flush;
  end Draw_Line;

  procedure Draw_Box (X1, Y1, X2, Y2 : in Integer;
     Hue            : in Color_Type := White;
     Filled         : in Fill_Type  := No_Fill) is
       X : Interfaces.Integer_16 := Interfaces.Integer_16(X1);
       Y : Interfaces.Integer_16 := Interfaces.Integer_16(Y1);
       Width : Interfaces.Unsigned_16 := Interfaces.Unsigned_16(X2 - X1);
       Height : Interfaces.Unsigned_16 := Interfaces.Unsigned_16(Y2 - Y1);
       Rectangle : XCB.Rectangle_Type := (X, Y, Width, Height);
       Rectangles : XCB.Rectangle_Array_Type(1..1) := (1 => Rectangle);
  begin
    Change_Gc_Color(Hue);
    if Filled = Fill then
      Unused_Cookie := XCB.Poly_Fill_Rectangle(Connection, Window, Graphics, Rectangles'Length, Rectangles);
    else
      Unused_Cookie := XCB.Poly_Rectangle(Connection, Window, Graphics, Rectangles'Length, Rectangles);
    end if;
    Flush;
  end Draw_Box;

  procedure Draw_Circle (X, Y, Radius : in Integer;
     Hue          : in Color_Type := White;
     Filled       : in Fill_Type  := No_Fill) is
      Arc : XCB.Arc_Type := (
        X => Interfaces.Integer_16(X - Radius),
        Y => Interfaces.Integer_16(Y - Radius),
        Width => Interfaces.Unsigned_16(Radius * 2),
        Height => Interfaces.Unsigned_16(Radius * 2),
        Angle_1 => Interfaces.Integer_16(0 * 64),
        Angle_2 => Interfaces.Integer_16(360 * 64)
      );
      Arcs : XCB.Arc_Array_Type(1..1) := (1 => Arc);
  begin
    Change_Gc_Color(Hue);
    if Filled = Fill then
      Unused_Cookie := XCB.Poly_Fill_Arc(Connection, Window, Graphics, Arcs'Length, Arcs);
    else
      Unused_Cookie := XCB.Poly_Arc(Connection, Window, Graphics, Arcs'Length, Arcs);
    end if;
    Flush;
  end Draw_Circle;

  procedure Display_Text (X, Y : in Integer;
     Text : in String;
     Hue  : in Color_Type := White) is

    type Byte is mod 2**8;

    function Generate_Item(Text : String) return XCB.Byte_Array_Type is
      type Items_Type is record
        Length : Interfaces.Unsigned_8 := Text'Length;
        Dlta : Interfaces.Unsigned_8 := 0;
        C_Str : C.char_array(0..Text'Length) := C.To_C(Text);
      end record with
        Convention => C_Pass_By_Copy;

      subtype Items_Byte_Array_Type is XCB.Byte_Array_Type(0..(Items_Type'Size / Byte'Size));

      function Convert_To_Item_Type is new Ada.Unchecked_Conversion(Source => Items_Type, Target => Items_Byte_Array_Type);
      Item : Items_Type;
    begin
      return Convert_To_Item_Type(Item);
    end Generate_Item;

    Items_Byte_Array : XCB.Byte_Array_Type := Generate_Item(Text);
  begin
    Change_Gc_Color(Hue);
    Unused_Cookie := XCB.Poly_Text_8(Connection, Window, Graphics, Interfaces.Integer_16(X), Interfaces.Integer_16(Y + Y_Char_Size), Interfaces.Unsigned_32(Items_Byte_Array'Size / Byte'Size), Items_Byte_Array);
    Flush;
  end Display_Text;

begin
  Initialize;
end Adagraph;
