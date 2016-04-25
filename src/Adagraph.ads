-----------------------------------------------------------------------
--
--  File:        adagraph.ads
--  Description: basic Win32 graphics
--  Rev:         0.5c
--  Date:        23-jan-1999
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997, 1998, 1999
--  Billie Hollidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int +31 (0)71 531 4365
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
-----------------------------------------------------------------------

package Adagraph is

   -------------------
   -- Library types --
   -------------------

   type Event_Type is (None, Moved, Left_Up, Left_Down, Right_Up, Right_Down);

   type Mouse_Type is
   record
      Event : Event_Type;
      X_Pos : Integer;
      Y_Pos : Integer;
   end record;

   type Color_Type is (Black, Blue, Green, Cyan, Red, Magenta, Brown,
      Light_Gray, Dark_Gray, Light_Blue, Light_Green,
      Light_Cyan, Light_Red, Light_Magenta, Yellow, White);

   type Fill_Type is (No_Fill, Fill);


   -----------------------
   -- Library constants --
   -----------------------

   Adagraph_Dll_Version : constant Integer := 5;


   ------------------------------
   -- Extended character codes --
   ------------------------------

   Vk_Nul    : constant Character := Character'Val (16#00#);
   Vk_Prior  : constant Character := Character'Val (16#21#);
   Vk_Next   : constant Character := Character'Val (16#22#);
   Vk_End    : constant Character := Character'Val (16#23#);
   Vk_Home   : constant Character := Character'Val (16#24#);
   Vk_Left   : constant Character := Character'Val (16#25#);
   Vk_Up     : constant Character := Character'Val (16#26#);
   Vk_Right  : constant Character := Character'Val (16#27#);
   Vk_Down   : constant Character := Character'Val (16#28#);
   Vk_Insert : constant Character := Character'Val (16#2D#);
   Vk_Delete : constant Character := Character'Val (16#2E#);


   ----------------------------------
   -- System information functions --
   ----------------------------------

   function Get_Dll_Version return Integer;

  -- procedure Get_Max_Size (X_Size, Y_Size : out Integer);


   -----------------------
   -- Window management --
   -----------------------

  -- procedure Create_Graph_Window (X_Max,  Y_Max  : out Integer;
  --    X_Char, Y_Char : out Integer);
   --
  -- procedure Ext_Create_Graph_Window (X_Max,  Y_Max  : out Integer;
  --    X_Char, Y_Char : out Integer);

   procedure Create_Sized_Graph_Window (X_Size, Y_Size : in     Integer;
      X_Max,  Y_Max  :    out Integer;
      X_Char, Y_Char :    out Integer);

  -- procedure Destroy_Graph_Window;

  -- function Is_Open return Boolean;

   procedure Set_Window_Title (Title : in String);


   ---------------------
   -- Input functions --
   ---------------------

   function Key_Hit return Boolean;

   function Get_Key return Character;

  -- function Mouse_Event return Boolean;

  -- function Get_Mouse return Mouse_Type;


   -----------------------
   -- Graphic functions --
   -----------------------

  -- procedure Clear_Window (Hue : in Color_Type := Black);

  -- function Get_Pixel (X, Y : in Integer) return Color_Type;
   --
  -- procedure Put_Pixel (X, Y : in Integer; Hue : in Color_Type := White);

   procedure Draw_Line (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White);

   procedure Draw_Box (X1, Y1, X2, Y2 : in Integer;
      Hue            : in Color_Type := White;
      Filled         : in Fill_Type  := No_Fill);

   procedure Draw_Circle (X, Y, Radius : in Integer;
      Hue          : in Color_Type := White;
      Filled       : in Fill_Type  := No_Fill);

  -- procedure Draw_Ellipse (X1, Y1, X2, Y2 : in Integer;
  --    Hue            : in Color_Type := White;
  --    Filled         : in Fill_Type  := No_Fill);

  -- procedure Flood_Fill (X, Y : in Integer; Hue : in Color_Type := White);

   procedure Display_Text (X, Y : in Integer;
      Text : in String;
      Hue  : in Color_Type := White);


   ----------------------
   -- Polydraw support --
   ----------------------

  -- function Where_X return Integer;

  -- function Where_Y return Integer;

  -- procedure Goto_Xy (X, Y : in Integer);

  -- procedure Draw_To (X, Y : in Integer; Hue : in Color_Type := White);


   ----------------
   -- Exceptions --
   ----------------

   Close_Handle_Failed     : exception;
   Create_Event_Failed     : exception;
   Create_Thread_Failed    : exception;
   Dll_Version_Error       : exception;
   Ellipse_Error           : exception;
   Error_Copying_Cmdline   : exception;
   Error_Copying_Title     : exception;
   Error_Reading_Font      : exception;
   Error_Reading_Size      : exception;
   Fill_Rect_Error         : exception;
   Flood_Fill_Error        : exception;
   Get_Pixel_Error         : exception;
   Get_Position_Error      : exception;
   Invalid_Color_Value     : exception;
   Invalid_Coordinate      : exception;
   Invalid_Window_Size     : exception;
   Invalidate_Rect_Error   : exception;
   Line_To_Error           : exception;
   Move_To_Error           : exception;
   Mouse_Event_Error       : exception;
   Rectangle_Error         : exception;
   Select_Brush_Error      : exception;
   Select_Pen_Error        : exception;
   Set_Pixel_Error         : exception;
   Set_Textcolor_Error     : exception;
   Set_Title_Error         : exception;
   Text_Out_Error          : exception;
   Thread_Status_Error     : exception;
   Unknown_Adagraph_Error  : exception;
   Update_Window_Error     : exception;
   Wait_Failed_Error       : exception;
   Window_Already_Closed   : exception;
   Window_Already_Open     : exception;
   Window_Not_Open         : exception;

end Adagraph;
