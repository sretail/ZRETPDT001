MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
ENDMODULE.

MODULE user_command_0100 INPUT.
  perform f_user_command_0100.
ENDMODULE.

MODULE m_0100_pbo_init_alv OUTPUT.
  perform f_0100_pbo_init_alv.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  perform f_user_command_0200.
ENDMODULE.
