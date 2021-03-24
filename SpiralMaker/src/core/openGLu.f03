! This is module for freeglut for Unix 32/64 only
! (c) Mykola Yas'ko  It's free!
module OpenGL
USE, INTRINSIC :: ISO_C_BINDING !, only : C_INTPTR_T, C_FUNPTR
integer(4),parameter,public :: GLbyte = 1      
integer(4),parameter,public :: GLshort = 2     
integer(4),parameter,public :: GLint = 4,GLcint=GLint
integer(4),parameter,public :: GLsizei = 4     
integer(4),parameter,public :: GLfloat = 4     
integer(4),parameter,public :: GLclampf = 4    
integer(4),parameter,public :: GLdouble = 8    
integer(4),parameter,public :: GLclampd = 8    
integer(4),parameter,public :: GLubyte = 1     
integer(4),parameter,public :: GLboolean = 1   
integer(4),parameter,public :: GLushort = 2    
integer(4),parameter,public :: GLuint = 4      
integer(4),parameter,public :: GLenum = 4      
integer(4),parameter,public :: GLbitfield = 4  
integer(4),parameter,public :: GLvoid = 4      
integer(4),parameter,public :: GLWORD  = 2     
integer(4),parameter,public :: GLDWORD = 4     
integer(4),parameter,public :: GLBOOL  = 4     
integer(8),parameter,public :: GL_NULL = 0  !????????
integer(4),parameter,public :: GL_ACCUM  =  Z'0100' 
integer(4),parameter,public :: GL_LOAD   =  Z'0101' 
integer(4),parameter,public :: GL_RETURN =  Z'0102' 
integer(4),parameter,public :: GL_MULT   =  Z'0103' 
integer(4),parameter,public :: GL_ADD    =  Z'0104' 
integer(4),parameter,public :: GL_NEVER      =  Z'0200' 
integer(4),parameter,public :: GL_LESS       =  Z'0201' 
integer(4),parameter,public :: GL_EQUAL      =  Z'0202' 
integer(4),parameter,public :: GL_LEQUAL     =  Z'0203' 
integer(4),parameter,public :: GL_GREATER    =  Z'0204' 
integer(4),parameter,public :: GL_NOTEQUAL   =  Z'0205' 
integer(4),parameter,public :: GL_GEQUAL     =  Z'0206' 
integer(4),parameter,public :: GL_ALWAYS     =  Z'0207' 
integer(4),parameter,public :: GL_CURRENT_BIT            =  Z'00000001' 
integer(4),parameter,public :: GL_POINT_BIT              =  Z'00000002' 
integer(4),parameter,public :: GL_LINE_BIT               =  Z'00000004' 
integer(4),parameter,public :: GL_POLYGON_BIT            =  Z'00000008' 
integer(4),parameter,public :: GL_POLYGON_STIPPLE_BIT    =  Z'00000010' 
integer(4),parameter,public :: GL_PIXEL_MODE_BIT         =  Z'00000020' 
integer(4),parameter,public :: GL_LIGHTING_BIT           =  Z'00000040' 
integer(4),parameter,public :: GL_FOG_BIT                =  Z'00000080' 
integer(4),parameter,public :: GL_DEPTH_BUFFER_BIT       =  Z'00000100' 
integer(4),parameter,public :: GL_ACCUM_BUFFER_BIT       =  Z'00000200' 
integer(4),parameter,public :: GL_STENCIL_BUFFER_BIT     =  Z'00000400' 
integer(4),parameter,public :: GL_VIEWPORT_BIT           =  Z'00000800' 
integer(4),parameter,public :: GL_TRANSFORM_BIT          =  Z'00001000' 
integer(4),parameter,public :: GL_ENABLE_BIT             =  Z'00002000' 
integer(4),parameter,public :: GL_COLOR_BUFFER_BIT       =  Z'00004000' 
integer(4),parameter,public :: GL_HINT_BIT               =  Z'00008000' 
integer(4),parameter,public :: GL_EVAL_BIT               =  Z'00010000' 
integer(4),parameter,public :: GL_LIST_BIT               =  Z'00020000' 
integer(4),parameter,public :: GL_TEXTURE_BIT            =  Z'00040000' 
integer(4),parameter,public :: GL_SCISSOR_BIT            =  Z'00080000' 
integer(4),parameter,public :: GL_ALL_ATTRIB_BITS        =  Z'000fffff' 
integer(4),parameter,public :: GL_POINTS         =  Z'0000' 
integer(4),parameter,public :: GL_LINES          =  Z'0001' 
integer(4),parameter,public :: GL_LINE_LOOP      =  Z'0002' 
integer(4),parameter,public :: GL_LINE_STRIP     =  Z'0003' 
integer(4),parameter,public :: GL_TRIANGLES      =  Z'0004' 
integer(4),parameter,public :: GL_TRIANGLE_STRIP =  Z'0005' 
integer(4),parameter,public :: GL_TRIANGLE_FAN   =  Z'0006' 
integer(4),parameter,public :: GL_QUADS          =  Z'0007' 
integer(4),parameter,public :: GL_QUAD_STRIP     =  Z'0008' 
integer(4),parameter,public :: GL_POLYGON        =  Z'0009' 
integer(4),parameter,public :: GL_ZERO                   =  0 
integer(4),parameter,public :: GL_ONE                    =  1 
integer(4),parameter,public :: GL_SRC_COLOR              =  Z'0300' 
integer(4),parameter,public :: GL_ONE_MINUS_SRC_COLOR    =  Z'0301' 
integer(4),parameter,public :: GL_SRC_ALPHA              =  Z'0302' 
integer(4),parameter,public :: GL_ONE_MINUS_SRC_ALPHA    =  Z'0303' 
integer(4),parameter,public :: GL_DST_ALPHA              =  Z'0304' 
integer(4),parameter,public :: GL_ONE_MINUS_DST_ALPHA    =  Z'0305' 
integer(4),parameter,public :: GL_DST_COLOR              =  Z'0306' 
integer(4),parameter,public :: GL_ONE_MINUS_DST_COLOR    =  Z'0307' 
integer(4),parameter,public :: GL_SRC_ALPHA_SATURATE     =  Z'0308' 
integer(4),parameter,public :: GL_TRUE   = 1 
integer(4),parameter,public :: GL_FALSE  = 0 
integer(4),parameter,public :: GL_CLIP_PLANE0 =  Z'3000' 
integer(4),parameter,public :: GL_CLIP_PLANE1 =  Z'3001' 
integer(4),parameter,public :: GL_CLIP_PLANE2 =  Z'3002' 
integer(4),parameter,public :: GL_CLIP_PLANE3 =  Z'3003' 
integer(4),parameter,public :: GL_CLIP_PLANE4 =  Z'3004' 
integer(4),parameter,public :: GL_CLIP_PLANE5 =  Z'3005' 
integer(4),parameter,public :: GL_BYTE = Z'1400'
integer(4),parameter,public :: GL_UNSIGNED_BYTE = Z'1401'
integer(4),parameter,public :: GL_SHORT = Z'1402'
integer(4),parameter,public :: GL_UNSIGNED_SHORT = Z'1403'
integer(4),parameter,public :: GL_INT = Z'1404'
integer(4),parameter,public :: GL_UNSIGNED_INT = Z'1405'
integer(4),parameter,public :: GL_FLOAT = Z'1406'
integer(4),parameter,public :: GL_2_BYTES = Z'1407'
integer(4),parameter,public :: GL_3_BYTES = Z'1408'
integer(4),parameter,public :: GL_4_BYTES = Z'1409'
integer(4),parameter,public :: GL_DOUBLE = Z'140A'
integer(4),parameter,public :: GL_NONE           =  0 
integer(4),parameter,public :: GL_FRONT_LEFT     =  Z'0400' 
integer(4),parameter,public :: GL_FRONT_RIGHT    =  Z'0401' 
integer(4),parameter,public :: GL_BACK_LEFT      =  Z'0402' 
integer(4),parameter,public :: GL_BACK_RIGHT     =  Z'0403' 
integer(4),parameter,public :: GL_FRONT          =  Z'0404' 
integer(4),parameter,public :: GL_BACK           =  Z'0405' 
integer(4),parameter,public :: GL_LEFT           =  Z'0406' 
integer(4),parameter,public :: GL_RIGHT          =  Z'0407' 
integer(4),parameter,public :: GL_FRONT_AND_BACK =  Z'0408' 
integer(4),parameter,public :: GL_AUX0           =  Z'0409' 
integer(4),parameter,public :: GL_AUX1           =  Z'040A' 
integer(4),parameter,public :: GL_AUX2           =  Z'040B' 
integer(4),parameter,public :: GL_AUX3           =  Z'040C' 
integer(4),parameter,public :: GL_NO_ERROR           = 0
integer(4),parameter,public :: GL_INVALID_ENUM       =  Z'0500' 
integer(4),parameter,public :: GL_INVALID_VALUE      =  Z'0501' 
integer(4),parameter,public :: GL_INVALID_OPERATION  =  Z'0502' 
integer(4),parameter,public :: GL_STACOVERFLOW     =  Z'0503' 
integer(4),parameter,public :: GL_STACUNDERFLOW    =  Z'0504' 
integer(4),parameter,public :: GL_OUT_OF_MEMORY      =  Z'0505' 
integer(4),parameter,public :: GL_2D                 =  Z'0600' 
integer(4),parameter,public :: GL_3D                 =  Z'0601' 
integer(4),parameter,public :: GL_3D_COLOR           =  Z'0602' 
integer(4),parameter,public :: GL_3D_COLOR_TEXTURE   =  Z'0603' 
integer(4),parameter,public :: GL_4D_COLOR_TEXTURE   =  Z'0604' 
integer(4),parameter,public :: GL_PASS_THROUGH_TOKEN =  Z'0700' 
integer(4),parameter,public :: GL_POINT_TOKEN        =  Z'0701' 
integer(4),parameter,public :: GL_LINE_TOKEN         =  Z'0702' 
integer(4),parameter,public :: GL_POLYGON_TOKEN      =  Z'0703' 
integer(4),parameter,public :: GL_BITMAP_TOKEN       =  Z'0704' 
integer(4),parameter,public :: GL_DRAW_PIXEL_TOKEN   =  Z'0705' 
integer(4),parameter,public :: GL_COPY_PIXEL_TOKEN   =  Z'0706' 
integer(4),parameter,public :: GL_LINE_RESET_TOKEN   =  Z'0707' 
integer(4),parameter,public :: GL_EXP    =  Z'0800' 
integer(4),parameter,public :: GL_EXP2   =  Z'0801' 
integer(4),parameter,public :: GL_CW     =  Z'0900' 
integer(4),parameter,public :: GL_CCW    =  Z'0901' 
integer(4),parameter,public :: GL_COEFF  =  Z'0A00' 
integer(4),parameter,public :: GL_ORDER  =  Z'0A01' 
integer(4),parameter,public :: GL_DOMAIN =  Z'0A02' 
integer(4),parameter,public :: GL_CURRENT_COLOR                  =  Z'0B00' 
integer(4),parameter,public :: GL_CURRENT_INDEX                  =  Z'0B01' 
integer(4),parameter,public :: GL_CURRENT_NORMAL                 =  Z'0B02' 
integer(4),parameter,public :: GL_CURRENT_TEXTURE_COORDS         =  Z'0B03' 
integer(4),parameter,public :: GL_CURRENT_RASTER_COLOR           =  Z'0B04' 
integer(4),parameter,public :: GL_CURRENT_RASTER_INDEX           =  Z'0B05' 
integer(4),parameter,public :: GL_CURRENT_RASTER_TEXTURE_COORD   =  Z'0B06' 
integer(4),parameter,public :: GL_CURRENT_RASTER_POSITION        =  Z'0B07' 
integer(4),parameter,public :: GL_CURRENT_RASTER_POSITION_VALD   =  Z'0B08' 
integer(4),parameter,public :: GL_CURRENT_RASTER_DISTANCE        =  Z'0B09' 
integer(4),parameter,public :: GL_POINT_SMOOTH                   =  Z'0B10' 
integer(4),parameter,public :: GL_POINT_SIZE                     =  Z'0B11' 
integer(4),parameter,public :: GL_POINT_SIZE_RANGE               =  Z'0B12' 
integer(4),parameter,public :: GL_POINT_SIZE_GRANULARITY         =  Z'0B13' 
integer(4),parameter,public :: GL_LINE_SMOOTH                    =  Z'0B20' 
integer(4),parameter,public :: GL_LINE_WIDTH                     =  Z'0B21' 
integer(4),parameter,public :: GL_LINE_WIDTH_RANGE               =  Z'0B22' 
integer(4),parameter,public :: GL_LINE_WIDTH_GRANULARITY         =  Z'0B23' 
integer(4),parameter,public :: GL_LINE_STIPPLE                   =  Z'0B24' 
integer(4),parameter,public :: GL_LINE_STIPPLE_PATTERN           =  Z'0B25' 
integer(4),parameter,public :: GL_LINE_STIPPLE_REPEAT            =  Z'0B26' 
integer(4),parameter,public :: GL_LIST_MODE                      =  Z'0B30' 
integer(4),parameter,public :: GL_MAX_LIST_NESTING               =  Z'0B31' 
integer(4),parameter,public :: GL_LIST_BASE                      =  Z'0B32' 
integer(4),parameter,public :: GL_LIST_INDEX                     =  Z'0B33' 
integer(4),parameter,public :: GL_POLYGON_MODE                   =  Z'0B40' 
integer(4),parameter,public :: GL_POLYGON_SMOOTH                 =  Z'0B41' 
integer(4),parameter,public :: GL_POLYGON_STIPPLE                =  Z'0B42' 
integer(4),parameter,public :: GL_EDGE_FLAG                      =  Z'0B43' 
integer(4),parameter,public :: GL_CULL_FACE                      =  Z'0B44' 
integer(4),parameter,public :: GL_CULL_FACE_MODE                 =  Z'0B45' 
integer(4),parameter,public :: GL_FRONT_FACE                     =  Z'0B46' 
integer(4),parameter,public :: GL_LIGHTING                       =  Z'0B50' 
integer(4),parameter,public :: GL_LIGHT_MODEL_LOCAL_VIEWER       =  Z'0B51' 
integer(4),parameter,public :: GL_LIGHT_MODEL_TWO_SIDE           =  Z'0B52' 
integer(4),parameter,public :: GL_LIGHT_MODEL_AMBIENT            =  Z'0B53' 
integer(4),parameter,public :: GL_SHADE_MODEL                    =  Z'0B54' 
integer(4),parameter,public :: GL_COLOR_MATERIAL_FACE            =  Z'0B55' 
integer(4),parameter,public :: GL_COLOR_MATERIAL_PARAMETER       =  Z'0B56' 
integer(4),parameter,public :: GL_COLOR_MATERIAL                 =  Z'0B57' 
integer(4),parameter,public :: GL_FOG                            =  Z'0B60' 
integer(4),parameter,public :: GL_FOG_INDEX                      =  Z'0B61' 
integer(4),parameter,public :: GL_FOG_DENSITY                    =  Z'0B62' 
integer(4),parameter,public :: GL_FOG_START                      =  Z'0B63' 
integer(4),parameter,public :: GL_FOG_END                        =  Z'0B64' 
integer(4),parameter,public :: GL_FOG_MODE                       =  Z'0B65' 
integer(4),parameter,public :: GL_FOG_COLOR                      =  Z'0B66' 
integer(4),parameter,public :: GL_DEPTH_RANGE                    =  Z'0B70' 
integer(4),parameter,public :: GL_DEPTH_TEST                     =  Z'0B71' 
integer(4),parameter,public :: GL_DEPTH_WRITEMASK                =  Z'0B72' 
integer(4),parameter,public :: GL_DEPTH_CLEAR_VALUE              =  Z'0B73' 
integer(4),parameter,public :: GL_DEPTH_FUNC                     =  Z'0B74' 
integer(4),parameter,public :: GL_ACCUM_CLEAR_VALUE              =  Z'0B80' 
integer(4),parameter,public :: GL_STENCIL_TEST                   =  Z'0B90' 
integer(4),parameter,public :: GL_STENCIL_CLEAR_VALUE            =  Z'0B91' 
integer(4),parameter,public :: GL_STENCIL_FUNC                   =  Z'0B92' 
integer(4),parameter,public :: GL_STENCIL_VALUE_MASK             =  Z'0B93' 
integer(4),parameter,public :: GL_STENCIL_FAIL                   =  Z'0B94' 
integer(4),parameter,public :: GL_STENCIL_PASS_DEPTH_FAIL        =  Z'0B95' 
integer(4),parameter,public :: GL_STENCIL_PASS_DEPTH_PASS        =  Z'0B96' 
integer(4),parameter,public :: GL_STENCIL_REF                    =  Z'0B97' 
integer(4),parameter,public :: GL_STENCIL_WRITEMASK              =  Z'0B98' 
integer(4),parameter,public :: GL_MATRIX_MODE                    =  Z'0BA0' 
integer(4),parameter,public :: GL_NORMALIZE                      =  Z'0BA1' 
integer(4),parameter,public :: GL_VIEWPORT                       =  Z'0BA2' 
integer(4),parameter,public :: GL_MODELVIEW_STACDEPTH          =  Z'0BA3' 
integer(4),parameter,public :: GL_PROJECTION_STACDEPTH         =  Z'0BA4' 
integer(4),parameter,public :: GL_TEXTURE_STACDEPTH            =  Z'0BA5' 
integer(4),parameter,public :: GL_MODELVIEW_MATRIX               =  Z'0BA6' 
integer(4),parameter,public :: GL_PROJECTION_MATRIX              =  Z'0BA7' 
integer(4),parameter,public :: GL_TEXTURE_MATRIX                 =  Z'0BA8' 
integer(4),parameter,public :: GL_ATTRIB_STACDEPTH             =  Z'0BB0' 
integer(4),parameter,public :: GL_CLIENT_ATTRIB_STACDEPTH      =  Z'0BB1'
integer(4),parameter,public :: GL_ALPHA_TEST                     =  Z'0BC0' 
integer(4),parameter,public :: GL_ALPHA_TEST_FUNC                =  Z'0BC1' 
integer(4),parameter,public :: GL_ALPHA_TEST_REF                 =  Z'0BC2' 
integer(4),parameter,public :: GL_DITHER                         =  Z'0BD0' 
integer(4),parameter,public :: GL_BLEND_DST                      =  Z'0BE0' 
integer(4),parameter,public :: GL_BLEND_SRC                      =  Z'0BE1' 
integer(4),parameter,public :: GL_BLEND                          =  Z'0BE2' 
integer(4),parameter,public :: GL_LOGIC_OP_MODE                  =  Z'0BF0' 
integer(4),parameter,public :: GL_INDEX_LOGIC_OP                 =  Z'0BF1' 
integer(4),parameter,public :: GL_COLOR_LOGIC_OP                 =  Z'0BF2'
integer(4),parameter,public :: GL_AUX_BUFFERS                    =  Z'0C00' 
integer(4),parameter,public :: GL_DRAW_BUFFER                    =  Z'0C01' 
integer(4),parameter,public :: GL_READ_BUFFER                    =  Z'0C02' 
integer(4),parameter,public :: GL_SCISSOR_BOX                    =  Z'0C10' 
integer(4),parameter,public :: GL_SCISSOR_TEST                   =  Z'0C11' 
integer(4),parameter,public :: GL_INDEX_CLEAR_VALUE              =  Z'0C20' 
integer(4),parameter,public :: GL_INDEX_WRITEMASK                =  Z'0C21' 
integer(4),parameter,public :: GL_COLOR_CLEAR_VALUE              =  Z'0C22' 
integer(4),parameter,public :: GL_COLOR_WRITEMASK                =  Z'0C23' 
integer(4),parameter,public :: GL_INDEX_MODE                     =  Z'0C30' 
integer(4),parameter,public :: GL_RGBA_MODE                      =  Z'0C31' 
integer(4),parameter,public :: GL_DOUBLEBUFFER                   =  Z'0C32' 
integer(4),parameter,public :: GL_STEREO                         =  Z'0C33' 
integer(4),parameter,public :: GL_RENDER_MODE                    =  Z'0C40' 
integer(4),parameter,public :: GL_PERSPECTIVE_CORRECTION_HINT    =  Z'0C50' 
integer(4),parameter,public :: GL_POINT_SMOOTH_HINT              =  Z'0C51' 
integer(4),parameter,public :: GL_LINE_SMOOTH_HINT               =  Z'0C52' 
integer(4),parameter,public :: GL_POLYGON_SMOOTH_HINT            =  Z'0C53' 
integer(4),parameter,public :: GL_FOG_HINT                       =  Z'0C54' 
integer(4),parameter,public :: GL_TEXTURE_GEN_S                  =  Z'0C60' 
integer(4),parameter,public :: GL_TEXTURE_GEN_T                  =  Z'0C61' 
integer(4),parameter,public :: GL_TEXTURE_GEN_R                  =  Z'0C62' 
integer(4),parameter,public :: GL_TEXTURE_GEN_Q                  =  Z'0C63' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_I               =  Z'0C70' 
integer(4),parameter,public :: GL_PIXEL_MAP_S_TO_S               =  Z'0C71' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_R               =  Z'0C72' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_G               =  Z'0C73' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_B               =  Z'0C74' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_A               =  Z'0C75' 
integer(4),parameter,public :: GL_PIXEL_MAP_R_TO_R               =  Z'0C76' 
integer(4),parameter,public :: GL_PIXEL_MAP_G_TO_G               =  Z'0C77' 
integer(4),parameter,public :: GL_PIXEL_MAP_B_TO_B               =  Z'0C78' 
integer(4),parameter,public :: GL_PIXEL_MAP_A_TO_A               =  Z'0C79' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_I_SIZE          =  Z'0CB0' 
integer(4),parameter,public :: GL_PIXEL_MAP_S_TO_S_SIZE          =  Z'0CB1' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_R_SIZE          =  Z'0CB2' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_G_SIZE          =  Z'0CB3' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_B_SIZE          =  Z'0CB4' 
integer(4),parameter,public :: GL_PIXEL_MAP_I_TO_A_SIZE          =  Z'0CB5' 
integer(4),parameter,public :: GL_PIXEL_MAP_R_TO_R_SIZE          =  Z'0CB6' 
integer(4),parameter,public :: GL_PIXEL_MAP_G_TO_G_SIZE          =  Z'0CB7' 
integer(4),parameter,public :: GL_PIXEL_MAP_B_TO_B_SIZE          =  Z'0CB8' 
integer(4),parameter,public :: GL_PIXEL_MAP_A_TO_A_SIZE          =  Z'0CB9' 
integer(4),parameter,public :: GL_UNPACSWAP_BYTES              =  Z'0CF0' 
integer(4),parameter,public :: GL_UNPACLSB_FIRST               =  Z'0CF1' 
integer(4),parameter,public :: GL_UNPACROW_LENGTH              =  Z'0CF2' 
integer(4),parameter,public :: GL_UNPACSKIP_ROWS               =  Z'0CF3' 
integer(4),parameter,public :: GL_UNPACSKIP_PIXELS             =  Z'0CF4' 
integer(4),parameter,public :: GL_UNPACALIGNMENT               =  Z'0CF5' 
integer(4),parameter,public :: GL_PACSWAP_BYTES                =  Z'0D00' 
integer(4),parameter,public :: GL_PACLSB_FIRST                 =  Z'0D01' 
integer(4),parameter,public :: GL_PACROW_LENGTH                =  Z'0D02' 
integer(4),parameter,public :: GL_PACSKIP_ROWS                 =  Z'0D03' 
integer(4),parameter,public :: GL_PACSKIP_PIXELS               =  Z'0D04' 
integer(4),parameter,public :: GL_PACALIGNMENT                 =  Z'0D05' 
integer(4),parameter,public :: GL_MAP_COLOR                      =  Z'0D10' 
integer(4),parameter,public :: GL_MAP_STENCIL                    =  Z'0D11' 
integer(4),parameter,public :: GL_INDEX_SHIFT                    =  Z'0D12' 
integer(4),parameter,public :: GL_INDEX_OFFSET                   =  Z'0D13' 
integer(4),parameter,public :: GL_RED_SCALE                      =  Z'0D14' 
integer(4),parameter,public :: GL_RED_BIAS                       =  Z'0D15' 
integer(4),parameter,public :: GL_ZOOM_X                         =  Z'0D16' 
integer(4),parameter,public :: GL_ZOOM_Y                         =  Z'0D17' 
integer(4),parameter,public :: GL_GREEN_SCALE                    =  Z'0D18' 
integer(4),parameter,public :: GL_GREEN_BIAS                     =  Z'0D19' 
integer(4),parameter,public :: GL_BLUE_SCALE                     =  Z'0D1A' 
integer(4),parameter,public :: GL_BLUE_BIAS                      =  Z'0D1B' 
integer(4),parameter,public :: GL_ALPHA_SCALE                    =  Z'0D1C' 
integer(4),parameter,public :: GL_ALPHA_BIAS                     =  Z'0D1D' 
integer(4),parameter,public :: GL_DEPTH_SCALE                    =  Z'0D1E' 
integer(4),parameter,public :: GL_DEPTH_BIAS                     =  Z'0D1F' 
integer(4),parameter,public :: GL_MAX_EVAL_ORDER                 =  Z'0D30' 
integer(4),parameter,public :: GL_MAX_LIGHTS                     =  Z'0D31' 
integer(4),parameter,public :: GL_MAX_CLIP_PLANES                =  Z'0D32' 
integer(4),parameter,public :: GL_MAX_TEXTURE_SIZE               =  Z'0D33' 
integer(4),parameter,public :: GL_MAX_PIXEL_MAP_TABLE            =  Z'0D34' 
integer(4),parameter,public :: GL_MAX_ATTRIB_STACDEPTH         =  Z'0D35' 
integer(4),parameter,public :: GL_MAX_MODELVIEW_STACDEPTH      =  Z'0D36' 
integer(4),parameter,public :: GL_MAX_NAME_STACDEPTH           =  Z'0D37' 
integer(4),parameter,public :: GL_MAX_PROJECTION_STACDEPTH     =  Z'0D38' 
integer(4),parameter,public :: GL_MAX_CLIENT_ATTRIB_STACDEPTH  =  Z'0D3B'
integer(4),parameter,public :: GL_MAX_TEXTURE_STACDEPTH        =  Z'0D39' 
integer(4),parameter,public :: GL_MAX_VIEWPORT_DIMS              =  Z'0D3A' 
integer(4),parameter,public :: GL_SUBPIXEL_BITS                  =  Z'0D50' 
integer(4),parameter,public :: GL_INDEX_BITS                     =  Z'0D51' 
integer(4),parameter,public :: GL_RED_BITS                       =  Z'0D52' 
integer(4),parameter,public :: GL_GREEN_BITS                     =  Z'0D53' 
integer(4),parameter,public :: GL_BLUE_BITS                      =  Z'0D54' 
integer(4),parameter,public :: GL_ALPHA_BITS                     =  Z'0D55' 
integer(4),parameter,public :: GL_DEPTH_BITS                     =  Z'0D56' 
integer(4),parameter,public :: GL_STENCIL_BITS                   =  Z'0D57' 
integer(4),parameter,public :: GL_ACCUM_RED_BITS                 =  Z'0D58' 
integer(4),parameter,public :: GL_ACCUM_GREEN_BITS               =  Z'0D59' 
integer(4),parameter,public :: GL_ACCUM_BLUE_BITS                =  Z'0D5A' 
integer(4),parameter,public :: GL_ACCUM_ALPHA_BITS               =  Z'0D5B' 
integer(4),parameter,public :: GL_NAME_STACDEPTH               =  Z'0D70' 
integer(4),parameter,public :: GL_AUTO_NORMAL                    =  Z'0D80' 
integer(4),parameter,public :: GL_MAP1_COLOR_4                   =  Z'0D90' 
integer(4),parameter,public :: GL_MAP1_INDEX                     =  Z'0D91' 
integer(4),parameter,public :: GL_MAP1_NORMAL                    =  Z'0D92' 
integer(4),parameter,public :: GL_MAP1_TEXTURE_COORD_1           =  Z'0D93' 
integer(4),parameter,public :: GL_MAP1_TEXTURE_COORD_2           =  Z'0D94' 
integer(4),parameter,public :: GL_MAP1_TEXTURE_COORD_3           =  Z'0D95' 
integer(4),parameter,public :: GL_MAP1_TEXTURE_COORD_4           =  Z'0D96' 
integer(4),parameter,public :: GL_MAP1_VERTEX_3                  =  Z'0D97' 
integer(4),parameter,public :: GL_MAP1_VERTEX_4                  =  Z'0D98' 
integer(4),parameter,public :: GL_MAP2_COLOR_4                   =  Z'0DB0' 
integer(4),parameter,public :: GL_MAP2_INDEX                     =  Z'0DB1' 
integer(4),parameter,public :: GL_MAP2_NORMAL                    =  Z'0DB2' 
integer(4),parameter,public :: GL_MAP2_TEXTURE_COORD_1           =  Z'0DB3' 
integer(4),parameter,public :: GL_MAP2_TEXTURE_COORD_2           =  Z'0DB4' 
integer(4),parameter,public :: GL_MAP2_TEXTURE_COORD_3           =  Z'0DB5' 
integer(4),parameter,public :: GL_MAP2_TEXTURE_COORD_4           =  Z'0DB6' 
integer(4),parameter,public :: GL_MAP2_VERTEX_3                  =  Z'0DB7' 
integer(4),parameter,public :: GL_MAP2_VERTEX_4                  =  Z'0DB8' 
integer(4),parameter,public :: GL_MAP1_GRID_DOMAIN               =  Z'0DD0' 
integer(4),parameter,public :: GL_MAP1_GRID_SEGMENTS             =  Z'0DD1' 
integer(4),parameter,public :: GL_MAP2_GRID_DOMAIN               =  Z'0DD2' 
integer(4),parameter,public :: GL_MAP2_GRID_SEGMENTS             =  Z'0DD3' 
integer(4),parameter,public :: GL_TEXTURE_1D                     =  Z'0DE0' 
integer(4),parameter,public :: GL_TEXTURE_2D                     =  Z'0DE1' 
integer(4),parameter,public :: GL_FEEDBACBUFFER_POINTER        =  Z'0DF0'
integer(4),parameter,public :: GL_FEEDBACBUFFER_SIZE           =  Z'0DF1'
integer(4),parameter,public :: GL_FEEDBACBUFFER_TYPE           =  Z'0DF2'
integer(4),parameter,public :: GL_SELECTION_BUFFER_POINTER       =  Z'0DF3'
integer(4),parameter,public :: GL_SELECTION_BUFFER_SIZE          =  Z'0DF4'
integer(4),parameter,public :: GL_TEXTURE_WIDTH          =  Z'1000' 
integer(4),parameter,public :: GL_TEXTURE_HEIGHT         =  Z'1001' 
integer(4),parameter,public :: GL_TEXTURE_INTERNAL_FORMAT=  Z'1003' 
integer(4),parameter,public :: GL_TEXTURE_BORDER_COLOR   =  Z'1004' 
integer(4),parameter,public :: GL_TEXTURE_BORDER         =  Z'1005' 
integer(4),parameter,public :: GL_DONT_CARE  =  Z'1100' 
integer(4),parameter,public :: GL_FASTEST    =  Z'1101' 
integer(4),parameter,public :: GL_NICEST     =  Z'1102' 
integer(4),parameter,public :: GL_LIGHT0 =  Z'4000' 
integer(4),parameter,public :: GL_LIGHT1 =  Z'4001' 
integer(4),parameter,public :: GL_LIGHT2 =  Z'4002' 
integer(4),parameter,public :: GL_LIGHT3 =  Z'4003' 
integer(4),parameter,public :: GL_LIGHT4 =  Z'4004' 
integer(4),parameter,public :: GL_LIGHT5 =  Z'4005' 
integer(4),parameter,public :: GL_LIGHT6 =  Z'4006' 
integer(4),parameter,public :: GL_LIGHT7 =  Z'4007' 
integer(4),parameter,public :: GL_AMBIENT                =  Z'1200' 
integer(4),parameter,public :: GL_DIFFUSE                =  Z'1201' 
integer(4),parameter,public :: GL_SPECULAR               =  Z'1202' 
integer(4),parameter,public :: GL_POSITION               =  Z'1203' 
integer(4),parameter,public :: GL_SPOT_DIRECTION         =  Z'1204' 
integer(4),parameter,public :: GL_SPOT_EXPONENT          =  Z'1205' 
integer(4),parameter,public :: GL_SPOT_CUTOFF            =  Z'1206' 
integer(4),parameter,public :: GL_CONSTANT_ATTENUATION   =  Z'1207' 
integer(4),parameter,public :: GL_LINEAR_ATTENUATION     =  Z'1208' 
integer(4),parameter,public :: GL_QUADRATIC_ATTENUATION  =  Z'1209' 
integer(4),parameter,public :: GL_COMPILE                =  Z'1300' 
integer(4),parameter,public :: GL_COMPILE_AND_EXECUTE    =  Z'1301' 
integer(4),parameter,public :: GL_CLEAR          =  Z'1500' 
integer(4),parameter,public :: GL_AND            =  Z'1501' 
integer(4),parameter,public :: GL_AND_REVERSE    =  Z'1502' 
integer(4),parameter,public :: GL_COPY           =  Z'1503' 
integer(4),parameter,public :: GL_AND_INVERTED   =  Z'1504' 
integer(4),parameter,public :: GL_NOOP           =  Z'1505' 
integer(4),parameter,public :: GL_XOR            =  Z'1506' 
integer(4),parameter,public :: GL_OR             =  Z'1507' 
integer(4),parameter,public :: GL_NOR            =  Z'1508' 
integer(4),parameter,public :: GL_EQUIV          =  Z'1509' 
integer(4),parameter,public :: GL_INVERT         =  Z'150A' 
integer(4),parameter,public :: GL_OR_REVERSE     =  Z'150B' 
integer(4),parameter,public :: GL_COPY_INVERTED  =  Z'150C' 
integer(4),parameter,public :: GL_OR_INVERTED    =  Z'150D' 
integer(4),parameter,public :: GL_NAND           =  Z'150E' 
integer(4),parameter,public :: GL_SET            =  Z'150F' 
integer(4),parameter,public :: GL_EMISSION               =  Z'1600' 
integer(4),parameter,public :: GL_SHININESS              =  Z'1601' 
integer(4),parameter,public :: GL_AMBIENT_AND_DIFFUSE    =  Z'1602' 
integer(4),parameter,public :: GL_COLOR_INDEXES          =  Z'1603' 
integer(4),parameter,public :: GL_MODELVIEW  =  Z'1700' 
integer(4),parameter,public :: GL_PROJECTION =  Z'1701' 
integer(4),parameter,public :: GL_TEXTURE    =  Z'1702' 
integer(4),parameter,public :: GL_COLOR      =  Z'1800' 
integer(4),parameter,public :: GL_DEPTH      =  Z'1801' 
integer(4),parameter,public :: GL_STENCIL    =  Z'1802' 
integer(4),parameter,public :: GL_COLOR_INDEX        =  Z'1900' 
integer(4),parameter,public :: GL_STENCIL_INDEX      =  Z'1901' 
integer(4),parameter,public :: GL_DEPTH_COMPONENT    =  Z'1902' 
integer(4),parameter,public :: GL_RED                =  Z'1903' 
integer(4),parameter,public :: GL_GREEN              =  Z'1904' 
integer(4),parameter,public :: GL_BLUE               =  Z'1905' 
integer(4),parameter,public :: GL_ALPHA              =  Z'1906' 
integer(4),parameter,public :: GL_RGB                =  Z'1907' 
integer(4),parameter,public :: GL_RGBA               =  Z'1908' 
integer(4),parameter,public :: GL_LUMINANCE          =  Z'1909' 
integer(4),parameter,public :: GL_LUMINANCE_ALPHA    =  Z'190A' 
integer(4),parameter,public :: GL_BITMAP =  Z'1A00' 
integer(4),parameter,public :: GL_POINT  =  Z'1B00' 
integer(4),parameter,public :: GL_LINE   =  Z'1B01' 
integer(4),parameter,public :: GL_FILL   =  Z'1B02' 
integer(4),parameter,public :: GL_RENDER     =  Z'1C00' 
integer(4),parameter,public :: GL_FEEDBACK   =  Z'1C01' 
integer(4),parameter,public :: GL_SELECT     =  Z'1C02' 
integer(4),parameter,public :: GL_FLAT   =  Z'1D00' 
integer(4),parameter,public :: GL_SMOOTH =  Z'1D01' 
integer(4),parameter,public :: GL_KEEP       =  Z'1E00' 
integer(4),parameter,public :: GL_REPLACE    =  Z'1E01' 
integer(4),parameter,public :: GL_INCR       =  Z'1E02' 
integer(4),parameter,public :: GL_DECR       =  Z'1E03' 
integer(4),parameter,public :: GL_VENDOR     =  Z'1F00' 
integer(4),parameter,public :: GL_RENDERER   =  Z'1F01' 
integer(4),parameter,public :: GL_VERSION    =  Z'1F02' 
integer(4),parameter,public :: GL_EXTENSIONS =  Z'1F03' 
integer(4),parameter,public :: GL_VERSION_1_1 = 1
integer(4),parameter,public :: GL_S =  Z'2000' 
integer(4),parameter,public :: GL_T =  Z'2001' 
integer(4),parameter,public :: GL_R =  Z'2002' 
integer(4),parameter,public :: GL_Q =  Z'2003' 
integer(4),parameter,public :: GL_MODULATE   =  Z'2100' 
integer(4),parameter,public :: GL_DECAL      =  Z'2101' 
integer(4),parameter,public :: GL_TEXTURE_ENV_MODE   =  Z'2200' 
integer(4),parameter,public :: GL_TEXTURE_ENV_COLOR  =  Z'2201' 
integer(4),parameter,public :: GL_TEXTURE_ENV =  Z'2300' 
integer(4),parameter,public :: GL_EYE_LINEAR     =  Z'2400' 
integer(4),parameter,public :: GL_OBJECT_LINEAR  =  Z'2401' 
integer(4),parameter,public :: GL_SPHERE_MAP     =  Z'2402' 
integer(4),parameter,public :: GL_TEXTURE_GEN_MODE   =  Z'2500' 
integer(4),parameter,public :: GL_OBJECT_PLANE       =  Z'2501' 
integer(4),parameter,public :: GL_EYE_PLANE          =  Z'2502' 
integer(4),parameter,public :: GL_NEAREST    =  Z'2600' 
integer(4),parameter,public :: GL_LINEAR     =  Z'2601' 
integer(4),parameter,public :: GL_NEAREST_MIPMAP_NEAREST =  Z'2700' 
integer(4),parameter,public :: GL_LINEAR_MIPMAP_NEAREST  =  Z'2701' 
integer(4),parameter,public :: GL_NEAREST_MIPMAP_LINEAR  =  Z'2702' 
integer(4),parameter,public :: GL_LINEAR_MIPMAP_LINEAR   =  Z'2703' 
integer(4),parameter,public :: GL_TEXTURE_MAG_FILTER =  Z'2800' 
integer(4),parameter,public :: GL_TEXTURE_MIN_FILTER =  Z'2801' 
integer(4),parameter,public :: GL_TEXTURE_WRAP_S     =  Z'2802' 
integer(4),parameter,public :: GL_TEXTURE_WRAP_T     =  Z'2803' 
integer(4),parameter,public :: GL_CLAMP  =  Z'2900' 
integer(4),parameter,public :: GL_REPEAT =  Z'2901' 
integer(4),parameter,public :: GL_UNPACK_ALIGNMENT = Z'0CF5'
integer(4),parameter,public ::  GL_CLIENT_PIXEL_STORE_BIT         = Z'00000001'                                
integer(4),parameter,public ::  GL_CLIENT_VERTEX_ARRAY_BIT        = Z'00000002'
!integer(4),parameter,public ::  GL_CLIENT_ALL_ATTRIB_BITS         = Z'ffffffff'
integer(4),parameter,public ::  GL_POLYGON_OFFSET_FACTOR          = Z'8038'
integer(4),parameter,public ::  GL_POLYGON_OFFSET_UNITS           = Z'2A00'
integer(4),parameter,public ::  GL_POLYGON_OFFSET_POINT           = Z'2A01'
integer(4),parameter,public ::  GL_POLYGON_OFFSET_LINE            = Z'2A02'
integer(4),parameter,public ::  GL_POLYGON_OFFSET_FILL            = Z'8037'
integer(4),parameter,public ::  GL_ALPHA4                         = Z'803B'
integer(4),parameter,public ::  GL_ALPHA8                         = Z'803C'
integer(4),parameter,public ::  GL_ALPHA12                        = Z'803D'
integer(4),parameter,public ::  GL_ALPHA16                        = Z'803E'
integer(4),parameter,public ::  GL_LUMINANCE4                     = Z'803F'
integer(4),parameter,public ::  GL_LUMINANCE8                     = Z'8040'
integer(4),parameter,public ::  GL_LUMINANCE12                    = Z'8041'
integer(4),parameter,public ::  GL_LUMINANCE16                    = Z'8042'
integer(4),parameter,public ::  GL_LUMINANCE4_ALPHA4              = Z'8043'
integer(4),parameter,public ::  GL_LUMINANCE6_ALPHA2              = Z'8044'
integer(4),parameter,public ::  GL_LUMINANCE8_ALPHA8              = Z'8045'
integer(4),parameter,public ::  GL_LUMINANCE12_ALPHA4             = Z'8046'
integer(4),parameter,public ::  GL_LUMINANCE12_ALPHA12            = Z'8047'
integer(4),parameter,public ::  GL_LUMINANCE16_ALPHA16            = Z'8048'
integer(4),parameter,public ::  GL_INTENSITY                      = Z'8049'
integer(4),parameter,public ::  GL_INTENSITY4                     = Z'804A'
integer(4),parameter,public ::  GL_INTENSITY8                     = Z'804B'
integer(4),parameter,public ::  GL_INTENSITY12                    = Z'804C'
integer(4),parameter,public ::  GL_INTENSITY16                    = Z'804D'
integer(4),parameter,public ::  GL_R3_G3_B2                       = Z'2A10'
integer(4),parameter,public ::  GL_RGB4                           = Z'804F'
integer(4),parameter,public ::  GL_RGB5                           = Z'8050'
integer(4),parameter,public ::  GL_RGB8                           = Z'8051'
integer(4),parameter,public ::  GL_RGB10                          = Z'8052'
integer(4),parameter,public ::  GL_RGB12                          = Z'8053'
integer(4),parameter,public ::  GL_RGB16                          = Z'8054'
integer(4),parameter,public ::  GL_RGBA2                          = Z'8055'
integer(4),parameter,public ::  GL_RGBA4                          = Z'8056'
integer(4),parameter,public ::  GL_RGB5_A1                        = Z'8057'
integer(4),parameter,public ::  GL_RGBA8                          = Z'8058'
integer(4),parameter,public ::  GL_RGB10_A2                       = Z'8059'
integer(4),parameter,public ::  GL_RGBA12                         = Z'805A'
integer(4),parameter,public ::  GL_RGBA16                         = Z'805B'
integer(4),parameter,public ::  GL_TEXTURE_RED_SIZE               = Z'805C'
integer(4),parameter,public ::  GL_TEXTURE_GREEN_SIZE             = Z'805D'
integer(4),parameter,public ::  GL_TEXTURE_BLUE_SIZE              = Z'805E'
integer(4),parameter,public ::  GL_TEXTURE_ALPHA_SIZE             = Z'805F'
integer(4),parameter,public ::  GL_TEXTURE_LUMINANCE_SIZE         = Z'8060'
integer(4),parameter,public ::  GL_TEXTURE_INTENSITY_SIZE         = Z'8061'
integer(4),parameter,public ::  GL_PROXY_TEXTURE_1D               = Z'8063'
integer(4),parameter,public ::  GL_PROXY_TEXTURE_2D               = Z'8064'
integer(4),parameter,public ::  GL_TEXTURE_PRIORITY               = Z'8066'
integer(4),parameter,public ::  GL_TEXTURE_RESIDENT               = Z'8067'
integer(4),parameter,public ::  GL_TEXTURE_BINDING_1D             = Z'8068'
integer(4),parameter,public ::  GL_TEXTURE_BINDING_2D             = Z'8069'
integer(4),parameter,public ::  GL_VERTEX_ARRAY                   = Z'8074'
integer(4),parameter,public ::  GL_NORMAL_ARRAY                   = Z'8075'
integer(4),parameter,public ::  GL_COLOR_ARRAY                    = Z'8076'
integer(4),parameter,public ::  GL_INDEX_ARRAY                    = Z'8077'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY            = Z'8078'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY                = Z'8079'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_SIZE              = Z'807A'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_TYPE              = Z'807B'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_STRIDE            = Z'807C'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_TYPE              = Z'807E'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_STRIDE            = Z'807F'
integer(4),parameter,public ::  GL_COLOR_ARRAY_SIZE               = Z'8081'
integer(4),parameter,public ::  GL_COLOR_ARRAY_TYPE               = Z'8082'
integer(4),parameter,public ::  GL_COLOR_ARRAY_STRIDE             = Z'8083'
integer(4),parameter,public ::  GL_INDEX_ARRAY_TYPE               = Z'8085'
integer(4),parameter,public ::  GL_INDEX_ARRAY_STRIDE             = Z'8086'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_SIZE       = Z'8088'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_TYPE       = Z'8089'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_STRIDE     = Z'808A'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_STRIDE         = Z'808C'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_POINTER           = Z'808E'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_POINTER           = Z'808F'
integer(4),parameter,public ::  GL_COLOR_ARRAY_POINTER            = Z'8090'
integer(4),parameter,public ::  GL_INDEX_ARRAY_POINTER            = Z'8091'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_POINTER    = Z'8092'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_POINTER        = Z'8093'
integer(4),parameter,public ::  GL_V2F                            = Z'2A20'
integer(4),parameter,public ::  GL_V3F                            = Z'2A21'
integer(4),parameter,public ::  GL_C4UB_V2F                       = Z'2A22'
integer(4),parameter,public ::  GL_C4UB_V3F                       = Z'2A23'
integer(4),parameter,public ::  GL_C3F_V3F                        = Z'2A24'
integer(4),parameter,public ::  GL_N3F_V3F                        = Z'2A25'
integer(4),parameter,public ::  GL_C4F_N3F_V3F                    = Z'2A26'
integer(4),parameter,public ::  GL_T2F_V3F                        = Z'2A27'
integer(4),parameter,public ::  GL_T4F_V4F                        = Z'2A28'
integer(4),parameter,public ::  GL_T2F_C4UB_V3F                   = Z'2A29'
integer(4),parameter,public ::  GL_T2F_C3F_V3F                    = Z'2A2A'
integer(4),parameter,public ::  GL_T2F_N3F_V3F                    = Z'2A2B'
integer(4),parameter,public ::  GL_T2F_C4F_N3F_V3F                = Z'2A2C'
integer(4),parameter,public ::  GL_T4F_C4F_N3F_V4F                = Z'2A2D'
integer(4),parameter,public ::  GL_EXT_vertex_array               = 1
integer(4),parameter,public ::  GL_WIN_swap_hint                  = 1
integer(4),parameter,public ::  GL_EXT_bgra                       = 1
integer(4),parameter,public ::  GL_EXT_paletted_texture           = 1
integer(4),parameter,public ::  GL_WIN_draw_range_elements        = 1
integer(4),parameter,public ::  GL_VERTEX_ARRAY_EXT               = Z'8074'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_EXT               = Z'8075'
integer(4),parameter,public ::  GL_COLOR_ARRAY_EXT                = Z'8076'
integer(4),parameter,public ::  GL_INDEX_ARRAY_EXT                = Z'8077'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_EXT        = Z'8078'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_EXT            = Z'8079'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_SIZE_EXT          = Z'807A'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_TYPE_EXT          = Z'807B'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_STRIDE_EXT        = Z'807C'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_COUNT_EXT         = Z'807D'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_TYPE_EXT          = Z'807E'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_STRIDE_EXT        = Z'807F'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_COUNT_EXT         = Z'8080'
integer(4),parameter,public ::  GL_COLOR_ARRAY_SIZE_EXT           = Z'8081'
integer(4),parameter,public ::  GL_COLOR_ARRAY_TYPE_EXT           = Z'8082'
integer(4),parameter,public ::  GL_COLOR_ARRAY_STRIDE_EXT         = Z'8083'
integer(4),parameter,public ::  GL_COLOR_ARRAY_COUNT_EXT          = Z'8084'
integer(4),parameter,public ::  GL_INDEX_ARRAY_TYPE_EXT           = Z'8085'
integer(4),parameter,public ::  GL_INDEX_ARRAY_STRIDE_EXT         = Z'8086'
integer(4),parameter,public ::  GL_INDEX_ARRAY_COUNT_EXT          = Z'8087'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_SIZE_EXT   = Z'8088'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_TYPE_EXT   = Z'8089'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = Z'808A'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_COUNT_EXT  = Z'808B'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_STRIDE_EXT     = Z'808C'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_COUNT_EXT      = Z'808D'
integer(4),parameter,public ::  GL_VERTEX_ARRAY_POINTER_EXT       = Z'808E'
integer(4),parameter,public ::  GL_NORMAL_ARRAY_POINTER_EXT       = Z'808F'
integer(4),parameter,public ::  GL_COLOR_ARRAY_POINTER_EXT        = Z'8090'
integer(4),parameter,public ::  GL_INDEX_ARRAY_POINTER_EXT        = Z'8091'
integer(4),parameter,public ::  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = Z'8092'
integer(4),parameter,public ::  GL_EDGE_FLAG_ARRAY_POINTER_EXT    = Z'8093'
integer(4),parameter,public ::  GL_DOUBLE_EXT                    =  GL_DOUBLE
integer(4),parameter,public ::  GL_BGR_EXT                        = Z'80E0'
integer(4),parameter,public ::  GL_BGRA_EXT                       = Z'80E1'
integer(4),parameter,public ::  GL_COLOR_TABLE_FORMAT_EXT         = Z'80D8'
integer(4),parameter,public ::  GL_COLOR_TABLE_WIDTH_EXT          = Z'80D9'
integer(4),parameter,public ::  GL_COLOR_TABLE_RED_SIZE_EXT       = Z'80DA'
integer(4),parameter,public ::  GL_COLOR_TABLE_GREEN_SIZE_EXT     = Z'80DB'
integer(4),parameter,public ::  GL_COLOR_TABLE_BLUE_SIZE_EXT      = Z'80DC'
integer(4),parameter,public ::  GL_COLOR_TABLE_ALPHA_SIZE_EXT     = Z'80DD'
integer(4),parameter,public ::  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = Z'80DE'
integer(4),parameter,public ::  GL_COLOR_TABLE_INTENSITY_SIZE_EXT = Z'80DF'
integer(4),parameter,public ::  GL_COLOR_INDEX1_EXT               = Z'80E2'
integer(4),parameter,public ::  GL_COLOR_INDEX2_EXT               = Z'80E3'
integer(4),parameter,public ::  GL_COLOR_INDEX4_EXT               = Z'80E4'
integer(4),parameter,public ::  GL_COLOR_INDEX8_EXT               = Z'80E5'
integer(4),parameter,public ::  GL_COLOR_INDEX12_EXT              = Z'80E6'
integer(4),parameter,public ::  GL_COLOR_INDEX16_EXT              = Z'80E7'
integer(4),parameter,public ::  GL_MAX_ELEMENTS_VERTICES_WIN      = Z'80E8'
integer(4),parameter,public ::  GL_MAX_ELEMENTS_INDICES_WIN       = Z'80E9'
integer(4),parameter,public ::  GL_PHONG_WIN                      = Z'80EA'
integer(4),parameter,public ::  GL_PHONG_HINT_WIN                 = Z'80EB'
integer(4),parameter,public ::  GL_FOG_SPECULAR_TEXTURE_WIN = Z'80EC'
integer(4),parameter,public ::  GL_LOGIC_OP = GL_INDEX_LOGIC_OP
integer(4),parameter,public ::  GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT
integer(4),parameter,public :: GLUtesselator = 4 
integer(4),parameter,public :: GLU_INVALID_ENUM  = 100900 
integer(4),parameter,public :: GLU_INVALID_VALUE = 100901 
integer(4),parameter,public :: GLU_OUT_OF_MEMORY = 100902 
integer(4),parameter,public :: GLU_TRUE  = GL_TRUE
integer(4),parameter,public :: GLU_FALSE = GL_FALSE
integer(4),parameter,public :: GLU_VERSION_1_1 = 1
integer(4),parameter,public :: GLU_VERSION_1_2 = 1
integer(4),parameter,public :: GLU_INCOMPATIBLE_GL_VERSION = 100903
integer(4),parameter,public :: GLU_VERSION = 100800
integer(4),parameter,public :: GLU_EXTENSIONS = 100801
integer(4),parameter,public :: GLU_SMOOTH    = 100000 
integer(4),parameter,public :: GLU_FLAT      = 100001 
integer(4),parameter,public :: GLU_NONE      = 100002 
integer(4),parameter,public :: GLU_POINT         = 100010 
integer(4),parameter,public :: GLU_LINE          = 100011 
integer(4),parameter,public :: GLU_FILL          = 100012 
integer(4),parameter,public :: GLU_SILHOUETTE    = 100013 
integer(4),parameter,public :: GLU_OUTSIDE   = 100020 
integer(4),parameter,public :: GLU_INSIDE    = 100021 
integer(4),parameter,public :: GLU_BEGIN     = 100100        
integer(4),parameter,public :: GLU_VERTEX    = 100101       
integer(4),parameter,public :: GLU_END       = 100102          
integer(4),parameter,public :: GLU_ERROR     = 100103        
integer(4),parameter,public :: GLU_EDGE_FLAG = 100104    
integer(4),parameter,public :: GLU_CW            = 100120 
integer(4),parameter,public :: GLU_CCW           = 100121 
integer(4),parameter,public :: GLU_INTERIOR      = 100122 
integer(4),parameter,public :: GLU_EXTERIOR      = 100123 
integer(4),parameter,public :: GLU_UNKNOWN       = 100124 
integer(4),parameter,public :: GLU_TESS_ERROR1   = 100151 
integer(4),parameter,public :: GLU_TESS_ERROR2   = 100152 
integer(4),parameter,public :: GLU_TESS_ERROR3   = 100153 
integer(4),parameter,public :: GLU_TESS_ERROR4   = 100154 
integer(4),parameter,public :: GLU_TESS_ERROR5   = 100155
integer(4),parameter,public :: GLU_TESS_ERROR6   = 100156
integer(4),parameter,public :: GLU_TESS_ERROR7   = 100157
integer(4),parameter,public :: GLU_TESS_ERROR8   = 100158
integer(4),parameter,public :: GLU_TESS_MISSING_BEGIN_POLYGON = 100151
integer(4),parameter,public :: GLU_TESS_MISSING_BEGIN_CONTOUR = 100152
integer(4),parameter,public :: GLU_TESS_MISSING_END_POLYGON = 100153
integer(4),parameter,public :: GLU_TESS_MISSING_END_CONTOUR = 100154
integer(4),parameter,public :: GLU_TESS_COORD_TOO_LARGE = 100155
integer(4),parameter,public :: GLU_TESS_NEED_COMBINE_CALLBACK = 100156
real(8),parameter,public :: GLU_TESS_MAX_COORD = 1.D150
integer(4),parameter,public :: GLU_TESS_WINDING_RULE = 100140
integer(4),parameter,public :: GLU_TESS_BOUNDARY_ONLY = 100141
integer(4),parameter,public :: GLU_TESS_TOLERANCE = 100142
integer(4),parameter,public :: GLU_TESS_WINDING_ODD = 100130
integer(4),parameter,public :: GLU_TESS_WINDING_NONZERO = 100131
integer(4),parameter,public :: GLU_TESS_WINDING_POSITIVE = 100132
integer(4),parameter,public :: GLU_TESS_WINDING_NEGATIVE = 100133
integer(4),parameter,public :: GLU_TESS_WINDING_ABS_GEQ_TWO = 100134
integer(4),parameter,public :: GLU_TESS_BEGIN = 100100
integer(4),parameter,public :: GLU_TESS_VERTEX = 100101
integer(4),parameter,public :: GLU_TESS_END = 100102
integer(4),parameter,public :: GLU_TESS_ERROR = 100103
integer(4),parameter,public :: GLU_TESS_EDGE_FLAG = 100104
integer(4),parameter,public :: GLU_TESS_COMBINE = 100105
integer(4),parameter,public :: GLU_TESS_BEGIN_DATA = 100106
integer(4),parameter,public :: GLU_TESS_VERTEX_DATA = 100107
integer(4),parameter,public :: GLU_TESS_END_DATA = 100108
integer(4),parameter,public :: GLU_TESS_ERROR_DATA = 100109
integer(4),parameter,public :: GLU_TESS_EDGE_FLAG_DATA = 100110
integer(4),parameter,public :: GLU_TESS_COMBINE_DATA = 100111
integer(4),parameter,public :: GLU_AUTO_LOAD_MATRIX      = 100200 
integer(4),parameter,public :: GLU_CULLING               = 100201 
integer(4),parameter,public :: GLU_SAMPLING_TOLERANCE    = 100203 
integer(4),parameter,public :: GLU_DISPLAY_MODE          = 100204 
integer(4),parameter,public :: GLU_PARAMETRIC_TOLERANCE = 100202
integer(4),parameter,public :: GLU_SAMPLING_METHOD = 100205
integer(4),parameter,public :: GLU_U_STEP = 100206
integer(4),parameter,public :: GLU_V_STEP = 100207
integer(4),parameter,public :: GLU_PATH_LENGTH = 100215
integer(4),parameter,public :: GLU_PARAMETRIC_ERROR = 100216
integer(4),parameter,public :: GLU_DOMAIN_DISTANCE = 100217
integer(4),parameter,public :: GLU_MAP1_TRIM_2 = 100210 
integer(4),parameter,public :: GLU_MAP1_TRIM_3 = 100211 
integer(4),parameter,public :: GLU_OUTLINE_POLYGON   = 100240 
integer(4),parameter,public :: GLU_OUTLINE_PATCH     = 100241 
integer(4),parameter,public :: GLU_NURBS_ERROR1 = 100251 
integer(4),parameter,public :: GLU_NURBS_ERROR2 = 100252 
integer(4),parameter,public :: GLU_NURBS_ERROR3 = 100253 
integer(4),parameter,public :: GLU_NURBS_ERROR4 = 100254 
integer(4),parameter,public :: GLU_NURBS_ERROR5 = 100255
integer(4),parameter,public :: GLU_NURBS_ERROR6 = 100256
integer(4),parameter,public :: GLU_NURBS_ERROR7 = 100257
integer(4),parameter,public :: GLU_NURBS_ERROR8 = 100258
integer(4),parameter,public :: GLU_NURBS_ERROR9 = 100259
integer(4),parameter,public :: GLU_NURBS_ERROR10= 100260
integer(4),parameter,public :: GLU_NURBS_ERROR11= 100261
integer(4),parameter,public :: GLU_NURBS_ERROR12= 100262
integer(4),parameter,public :: GLU_NURBS_ERROR13= 100263
integer(4),parameter,public :: GLU_NURBS_ERROR14= 100264
integer(4),parameter,public :: GLU_NURBS_ERROR15= 100265
integer(4),parameter,public :: GLU_NURBS_ERROR16= 100266
integer(4),parameter,public :: GLU_NURBS_ERROR17= 100267
integer(4),parameter,public :: GLU_NURBS_ERROR18= 100268
integer(4),parameter,public :: GLU_NURBS_ERROR19= 100269
integer(4),parameter,public :: GLU_NURBS_ERROR20= 100270
integer(4),parameter,public :: GLU_NURBS_ERROR21= 100271
integer(4),parameter,public :: GLU_NURBS_ERROR22= 100272
integer(4),parameter,public :: GLU_NURBS_ERROR23= 100273
integer(4),parameter,public :: GLU_NURBS_ERROR24= 100274
integer(4),parameter,public :: GLU_NURBS_ERROR25= 100275
integer(4),parameter,public :: GLU_NURBS_ERROR26= 100276
integer(4),parameter,public :: GLU_NURBS_ERROR27= 100277
integer(4),parameter,public :: GLU_NURBS_ERROR28= 100278
integer(4),parameter,public :: GLU_NURBS_ERROR29= 100279
integer(4),parameter,public :: GLU_NURBS_ERROR30= 100280
integer(4),parameter,public :: GLU_NURBS_ERROR31= 100281
integer(4),parameter,public :: GLU_NURBS_ERROR32= 100282
integer(4),parameter,public :: GLU_NURBS_ERROR33= 100283
integer(4),parameter,public :: GLU_NURBS_ERROR34= 100284
integer(4),parameter,public :: GLU_NURBS_ERROR35= 100285
integer(4),parameter,public :: GLU_NURBS_ERROR36= 100286
integer(4),parameter,public :: GLU_NURBS_ERROR37= 100287

integer(4),parameter,public :: AUX_USE_ID            = 1
integer(4),parameter,public :: AUX_EXACT_MATCH       = 2
integer(4),parameter,public :: AUX_MINIMUM_CRITERIA  = 3
integer(4),parameter,public :: WGL_FONT_LINES = 0
integer(4),parameter,public :: WGL_FONT_POLYGONS = 1
integer(4),parameter,public :: LPD_DOUBLEBUFFER = Z'00000001'
integer(4),parameter,public :: LPD_STEREO = Z'00000002'
integer(4),parameter,public :: LPD_SUPPORT_GDI = Z'00000010'
integer(4),parameter,public :: LPD_SUPPORT_OPENGL = Z'00000020'
integer(4),parameter,public :: LPD_SHARE_DEPTH = Z'00000040'
integer(4),parameter,public :: LPD_SHARE_STENCIL = Z'00000080'
integer(4),parameter,public :: LPD_SHARE_ACCUM = Z'00000100'
integer(4),parameter,public :: LPD_SWAP_EXCHANGE = Z'00000200'
integer(4),parameter,public :: LPD_SWAP_COPY = Z'00000400'
integer(4),parameter,public :: LPD_TRANSPARENT = Z'00001000'
integer(4),parameter,public :: LPD_TYPE_RGBA = 0
integer(4),parameter,public :: LPD_TYPE_COLORINDEX = 1
integer(4),parameter,public :: WGL_SWAP_MAIN_PLANE = Z'00000001'
integer(4),parameter,public :: WGL_SWAP_OVERLAY1 = Z'00000002'
integer(4),parameter,public :: WGL_SWAP_OVERLAY2 = Z'00000004'
integer(4),parameter,public :: WGL_SWAP_OVERLAY3 = Z'00000008'
integer(4),parameter,public :: WGL_SWAP_OVERLAY4 = Z'00000010'
integer(4),parameter,public :: WGL_SWAP_OVERLAY5 = Z'00000020'
integer(4),parameter,public :: WGL_SWAP_OVERLAY6 = Z'00000040'
integer(4),parameter,public :: WGL_SWAP_OVERLAY7 = Z'00000080'
integer(4),parameter,public :: WGL_SWAP_OVERLAY8 = Z'00000100'
integer(4),parameter,public :: WGL_SWAP_OVERLAY9 = Z'00000200'
integer(4),parameter,public :: WGL_SWAP_OVERLAY10 = Z'00000400'
integer(4),parameter,public :: WGL_SWAP_OVERLAY11 = Z'00000800'
integer(4),parameter,public :: WGL_SWAP_OVERLAY12 = Z'00001000'
integer(4),parameter,public :: WGL_SWAP_OVERLAY13 = Z'00002000'
integer(4),parameter,public :: WGL_SWAP_OVERLAY14 = Z'00004000'
integer(4),parameter,public :: WGL_SWAP_OVERLAY15 = Z'00008000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY1 = Z'00010000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY2 = Z'00020000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY3 = Z'00040000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY4 = Z'00080000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY5 = Z'00100000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY6 = Z'00200000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY7 = Z'00400000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY8 = Z'00800000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY9 = Z'01000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY10 = Z'02000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY11 = Z'04000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY12 = Z'08000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY13 = Z'10000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY14 = Z'20000000'
integer(4),parameter,public :: WGL_SWAP_UNDERLAY15 = Z'40000000'
character(*),parameter,public :: GL_COPYRIGHT="(c) Mykola Yas'ko. All rights reserved"
TYPE T_POINTFLOAT
 SEQUENCE
 real(GLfloat) x
 real(GLfloat) y
END TYPE
TYPE T_GLYPHMETRICSFLOAT
 SEQUENCE
 real(GLfloat) gmfBlackBoxX
 real(GLfloat) gmfBlackBoxY
 type(T_POINTFLOAT) gmfptGlyphOrigin
 real(GLfloat) gmfCellIncX
 real(GLfloat) gmfCellIncY
END TYPE
TYPE T_LAYERPLANEDESCRIPTOR
 SEQUENCE
 integer(GLWORD) nSize
 integer(GLWORD) nVersion
 integer(GLDWORD)dwFlags
 integer(GLbyte) iPixelType
 integer(GLbyte) cColorBits
 integer(GLbyte) cRedBits
 integer(GLbyte) cRedShift
 integer(GLbyte) cGreenBits
 integer(GLbyte) cGreenShift
 integer(GLbyte) cBlueBits
 integer(GLbyte) cBlueShift
 integer(GLbyte) cAlphaBits
 integer(GLbyte) cAlphaShift
 integer(GLbyte) cAccumBits
 integer(GLbyte) cAccumRedBits
 integer(GLbyte) cAccumGreenBits
 integer(GLbyte) cAccumBlueBits
 integer(GLbyte) cAccumAlphaBits
 integer(GLbyte) cDepthBits
 integer(GLbyte) cStencilBits
 integer(GLbyte) cAuxBuffers
 integer(GLbyte) iLayerPlane
 integer(GLbyte) bReserved
 integer(GLDWORD) crTransparent 
END TYPE
public
!---------------------------- OpenGL --------------------------------------
interface
subroutine glAccum(op,value) bind(c,name='glAccum')
integer(4), intent(in), value :: op 
real(4), intent(in), value :: value
end subroutine glAccum
end interface

interface
subroutine glAlphaFunc(func,ref) bind(c,name='glAlphaFunc')
integer(4), intent(in), value :: func 
real(4), intent(in), value :: ref
end subroutine glAlphaFunc
end interface

interface
integer(1) function glAreTexturesResident(n,textures,residences) bind(c,name='glAreTexturesResident')
integer(4), intent(in), value :: n
integer(4) :: textures(*)
integer(1) :: residences(*)
end function glAreTexturesResident
end interface

interface
subroutine glArrayElement(index) bind(c,name='glArrayElement')
integer(4), intent(in), value :: index
end subroutine glArrayElement
end interface

interface
subroutine glBegin(mode) bind(c,name='glBegin')
integer(4), intent(in), value :: mode 
end subroutine glBegin
end interface

interface
subroutine glBindTexture(target,texture) bind(c,name='glBindTexture')
integer(4), intent(in), value :: target, texture
end subroutine glBindTexture
end interface

!interface
!subroutine glBitmap(width,height,xorig,yorig,xmove,ymove,bitmap) bind(c,name='glBitmap')
!integer(4), intent(in), value :: width,height
!real(4), intent(in), value :: xorig,yorig,xmove,ymove
!integer(1) :: bitmap(*)
!end subroutine glBitmap
!end interface

interface
subroutine glBlendFunc(sfactor,dfactor) bind(c,name='glBlendFunc')
integer(4),intent(in), value :: sfactor,dfactor
end subroutine glBlendFunc
end interface

interface
subroutine glCallList(list) bind(c,name='glCallList')
integer(4), intent(in), value :: list 
end subroutine glCallList
end interface

interface
subroutine glCallLists(n,type,lists) bind(c,name='glCallLists')
integer(4), intent(in), value :: n,type
integer(4), intent(in) :: lists(*)
end subroutine glCallLists
end interface

interface
subroutine glClear(mask) bind(c,name='glClear')
integer(4), intent(in), value ::  mask 
end subroutine glClear
end interface

interface
subroutine glClearAccum(red,green,blue,alpha) bind(c,name='glClearAccum')
real(4), intent(in), value :: red,green,blue,alpha
end subroutine glClearAccum
end interface

interface
subroutine glClearColor(red,green,blue,alpha) bind(c,name='glClearColor')
real(4),intent(in), value :: red,green,blue,alpha
end subroutine glClearColor
end interface

interface
subroutine glClearDepth(depth) bind(c,name='glClearDepth')
real(8), intent(in), value :: depth 
end subroutine glClearDepth
end interface

interface
subroutine glClearIndex(c) bind(c,name='glClearIndex')
real(4), intent(in), value :: c 
end subroutine glClearIndex
end interface

interface
subroutine glClearStencil(s) bind(c,name='glClearStencil')
integer(4), intent(in), value :: s 
end subroutine glClearStencil
end interface

interface
subroutine glClipPlane(plane,equation) bind(c,name='glClipPlane')
integer(4), intent(in), value :: plane, equation
end subroutine glClipPlane
end interface

interface
subroutine glColor3b(red,green,blue) bind(c,name='glColor3b')
integer(1), intent(in), value ::  red,green,blue
end subroutine glColor3b
end interface

interface
subroutine glColor3bv(v) bind(c,name='glColor3bv')
integer(1), intent(in) :: v(3)
end subroutine glColor3bv
end interface

interface
subroutine glColor3d(red,green,blue ) bind(c,name='glColor3d')
real(8), intent(in), value :: red,green,blue
end subroutine glColor3d
end interface

interface
subroutine glColor3dv(v) bind(c,name='glColor3dv')
real(8), intent(in) :: v(3)
end subroutine glColor3dv
end interface

interface
subroutine glColor3f(red,green,blue) bind(c,name='glColor3f')
real(4), intent(in), value :: red,green,blue
end subroutine glColor3f
end interface

interface
subroutine glColor3fv(v) bind(c,name='glColor3fv')
real(4), intent(in) :: v(3)
end subroutine glColor3fv
end interface

interface
subroutine glColor3i(red,green,blue) bind(c,name='glColor3i')
integer(4), intent(in), value :: red,green,blue
end subroutine glColor3i
end interface

interface
subroutine glColor3iv(v) bind(c,name='glColor3iv')
integer(4), intent(in) :: v(3)
end subroutine glColor3iv
end interface

interface
subroutine glColor3s(red,green,blue) bind(c,name='glColor3s')
integer(2), intent(in), value :: red,green,blue
end subroutine glColor3s
end interface

interface
subroutine glColor3sv(v) bind(c,name='glColor3sv')
integer(2), intent(in) :: v(3)
end subroutine glColor3sv
end interface

interface
subroutine glColor3ub(red,green,blue) bind(c,name='glColor3ub')
integer(1), intent(in), value :: red,green,blue
end subroutine glColor3ub
end interface

interface
subroutine glColor3ubv(v) bind(c,name='glColor3ubv')
integer(1), intent(in) :: v(3)
end subroutine glColor3ubv
end interface

interface
subroutine glColor3ui(red,green,blue) bind(c,name='glColor3ui')
integer(4), intent(in), value :: red,green,blue
end subroutine glColor3ui
end interface

interface
subroutine glColor3uiv(v) bind(c,name='glColor3uiv')
integer(4), intent(in) :: v(3)
end subroutine glColor3uiv
end interface

interface
subroutine glColor3us(red,green,blue) bind(c,name='glColor3us')
integer(2), intent(in), value :: red,green,blue
end subroutine glColor3us
end interface

interface
subroutine glColor3usv(v) bind(c,name='glColor3usv')
integer(2), intent(in) :: v(3)
end subroutine glColor3usv
end interface

interface
subroutine glColor4b(red,green,blue,alpha) bind(c,name='glColor4b')
integer(1), intent(in), value :: red,green,blue,alpha
end subroutine glColor4b
end interface

interface
subroutine glColor4bv(v) bind(c,name='glColor4bv')
integer(1), intent(in) :: v(4)
end subroutine glColor4bv
end interface

interface
subroutine glColor4d(red,green,blue,alpha) bind(c,name='glColor4d')
real(8), intent(in), value ::  red,green,blue,alpha
end subroutine glColor4d
end interface

interface
subroutine glColor4dv(v) bind(c,name='glColor4dv')
real(8), intent(in) :: v(4)
end subroutine glColor4dv
end interface

interface
subroutine glColor4f(red,green,blue,alpha) bind(c,name='glColor4f')
real(4), intent(in), value :: red,green,blue,alpha
end subroutine glColor4f
end interface

interface
subroutine glColor4fv(v) bind(c,name='glColor4fv')
real(4), intent(in) :: v(4)
end subroutine glColor4fv
end interface

interface
subroutine glColor4i(red,green,blue,alpha) bind(c,name='glColor4i')
integer(4), intent(in), value :: red,green,blue,alpha
end subroutine glColor4i
end interface

interface
subroutine glColor4iv(v) bind(c,name='glColor4iv')
integer(4), intent(in) :: v(4)
end subroutine glColor4iv
end interface

interface
subroutine glColor4s(red,green,blue,alpha) bind(c,name='glColor4s')
integer(2), intent(in), value :: red,green,blue,alpha
end subroutine glColor4s
end interface

interface
subroutine glColor4sv(v) bind(c,name='glColor4sv')
integer(2), intent(in) :: v(4)
end subroutine glColor4sv
end interface

interface
subroutine glColor4ub(red,green,blue,alpha) bind(c,name='glColor4ub')
integer(1), intent(in), value :: red,green,blue,alpha
end subroutine glColor4ub
end interface

interface
subroutine glColor4ubv(v) bind(c,name='glColor4ubv')
integer(1), intent(in) :: v(4)
end subroutine glColor4ubv
end interface

interface
subroutine glColor4ui(red,green,blue,alpha) bind(c,name='glColor4ui')
integer(4), intent(in), value :: red,green,blue,alpha
end subroutine glColor4ui
end interface

interface
subroutine glColor4uiv(v) bind(c,name='glColor4uiv')
integer(4), intent(in) :: v(4)
end subroutine glColor4uiv
end interface

interface
subroutine glColor4us(red,green,blue,alpha) bind(c,name='glColor4us')
integer(2), intent(in), value :: red,green,blue,alpha
end subroutine glColor4us
end interface

interface
subroutine glColor4usv(v) bind(c,name='glColor4usv')
integer(2), intent(in) :: v(4)
end subroutine glColor4usv
end interface

interface
subroutine glColorMask(red,green,blue,alpha) bind(c,name='glColorMask')
integer(1), intent(in), value :: red,green,blue,alpha
end subroutine glColorMask
end interface

interface
subroutine glColorMaterial(face,mode) bind(c,name='glColorMaterial')
integer(4), intent(in), value :: face,mode
end subroutine glColorMaterial
end interface

interface
subroutine glColorPointer(size,type,stride,pointer) bind(c,name='glColorPointer')
import
INTEGER(4), intent(in), value :: size,type,stride
integer(C_INTPTR_T), intent(in), value :: pointer
end subroutine glColorPointer
end interface

interface
subroutine glCopyPixels(x,y,width,height,type) bind(c,name='glCopyPixels')
integer(4), intent(in), value :: x,y,width,height,type
end subroutine glCopyPixels
end interface

interface
subroutine glCopyTexImage1D(target,level,internalFormat,x,y,width,border) bind(c,name='glCopyTexImage1D')
integer(4), intent(in), value :: target,level,internalFormat,x,y,width,border
end subroutine glCopyTexImage1D
end interface

interface
subroutine glCopyTexImage2D(target,level,internalFormat,x,y,width,height,border) bind(c,name='glCopyTexImage2D')
integer(4), intent(in), value :: target,level,internalFormat,x,y,width,height,border
end subroutine glCopyTexImage2D
end interface

interface
subroutine glCopyTexSubImage1D(target,level,xoffset,x,y,width) bind(c,name='glCopyTexSubImage1D')
integer(4), intent(in), value :: target,level,xoffset,x,y,width
end subroutine glCopyTexSubImage1D
end interface

interface
subroutine glCopyTexSubImage2D(target,level,xoffset,yoffset,x,y,width,height) bind(c,name='glCopyTexSubImage2D')
integer(4), intent(in), value :: target,level,xoffset,yoffset,x,y,width,height
end subroutine glCopyTexSubImage2D
end interface

interface
subroutine glCullFace(mode) bind(c,name='glCullFace')
integer(4), intent(in), value :: mode 
end subroutine glCullFace
end interface

interface
subroutine glDeleteLists(list,range) bind(c,name='glDeleteLists')
integer(4), intent(in), value :: list,range 
end subroutine glDeleteLists
end interface

interface
subroutine glDeleteTextures(n,textures) bind(c,name='glDeleteTextures')
integer(4), intent(in), value :: n
integer(4), intent(in) :: textures(*)
end subroutine glDeleteTextures
end interface

interface
subroutine glDepthFunc(func) bind(c,name='glDepthFunc')
integer(4), intent(in), value :: func 
end subroutine glDepthFunc
end interface

interface
subroutine glDepthMask(flag) bind(c,name='glDepthMask')
integer(1), intent(in), value :: flag 
end subroutine glDepthMask
end interface

interface
subroutine glDepthRange(zNear,zFar) bind(c,name='glDepthRange')
real(8), intent(in), value :: zNear,zFar
end subroutine glDepthRange
end interface

interface
subroutine glDisable(cap) bind(c,name='glDisable')
integer(4), intent(in), value :: cap 
end subroutine glDisable
end interface

interface
subroutine glDisableClientState(array) bind(c,name='glDisableClientState')
integer(4), intent(in), value :: array
end subroutine glDisableClientState
end interface

interface
subroutine glDrawArrays(mode,first,count) bind(c,name='glDrawArrays')
integer(4), intent(in), value :: mode,first,count
end subroutine glDrawArrays
end interface

interface
subroutine glDrawBuffer(mode) bind(c,name='glDrawBuffer')
integer(4), intent(in), value :: mode 
end subroutine glDrawBuffer
end interface

interface
subroutine glDrawElements(mode,count,type,indices) bind(c,name='glDrawElements')
import
integer(4), intent(in), value :: mode,count,type
integer(C_INTPTR_T), intent(in), value ::indices 
end subroutine glDrawElements
end interface

interface
subroutine glDrawPixels(width,height,format,type,pixels) bind(c,name='glDrawPixels')
import
integer(4), intent(in), value :: width,height,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
end subroutine glDrawPixels
end interface

interface
subroutine glEdgeFlag(flag) bind(c,name='glEdgeFlag')
integer(1), intent(in), value :: flag
end subroutine glEdgeFlag
end interface

interface
subroutine glEdgeFlagPointer(stride,pointer) bind(c,name='glEdgeFlagPointer')
INTEGER(4), intent(in), value :: stride
integer(1),intent(in) :: pointer(*)
end subroutine glEdgeFlagPointer
end interface

interface
subroutine glEdgeFlagv(flag) bind(c,name='glEdgeFlagv')
integer(1), intent(in) :: flag(*)
end subroutine glEdgeFlagv
end interface

interface
subroutine glEnable(cap) bind(c,name='glEnable')
integer(4), intent(in), value :: cap 
end subroutine glEnable
end interface

interface
subroutine glEnableClientState(array) bind(c,name='glEnableClientState')
integer(4), intent(in), value :: array
end subroutine glEnableClientState
end interface

interface
subroutine glEnd() bind(c,name='glEnd')
end subroutine glEnd
end interface

interface
subroutine glEndList() bind(c,name='glEndList')
end subroutine glEndList
end interface

interface
subroutine glEvalCoord1d(u) bind(c,name='glEvalCoord1d')
real(8), intent(in), value ::  u
end subroutine glEvalCoord1d
end interface

interface
subroutine glEvalCoord1dv(u) bind(c,name='glEvalCoord1dv')
real(8), intent(in) ::  u(*)
end subroutine glEvalCoord1dv
end interface

interface
subroutine glEvalCoord1f(u) bind(c,name='glEvalCoord1f')
real(4), intent(in), value ::   u
end subroutine glEvalCoord1f
end interface

interface
subroutine glEvalCoord1fv(u) bind(c,name='glEvalCoord1fv')
real(4), intent(in) ::   u
end subroutine glEvalCoord1fv
end interface

interface
subroutine glEvalCoord2d(u,v) bind(c,name='glEvalCoord2d')
real(8), intent(in), value ::   u,v
end subroutine glEvalCoord2d
end interface

interface
subroutine glEvalCoord2dv(v) bind(c,name='glEvalCoord2dv')
real(8), intent(in) :: v(2)
end subroutine glEvalCoord2dv
end interface

interface
subroutine glEvalCoord2f(u,v) bind(c,name='glEvalCoord2f')
real(4), intent(in), value ::   u,v
end subroutine glEvalCoord2f
end interface

interface
subroutine glEvalCoord2fv(v) bind(c,name='glEvalCoord2fv')
real(4), intent(in) :: v(2)
end subroutine glEvalCoord2fv
end interface

interface
subroutine glEvalMesh1(mode,i1,i2) bind(c,name='glEvalMesh1')
integer(4), intent(in), value :: mode,i1,i2
end subroutine glEvalMesh1
end interface

interface
subroutine glEvalMesh2(mode,i1,i2,j1,j2) bind(c,name='glEvalMesh2')
integer(4), intent(in), value :: mode,i1,i2,j1,j2
end subroutine glEvalMesh2
end interface

interface
subroutine glEvalPoint1(i) bind(c,name='glEvalPoint1')
integer(4), intent(in), value :: i
end subroutine glEvalPoint1
end interface

interface
subroutine glEvalPoint2(i,j) bind(c,name='glEvalPoint2')
integer(4), intent(in), value :: i,j
end subroutine glEvalPoint2
end interface

interface
subroutine glFeedbackBuffer(size,type,buffer) !???????????
integer(4), intent(in), value :: size,type
real(4) :: buffer(*)
end subroutine glFeedbackBuffer
end interface

interface
subroutine glFinish() bind(c,name='glFinish')
end subroutine glFinish
end interface

interface
subroutine glFlush() bind(c,name='glFlush')
end subroutine glFlush
end interface

interface
subroutine glFogf(pname,param) bind(c,name='glFogf')
integer(4), intent(in), value :: pname
real(4), intent(in), value :: param
end subroutine glFogf
end interface

interface
subroutine glFogfv(pname,params) bind(c,name='glFogfv')
integer(4), intent(in), value :: pname  
real(4), intent(in) ::   params(*)
end subroutine glFogfv
end interface

interface
subroutine glFogi(pname,param) bind(c,name='glFogi')
integer(4), intent(in), value :: pname, param
end subroutine glFogi
end interface

interface
subroutine glFogiv(pname,params) bind(c,name='glFogiv')
integer(4), intent(in), value :: pname
integer(4), intent(in) :: params(*)
end subroutine glFogiv
end interface

interface
subroutine glFrontFace(mode) bind(c,name='glFrontFace')
integer(4), intent(in), value :: mode  
end subroutine glFrontFace
end interface

interface
subroutine glFrustum(left,right,bottom,top,zNear,zFar) bind(c,name='glFrustum')
real(8), intent(in), value :: left,right,bottom,top,zNear,zFar
end subroutine glFrustum
end interface

interface
integer(4) function glGenLists(range) bind(c,name='glGenLists')
integer(4), intent(in), value :: range  
end function glGenLists
end interface

interface
integer(4) function glGenTextures(n,textures) bind(c,name='glGenTextures')
integer(4), intent(in), value :: n
integer(4) :: textures(*)
end function glGenTextures
end interface

interface
subroutine glGetBooleanv(pname,params) bind(c,name='glGetBooleanv')
integer(4), intent(in), value :: pname  
integer(4), intent(in) :: params(*)  
end subroutine glGetBooleanv
end interface

interface
subroutine glGetClipPlane(plane,equation) bind(c,name='glGetClipPlane')
integer(4), intent(in), value :: plane
real(8), intent(in) :: equation(*)
end subroutine glGetClipPlane
end interface

interface
subroutine glGetDoublev(pname,params) bind(c,name='glGetDoublev')
integer(4), intent(in), value :: pname  
real(8), intent(in) :: params(*)  
end subroutine glGetDoublev
end interface

interface
integer(4) function glGetError() bind(c,name='glGetError')
end function glGetError
end interface

interface
subroutine glGetFloatv(pname,params) bind(c,name='glGetFloatv')
integer(4), intent(in), value :: pname  
real(4) :: params(*)  
end subroutine glGetFloatv
end interface

interface
subroutine glGetIntegerv(pname,params) bind(c,name='glGetIntegerv')
integer(4), intent(in), value :: pname  
integer(4) :: params(*)  
end subroutine glGetIntegerv
end interface

interface
subroutine glGetLightfv(light,pname,params) bind(c,name='glGetLightfv')
integer(4), intent(in), value :: light,pname  
real(4) :: params(*) 
end subroutine glGetLightfv
end interface

interface
subroutine glGetLightiv(light,pname,params) bind(c,name='glGetLightiv')
integer(4), intent(in), value :: light,pname  
integer(4) :: params(*) 
end subroutine glGetLightiv
end interface

interface
subroutine glGetMapdv(target,query,v) bind(c,name='glGetMapdv')
integer(4), intent(in), value :: target,query  
real(8) :: v(*)  
end subroutine glGetMapdv
end interface

interface
subroutine glGetMapfv(target,query,v) bind(c,name='glGetMapfv')
integer(4), intent(in), value :: target,query  
real(4) :: v(*)
end subroutine glGetMapfv
end interface

interface
subroutine glGetMapiv(target,query,v) bind(c,name='glGetMapiv')
integer(4), intent(in), value :: target,query  
integer(4) :: v(*)
end subroutine glGetMapiv
end interface

interface
subroutine glGetMaterialfv(face,pname,params) bind(c,name='glGetMaterialfv')
integer(4), intent(in), value :: face,pname  
real(4) :: params(*)
end subroutine glGetMaterialfv
end interface

interface
subroutine glGetMaterialiv(face,pname,params) bind(c,name='glGetMaterialiv')
integer(4), intent(in), value :: face,pname  
integer(4) :: params(*)
end subroutine glGetMaterialiv
end interface

interface
subroutine glGetPixelMapfv(map,values) bind(c,name='glGetPixelMapfv')
integer(4), intent(in), value :: map
real(4) :: values(*)
end subroutine glGetPixelMapfv
end interface

interface
subroutine glGetPixelMapuiv(map,values) bind(c,name='glGetPixelMapuiv')
integer(4), intent(in), value :: map
integer(4) :: values(*)
end subroutine glGetPixelMapuiv
end interface

interface
subroutine glGetPixelMapusv(map,values) bind(c,name='glGetPixelMapusv')
integer(4), intent(in), value :: map
integer(2) :: values(*)
end subroutine glGetPixelMapusv
end interface

interface
subroutine glGetPointerv(pname,params) bind(c,name='glGetPointerv')
integer(4), intent(in), value :: pname  
integer(4), intent(out) :: params(*)  
end subroutine glGetPointerv
end interface

interface
subroutine glGetPolygonStipple(mask) bind(c,name='glGetPolygonStipple')
integer(4), intent(in), value :: mask  
end subroutine glGetPolygonStipple
end interface

!interface
!function glGetString(name) bind(c,name='glGetString')
!use iso_c_binding, only : c_char
!character(kind=c_char),pointer :: glGetString(100)
!integer(4), intent(in), value :: name
!end function glGetString
!end interface

interface
subroutine glGetTexEnvfv(target,pname,params) bind(c,name='glGetTexEnvfv')
integer(4), intent(in), value :: target,pname
real(4) :: params(*)
end subroutine glGetTexEnvfv
end interface

interface
subroutine glGetTexEnviv(target,pname,params) bind(c,name='glGetTexEnviv')
integer(4), intent(in), value :: target,pname
integer(4) :: params(*)
end subroutine glGetTexEnviv
end interface

interface
subroutine glGetTexGendv(target,pname,params) bind(c,name='glGetTexGendv')
integer(4), intent(in), value :: target,pname
real(8) :: params(*)
end subroutine glGetTexGendv
end interface

interface
subroutine glGetTexGenfv(target,pname,params) bind(c,name='glGetTexGenfv')
integer(4),intent(in), value :: target,pname  
real(4) :: params(*)
end subroutine glGetTexGenfv
end interface

interface
subroutine glGetTexGeniv(target,pname,params) bind(c,name='glGetTexGeniv')
integer(4),intent(in), value :: target,pname
integer(4) :: params(*)
end subroutine glGetTexGeniv
end interface

interface
subroutine glGetTexImage(target,level,format,type,pixels) bind(c,name='glGetTexImage')
import
integer(4), intent(in), value :: target,level,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
end subroutine glGetTexImage
end interface

interface
subroutine glGetTexLevelParameterfv(target,level,pname,params) bind(c,name='glGetTexLevelParameterfv')
integer(4), intent(in), value :: target,level,pname
real(4) :: params(*)
end subroutine glGetTexLevelParameterfv
end interface

interface
subroutine glGetTexLevelParameteriv(target,level,pname,params) bind(c,name='glGetTexLevelParameteriv')
integer(4), intent(in), value :: target,level,pname
integer(4) :: params(*)
end subroutine glGetTexLevelParameteriv
end interface

interface
subroutine glGetTexParameterfv(target,pname,params) bind(c,name='glGetTexParameterfv')
integer(4), intent(in), value :: target,pname
real(4) :: params(*)
end subroutine glGetTexParameterfv
end interface

interface
subroutine glGetTexParameteriv(target,pname,params) bind(c,name='glGetTexParameteriv')
integer(4), intent(in), value :: target,pname
integer(4) :: params(*)
end subroutine glGetTexParameteriv
end interface

interface
subroutine glHint(target,mode) bind(c,name='glHint')
integer(4), intent(in), value :: target,mode
end subroutine glHint
end interface

interface
subroutine glIndexd(c) bind(c,name='glIndexd')
real(8),intent(in),value :: c  
end subroutine glIndexd
end interface

interface
subroutine glIndexdv(c) bind(c,name='glIndexdv')
real(8),dimension(1),intent(in) :: c
end subroutine glIndexdv
end interface

interface
subroutine glIndexf(c) bind(c,name='glIndexf')
real(4),intent(in),value :: c  
end subroutine glIndexf
end interface

interface
subroutine glIndexfv(c) bind(c,name='glIndexfv')
real(4),dimension(1),intent(in) ::  c  
end subroutine glIndexfv
end interface

interface
subroutine glIndexi(c) bind(c,name='glIndexi')
integer(4), intent(in), value :: c  
end subroutine glIndexi
end interface

interface
subroutine glIndexiv(c) bind(c,name='glIndexiv')
integer(4),dimension(1),intent(in) :: c  
end subroutine glIndexiv
end interface

interface
subroutine glIndexs(c) bind(c,name='glIndexs')
INTEGER(2), intent(in), value :: c  
END SUBROUTINE glIndexs
end interface

interface
subroutine glIndexsv(c) bind(c,name='glIndexsv')
integer(2),dimension(1),intent(in) ::  c  
end subroutine glIndexsv
end interface

interface
subroutine glIndexub(c) bind(c,name='glIndexub')
INTEGER(1), intent(in), value :: c  
END SUBROUTINE glIndexub
end interface

interface
subroutine glIndexubv(c) bind(c,name='glIndexubv')
INTEGER(1),DIMENSION(1),INTENT(IN) :: c
END SUBROUTINE glIndexubv
end interface

interface
subroutine glIndexMask(mask) bind(c,name='glIndexMask')
integer(4), intent(in), value :: mask  
end subroutine glIndexMask
end interface

interface
subroutine glIndexPointer(type,stride,pointer) bind(c,name='glIndexPointer')
import
INTEGER(4), intent(in), value :: type,stride
integer(C_INTPTR_T), intent(in), value :: pointer
END SUBROUTINE glIndexPointer
end interface

interface
subroutine glInitNames() bind(c,name='glInitNames')
end subroutine glInitNames
end interface

interface
subroutine glInterleavedArrays(format,stride,pointer) bind(c,name='glInterleavedArrays')
import
INTEGER(4), intent(in), value :: format,stride
integer(C_INTPTR_T), intent(in), value :: pointer
END SUBROUTINE glInterleavedArrays
end interface

interface
logical(4) function glIsEnabled(cap) bind(c,name='glIsEnabled')
integer(4), intent(in), value :: cap
end function glIsEnabled
end interface

interface
logical(4) function glIsList(list) bind(c,name='glIsList')
integer(4), intent(in), value :: list  
end function glIsList
end interface

interface
subroutine glIsTexture(texture) bind(c,name='glIsTexture')
INTEGER(4), intent(in), value :: texture
END SUBROUTINE glIsTexture
end interface

interface
subroutine glLightModelf(pname,param) bind(c,name='glLightModelf')
integer(4), intent(in), value :: pname  
real(4), intent(in), value :: param  
end subroutine glLightModelf
end interface

interface
subroutine glLightModelfv(pname,params) bind(c,name='glLightModelfv')
integer(4), intent(in), value :: pname  
real(4) ::  params(*)
end subroutine glLightModelfv
end interface

interface
subroutine glLightModeli(pname,param) bind(c,name='glLightModeli')
integer(4), intent(in), value :: pname,param
end subroutine glLightModeli
end interface

interface
subroutine glLightModeliv(pname,params) bind(c,name='glLightModeliv')
integer(4), intent(in), value :: pname
integer(4) :: params(*)
end subroutine glLightModeliv
end interface

interface
subroutine glLightf(light,pname,param) bind(c,name='glLightf')
integer(4), intent(in), value :: light,pname
real(4) :: param
end subroutine glLightf
end interface

interface
subroutine glLightfv(light,pname,params) bind(c,name='glLightfv')
integer(4), intent(in), value :: light,pname
real(4) ::  params(*)
end subroutine glLightfv
end interface

interface
subroutine glLighti(light,pname,param) bind(c,name='glLighti')
integer(4), intent(in), value :: light,pname,param  
end subroutine glLighti
end interface

interface
subroutine glLightiv(light,pname,params) bind(c,name='glLightiv')
integer(4), intent(in), value :: light,pname
integer(4) :: params(*)
end subroutine glLightiv
end interface

interface
subroutine glLineStipple(factor,pattern) bind(c,name='glLineStipple')
integer(4), intent(in), value :: factor  
integer(2), intent(in), value :: pattern
end subroutine glLineStipple
end interface

interface
subroutine glLineWidth(width) bind(c,name='glLineWidth')
real(4), intent(in), value :: width  
end subroutine glLineWidth
end interface

interface
subroutine glListBase(base) bind(c,name='glListBase')
integer(4), intent(in), value :: base  
end subroutine glListBase
end interface

interface
subroutine glLoadIdentity() bind(c,name='glLoadIdentity')
end subroutine glLoadIdentity
end interface

interface
subroutine glLoadMatrixd(m) bind(c,name='glLoadMatrixd')
real(8) :: m(16)
end subroutine glLoadMatrixd
end interface

interface
subroutine glLoadMatrixf(m) bind(c,name='glLoadMatrixf')
real(4) :: m(16)
end subroutine glLoadMatrixf
end interface

interface
subroutine glLoadName(name) bind(c,name='glLoadName')
integer(4), intent(in), value :: name  
end subroutine glLoadName
end interface

interface
subroutine glLogicOp(opcode) bind(c,name='glLogicOp')
integer(4), intent(in), value :: opcode
end subroutine glLogicOp
end interface

interface
subroutine glMap1d(target,u1,u2,stride,order,points) bind(c,name='glMap1d')
integer(4), intent(in), value :: target, stride,  order
real(8), intent(in), value :: u1, u2
real(8) :: points(*)
end subroutine glMap1d
end interface

interface
subroutine glMap1f(target,u1,u2,stride,order,points) bind(c,name='glMap1f')
integer(4), intent(in), value :: target, stride,  order
real(4), intent(in), value :: u1, u2
real(4) :: points(*)
end subroutine glMap1f
end interface
                
interface
subroutine glMap2d(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,ps) bind(c,name='glMap2d')
integer(4), intent(in), value :: target, ustride, uorder, vstride, vorder
real(8), intent(in), value :: u1, u2, v1, v2
real(8) :: ps(*)
end subroutine glMap2d
end interface

interface
subroutine glMap2f(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,ps) bind(c,name='glMap2f')
integer(4), intent(in), value :: target, ustride, uorder, vstride, vorder
real(4), intent(in), value :: u1, u2, v1, v2
real(4) :: ps(*)
end subroutine glMap2f
end interface

interface
subroutine glMapGrid1d(un,u1,u2) bind(c,name='glMapGrid1d')
integer(4), intent(in), value :: un 
real(8), intent(in), value :: u1,u2
end subroutine glMapGrid1d
end interface

interface
subroutine glMapGrid1f(un,u1,u2) bind(c,name='glMapGrid1f')
integer(4), intent(in), value :: un 
real(4), intent(in), value :: u1,u2
end subroutine glMapGrid1f
end interface

interface
subroutine glMapGrid2d(un,u1,u2,vn,v1,v2) bind(c,name='glMapGrid2d')
integer(4), intent(in), value :: un,vn
real(8), intent(in), value :: u1,u2,v1,v2
end subroutine glMapGrid2d
end interface

interface
subroutine glMapGrid2f(un,u1,u2,vn,v1,v2) bind(c,name='glMapGrid2f')
integer(4), intent(in), value :: un,vn
real(4), intent(in), value :: u1,u2,v1,v2
end subroutine glMapGrid2f
end interface

interface
subroutine glMaterialf(face,pname,param) bind(c,name='glMaterialf')
integer(4), intent(in), value :: face,pname
real(4), intent(in), value :: param
end subroutine glMaterialf
end interface

interface
subroutine glMaterialfv(face,pname,params) bind(c,name='glMaterialfv')
integer(4), intent(in), value :: face,pname
real(4), intent(in) :: params(*)
end subroutine glMaterialfv
end interface

interface
subroutine glMateriali(face,pname,param) bind(c,name='glMateriali')
integer(4), intent(in), value :: face,pname,param
end subroutine glMateriali
end interface

interface
subroutine glMaterialiv(face,pname,params) bind(c,name='glMaterialiv')
integer(4), intent(in), value :: face,pname
integer(4), intent(in) :: params(*)
end subroutine glMaterialiv
end interface

interface
subroutine glMatrixMode(mode) bind(c,name='glMatrixMode')
integer(4), intent(in), value :: mode
end subroutine glMatrixMode
end interface

interface
subroutine glMultMatrixd(m) bind(c,name='glMultMatrixd')
real(8), intent(in) :: m(16)
end subroutine glMultMatrixd
end interface

interface
subroutine glMultMatrixf(m) bind(c,name='glMultMatrixf')
real(4), intent(in) :: m(16)
end subroutine glMultMatrixf
end interface

interface
subroutine glNewList(list,mode) bind(c,name='glNewList')
integer(4), intent(in), value :: list,mode
end subroutine glNewList
end interface

interface
subroutine glNormal3b(nx,ny,nz) bind(c,name='glNormal3b')
integer(1), intent(in), value :: nx,ny,nz
end subroutine glNormal3b
end interface

interface
subroutine glNormal3bv(v) bind(c,name='glNormal3bv')
integer(1) :: v(3)
end subroutine glNormal3bv
end interface

interface
subroutine glNormal3d(nx,ny,nz) bind(c,name='glNormal3d')
real(8), intent(in), value :: nx,ny,nz
end subroutine glNormal3d
end interface

interface
subroutine glNormal3dv(v) bind(c,name='glNormal3dv')
real(8) :: v(3)
end subroutine glNormal3dv
end interface

interface
subroutine glNormal3f(nx,ny,nz) bind(c,name='glNormal3f')
real(4), intent(in), value :: nx,ny,nz
end subroutine glNormal3f
end interface

interface
subroutine glNormal3fv(v) bind(c,name='glNormal3fv')
real(4), intent(in)  :: v(3)
end subroutine glNormal3fv
end interface

interface
subroutine glNormal3i(nx,ny,nz) bind(c,name='glNormal3i')
integer(4), intent(in), value :: nx,ny,nz
end subroutine glNormal3i
end interface

interface
subroutine glNormal3iv(v) bind(c,name='glNormal3iv')
integer(4), intent(in)  :: v(3)
end subroutine glNormal3iv
end interface

interface
subroutine glNormal3s(nx,ny,nz) bind(c,name='glNormal3s')
integer(2), intent(in), value :: nx,ny,nz
end subroutine glNormal3s
end interface

interface
subroutine glNormal3sv(v) bind(c,name='glNormal3sv')
integer(2), intent(in) :: v(3)
end subroutine glNormal3sv
end interface

interface
subroutine glNormalPointer(type,stride,pointer) bind(c,name='glNormalPointer')
import
integer(4),intent(in), value :: type,stride
integer(C_INTPTR_T), intent(in), value :: pointer
end subroutine glNormalPointer
end interface

interface
subroutine glOrtho(left,right,bottom,top,zNear,zFar) bind(c,name='glOrtho')
real(8),intent(in), value :: left,right,bottom,top,zNear,zFar
end subroutine glOrtho
end interface

interface
subroutine glPassThrough(token) bind(c,name='glPassThrough')
real(4), intent(in), value :: token  
end subroutine glPassThrough
end interface

interface
subroutine glPixelMapfv(map,mapsize,values) bind(c,name='glPixelMapfv')
integer(4), intent(in), value :: map,mapsize
real(4), intent(in) ::  values(*)
end subroutine glPixelMapfv
end interface

interface
subroutine glPixelMapuiv(map,mapsize,values) bind(c,name='glPixelMapuiv')
integer(4), intent(in), value :: map,mapsize
integer(4), intent(in) :: values(*)
end subroutine glPixelMapuiv
end interface

interface
subroutine glPixelMapusv(map,mapsize,values) bind(c,name='glPixelMapusv')
integer(4), intent(in), value :: map,mapsize
integer(2), intent(in) :: values(*)
end subroutine glPixelMapusv
end interface

interface
subroutine glPixelStoref(pname,param) bind(c,name='glPixelStoref')
integer(4), intent(in), value :: pname  
real(4), intent(in), value :: param  
end subroutine glPixelStoref
end interface

interface
subroutine glPixelStorei(pname,param) bind(c,name='glPixelStorei')
integer(4), intent(in), value :: pname, param
end subroutine glPixelStorei
end interface

interface
subroutine glPixelTransferf(pname,param) bind(c,name='glPixelTransferf')
integer(4), intent(in), value :: pname  
real(4), intent(in), value :: param
end subroutine glPixelTransferf
end interface

interface
subroutine glPixelTransferi(pname,param) bind(c,name='glPixelTransferi')
integer(4), intent(in), value :: pname, param
end subroutine glPixelTransferi
end interface

interface
subroutine glPixelZoom(xfactor,yfactor) bind(c,name='glPixelZoom')
real(4), intent(in), value :: xfactor,yfactor  
end subroutine glPixelZoom
end interface

interface
subroutine glPointSize(size) bind(c,name='glPointSize')
real(4), intent(in), value :: size  
end subroutine glPointSize
end interface

interface
subroutine glPolygonMode(face,mode) bind(c,name='glPolygonMode')
integer(4), intent(in), value :: face,mode
end subroutine glPolygonMode
end interface

interface
subroutine glPolygonOffset(factor,units) bind(c,name='glPolygonOffset')
real(4), intent(in), value :: factor,units
END SUBROUTINE glPolygonOffset
end interface

interface
subroutine glPolygonStipple(mask) bind(c,name='glPolygonStipple')
integer(1), intent(in) :: mask(*)
end subroutine glPolygonStipple
end interface

interface
subroutine glPopAttrib() bind(c,name='glPopAttrib')
end subroutine glPopAttrib
end interface

interface
SUBROUTINE glPopClientAttrib() bind(c,name='glPopClientAttrib')
END SUBROUTINE glPopClientAttrib
end interface

interface
subroutine glPopMatrix() bind(c,name='glPopMatrix')
end subroutine glPopMatrix
end interface

interface
subroutine glPopName() bind(c,name='glPopName')
end subroutine glPopName
end interface

interface
subroutine glPrioritizeTextures(n,textures,priorities) bind(c,name='glPrioritizeTextures')
INTEGER(4),INTENT(IN), value :: n
INTEGER(4),INTENT(IN) :: textures(*)
REAL(4),INTENT(IN) :: priorities(*)
END SUBROUTINE glPrioritizeTextures
end interface

interface
subroutine glPushAttrib(mask) bind(c,name='glPushAttrib')
integer(4), intent(in), value :: mask
end subroutine glPushAttrib
end interface

interface
subroutine glPushClientAttrib(mask) bind(c,name='glPushClientAttrib')
INTEGER(4), intent(in), value :: mask
END SUBROUTINE glPushClientAttrib
end interface

interface
subroutine glPushMatrix() bind(c,name='glPushMatrix')
end subroutine glPushMatrix
end interface

interface
subroutine glPushName(name) bind(c,name='glPushName')
integer(4), intent(in), value :: name
end subroutine glPushName
end interface

interface
subroutine glRasterPos2d(x,y) bind(c,name='glRasterPos2d')
real(8), intent(in), value :: x,y
end subroutine glRasterPos2d
end interface

interface
subroutine glRasterPos2dv(v) bind(c,name='glRasterPos2dv')
real(8), intent(in) :: v(2)
end subroutine glRasterPos2dv
end interface

interface
subroutine glRasterPos2f(x,y) bind(c,name='glRasterPos2f')
real(4), intent(in), value :: x,y
end subroutine glRasterPos2f
end interface

interface
subroutine glRasterPos2fv(v) bind(c,name='glRasterPos2fv')
real(4), intent(in) :: v(2)
end subroutine glRasterPos2fv
end interface

interface
subroutine glRasterPos2i(x,y) bind(c,name='glRasterPos2i')
integer(4), intent(in), value :: x,y
end subroutine glRasterPos2i
end interface

interface
subroutine glRasterPos2iv(v) bind(c,name='glRasterPos2iv')
integer(4), intent(in) :: v(2)
end subroutine glRasterPos2iv
end interface

interface
subroutine glRasterPos2s(x,y) bind(c,name='glRasterPos2s')
integer(2), intent(in), value :: x,y
end subroutine glRasterPos2s
end interface

interface
subroutine glRasterPos2sv(v) bind(c,name='glRasterPos2sv')
integer(2), intent(in) :: v(2)
end subroutine glRasterPos2sv
end interface

interface
subroutine glRasterPos3d(x,y,z) bind(c,name='glRasterPos3d')
real(8), intent(in), value ::  x,y,z
end subroutine glRasterPos3d
end interface

interface
subroutine glRasterPos3dv(v) bind(c,name='glRasterPos3dv')
real(8), intent(in) :: v(3)
end subroutine glRasterPos3dv
end interface

interface
subroutine glRasterPos3f(x,y,z) bind(c,name='glRasterPos3f')
real(4), intent(in), value :: x,y,z
end subroutine glRasterPos3f
end interface

interface
subroutine glRasterPos3fv(v) bind(c,name='glRasterPos3fv')
real(4), intent(in) :: v(3)
end subroutine glRasterPos3fv
end interface

interface
subroutine glRasterPos3i(x,y,z) bind(c,name='glRasterPos3i')
integer(4), intent(in), value :: x,y,z
end subroutine glRasterPos3i
end interface

interface
subroutine glRasterPos3iv(v) bind(c,name='glRasterPos3iv')
integer(4), intent(in) :: v(3)
end subroutine glRasterPos3iv
end interface

interface
subroutine glRasterPos3s(x,y,z) bind(c,name='glRasterPos3s')
integer(2), intent(in), value :: x,y,z
end subroutine glRasterPos3s
end interface

interface
subroutine glRasterPos3sv(v) bind(c,name='glRasterPos3sv')
integer(2), intent(in) :: v(3)
end subroutine glRasterPos3sv
end interface

interface
subroutine glRasterPos4d(x,y,z,w) bind(c,name='glRasterPos4d')
real(8), intent(in), value :: x,y,z,w
end subroutine glRasterPos4d
end interface

interface
subroutine glRasterPos4dv(v) bind(c,name='glRasterPos4dv')
real(8), intent(in) :: v(4)
end subroutine glRasterPos4dv
end interface

interface
subroutine glRasterPos4f(x,y,z,w) bind(c,name='glRasterPos4f')
real(4), intent(in), value :: x,y,z,w
end subroutine glRasterPos4f
end interface

interface
subroutine glRasterPos4fv(v) bind(c,name='glRasterPos4fv')
real(4), intent(in) :: v(4)
end subroutine glRasterPos4fv
end interface

interface
subroutine glRasterPos4i(x,y,z,w) bind(c,name='glRasterPos4i')
integer(4), intent(in), value :: x,y,z,w
end subroutine glRasterPos4i
end interface

interface
subroutine glRasterPos4iv(v) bind(c,name='glRasterPos4iv')
integer(4), intent(in) :: v(4)
end subroutine glRasterPos4iv
end interface

interface
subroutine glRasterPos4s(x,y,z,w) bind(c,name='glRasterPos4s')
integer(2), intent(in), value :: x,y,z,w
end subroutine glRasterPos4s
end interface

interface
subroutine glRasterPos4sv(v) bind(c,name='glRasterPos4sv')
integer(2), intent(in) :: v(4)
end subroutine glRasterPos4sv
end interface

interface
subroutine glReadBuffer(mode) bind(c,name='glReadBuffer')
integer(4), intent(in), value :: mode  
end subroutine glReadBuffer
end interface

interface
subroutine glReadPixels(x,y,width,height,format,type,pixels) bind(c,name='glReadPixels')
import
integer(4), intent(in), value :: x,y,width,height,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
end subroutine glReadPixels
end interface

interface
subroutine glRectd(x1,y1,x2,y2) bind(c,name='glRectd')
real(8), intent(in), value :: x1,y1,x2,y2
end subroutine glRectd
end interface

interface
subroutine glRectdv(va,vb) bind(c,name='glRectdv')
real(8), intent(in) :: va(*),vb(*)
end subroutine glRectdv
end interface

interface
subroutine glRectf(x1,y1,x2,y2) bind(c,name='glRectf')
real(4), intent(in), value :: x1,y1,x2,y2
end subroutine glRectf
end interface

interface
subroutine glRectfv(va,vb) bind(c,name='glRectfv')
real(4), intent(in) :: va(2),vb(2)
end subroutine glRectfv
end interface

interface
subroutine glRecti(x1,y1,x2,y2) bind(c,name='glRecti')
integer(4), intent(in), value :: x1,y1,x2,y2
end subroutine glRecti
end interface

interface
subroutine glRectiv(va,vb) bind(c,name='glRectiv')
integer(4), intent(in) :: va(2),vb(2)
end subroutine glRectiv
end interface

interface
subroutine glRects(x1,y1,x2,y2) bind(c,name='glRects')
integer(2), intent(in), value :: x1,y1,x2,y2
end subroutine glRects
end interface

interface
subroutine glRectsv(va,vb) bind(c,name='glRectsv')
integer(2), intent(in) :: va(2),vb(2)
end subroutine glRectsv
end interface

interface
integer(4) function glRenderMode(mode) bind(c,name='glRenderMode')
integer(4), intent(in), value :: mode  
end function glRenderMode
end interface

interface
subroutine glRotated(angle,x,y,z) bind(c,name='glRotated')
real(8), intent(in), value :: angle,x,y,z
end subroutine glRotated
end interface

interface
subroutine glRotatef(angle,x,y,z) bind(c,name='glRotatef')
real(4), intent(in), value :: angle,x,y,z
end subroutine glRotatef
end interface

interface
subroutine glScaled(x,y,z) bind(c,name='glScaled')
real(8), intent(in), value :: x,y,z
end subroutine glScaled
end interface

interface
subroutine glScalef(x,y,z) bind(c,name='glScalef')
real(4), intent(in), value :: x,y,z
end subroutine glScalef
end interface

interface
subroutine glScissor(x,y,width,height) bind(c,name='glScissor')
integer(4), intent(in), value :: x,y,width,height
end subroutine glScissor
end interface

interface
subroutine glSelectBuffer(size,buffer) bind(c,name='glSelectBuffer')
integer(4), intent(in), value :: size
integer(4), intent(in out) :: buffer(*)
end subroutine glSelectBuffer
end interface

interface
subroutine glShadeModel(mode) bind(c,name='glShadeModel')
integer(4), intent(in), value :: mode  
end subroutine glShadeModel
end interface

interface
subroutine glStencilFunc(func,ref,mask) bind(c,name='glStencilFunc')
integer(4), intent(in), value :: func,ref,mask
end subroutine glStencilFunc
end interface

interface
subroutine glStencilMask(mask) bind(c,name='glStencilMask')
integer(4), intent(in), value :: mask
end subroutine glStencilMask
end interface

interface
subroutine glStencilOp(fail,zfail,zpass) bind(c,name='glStencilOp')
integer(4), intent(in), value :: fail,zfail,zpass
end subroutine glStencilOp
end interface

interface
subroutine glTexCoord1d(s) bind(c,name='glTexCoord1d')
real(8), intent(in), value ::   s
end subroutine glTexCoord1d
end interface

interface
subroutine glTexCoord1dv(v) bind(c,name='glTexCoord1dv')
integer(4), intent(in)  :: v
end subroutine glTexCoord1dv
end interface

interface
subroutine glTexCoord1f(s) bind(c,name='glTexCoord1f')
real(4), intent(in), value :: s
end subroutine glTexCoord1f
end interface

interface
subroutine glTexCoord1fv(v) bind(c,name='glTexCoord1fv')
integer(4), intent(in)  :: v
end subroutine glTexCoord1fv
end interface

interface
subroutine glTexCoord1i(s) bind(c,name='glTexCoord1i')
integer(4), intent(in), value :: s
end subroutine glTexCoord1i
end interface

interface
subroutine glTexCoord1iv(v) bind(c,name='glTexCoord1iv')
integer(4), intent(in) :: v
end subroutine glTexCoord1iv
end interface

interface
subroutine glTexCoord1s(s) bind(c,name='glTexCoord1s')
integer(2), intent(in), value :: s
end subroutine glTexCoord1s
end interface

interface
subroutine glTexCoord1sv(v) bind(c,name='glTexCoord1sv')
integer(4), intent(in) :: v
end subroutine glTexCoord1sv
end interface

interface
subroutine glTexCoord2d(s,t) bind(c,name='glTexCoord2d')
real(8), intent(in), value :: s,t
end subroutine glTexCoord2d
end interface

interface
subroutine glTexCoord2dv(v) bind(c,name='glTexCoord2dv')
real(8), intent(in) :: v(2)
end subroutine glTexCoord2dv
end interface

interface
subroutine glTexCoord2f(s,t) bind(c,name='glTexCoord2f')
real(4), intent(in), value :: s,t
end subroutine glTexCoord2f
end interface

interface
subroutine glTexCoord2fv(v) bind(c,name='glTexCoord2fv')
real(4), intent(in) :: v(2)
end subroutine glTexCoord2fv
end interface

interface
subroutine glTexCoord2i(s,t) bind(c,name='glTexCoord2i')
integer(4), intent(in), value :: s,t
end subroutine glTexCoord2i
end interface

interface
subroutine glTexCoord2iv(v) bind(c,name='glTexCoord2iv')
integer(4), intent(in) :: v(2)
end subroutine glTexCoord2iv
end interface

interface
subroutine glTexCoord2s(s,t) bind(c,name='glTexCoord2s')
integer(2), intent(in), value :: s,t
end subroutine glTexCoord2s
end interface

interface
subroutine glTexCoord2sv(v) bind(c,name='glTexCoord2sv')
integer(2), intent(in) :: v(2)
end subroutine glTexCoord2sv
end interface

interface
subroutine glTexCoord3d(s,t,r) bind(c,name='glTexCoord3d')
real(8), intent(in), value :: s,t,r
end subroutine glTexCoord3d
end interface

interface
subroutine glTexCoord3dv(v) bind(c,name='glTexCoord3dv')
real(8), intent(in) :: v(3)
end subroutine glTexCoord3dv
end interface

interface
subroutine glTexCoord3f(s,t,r) bind(c,name='glTexCoord3f')
real(4), intent(in), value :: s,t,r
end subroutine glTexCoord3f
end interface

interface
subroutine glTexCoord3fv(v) bind(c,name='glTexCoord3fv')
real(4), intent(in) :: v(3)
end subroutine glTexCoord3fv
end interface

interface
subroutine glTexCoord3i(s,t,r) bind(c,name='glTexCoord3i')
integer(4), intent(in), value :: s,t,r
end subroutine glTexCoord3i
end interface

interface
subroutine glTexCoord3iv(v) bind(c,name='glTexCoord3iv')
integer(4), intent(in) :: v(3)
end subroutine glTexCoord3iv
end interface

interface
subroutine glTexCoord3s(s,t,r) bind(c,name='glTexCoord3s')
integer(2), intent(in), value :: s,t,r
end subroutine glTexCoord3s
end interface

interface
subroutine glTexCoord3sv(v) bind(c,name='glTexCoord3sv')
integer(2), intent(in) :: v(3)
end subroutine glTexCoord3sv
end interface

interface
subroutine glTexCoord4d(s,t,r,q) bind(c,name='glTexCoord4d')
real(8), intent(in), value :: s,t,r,q
end subroutine glTexCoord4d
end interface

interface
subroutine glTexCoord4dv(v) bind(c,name='glTexCoord4dv')
real(8), intent(in) :: v(4)
end subroutine glTexCoord4dv
end interface

interface
subroutine glTexCoord4f(s,t,r,q) bind(c,name='glTexCoord4f')
real(4), intent(in), value :: s,t,r,q
end subroutine glTexCoord4f
end interface

interface
subroutine glTexCoord4fv(v) bind(c,name='glTexCoord4fv')
real(4), intent(in) :: v(4)
end subroutine glTexCoord4fv
end interface

interface
subroutine glTexCoord4i(s,t,r,q) bind(c,name='glTexCoord4i')
integer(4), intent(in), value :: s,t,r,q
end subroutine glTexCoord4i
end interface

interface
subroutine glTexCoord4iv(v) bind(c,name='glTexCoord4iv')
integer(4), intent(in) :: v(4)
end subroutine glTexCoord4iv
end interface

interface
subroutine glTexCoord4s(s,t,r,q) bind(c,name='glTexCoord4s')
integer(2), intent(in), value :: s,t,r,q
end subroutine glTexCoord4s
end interface

interface
subroutine glTexCoord4sv(v) bind(c,name='glTexCoord4sv')
integer(2), intent(in) :: v(4)
end subroutine glTexCoord4sv
end interface

interface
subroutine glTexCoordPointer(size,type,stride,pointer) bind(c,name='glTexCoordPointer')
import
INTEGER(4), intent(in), value :: size,type,stride
integer(C_INTPTR_T), intent(in), value :: pointer
END SUBROUTINE glTexCoordPointer
end interface

interface
subroutine glTexEnvf(target,pname,param) bind(c,name='glTexEnvf')
integer(4), intent(in), value :: target,pname  
real(4), intent(in), value :: param
end subroutine glTexEnvf
end interface

interface
subroutine glTexEnvfv(target,pname,params) bind(c,name='glTexEnvfv')
integer(4), intent(in), value :: target,pname
integer(4) :: params(*)
end subroutine glTexEnvfv
end interface

interface
subroutine glTexEnvi(target,pname,param) bind(c,name='glTexEnvi')
integer(4), intent(in), value :: target,pname,param
end subroutine glTexEnvi
end interface

interface
subroutine glTexEnviv(target,pname,params) bind(c,name='glTexEnviv')
integer(4), intent(in), value :: target,pname
integer(4), intent(in) :: params(*)
end subroutine glTexEnviv
end interface

interface
subroutine glTexGend(coord,pname,param) bind(c,name='glTexGend')
integer(4), intent(in), value :: coord,pname  
real(8), intent(in), value :: param
end subroutine glTexGend
end interface

interface
subroutine glTexGendv(coord,pname,params) bind(c,name='glTexGendv')
integer(4), intent(in), value :: coord,pname  
real(8), intent(in) :: params(*)
end subroutine glTexGendv
end interface

interface
subroutine glTexGenf(coord,pname,param) bind(c,name='glTexGenf')
integer(4), intent(in), value :: coord,pname
real(4), intent(in), value :: param
end subroutine glTexGenf
end interface

interface
subroutine glTexGenfv(coord,pname,params) bind(c,name='glTexGenfv')
integer(4), intent(in), value :: coord,pname
real(4), intent(in) :: params(*)
end subroutine glTexGenfv
end interface

interface
subroutine glTexGeni(coord,pname,param) bind(c,name='glTexGeni')
integer(4), intent(in), value :: coord,pname,param
end subroutine glTexGeni
end interface

interface
subroutine glTexGeniv(coord,pname,params) bind(c,name='glTexGeniv')
integer(4), intent(in), value :: coord,pname
integer(4), intent(in) :: params(*)
end subroutine glTexGeniv
end interface

interface
subroutine glTexImage1D(target,level,components,width,border,format,type,pixels) bind(c,name='glTexImage1D')
import
integer(4), intent(in), value :: target,level,components,width,border,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
end subroutine glTexImage1d
end interface

interface
subroutine glTexImage2D(target,level,components,width,height,border,format,type,pixels) bind(c,name='glTexImage2D')
import
integer(4), intent(in), value :: target,level,components,width,height,border,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
end subroutine glTexImage2D
end interface

interface
subroutine glTexParameterf(target,pname,param) bind(c,name='glTexParameterf')
integer(4), intent(in), value :: target,pname
real(4), intent(in), value :: param
end subroutine glTexParameterf
end interface

interface
subroutine glTexParameterfv(target,pname,params) bind(c,name='glTexParameterfv')
integer(4), intent(in), value :: target,pname
real(4), intent(in) :: params(*)
end subroutine glTexParameterfv
end interface

interface
subroutine glTexParameteri(target,pname,param) bind(c,name='glTexParameteri')
integer(4), intent(in), value :: target,pname,param
end subroutine glTexParameteri
end interface

interface
subroutine glTexParameteriv(target,pname,params) bind(c,name='glTexParameteriv')
integer(4), intent(in), value :: target,pname
integer(4), intent(in) :: params(*)
end subroutine glTexParameteriv
end interface

interface
subroutine glTexSubImage1D(target,level,xoffset,width,format,type,pixels) bind(c,name='glTexSubImage1D')
import
INTEGER(4), intent(in), value :: target,level,xoffset,width,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
END SUBROUTINE glTexSubImage1D
end interface

interface
subroutine glTexSubImage2D(target,level,xoffset,yoffset,width,height,format,type,pixels) bind(c,name='glTexSubImage2D')
import
INTEGER(4), intent(in), value :: target,level,xoffset,yoffset,width, height,format,type
integer(C_INTPTR_T), intent(in), value :: pixels
END SUBROUTINE glTexSubImage2D
end interface

interface
subroutine glTranslated(x,y,z) bind(c,name='glTranslated')
real(8), intent(in), value :: x,y,z
end subroutine glTranslated
end interface

interface
subroutine glTranslatef(x,y,z) bind(c,name='glTranslatef')
real(4), intent(in), value :: x,y,z
end subroutine glTranslatef
end interface

interface
subroutine glVertex2d(x,y) bind(c,name='glVertex2d')
real(8), intent(in), value :: x,y
end subroutine glVertex2d
end interface

interface
subroutine glVertex2dv(v) bind(c,name='glVertex2dv')
real(8), intent(in) :: v(2)
end subroutine glVertex2dv
end interface

interface
subroutine glVertex2f(x,y) bind(c,name='glVertex2f')
real(4), intent(in), value :: x,y
end subroutine glVertex2f
end interface

interface
subroutine glVertex2fv(v) bind(c,name='glVertex2fv')
real(4), intent(in) :: v(2)
end subroutine glVertex2fv
end interface

interface
subroutine glVertex2i(x,y) bind(c,name='glVertex2i')
integer(4), intent(in), value :: x,y
end subroutine glVertex2i
end interface

interface
subroutine glVertex2iv(v) bind(c,name='glVertex2iv')
integer(4), intent(in) :: v(2)
end subroutine glVertex2iv
end interface

interface
subroutine glVertex2s(x,y) bind(c,name='glVertex2s')
integer(2), intent(in), value :: x,y
end subroutine glVertex2s
end interface

interface
subroutine glVertex2sv(v) bind(c,name='glVertex2sv')
integer(2), intent(in) :: v(2)
end subroutine glVertex2sv
end interface

interface
subroutine glVertex3d(x,y,z) bind(c,name='glVertex3d')
real(8), intent(in), value :: x,y,z
end subroutine glVertex3d
end interface

interface
subroutine glVertex3dv(v) bind(c,name='glVertex3dv')
real(8), intent(in) :: v(3)
end subroutine glVertex3dv
end interface

interface
subroutine glVertex3f(x,y,z) bind(c,name='glVertex3f')
real(4), intent(in), value :: x,y,z
end subroutine glVertex3f
end interface

interface
subroutine glVertex3fv(v) bind(c,name='glVertex3fv')
real(4), intent(in) :: v(3)
end subroutine glVertex3fv
end interface

interface
subroutine glVertex3i(x,y,z) bind(c,name='glVertex3i')
integer(4), intent(in), value :: x,y,z
end subroutine glVertex3i
end interface

interface
subroutine glVertex3iv(v) bind(c,name='glVertex3iv')
integer(4), intent(in) :: v(3)
end subroutine glVertex3iv
end interface

interface
subroutine glVertex3s(x,y,z) bind(c,name='glVertex3s')
integer(2), intent(in), value :: x,y,z
end subroutine glVertex3s
end interface

interface
subroutine glVertex3sv(v) bind(c,name='glVertex3sv')
integer(2), intent(in) :: v(3)
end subroutine glVertex3sv
end interface

interface
subroutine glVertex4d(x,y,z,w) bind(c,name='glVertex4d')
real(8), intent(in), value :: x,y,z,w
end subroutine glVertex4d
end interface

interface
subroutine glVertex4dv(v) bind(c,name='glVertex4dv')
real(8), intent(in) :: v(4)
end subroutine glVertex4dv
end interface

interface
subroutine glVertex4f(x,y,z,w) bind(c,name='glVertex4f')
real(4), intent(in), value :: x,y,z,w
end subroutine glVertex4f
end interface

interface
subroutine glVertex4fv(v) bind(c,name='glVertex4fv')
real(4), intent(in) :: v(4)
end subroutine glVertex4fv
end interface

interface
subroutine glVertex4i(x,y,z,w) bind(c,name='glVertex4i')
integer(4), intent(in), value :: x,y,z,w
end subroutine glVertex4i
end interface

interface
subroutine glVertex4iv(v) bind(c,name='glVertex4iv')
integer(4), intent(in) :: v(4)
end subroutine glVertex4iv
end interface

interface
subroutine glVertex4s(x,y,z,w) bind(c,name='glVertex4s')
integer(2), intent(in), value :: x,y,z,w
end subroutine glVertex4s
end interface

interface
subroutine glVertex4sv(v) bind(c,name='glVertex4sv')
integer(2), intent(in) :: v(4)
end subroutine glVertex4sv
end interface

interface
subroutine glVertexPointer(size,type,stride,pointer) bind(c,name='glVertexPointer')
import
INTEGER(4), intent(in), value :: size,type,stride
integer(C_INTPTR_T), intent(in), value :: pointer
END SUBROUTINE glVertexPointer
end interface

interface
subroutine glViewport(x,y,width,height) bind(c,name='glViewport')
integer(4), intent(in), value :: x,y,width,height
end subroutine glViewport
end interface
!------------------------------- GLU ------------------------------------------
!interface
!function gluErrorString(errCode) bind(c,name='gluErrorString')
!character(100), pointer :: gluErrorString
!integer(4), intent(in), value :: errCode
!end function gluErrorString
!end interface

interface
subroutine gluOrtho2D(left,right,bottom,top) bind(c,name='gluOrtho2D')
real(8), intent(in), value :: left,right,bottom,top
end subroutine gluOrtho2D
end interface

interface
subroutine gluPerspective(fovy,aspect,zNear,zFar) bind(c,name='gluPerspective')
real(8),intent(in), value :: fovy,aspect,zNear,zFar
end subroutine gluPerspective
end interface

interface
subroutine gluPickMatrix(x,y,width,height,viewport) bind(c,name='gluPickMatrix')
real(8), intent(in), value :: x,y,width,height
integer(4), intent(in) :: viewport(4) 
end subroutine gluPickMatrix
end interface

interface
subroutine gluLookAt(eyex,eyey,eyez,centerx,centery,centerz,upx,upy,upz) bind(c,name='gluLookAt')
real(8), intent(in), value :: eyex,eyey,eyez,centerx,centery,centerz,upx,upy,upz
end subroutine gluLookAt
end interface

interface
integer(4) function gluProject(objx,objy,objz,modelMatrix,projMatrix,viewport,winx,winy,winz) bind(c,name='gluProject')
real(8), intent(in), value :: objx,objy,objz
real(8), intent(in) :: winx,winy,winz,modelMatrix(16),projMatrix(16)
integer(4), intent(in) :: viewport(4) 
end function gluProject
end interface

interface
integer(4) function gluUnProject(winx,winy,winz,modelMatrix,projMatrix,viewport,objx,objy,objz) bind(c,name='gluUnProject')
real(8), intent(in), value :: objx,objy,objz
real(8) :: winx,winy,winz,modelMatrix(16),projMatrix(16)
integer(4), intent(in) :: viewport(4) 
end function gluUnProject
end interface

interface
integer(4) function gluScaleImage(format,win,hin,typein,datain,wout,hout,typeout,dataout) bind(c,name='gluScaleImage')
integer(4), intent(in), value :: format,win,hin,typein,datain,wout,hout,typeout,dataout
end function gluScaleImage
end interface

interface
integer(4) function gluBuild1DMipmaps(target,components,width,format,type,data) bind(c,name='gluBuild1DMipmaps')
import
integer(4), intent(in), value :: target,components,width,format,type
integer(C_INTPTR_T), intent(in), value :: data
end function gluBuild1DMipmaps
end interface

interface
integer(4) function gluBuild2DMipmaps(target,components,width,height,format,type,data) bind(c,name='gluBuild2DMipmaps')
import
integer(4), intent(in), value :: target,components,width,height,format,type
integer(C_INTPTR_T), intent(in), value :: data
end function gluBuild2DMipmaps
end interface

interface
function gluNewQuadric() bind(c,name='gluNewQuadric')
import
integer(C_INTPTR_T) :: gluNewQuadric
end function gluNewQuadric
end interface

interface
subroutine gluDeleteQuadric(state) bind(c,name='gluDeleteQuadric')
import
integer(C_INTPTR_T), intent(in), value :: state 
end subroutine gluDeleteQuadric
end interface

interface
subroutine gluQuadricNormals(quadObject,normals) bind(c,name='gluQuadricNormals')
import
integer(C_INTPTR_T), intent(in), value :: quadObject
integer(4), intent(in), value :: normals
end subroutine gluQuadricNormals
end interface

interface
subroutine gluQuadricTexture(quadObject,textureCoords) bind(c,name='gluQuadricTexture')
import
integer(C_INTPTR_T), intent(in), value :: quadObject 
integer(1), intent(in) :: textureCoords(*)
end subroutine gluQuadricTexture
end interface

interface
subroutine gluQuadricOrientation(quadObject,orientation) bind(c,name='gluQuadricOrientation')
import
integer(C_INTPTR_T), intent(in), value :: quadObject 
integer(4), intent(in), value :: orientation 
end subroutine gluQuadricOrientation
end interface

interface
subroutine gluQuadricDrawStyle(quadObject,drawStyle) bind(c,name='gluQuadricDrawStyle')
import
integer(C_INTPTR_T), intent(in), value :: quadObjecT
integer(4), intent(in), value :: drawStyle 
end subroutine gluQuadricDrawStyle
end interface

interface
subroutine gluCylinder(qobj,baseRadius,topRadius,height,slices,stacks) bind(c,name='gluCylinder')
import
integer(C_INTPTR_T), intent(in), value :: qobj
integer(4), intent(in), value :: slices,stacks
real(8), intent(in), value :: baseRadius,topRadius,height
end subroutine gluCylinder
end interface

interface
subroutine gluDisk(qobj,innerRadius,outerRadius,slices,loops) bind(c,name='gluDisk')
import
integer(C_INTPTR_T), intent(in), value :: qobj
integer(4), intent(in), value :: slices,loops
real(8), intent(in), value :: innerRadius,outerRadius
end subroutine gluDisk
end interface

interface
subroutine gluPartialDisk(qobj,innerRad,outerRad,slices,loops,startAngle,sweepAngle) bind(c,name='gluPartialDisk')
import
integer(C_INTPTR_T), intent(in), value :: qobj
integer(4), intent(in), value :: slices,loops
real(8), intent(in), value :: innerRad,outerRad,startAngle,sweepAngle
end subroutine gluPartialDisk
end interface

interface
subroutine gluSphere(qobj,radius,slices,stacks) bind(c,name='gluSphere')
import
integer(C_INTPTR_T), intent(in), value :: qobj
integer(4), intent(in), value :: slices,stacks
real(8), intent(in), value :: radius
end subroutine gluSphere
end interface

!interface
!subroutine gluQuadricCallback(qobj,which,fun ) bind(c,name='gluQuadricCallback')
!import
!integer(C_INTPTR_T), intent(in), value :: qobj
!integer(4), intent(in), value :: which
!external fun
!end subroutine gluQuadricCallback
!end interface

interface
function gluNewTess() bind(c,name='gluNewTess')
 import
 integer(C_INTPTR_T) :: gluNewTess
end function gluNewTess
end interface

!interface
!subroutine gluTessCallback(tobj,which,fun ) bind(c,name='gluTessCallback')
!import
!integer(C_INTPTR_T), intent(in), value :: tobj
!INTEGER(4), INTENT(IN), value :: which
!external fun
!END SUBROUTINE gluTessCallback
!end interface

interface
subroutine gluDeleteTess(tobj) bind(c,name='gluDeleteTess')
import
integer(C_INTPTR_T), intent(in), value :: tobj 
end subroutine gluDeleteTess
end interface

interface
subroutine gluBeginPolygon(tobj) bind(c,name='gluBeginPolygon')
import
integer(C_INTPTR_T), intent(in), value :: tobj 
end subroutine gluBeginPolygon
end interface

interface
subroutine gluEndPolygon(tobj) bind(c,name='gluEndPolygon')
import
integer(C_INTPTR_T), intent(in), value :: tobj
end subroutine gluEndPolygon
end interface

interface
subroutine gluNextContour(tobj,type) bind(c,name='gluNextContour')
import
integer(C_INTPTR_T), intent(in), value :: tobj
integer(4), intent(in), value :: type
end subroutine gluNextContour
end interface

interface
subroutine gluTessVertex(tess,coords,data) bind(c,name='gluTessVertex')
import
integer(C_INTPTR_T), intent(in), value :: tess
real(8), intent(in) :: coords(3)
real(4), intent(in) :: data(*)
end subroutine gluTessVertex
end interface

interface
function gluNewNurbsRenderer() bind(c,name='gluNewNurbsRenderer')
import
integer(C_INTPTR_T) :: gluNewNurbsRenderer
end function gluNewNurbsRenderer
end interface

interface
subroutine gluDeleteNurbsRenderer(nobj) bind(c,name='gluDeleteNurbsRenderer')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluDeleteNurbsRenderer
end interface

interface
subroutine gluBeginSurface(nobj) bind(c,name='gluBeginSurface')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluBeginSurface
end interface

interface
subroutine gluBeginCurve(nobj) bind(c,name='gluBeginCurve')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluBeginCurve
end interface

interface
subroutine gluEndCurve(nobj) bind(c,name='gluEndCurve')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluEndCurve
end interface

interface
subroutine gluEndSurface(nobj) bind(c,name='gluEndSurface')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluEndSurface
end interface

interface
subroutine gluBeginTrim(nobj) bind(c,name='gluBeginTrim')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluBeginTrim
end interface

interface
subroutine gluEndTrim(nobj) bind(c,name='gluEndTrim')
import
integer(C_INTPTR_T), intent(in), value :: nobj
end subroutine gluEndTrim
end interface

interface
subroutine gluPwlCurve(nobj,count,array,stride,type) bind(c,name='gluPwlCurve')
import
integer(C_INTPTR_T), intent(in), value :: nobj
integer(4), intent(in), value :: count,stride,type
real(4) :: array(*)
end subroutine gluPwlCurve
end interface

interface
subroutine gluNurbsCurve(nobj,nknots,knot,stride,ctlarray,order,type) bind(c,name='gluNurbsCurve')
import
integer(C_INTPTR_T), intent(in), value :: nobj
integer(4), intent(in), value :: nknots, stride, order,type
real(4) :: knot(*),ctlarray(*)
end subroutine gluNurbsCurve
end interface

interface
subroutine gluNurbsSurface(nobj,sknot_c,sknot,tknot_c,tknot,s_s,t_s,ctlarray,sorder,torder,type) bind(c,name='gluNurbsSurface')
import
integer(C_INTPTR_T), intent(in), value :: nobj
integer(4), intent(in), value :: sknot_c,tknot_c,s_s,t_s,sorder,torder,type
real(4), dimension(*) :: sknot,tknot,ctlarray
end subroutine gluNurbsSurface
end interface

interface
subroutine gluLoadSamplingMatrices(nobj,modelMatrix,projMatrix,viewport) bind(c,name='gluLoadSamplingMatrices')
import
integer(C_INTPTR_T), intent(in), value :: nobj 
real(4), intent(in) :: modelMatrix(16),projMatrix(16) 
integer(4), intent(in) :: viewport(4) 
end subroutine gluLoadSamplingMatrices
end interface

interface
subroutine gluNurbsProperty(nobj,property,value) bind(c,name='gluNurbsProperty')
import
integer(C_INTPTR_T), intent(in), value :: nobj
integer(4), intent(in), value :: property
real(4), intent(in), value :: value
end subroutine gluNurbsProperty
end interface

interface
subroutine gluGetNurbsProperty(nobj,property,value) bind(c,name='gluGetNurbsProperty')
import
integer(C_INTPTR_T), intent(in), value :: nobj
integer(4), intent(in), value :: property 
real(4),intent(in out) :: value 
end subroutine gluGetNurbsProperty
end interface

!interface
!subroutine gluNurbsCallback(nobj,which,fun ) bind(c,name='gluNurbsCallback')
!import
!integer(C_INTPTR_T), intent(in), value :: nobj
!integer(4), intent(in), value :: which 
!external fun
!end subroutine gluNurbsCallback
!end interface

!interface
!function gluGetString(name) bind(c,name='gluGetString')
!character(100), pointer :: gluGetString
!integer(4),intent(in) :: name
!end function gluGetString
!end interface

interface
subroutine gluTessBeginPolygon(tess,polygon_data) bind(c,name='gluTessBeginPolygon')
 import
 integer(C_INTPTR_T), intent(in), value :: tess
 INTEGER(4), intent(in), value :: polygon_data
 END SUBROUTINE gluTessBeginPolygon
end interface

interface
subroutine gluTessBeginContour(tess) bind(c,name='gluTessBeginContour')
import
 integer(C_INTPTR_T), intent(in), value :: tess
 END SUBROUTINE gluTessBeginContour
end interface

interface
subroutine gluTessEndContour(tess) bind(c,name='gluTessEndContour')
import
 integer(C_INTPTR_T), intent(in), value :: tess
 END SUBROUTINE gluTessEndContour
end interface

interface
subroutine gluTessEndPolygon(tess) bind(c,name='gluTessEndPolygon')
import
 integer(C_INTPTR_T), intent(in), value :: tess
 END SUBROUTINE gluTessEndPolygon
end interface

interface
subroutine gluTessProperty(tess,which,value) bind(c,name='gluTessProperty')
import
 integer(C_INTPTR_T), intent(in), value :: tess
 INTEGER(4), intent(in), value :: which
 REAL(8) :: value
 END SUBROUTINE gluTessProperty
end interface

interface
subroutine gluTessNormal(tess,x,y,z) bind(c,name='gluTessNormal')
import
 integer(C_INTPTR_T), intent(in), value :: tess 
 REAL(8), intent(in), value :: x,y,z
end subroutine gluTessNormal
end interface

interface
subroutine gluGetTessProperty(tess, which,value) bind(c,name='gluGetTessProperty')
 import
 integer(C_INTPTR_T), intent(in), value :: tess
 INTEGER(4), intent(in), value :: which
 REAL(8),intent(in out) :: value    
 END SUBROUTINE gluGetTessProperty
end interface
!------------------------------- GLUT --------------------------------
integer(4),parameter,public :: GLUT_API_VERSION=3
integer(4),parameter,public :: GLUT_XLIB_IMPLEMENTATION=15
integer(4),parameter,public :: GLUT_RGB=0
integer(4),parameter,public :: GLUT_INDEX=1
integer(4),parameter,public :: GLUT_SINGLE=0
integer(4),parameter,public :: GLUT_DOUBLE=2
integer(4),parameter,public :: GLUT_ACCUM=4
integer(4),parameter,public :: GLUT_ALPHA=8
integer(4),parameter,public :: GLUT_DEPTH=16
integer(4),parameter,public :: GLUT_STENCIL=32
integer(4),parameter,public :: GLUT_MULTISAMPLE=128
integer(4),parameter,public :: GLUT_STEREO=256
integer(4),parameter,public :: GLUT_LUMINANCE=512
integer(4),parameter,public :: GLUT_LEFT_BUTTON=0
integer(4),parameter,public :: GLUT_MIDDLE_BUTTON=1
integer(4),parameter,public :: GLUT_RIGHT_BUTTON=2
integer(4),parameter,public :: GLUT_DOWN=0
integer(4),parameter,public :: GLUT_UP=1
integer(4),parameter,public :: GLUT_KEY_F1=1
integer(4),parameter,public :: GLUT_KEY_F2=2
integer(4),parameter,public :: GLUT_KEY_F3=3
integer(4),parameter,public :: GLUT_KEY_F4=4
integer(4),parameter,public :: GLUT_KEY_F5=5
integer(4),parameter,public :: GLUT_KEY_F6=6
integer(4),parameter,public :: GLUT_KEY_F7=7
integer(4),parameter,public :: GLUT_KEY_F8=8
integer(4),parameter,public :: GLUT_KEY_F9=9
integer(4),parameter,public :: GLUT_KEY_F10=10
integer(4),parameter,public :: GLUT_KEY_F11=11
integer(4),parameter,public :: GLUT_KEY_F12=12
integer(4),parameter,public :: GLUT_KEY_LEFT=100
integer(4),parameter,public :: GLUT_KEY_UP=101
integer(4),parameter,public :: GLUT_KEY_RIGHT=102
integer(4),parameter,public :: GLUT_KEY_DOWN=103
integer(4),parameter,public :: GLUT_KEY_PAGE_UP=104
integer(4),parameter,public :: GLUT_KEY_PAGE_DOWN=105
integer(4),parameter,public :: GLUT_KEY_HOME=106
integer(4),parameter,public :: GLUT_KEY_END=107
integer(4),parameter,public :: GLUT_KEY_INSERT=108
integer(4),parameter,public :: GLUT_LEFT=0
integer(4),parameter,public :: GLUT_ENTERED=1
integer(4),parameter,public :: GLUT_MENU_NOT_IN_USE=0
integer(4),parameter,public :: GLUT_MENU_IN_USE=1
integer(4),parameter,public :: GLUT_NOT_VISIBLE=0
integer(4),parameter,public :: GLUT_VISIBLE=1
integer(4),parameter,public :: GLUT_HIDDEN=0
integer(4),parameter,public :: GLUT_FULLY_RETAINED=1
integer(4),parameter,public :: GLUT_PARTIALLY_RETAINED=2
integer(4),parameter,public :: GLUT_FULLY_COVERED=3
integer(4),parameter,public :: GLUT_RED=0
integer(4),parameter,public :: GLUT_GREEN=1
integer(4),parameter,public :: GLUT_BLUE=2
integer(4),parameter,public :: GLUT_WINDOW_X=100
integer(4),parameter,public :: GLUT_WINDOW_Y=101
integer(4),parameter,public :: GLUT_WINDOW_WIDTH=102
integer(4),parameter,public :: GLUT_WINDOW_HEIGHT=103
integer(4),parameter,public :: GLUT_WINDOW_BUFFER_SIZE=104
integer(4),parameter,public :: GLUT_WINDOW_STENCIL_SIZE=105
integer(4),parameter,public :: GLUT_WINDOW_DEPTH_SIZE=106
integer(4),parameter,public :: GLUT_WINDOW_RED_SIZE=107
integer(4),parameter,public :: GLUT_WINDOW_GREEN_SIZE=108
integer(4),parameter,public :: GLUT_WINDOW_BLUE_SIZE=109
integer(4),parameter,public :: GLUT_WINDOW_ALPHA_SIZE=110
integer(4),parameter,public :: GLUT_WINDOW_ACCUM_RED_SIZE=111
integer(4),parameter,public :: GLUT_WINDOW_ACCUM_GREEN_SIZE=112
integer(4),parameter,public :: GLUT_WINDOW_ACCUM_BLUE_SIZE=113
integer(4),parameter,public :: GLUT_WINDOW_ACCUM_ALPHA_SIZE=114
integer(4),parameter,public :: GLUT_WINDOW_DOUBLEBUFFER=115
integer(4),parameter,public :: GLUT_WINDOW_RGBA=116
integer(4),parameter,public :: GLUT_WINDOW_PARENT=117
integer(4),parameter,public :: GLUT_WINDOW_NUM_CHILDREN=118
integer(4),parameter,public :: GLUT_WINDOW_COLORMAP_SIZE=119
integer(4),parameter,public :: GLUT_WINDOW_NUM_SAMPLES=120
integer(4),parameter,public :: GLUT_WINDOW_STEREO=121
integer(4),parameter,public :: GLUT_WINDOW_CURSOR=122
integer(4),parameter,public :: GLUT_SCREEN_WIDTH=200
integer(4),parameter,public :: GLUT_SCREEN_HEIGHT=201
integer(4),parameter,public :: GLUT_SCREEN_WIDTH_MM=202
integer(4),parameter,public :: GLUT_SCREEN_HEIGHT_MM=203
integer(4),parameter,public :: GLUT_MENU_NUM_ITEMS=300
integer(4),parameter,public :: GLUT_DISPLAY_MODE_POSSIBLE=400
integer(4),parameter,public :: GLUT_INIT_WINDOW_X=500
integer(4),parameter,public :: GLUT_INIT_WINDOW_Y=501
integer(4),parameter,public :: GLUT_INIT_WINDOW_WIDTH=502
integer(4),parameter,public :: GLUT_INIT_WINDOW_HEIGHT=503
integer(4),parameter,public :: GLUT_INIT_DISPLAY_MODE=504
integer(4),parameter,public :: GLUT_ELAPSED_TIME=700
integer(4),parameter,public :: GLUT_WINDOW_FORMAT_ID=123
integer(4),parameter,public :: GLUT_HAS_KEYBOARD=600
integer(4),parameter,public :: GLUT_HAS_MOUSE=601
integer(4),parameter,public :: GLUT_HAS_SPACEBALL=602
integer(4),parameter,public :: GLUT_HAS_DIAL_AND_BUTTON_BOX=603
integer(4),parameter,public :: GLUT_HAS_TABLET=604
integer(4),parameter,public :: GLUT_NUM_MOUSE_BUTTONS=605
integer(4),parameter,public :: GLUT_NUM_SPACEBALL_BUTTONS=606
integer(4),parameter,public :: GLUT_NUM_BUTTON_BOX_BUTTONS=607
integer(4),parameter,public :: GLUT_NUM_DIALS=608
integer(4),parameter,public :: GLUT_NUM_TABLET_BUTTONS=609
integer(4),parameter,public :: GLUT_DEVICE_IGNORE_KEY_REPEAT=610
integer(4),parameter,public :: GLUT_DEVICE_KEY_REPEAT=611
integer(4),parameter,public :: GLUT_HAS_JOYSTICK=612
integer(4),parameter,public :: GLUT_OWNS_JOYSTICK=613
integer(4),parameter,public :: GLUT_JOYSTICK_BUTTONS=614
integer(4),parameter,public :: GLUT_JOYSTICK_AXES=615
integer(4),parameter,public :: GLUT_JOYSTICK_POLL_RATE=616
integer(4),parameter,public :: GLUT_OVERLAY_POSSIBLE=800
integer(4),parameter,public :: GLUT_LAYER_IN_USE=801
integer(4),parameter,public :: GLUT_HAS_OVERLAY=802
integer(4),parameter,public :: GLUT_TRANSPARENT_INDEX=803
integer(4),parameter,public :: GLUT_NORMAL_DAMAGED=804
integer(4),parameter,public :: GLUT_OVERLAY_DAMAGED=805
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_POSSIBLE=900
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_IN_USE=901
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_X_DELTA=902
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_Y_DELTA=903
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_WIDTH_DELTA=904
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_HEIGHT_DELTA=905
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_X=906
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_Y=907
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_WIDTH=908
integer(4),parameter,public :: GLUT_VIDEO_RESIZE_HEIGHT=909
integer(4),parameter,public :: GLUexternal=0
integer(4),parameter,public :: GLUT_OVERLAY=1
integer(4),parameter,public :: GLUT_ACTIVE_SHIFT=1
integer(4),parameter,public :: GLUT_ACTIVE_CTRL=2
integer(4),parameter,public :: GLUT_ACTIVE_ALT=4
integer(4),parameter,public :: GLUT_CURSOR_RIGHT_ARROW=0
integer(4),parameter,public :: GLUT_CURSOR_LEFT_ARROW=1
integer(4),parameter,public :: GLUT_CURSOR_INFO=2
integer(4),parameter,public :: GLUT_CURSOR_DESTROY=3
integer(4),parameter,public :: GLUT_CURSOR_HELP=4
integer(4),parameter,public :: GLUT_CURSOR_CYCLE=5
integer(4),parameter,public :: GLUT_CURSOR_SPRAY=6
integer(4),parameter,public :: GLUT_CURSOR_WAIT=7
integer(4),parameter,public :: GLUT_CURSOR_TEXT=8
integer(4),parameter,public :: GLUT_CURSOR_CROSSHAIR=9
integer(4),parameter,public :: GLUT_CURSOR_UP_DOWN=10
integer(4),parameter,public :: GLUT_CURSOR_LEFT_RIGHT=11
integer(4),parameter,public :: GLUT_CURSOR_TOP_SIDE=12
integer(4),parameter,public :: GLUT_CURSOR_BOTTOM_SIDE=13
integer(4),parameter,public :: GLUT_CURSOR_LEFT_SIDE=14
integer(4),parameter,public :: GLUT_CURSOR_RIGHT_SIDE=15
integer(4),parameter,public :: GLUT_CURSOR_TOP_LEFT_CORNER=16
integer(4),parameter,public :: GLUT_CURSOR_TOP_RIGHT_CORNER=17
integer(4),parameter,public :: GLUT_CURSOR_BOTTOM_RIGHT_CORNER=18
integer(4),parameter,public :: GLUT_CURSOR_BOTTOM_LEFT_CORNER=19
integer(4),parameter,public :: GLUT_CURSOR_INHERIT=100
integer(4),parameter,public :: GLUT_CURSOR_NONE=101
integer(4),parameter,public :: GLUT_CURSOR_FULL_CROSSHAIR=102
integer(4),parameter,public :: GLUT_KEY_REPEAT_OFF=0
integer(4),parameter,public :: GLUT_KEY_REPEAT_ON=1
integer(4),parameter,public :: GLUT_KEY_REPEAT_DEFAULT=2
integer(4),parameter,public :: GLUT_JOYSTICK_BUTTON_A=1
integer(4),parameter,public :: GLUT_JOYSTICK_BUTTON_B=2
integer(4),parameter,public :: GLUT_JOYSTICK_BUTTON_C=4
integer(4),parameter,public :: GLUT_JOYSTICK_BUTTON_D=8
integer(4),parameter,public :: GLUT_GAME_MODE_ACTIVE=0
integer(4),parameter,public :: GLUT_GAME_MODE_POSSIBLE=1
integer(4),parameter,public :: GLUT_GAME_MODE_WIDTH=2
integer(4),parameter,public :: GLUT_GAME_MODE_HEIGHT=3
integer(4),parameter,public :: GLUT_GAME_MODE_PIXEL_DEPTH=4
integer(4),parameter,public :: GLUT_GAME_MODE_REFRESH_RATE=5
integer(4),parameter,public :: GLUT_GAME_MODE_DISPLAY_CHANGED=6

integer(4), parameter :: GLUT_ACTION_EXIT=0
integer(4), parameter :: GLUT_ACTION_GLUTMAINLOOP_RETURNS=1
integer(4), parameter :: GLUT_ACTION_CONTINUE_EXECUTION=2
integer(4), parameter :: GLUT_CREATE_NEW_CONTEXT=0
integer(4), parameter :: GLUT_USE_CURRENT_CONTEXT=1
integer(4), parameter :: GLUT_FORCE_INDIRECT_CONTEXT=0
integer(4), parameter :: GLUT_ALLOW_DIRECT_CONTEXT=1
integer(4), parameter :: GLUT_TRY_DIRECT_CONTEXT=2
integer(4), parameter :: GLUT_FORCE_DIRECT_CONTEXT=3
integer(4), parameter :: GLUT_ACTION_ON_WINDOW_CLOSE=z'01F9'
integer(4), parameter :: GLUT_WINDOW_BORDER_WIDTH=z'01FA'
integer(4), parameter :: GLUT_WINDOW_HEADER_HEIGHT=z'01FB'
integer(4), parameter :: GLUT_VERSION=z'01FC'
integer(4), parameter :: GLUT_RENDERING_CONTEXT =z'01FD'
integer(4), parameter :: GLUT_DIRECT_RENDERING=z'01FE'
integer(4), parameter :: GLUT_AUX1=z'1000'
integer(4), parameter :: GLUT_AUX2=z'2000'
integer(4), parameter :: GLUT_AUX3=z'4000'
integer(4), parameter :: GLUT_AUX4=z'8000'

!interface
! subroutine GLUT_STROKE_ROMAN() bind(c,name='glutStrokeRoman')
! end subroutine GLUT_STROKE_ROMAN
!end interface

!interface
! subroutine GLUT_STROKE_MONO_ROMAN() bind(c,name='glutStrokeMonoRoman')
! end subroutine GLUT_STROKE_MONO_ROMAN
!end interface

interface
 subroutine GLUT_BITMAP_9_BY_15() bind(c,name='glutBitmap9By15')
 end subroutine GLUT_BITMAP_9_BY_15
end interface

interface
 subroutine GLUT_BITMAP_8_BY_13() bind(c,name='glutBitmap8By13')
 end subroutine GLUT_BITMAP_8_BY_13
end interface

interface
 subroutine GLUT_BITMAP_TIMES_ROMAN_10() bind(c,name='glutBitmapTimesRoman10')
 end subroutine GLUT_BITMAP_TIMES_ROMAN_10
end interface

interface
 subroutine GLUT_BITMAP_TIMES_ROMAN_24() bind(c,name='glutBitmapTimesRoman24')
 end subroutine GLUT_BITMAP_TIMES_ROMAN_24
end interface

interface
 subroutine GLUT_BITMAP_HELVETICA_10() bind(c,name='glutBitmapHelvetica10')
 end subroutine GLUT_BITMAP_HELVETICA_10
end interface

interface
 subroutine GLUT_BITMAP_HELVETICA_12() bind(c,name='glutBitmapHelvetica12')
 end subroutine GLUT_BITMAP_HELVETICA_12
end interface

interface
 subroutine GLUT_BITMAP_HELVETICA_18() bind(c,name='glutBitmapHelvetica18')
 end subroutine GLUT_BITMAP_HELVETICA_18
end interface

interface
 subroutine glutInit(argc,argv) bind(c,name='glutInit')
  import
  integer(4), intent(in) :: argc
  integer(C_INTPTR_T), intent(in) :: argv
 end subroutine glutInit
end interface

interface
 subroutine glutInitDisplayMode(mode) bind(c,name='glutInitDisplayMode')
  integer(4), intent(in), value :: mode
 end subroutine glutInitDisplayMode
end interface

interface
 subroutine glutInitDisplayString(str) bind(c,name='glutInitDisplayString')
 use iso_c_binding, only : c_char
 character(kind=c_char), intent(in) :: str(*)
 end subroutine glutInitDisplayString
end interface

interface
 integer function glutCreateWindow(title) bind(c,name='glutCreateWindow')
  use iso_c_binding, only: c_char
  character(kind=c_char),intent(in) :: title(*)
 end function glutCreateWindow
end interface

interface
 subroutine glutDisplayFunc(fun) bind(c,name='glutDisplayFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutDisplayFunc
end interface

interface
 subroutine glutCloseFunc(fun) bind(c,name='glutCloseFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutCloseFunc
end interface

interface
 subroutine glutWMCloseFunc(fun) bind(c,name='glutWMCloseFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutWMCloseFunc
end interface

interface
 subroutine glutMenuDestroyFunc(fun) bind(c,name='glutMenuDestroyFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutMenuDestroyFunc
end interface

interface
 subroutine glutMouseWheelFunc(fun) bind(c,name='glutMouseWheelFunc')
  interface
   subroutine fun(i1,i2,i3,i4) bind(c)
    integer(4), intent(in), value :: i1,i2,i3,i4
   end subroutine fun
  end interface
 end subroutine glutMouseWheelFunc
end interface

interface
 subroutine glutReshapeFunc(fun) bind(c,name='glutReshapeFunc')
  interface
   subroutine fun(w,h) bind(C)
    import
    integer(GLint), intent(in), value :: w,h
   end subroutine fun
  end interface
 end subroutine glutReshapeFunc
end interface

interface
 subroutine glutTimerFunc(millis,fun,value) bind(c,name='glutTimerFunc')
  integer(4), intent(in), value :: millis,value
  interface
   subroutine fun(t) bind(C)
    import
    integer(GLint), intent(in), value :: t
   end subroutine fun
  end interface
 end subroutine glutTimerFunc
end interface

interface
 subroutine glutMainLoop() bind(c,name='glutMainLoop')
 end subroutine glutMainLoop
end interface

interface
 subroutine glutInitWindowSize(w,h) bind(c,name='glutInitWindowSize')
  integer(4), intent(in), value :: w,h
 end subroutine glutInitWindowSize
end interface

interface
 subroutine glutInitWindowPosition(x,y) bind(c,name='glutInitWindowPosition')
  integer(4), intent(in), value :: x,y
 end subroutine glutInitWindowPosition
end interface

interface
 subroutine glutSolidDodecahedron() bind(c,name='glutSolidDodecahedron')
 end subroutine glutSolidDodecahedron
end interface

interface
 subroutine glutSolidRhombicDodecahedron() bind(c,name='glutSolidRhombicDodecahedron')
 end subroutine glutSolidRhombicDodecahedron
end interface

interface
 subroutine glutWireDodecahedron() bind(c,name='glutWireDodecahedron')
 end subroutine glutWireDodecahedron
end interface

interface
 subroutine glutWireRhombicDodecahedron() bind(c,name='glutWireRhombicDodecahedron')
 end subroutine glutWireRhombicDodecahedron
end interface

interface
 subroutine glutSolidOctahedron() bind(c,name='glutSolidOctahedron')
 end subroutine glutSolidOctahedron
end interface

interface
 subroutine glutWireOctahedron() bind(c,name='glutWireOctahedron')
 end subroutine glutWireOctahedron
end interface

interface
 subroutine glutWireTeapot(size) bind(c,name='glutWireTeapot')
  real(8), intent(in), value :: size
 end subroutine glutWireTeapot
end interface

interface
 subroutine glutSolidTeapot(size) bind(c,name='glutSolidTeapot')
  real(8), intent(in), value :: size
 end subroutine glutSolidTeapot
end interface

interface
 subroutine glutWireCube(size) bind(c,name='glutWireCube')
  real(8), intent(in), value :: size
 end subroutine glutWireCube
end interface

interface
 subroutine glutSolidCube(size) bind(c,name='glutSolidCube')
  real(8), intent(in), value :: size
 end subroutine glutSolidCube
end interface

interface
 integer(4) function glutEnterGameMode() bind(c,name='glutEnterGameMode')
 end function glutEnterGameMode
end interface

interface
 subroutine glutLeaveGameMode() bind(c,name='glutLeaveGameMode')
 end subroutine glutLeaveGameMode
end interface

interface
 subroutine glutForceJoystickFunc() bind(c,name='glutForceJoystickFunc')
 end subroutine glutForceJoystickFunc
end interface

interface
 subroutine glutPostRedisplay() bind(c,name='glutPostRedisplay')
 end subroutine glutPostRedisplay
end interface

interface
 integer(4) function glutCreateMenu(fun) bind(c,name='glutCreateMenu')
  interface
   subroutine fun(val) bind(C)
    integer(4), intent(in), value :: val
   end subroutine fun
  end interface
 end function glutCreateMenu
end interface

interface
 subroutine glutAttachMenu(button) bind(c,name='glutAttachMenu')
  integer(4), intent(in), value :: button
 end subroutine glutAttachMenu
end interface

interface
 subroutine glutAddMenuEntry(string,value) bind(c,name='glutAddMenuEntry')
  use iso_c_binding, only : c_char
  character(kind=c_char), intent(in) :: string(*)
  integer(4), intent(in), value :: value
 end subroutine glutAddMenuEntry
end interface

interface
 subroutine glutAddSubMenu(string,submenu) bind(c,name='glutAddSubMenu')
  use iso_c_binding, only : c_char
  character(kind=c_char), intent(in) :: string(*)
  integer(4), intent(in), value :: submenu
 end subroutine glutAddSubMenu
end interface

interface
 subroutine glutVisibilityFunc(fun) bind(c,name='glutVisibilityFunc')
  interface
   subroutine fun(vis) bind(C)
    import
    integer(GLint), intent(in), value :: vis
   end subroutine fun
  end interface
 end subroutine glutVisibilityFunc
end interface

interface
 subroutine glutIdleFunc(fun) bind(c,name='glutIdleFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutIdleFunc
end interface

interface
 subroutine glutSwapBuffers() bind(c,name='glutSwapBuffers')
 end subroutine glutSwapBuffers
end interface

interface
 subroutine glutSetColor(cell,red,green,blue) bind(c,name='glutSetColor')
  integer(4), intent(in), value :: cell
  real(4), intent(in), value :: red,green,blue
 end subroutine glutSetColor
end interface

interface
 integer(4) function glutGet(type) bind(c,name='glutGet')
   integer(4), intent(in), value :: type
 end function glutGet
end interface

interface
 subroutine glutKeyboardFunc(fun) bind(c,name='glutKeyboardFunc')
  interface
   subroutine fun(ikey,x,y) bind(C)
    import
    integer(GLubyte), intent(in), value ::  ikey
    integer(GLint), intent(in), value :: x,y
   end subroutine fun
  end interface
 end subroutine glutKeyboardFunc
end interface

interface
 subroutine glutMouseFunc(fun) bind(c,name='glutMouseFunc')
  interface
   subroutine fun(button,state,x,y) bind(C)
    import
    integer(GLint), intent(in), value :: button,state,x,y
   end subroutine fun
  end interface
 end subroutine glutMouseFunc
end interface

interface
 subroutine glutMotionFunc(fun) bind(c,name='glutMotionFunc')
  interface
   subroutine fun(x,y) bind(C)
    import
    integer(GLint), intent(in), value :: x,y
   end subroutine fun
  end interface
 end subroutine glutMotionFunc
end interface

interface
 subroutine glutPassiveMotionFunc(fun) bind(c,name='glutPassiveMotionFunc')
  interface
   subroutine fun(x,y) bind(C)
    import
    integer(GLint), intent(in), value :: x,y
   end subroutine fun
  end interface
 end subroutine glutPassiveMotionFunc
end interface

interface
 subroutine glutEntryFunc(func) bind(c,name='glutEntryFunc')
  interface
   subroutine fun(state) bind(C)
    import
    integer(GLint), intent(in), value :: state
   end subroutine fun
  end interface
 end subroutine glutEntryFunc
end interface

interface
 subroutine glutWireSphere(radius,slices,stacks) bind(c,name='glutWireSphere')
  real(8), intent(in), value :: radius
  integer(4), intent(in), value :: slices,stacks
 end subroutine glutWireSphere
end interface

interface
 subroutine glutSolidSphere(radius,slices,stacks) bind(c,name='glutSolidSphere')
  real(8), intent(in), value :: radius
  integer(4), intent(in), value :: slices, stacks
 end subroutine glutSolidSphere
end interface

interface
 integer(4) function glutCreateSubWindow(win,x,y,width,height) bind(c,name='glutCreateSubWindow')
  integer(4), intent(in), value :: win,x,y,width,height
 end function glutCreateSubWindow
end interface

interface
 subroutine glutDestroyWindow(win) bind(c,name='glutDestroyWindow')
  integer(4), intent(in), value :: win
 end subroutine glutDestroyWindow
end interface

interface
 subroutine glutPostWindowRedisplay(win) bind(c,name='glutPostWindowRedisplay')
  integer(4), intent(in), value :: win
 end subroutine glutPostWindowRedisplay
end interface

interface
 integer(4) function glutGetWindow() bind(c,name='glutGetWindow')
 end function glutGetWindow
end interface

interface
 subroutine glutSetWindow(win) bind(c,name='glutSetWindow')
  integer(4), intent(in), value :: win
 end subroutine glutSetWindow
end interface

interface
 subroutine glutSetWindowTitle(title) bind(c,name='glutSetWindowTitle')
 use iso_c_binding, only : c_char
 character(kind=c_char), intent(in) :: title(*)
 end subroutine glutSetWindowTitle
end interface

interface
 subroutine glutSetIconTitle(title) bind(c,name='glutSetIconTitle')
 use iso_c_binding, only : c_char
 character(kind=c_char), intent(in) :: title(*)
 end subroutine glutSetIconTitle
end interface

interface
 subroutine glutPositionWindow(x,y) bind(c,name='glutPositionWindow')
  integer(4), intent(in), value :: x,y
 end subroutine glutPositionWindow
end interface

interface
 subroutine glutReshapeWindow(width,height) bind(c,name='glutReshapeWindow')
  integer(4), intent(in), value :: width,height
 end subroutine glutReshapeWindow
end interface

interface
 subroutine glutPopWindow() bind(c,name='glutPopWindow')
 end subroutine glutPopWindow
end interface

interface
 subroutine glutPushWindow() bind(c,name='glutPushWindow')
 end subroutine glutPushWindow
end interface

interface
 subroutine glutIconifyWindow() bind(c,name='glutIconifyWindow')
 end subroutine glutIconifyWindow
end interface

interface
 subroutine glutShowWindow() bind(c,name='glutShowWindow')
 end subroutine glutShowWindow
end interface

interface
 subroutine glutHideWindow() bind(c,name='glutHideWindow')
 end subroutine glutHideWindow
end interface

interface
 subroutine glutFullScreen() bind(c,name='glutFullScreen')
 end subroutine glutFullScreen
end interface

interface
 subroutine glutSetCursor(cursor) bind(c,name='glutSetCursor')
  integer(4), intent(in), value :: cursor
 end subroutine glutSetCursor
end interface

interface
 subroutine glutWarpPointer(x,y) bind(c,name='glutWarpPointer')
  integer(4), intent(in), value :: x,y
 end subroutine glutWarpPointer
end interface

interface
 subroutine glutEstablishOverlay() bind(c,name='glutEstablishOverlay')
 end subroutine glutEstablishOverlay
end interface

interface
 subroutine glutRemoveOverlay() bind(c,name='glutRemoveOverlay')
 end subroutine glutRemoveOverlay
end interface

interface
 subroutine glutUseLayer(layer) bind(c,name='glutUseLayer')
  integer(4), intent(in), value :: layer
 end subroutine glutUseLayer
end interface

interface
 subroutine glutPostOverlayRedisplay() bind(c,name='glutPostOverlayRedisplay')
 end subroutine glutPostOverlayRedisplay
end interface

interface
 subroutine glutPostWindowOverlayRedisplay(win) bind(c,name='glutPostWindowOverlayRedisplay')
  integer(4), intent(in), value :: win
 end subroutine glutPostWindowOverlayRedisplay
end interface

interface
 subroutine glutShowOverlay() bind(c,name='glutShowOverlay')
 end subroutine glutShowOverlay
end interface

interface
 subroutine glutHideOverlay() bind(c,name='glutHideOverlay')
 end subroutine glutHideOverlay
end interface

interface
 subroutine glutDestroyMenu(menu) bind(c,name='glutDestroyMenu')
  integer(4), intent(in), value :: menu
 end subroutine glutDestroyMenu
end interface

interface
 integer(4) function glutGetMenu() bind(c,name='glutGetMenu')
 end function glutGetMenu
end interface

interface
 subroutine glutSetMenu(menu) bind(c,name='glutSetMenu')
  integer(4), intent(in), value :: menu
 end subroutine glutSetMenu
end interface

interface
 subroutine glutMenuStateFunc(fun) bind(c,name='glutMenuStateFunc')
  interface
   subroutine fun(state) bind(C)
    import
    integer(GLint), intent(in), value :: state
   end subroutine fun
  end interface
 end subroutine glutMenuStateFunc
end interface

interface
 subroutine glutSpecialFunc(fun) bind(c,name='glutSpecialFunc')
  interface
   subroutine fun(key,x,y) bind(C)
    import
    integer(GLint), intent(in), value :: key,x,y
   end subroutine fun
  end interface
 end subroutine glutSpecialFunc
end interface

interface
 subroutine glutWireCone(base,height,slices,stacks) bind(c,name='glutWireCone')
  real(8), intent(in), value :: base,height
  integer(4), intent(in), value :: slices,stacks
 end subroutine glutWireCone
end interface

interface
 subroutine glutSolidCone(base,height,slices,stacks) bind(c,name='glutSolidCone')
  real(8), intent(in), value :: base,height
  integer(4), intent(in), value :: slices,stacks
 end subroutine glutSolidCone
end interface

interface
 subroutine glutWireCylinder(Radius,Height,slices,stacks) bind(c,name='glutWireCylinder')
  real(8), intent(in), value :: Radius,Height
  integer(4), intent(in), value :: slices,stacks
 end subroutine glutWireCylinder
end interface

interface
 subroutine glutSolidCylinder(Radius,Height,slices,stacks) bind(c,name='glutSolidCylinder')
  real(8), intent(in), value :: Radius,Height
  integer(4), intent(in), value :: slices,stacks
 end subroutine glutSolidCylinder
end interface

interface
 subroutine glutWireTorus(innerRadius,outerRadius,sides,rings) bind(c,name='glutWireTorus')
  real(8), intent(in), value :: innerRadius,outerRadius
  integer(4), intent(in), value :: sides,rings
 end subroutine glutWireTorus
end interface

interface
 subroutine glutSolidTorus(innerRadius,outerRadius,sides,rings) bind(c,name='glutSolidTorus')
  real(8), intent(in), value :: innerRadius,outerRadius
  integer(4), intent(in), value :: sides,rings
 end subroutine glutSolidTorus
end interface

interface
 subroutine glutWireTetrahedron() bind(c,name='glutWireTetrahedron')
 end subroutine glutWireTetrahedron
end interface

interface
 subroutine glutSolidTetrahedron() bind(c,name='glutSolidTetrahedron')
 end subroutine glutSolidTetrahedron
end interface

interface
 subroutine glutWireIcosahedron() bind(c,name='glutWireIcosahedron')
 end subroutine glutWireIcosahedron
end interface

interface
 subroutine glutSolidIcosahedron() bind(c,name='glutSolidIcosahedron')
 end subroutine glutSolidIcosahedron
end interface

interface
 integer(4) function glutVideoResizeGet(param) bind(c,name='glutVideoResizeGet')
  integer(4), intent(in), value :: param
 end function glutVideoResizeGet
end interface

interface
 subroutine glutSetupVideoResizing() bind(c,name='glutSetupVideoResizing')
 end subroutine glutSetupVideoResizing
end interface

interface
 subroutine glutStopVideoResizing() bind(c,name='glutStopVideoResizing')
 end subroutine glutStopVideoResizing
end interface

interface
 subroutine glutVideoResize(x,y,width,height) bind(c,name='glutVideoResize')
  integer(4), intent(in), value :: x,y,width,height
 end subroutine glutVideoResize
end interface

interface
 subroutine glutVideoPan(x,y,width,height) bind(c,name='glutVideoPan')
  integer(4), intent(in), value :: x,y,width,height
 end subroutine glutVideoPan
end interface

interface
 subroutine glutReportErrors() bind(c,name='glutReportErrors')
 end subroutine glutReportErrors
end interface

interface
 subroutine glutIgnoreKeyRepeat(ignore) bind(c,name='glutIgnoreKeyRepeat')
  integer(4), intent(in), value :: ignore
 end subroutine glutIgnoreKeyRepeat
end interface

interface
 subroutine glutSetKeyRepeat(repeatMode) bind(c,name='glutSetKeyRepeat')
  integer(4), intent(in), value :: repeatMode
 end subroutine glutSetKeyRepeat
end interface

interface
 subroutine glutGameModeString(str) bind(c,name='glutGameModeString')
 use iso_c_binding, only : c_char
 character(kind=c_char), intent(in) :: str(*)
 end subroutine glutGameModeString
end interface

interface
 integer(4) function glutGameModeGet(mode) bind(c,name='glutGameModeGet')
  integer(4), intent(in), value :: mode
 end function glutGameModeGet
end interface

interface
 integer(4) function glutGetModifiers() bind(c,name='glutGetModifiers')
 end function glutGetModifiers
end interface

interface
 integer(4) function glutLayerGet(type) bind(c,name='glutLayerGet')
  integer(4), intent(in), value :: type
 end function glutLayerGet
end interface

interface
 subroutine glutSpaceballMotionFunc(fun) bind(c,name='glutSpaceballMotionFunc')
  interface
   subroutine fun(x,y,z) bind(C)
    import
    integer(GLint), intent(in), value :: x,y,z
   end subroutine fun
  end interface
 end subroutine glutSpaceballMotionFunc
end interface

interface
 subroutine glutSpaceballRotateFunc(fun) bind(c,name='glutSpaceballRotateFunc')
  interface
   subroutine fun(x,y,z) bind(C)
    import
    integer(GLint), intent(in), value :: x,y,z
   end subroutine fun
  end interface
 end subroutine glutSpaceballRotateFunc
end interface

interface
 subroutine glutSpaceballButtonFunc(fun) bind(c,name='glutSpaceballButtonFunc')
  interface
   subroutine fun(button,state) bind(C)
    import
    integer(GLint), intent(in), value :: button,state
   end subroutine fun
  end interface
 end subroutine glutSpaceballButtonFunc
end interface

interface
 subroutine glutButtonBoxFunc(fun) bind(c,name='glutButtonBoxFunc')
  interface
   subroutine fun(button,state) bind(C)
    import
    integer(GLint), intent(in), value :: button,state
   end subroutine fun
  end interface
 end subroutine glutButtonBoxFunc
end interface

interface
 subroutine glutDialsFunc(fun) bind(c,name='glutDialsFunc')
  interface
   subroutine fun(dial,value) bind(C)
    import
    integer(GLint), intent(in), value :: dial,value
   end subroutine fun
  end interface
 end subroutine glutDialsFunc
end interface

interface
 subroutine glutTabletMotionFunc(fun) bind(c,name='glutTabletMotionFunc')
  interface
   subroutine fun(x,y) bind(C)
    import
    integer(GLint), intent(in), value :: x,y
   end subroutine fun
  end interface
 end subroutine glutTabletMotionFunc
end interface

interface
 subroutine glutTabletButtonFunc(fun) bind(c,name='glutTabletButtonFunc')
  interface
   subroutine fun(button,state,x,y) bind(C)
    import
    integer(GLint), intent(in), value :: button,state,x,y
   end subroutine fun
  end interface
 end subroutine glutTabletButtonFunc
end interface

interface
 subroutine glutMenuStatusFunc(fun) bind(c,name='glutMenuStatusFunc')
  interface
   subroutine fun(status,x,y) bind(C)
    import
    integer(GLint), intent(in), value :: status,x,y
   end subroutine fun
  end interface
 end subroutine glutMenuStatusFunc
end interface

interface
 subroutine glutOverlayDisplayFunc(fun) bind(c,name='glutOverlayDisplayFunc')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine glutOverlayDisplayFunc
end interface

interface
 subroutine glutWindowStatusFunc(fun) bind(c,name='glutWindowStatusFunc')
  interface
   subroutine fun(state) bind(C)
    import
    integer(GLint), intent(in), value :: state
   end subroutine fun
  end interface
 end subroutine glutWindowStatusFunc
end interface

interface
 subroutine glutKeyboardUpFunc(fun) bind(c,name='glutKeyboardUpFunc')
  interface
   subroutine fun(key,x,y) bind(C)
    import
    integer(GLubyte), intent(in), value :: key
    integer(GLint), intent(in), value :: x,y
   end subroutine fun
  end interface
 end subroutine glutKeyboardUpFunc
end interface

interface
 subroutine glutSpecialUpFunc(fun) bind(c,name='glutSpecialUpFunc')
  interface
   subroutine fun(key,x,y) bind(C)
    import
    integer(GLint), intent(in), value :: key,x,y
   end subroutine fun
  end interface
 end subroutine glutSpecialUpFunc
end interface

interface
 subroutine glutJoystickFunc(fun,pollInterval) bind(c,name='glutJoystickFunc')
  interface
   subroutine fun(buttonMask, x,y,z) bind(C)
    import
    integer(GLint), intent(in), value :: buttonMask, x,y,z
   end subroutine fun
  end interface
  integer(4), intent(in), value :: pollInterval
 end subroutine glutJoystickFunc
end interface

interface
 subroutine glutGetColor(ndx,component) bind(c,name='glutGetColor')
  integer(4),intent(in), value :: ndx,component
 end subroutine glutGetColor
end interface

interface
 subroutine glutCopyColormap(win) bind(c,name='glutCopyColormap')
  integer(4),intent(in), value :: win
 end subroutine glutCopyColormap
end interface

interface
 integer(4) function glutDeviceGet(type) bind(c,name='glutDeviceGet')
  integer(4),intent(in), value :: type
 end function glutDeviceGet
end interface

!interface
! integer(4) function glutExtensionSupported(str) bind(c,name='glutExtensionSupported')
! character(kind=c_char), intent(in) :: str(*)
! end function glutExtensionSupported
!end interface

interface
 subroutine glutChangeToMenuEntry(item,char,value) bind(c,name='glutChangeToMenuEntry')
  use iso_c_binding, only : c_char
  integer(4), intent(in), value :: item,value
  character(kind=c_char), intent(in) :: char(*)
 end subroutine glutChangeToMenuEntry
end interface

interface
 subroutine glutChangeToSubMenu(item,char,submenu) bind(c,name='glutChangeToSubMenu')
  use iso_c_binding, only : c_char
  integer(4), intent(in), value :: item,submenu
  character(kind=c_char), intent(in) :: char(*)
 end subroutine glutChangeToSubMenu
end interface

interface
 subroutine glutRemoveMenuItem(item) bind(c,name='glutRemoveMenuItem')
  integer(4),intent(in), value :: item
 end subroutine glutRemoveMenuItem
end interface

interface
 subroutine glutDetachMenu(button) bind(c,name='glutDetachMenu')
  integer(4),intent(in), value :: button
 end subroutine glutDetachMenu
end interface

!interface
! subroutine glutBitmapCharacter(font,character) bind(c,name='glutBitmapCharacter')
!  external :: font
!!  integer(4), intent(in), value :: character
! end subroutine glutBitmapCharacter
!end interface

!interface
! subroutine glutBitmapString(font,string) bind(c,name='glutBitmapString')
!  external :: font
!  character(*), intent(in) :: string
! end subroutine glutBitmapString
!end interface

!interface
! integer(4) function glutBitmapWidth(font,character) bind(c,name='glutBitmapWidth')
!  external :: font
!  integer(4),intent(in), value :: character
! end function glutBitmapWidth
!end interface

!interface
! integer(4) function glutBitmapHeight(font) bind(c,name='glutBitmapHeight')
!  external :: font
! end function glutBitmapHeight
!end interface

!interface
! real(4) function glutStrokeHeight(font) bind(c,name='glutStrokeHeight')
!  external :: font
! end function glutStrokeHeight
!end interface

!interface
!subroutine glutStrokeCharacter(font,character) bind(c,name='glutStrokeCharacter')
!external :: font
!integer(4),intent(in), value :: character
!end subroutine glutStrokeCharacter
!end interface

!interface
! subroutine glutStrokeString(font,string) bind(c,name='glutStrokeString')
!  external :: font
!  character(*), intent(in) :: string
! end subroutine glutStrokeString
!end interface

!interface
!integer(4) function glutStrokeWidth(font,character) bind(c,name='glutStrokeWidth')
!external :: font
!integer(4),intent(in), value :: character
!end function glutStrokeWidth
!end interface

!interface
!integer(4) function glutBitmapLength(font,str) bind(c,name='glutBitmapLength')
!external :: font
!character(*), intent(in) :: str
!end function glutBitmapLength
!end interface

!interface
! integer(4) function glutStrokeLength(font,str) bind(c,name='glutStrokeLength')
!  external :: font
!  character(*), intent(in) :: str
! end function glutStrokeLength
!end interface

!interface
!function glutGetProcAddress(procName) bind(c,name="glutGetProcAddress")
!import
!TYPE(C_FUNPTR) :: glutGetProcAddress
!character(*), intent(in) :: procName
!end function glutGetProcAddress
!end interface

interface
subroutine glutWireSierpinskiSponge(num_levels,offset,scale) bind(c,name="glutWireSierpinskiSponge")
integer(4), intent(in), value:: num_levels
real(8), intent(in) :: offset(3)
real(8), intent(in), value :: scale
end subroutine glutWireSierpinskiSponge
end interface

interface
subroutine glutSolidSierpinskiSponge(num_levels,offset,scale) bind(c,name="glutSolidSierpinskiSponge")
integer(4), intent(in), value:: num_levels
real(8), intent(in) :: offset(3)
real(8), intent(in), value :: scale
end subroutine glutSolidSierpinskiSponge
end interface

interface
 subroutine glutMainLoopEvent() bind(c,name='glutMainLoopEvent')
 end subroutine glutMainLoopEvent
end interface

interface
 subroutine glutLeaveMainLoop() bind(c,name='glutLeaveMainLoop')
 end subroutine glutLeaveMainLoop
end interface

interface
 subroutine glutSetOption(name,value) bind(c,name='glutSetOption')
 integer(4), intent(in), value :: name,value
 end subroutine glutSetOption
end interface

interface
function glutGetWindowData() bind(c,name='glutGetWindowData')
import
integer(C_INTPTR_T) glutGetWindowData
end function glutGetWindowData
end interface

interface
 subroutine glutSetWindowData(data) bind(c,name='glutSetWindowData')
import
integer(C_INTPTR_T), intent(in), value :: data
end subroutine glutSetWindowData
end interface

interface
function glutGetMenuData() bind(c,name='glutGetMenuData')
import
integer(C_INTPTR_T) glutGetMenuData
end function glutGetMenuData
end interface

interface
 subroutine glutSetMenuData(data) bind(c,name='glutSetMenuData')
import
integer(C_INTPTR_T), intent(in), value :: data
end subroutine glutSetMenuData
end interface

!interface
! integer(4) function g_glutCreateWindowWithExit(title,fun) bind(c,name='__glutCreateWindowWithExit')
!  character(*), intent(in) :: title
!  interface
!   subroutine fun() bind(C)
!   end subroutine fun
!  end interface
! end function g_glutCreateWindowWithExit
!end interface

interface
 subroutine g_glutInitWithExit(argc,argv,fun) bind(c,name='__glutInitWithExit')
  integer,intent(in) :: argc,argv
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
 end subroutine g_glutInitWithExit
end interface

interface
 integer(4) function g_glutCreateMenuWithExit(fun,ExitFun) bind(c,name='__glutCreateMenuWithExit')
  interface
   subroutine fun() bind(C)
   end subroutine fun
  end interface
  interface
   subroutine ExitFun() bind(C)
   end subroutine ExitFun
  end interface
 end function g_glutCreateMenuWithExit
end interface

!interface
! subroutine g_glutSetFCB(n,func) bind(c,name='__glutSetFCB')
!  integer(4),intent(in), value :: n
!  external func 
! end subroutine g_glutSetFCB
!end interface
end module OpenGL

