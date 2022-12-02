REPORT ymbh_aoc_2022_day_02.

INTERFACE calculator_types.
  TYPES: BEGIN OF score_calc,
           opponent_code    TYPE char1,
           my_code          TYPE char1,
           opponent_decrypt TYPE char1,
           my_decrypt       TYPE char1,
           result           TYPE char1,
           round_score      TYPE i,
         END OF score_calc.
  TYPES score_calculator TYPE STANDARD TABLE OF score_calc WITH EMPTY KEY.

  TYPES: BEGIN OF shape_score,
           shape TYPE char1,
           score TYPE i,
         END OF shape_score.
  TYPES shape_scores TYPE STANDARD TABLE OF shape_score WITH EMPTY KEY.

  TYPES: BEGIN OF strategy,
           opponent_decrypt TYPE char1,
           my_decrypt       TYPE char1,
           decision         TYPE char1,
         END OF strategy.
  TYPES strategies TYPE STANDARD TABLE OF strategy WITH EMPTY KEY.
ENDINTERFACE.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221202 | ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS score_calculator DEFINITION.
  PUBLIC SECTION.
    CONSTANTS round_won TYPE char1 VALUE 'W'.
    CONSTANTS round_draw TYPE char1 VALUE 'D'.
    CONSTANTS round_lost TYPE char1 VALUE 'L'.

    CONSTANTS rock TYPE char1 VALUE 'R'.
    CONSTANTS paper TYPE char1 VALUE 'P'.
    CONSTANTS scissor TYPE char1 VALUE 'S'.

    METHODS constructor.

    METHODS calculate_score RETURNING VALUE(result) TYPE i.

  PROTECTED SECTION.
    DATA score_sheet TYPE calculator_types=>score_calculator.
    DATA wl_rules TYPE calculator_types=>strategies.

    METHODS calculate_round_results RETURNING VALUE(result) TYPE calculator_types=>score_calculator.
    METHODS translate_input_to_score_calc IMPORTING input TYPE stringtab.

  PRIVATE SECTION.
    DATA shape_score_sheet TYPE calculator_types=>shape_scores.

    METHODS build_rules.
    METHODS build_shape_scores.
    METHODS calculate_totals_per_round RETURNING VALUE(result) TYPE calculator_types=>score_calculator.

    METHODS calculate_overall_score RETURNING VALUE(result) TYPE i.
    METHODS calculate_score_per_round IMPORTING line          TYPE calculator_types=>score_calc
                                      RETURNING VALUE(result) TYPE i.
    METHODS get_shape_score
      IMPORTING
        decryption    TYPE char1
      RETURNING
        VALUE(result) TYPE i.

ENDCLASS.

CLASS score_calculator IMPLEMENTATION.

  METHOD constructor.
    build_rules( ).
    build_shape_scores( ).
  ENDMETHOD.

  METHOD translate_input_to_score_calc.
    "redefined by subclasses
  ENDMETHOD.

  METHOD build_rules.
    wl_rules = VALUE calculator_types=>strategies( ( opponent_decrypt = rock    my_decrypt = paper   decision = round_won )
                                                   ( opponent_decrypt = paper   my_decrypt = rock    decision = round_lost )
                                                   ( opponent_decrypt = paper   my_decrypt = paper   decision = round_draw )
                                                   ( opponent_decrypt = paper   my_decrypt = scissor decision = round_won )
                                                   ( opponent_decrypt = scissor my_decrypt = paper   decision = round_lost )
                                                   ( opponent_decrypt = rock    my_decrypt = scissor decision = round_lost )
                                                   ( opponent_decrypt = scissor my_decrypt = rock    decision = round_won )
                                                   ( opponent_decrypt = scissor my_decrypt = scissor decision = round_draw )
                                                   ( opponent_decrypt = rock    my_decrypt = rock    decision = round_draw ) ).
  ENDMETHOD.

  METHOD build_shape_scores.
    shape_score_sheet =  VALUE #( ( shape = rock    score = 1 )
                                  ( shape = paper   score = 2 )
                                  ( shape = scissor score = 3 ) ).
  ENDMETHOD.

  METHOD calculate_score.
    score_sheet = calculate_round_results( ).
    score_sheet = calculate_totals_per_round( ).
    result = calculate_overall_score( ).
  ENDMETHOD.

  METHOD calculate_round_results.
    "Redefined at subclasses
  ENDMETHOD.

  METHOD calculate_totals_per_round.
    result = VALUE #( FOR line IN score_sheet
                        ( opponent_code    = line-opponent_code
                          opponent_decrypt = line-opponent_decrypt
                          my_code          = line-my_code
                          my_decrypt       = line-my_decrypt
                          result           = line-result
                          round_score      = calculate_score_per_round( line ) ) ).
  ENDMETHOD.

  METHOD calculate_score_per_round.
    result = SWITCH #( line-result WHEN round_won   THEN get_shape_score( line-my_decrypt ) + 6
                                   WHEN round_draw  THEN get_shape_score( line-my_decrypt ) + 3
                                   WHEN round_lost  THEN get_shape_score( line-my_decrypt ) ).
  ENDMETHOD.

  METHOD get_shape_score.
    result = shape_score_sheet[ shape = decryption ]-score.
  ENDMETHOD.

  METHOD calculate_overall_score.
    result = REDUCE #( INIT sum = 0
                          FOR line IN score_sheet
                          NEXT sum = sum + line-round_score ).
  ENDMETHOD.

ENDCLASS.

CLASS score_calculator_1 DEFINITION INHERITING FROM score_calculator.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

  PROTECTED SECTION.
    METHODS translate_input_to_score_calc REDEFINITION.
    METHODS calculate_round_results REDEFINITION.

ENDCLASS.

CLASS score_calculator_1 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    translate_input_to_score_calc( input ).
  ENDMETHOD.

  METHOD calculate_round_results.
    result = VALUE #( FOR line IN score_sheet
                           ( opponent_code    = line-opponent_code
                             opponent_decrypt = line-opponent_decrypt
                             my_code          = line-my_code
                             my_decrypt       = line-my_decrypt
                             result           = wl_rules[ opponent_decrypt = line-opponent_decrypt
                                                          my_decrypt       = line-my_decrypt ]-decision ) ).
  ENDMETHOD.

  METHOD translate_input_to_score_calc.
    score_sheet = VALUE #( FOR line IN input
                             ( opponent_code    = line(1)
                               opponent_decrypt = COND #( WHEN line(1) = |A| THEN rock
                                                          WHEN line(1) = |B| THEN paper
                                                          WHEN line(1) = |C| THEN scissor )
                               my_code          = line+2(1)
                               my_decrypt       = COND #( WHEN line+2(1) = |X| THEN rock
                                                          WHEN line+2(1) = |Y| THEN paper
                                                          WHEN line+2(1) = |Z| THEN scissor ) )  ).
  ENDMETHOD.

ENDCLASS.

CLASS score_calculator_2 DEFINITION INHERITING FROM score_calculator.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

  PROTECTED SECTION.
    METHODS translate_input_to_score_calc REDEFINITION.
    METHODS calculate_round_results REDEFINITION.

ENDCLASS.

CLASS score_calculator_2 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    translate_input_to_score_calc( input ).
  ENDMETHOD.

  METHOD translate_input_to_score_calc.
    score_sheet = VALUE #( FOR line IN input
                            ( opponent_code    = line(1)
                              opponent_decrypt = COND #( WHEN line(1) = |A| THEN rock
                                                         WHEN line(1) = |B| THEN paper
                                                         WHEN line(1) = |C| THEN scissor )
                              my_code          = line+2(1)
                              result           = COND #( WHEN line+2(1) = |X| THEN round_lost
                                                         WHEN line+2(1) = |Y| THEN round_draw
                                                         WHEN line+2(1) = |Z| THEN round_won ) ) ).
  ENDMETHOD.

  METHOD calculate_round_results.
    result = VALUE #( FOR line IN score_sheet
                           ( opponent_code    = line-opponent_code
                             opponent_decrypt = line-opponent_decrypt
                             my_code          = line-my_code
                             my_decrypt       = wl_rules[ opponent_decrypt = line-opponent_decrypt
                                                          decision         = line-result ]-my_decrypt
                             result           = line-result ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_scoring DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS rock TYPE char1 VALUE 'R'.
    CONSTANTS paper TYPE char1 VALUE 'P'.
    CONSTANTS scissor TYPE char1 VALUE 'S'.

    CONSTANTS won TYPE char1  VALUE 'W'.
    CONSTANTS draw TYPE char1 VALUE 'D'.
    CONSTANTS lost TYPE char1 VALUE 'L'.

    DATA raw_input TYPE stringtab.

    METHODS setup.

    METHODS get_overall_score_variant_1 FOR TESTING.
    METHODS get_overall_score_variant_2 FOR TESTING.

ENDCLASS.

CLASS tc_scoring IMPLEMENTATION.

  METHOD setup.
    raw_input = VALUE stringtab( ( |A Y| )
                                 ( |B X| )
                                 ( |C Z| ) ).
  ENDMETHOD.

  METHOD get_overall_score_variant_1.
    DATA(cut) = NEW score_calculator_1( raw_input ).
    DATA(input) = VALUE calculator_types=>score_calculator(
                    ( opponent_code = |A| my_code = |Y| opponent_decrypt = rock    my_decrypt = paper   result = won  round_score = 8 )
                    ( opponent_code = |B| my_code = |X| opponent_decrypt = paper   my_decrypt = rock    result = lost round_score = 1 )
                    ( opponent_code = |C| my_code = |Z| opponent_decrypt = scissor my_decrypt = scissor result = draw round_score = 6 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 15 act = cut->calculate_score( )  ).
  ENDMETHOD.

  METHOD get_overall_score_variant_2.
    DATA(cut) = NEW score_calculator_2( raw_input ).
    DATA(input) = VALUE calculator_types=>score_calculator(
                    ( opponent_code = |A| my_code = |Y| opponent_decrypt = rock    my_decrypt = rock    result = draw round_score = 4 )
                    ( opponent_code = |B| my_code = |X| opponent_decrypt = paper   my_decrypt = scissor result = lost round_score = 1 )
                    ( opponent_code = |C| my_code = |Z| opponent_decrypt = scissor my_decrypt = rock    result = won  round_score = 7 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 12 act = cut->calculate_score( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(rock_paper_scissor1) = NEW score_calculator_1( input ).
  WRITE / |Solution part one:{ rock_paper_scissor1->calculate_score( ) }|.

  DATA(rock_paper_scissor2) = NEW score_calculator_2( input ).
  WRITE / |Solution part one:{ rock_paper_scissor2->calculate_score( ) }|.
