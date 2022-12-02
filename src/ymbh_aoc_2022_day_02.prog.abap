REPORT ymbh_aoc_2022_day_02.

INTERFACE calculator_types.
  TYPES: BEGIN OF score_calc,
           opponent_code    TYPE char1,
           my_code          TYPE char1,
           opponent_decrypt TYPE char1,
           my_decrypt       TYPE char1,
           my_shape_score   TYPE i,
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
           my_code          TYPE char1,
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
    METHODS calculate_shapes_scores RETURNING VALUE(result) TYPE calculator_types=>score_calculator.
    METHODS calculate_overall_score RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS score_calculator IMPLEMENTATION.

  METHOD translate_input_to_score_calc.
    "Redefined by subclasses
  ENDMETHOD.

  METHOD calculate_overall_score.
    result = REDUCE #( INIT sum = 0
                          FOR line IN score_sheet
                          NEXT sum = sum + line-round_score ).
  ENDMETHOD.

  METHOD calculate_totals_per_round.
    result = VALUE #( FOR line IN score_sheet
                        ( opponent_code = line-opponent_code
                          opponent_decrypt = line-opponent_decrypt
                          my_code = line-my_code
                          my_decrypt = line-my_decrypt
                          result = line-result
                          my_shape_score = line-my_shape_score
                          round_score = COND #( WHEN line-result = |W| THEN line-my_shape_score + 6
                                                WHEN line-result = |L| THEN line-my_shape_score
                                                WHEN line-result = |D| THEN line-my_shape_score + 3  ) ) ).
  ENDMETHOD.

  METHOD calculate_shapes_scores.
    result = VALUE #( FOR line IN score_sheet
                            ( opponent_code = line-opponent_code
                              opponent_decrypt = line-opponent_decrypt
                              my_code = line-my_code
                              my_decrypt = line-my_decrypt
                              result = line-result
                              my_shape_score = COND #( WHEN line-my_decrypt = |R| THEN 1
                                                       WHEN line-my_decrypt = |P| THEN 2
                                                       WHEN line-my_decrypt = |S| THEN 3 ) ) ).

  ENDMETHOD.

  METHOD build_rules.
    wl_rules = VALUE calculator_types=>strategies( ( opponent_decrypt = |R| my_decrypt = |P| decision = |W| )
                                                   ( opponent_decrypt = |P| my_decrypt = |R| decision = |L| )
                                                   ( opponent_decrypt = |P| my_decrypt = |P| decision = |D| )
                                                   ( opponent_decrypt = |P| my_decrypt = |S| decision = |W| )
                                                   ( opponent_decrypt = |S| my_decrypt = |P| decision = |L| )
                                                   ( opponent_decrypt = |R| my_decrypt = |S| decision = |L| )
                                                   ( opponent_decrypt = |S| my_decrypt = |R| decision = |W| )
                                                   ( opponent_decrypt = |S| my_decrypt = |S| decision = |D| )
                                                   ( opponent_decrypt = |R| my_decrypt = |R| decision = |D| ) ).
  ENDMETHOD.

  METHOD build_shape_scores.
    shape_score_sheet =  VALUE #( ( shape = |R| score = 1 )
                                  ( shape = |P| score = 2 )
                                  ( shape = |S| score = 3 ) ).
  ENDMETHOD.

  METHOD constructor.
    build_rules( ).
    build_shape_scores( ).
  ENDMETHOD.

  METHOD calculate_score.
    score_sheet = calculate_round_results( ).
    score_sheet = calculate_totals_per_round( ).
    score_sheet = calculate_shapes_scores( ).
    score_sheet = calculate_totals_per_round( ).
    result = calculate_overall_score( ).
  ENDMETHOD.

  METHOD calculate_round_results.
    "Redefined at subclasses
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
                           ( opponent_code = line-opponent_code
                             opponent_decrypt = line-opponent_decrypt
                             my_code = line-my_code
                             my_decrypt = line-my_decrypt
                             result = wl_rules[ opponent_decrypt = line-opponent_decrypt
                                                my_decrypt = line-my_decrypt ]-decision ) ).

  ENDMETHOD.

  METHOD translate_input_to_score_calc.
    score_sheet = VALUE #( FOR line IN input
                             ( opponent_code    = line(1)
                               opponent_decrypt = COND #( WHEN line(1) = |A| THEN |R|
                                                          WHEN line(1) = |B| THEN |P|
                                                          WHEN line(1) = |C| THEN |S| )
                               my_code          = line+2(1)
                               my_decrypt       = COND #( WHEN line+2(1) = |X| THEN |R|
                                                          WHEN line+2(1) = |Y| THEN |P|
                                                          WHEN line+2(1) = |Z| THEN |S| ) )  ).
  ENDMETHOD.

ENDCLASS.


CLASS score_calculator_2 DEFINITION INHERITING FROM score_calculator.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

  PROTECTED SECTION.
    METHODS translate_input_to_score_calc REDEFINITION.
    METHODS calculate_round_results REDEFINITION.

  PRIVATE SECTION.

















ENDCLASS.

CLASS score_calculator_2 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    translate_input_to_score_calc( input ).
  ENDMETHOD.

  METHOD translate_input_to_score_calc.
    score_sheet = VALUE #( FOR line IN input
                            ( opponent_code    = line(1)
                              opponent_decrypt = COND #( WHEN line(1) = |A| THEN |R|
                                                         WHEN line(1) = |B| THEN |P|
                                                         WHEN line(1) = |C| THEN |S| )
                              my_code          = line+2(1)
                              result           = COND #( WHEN line+2(1) = |X| THEN |L|
                                                         WHEN line+2(1) = |Y| THEN |D|
                                                         WHEN line+2(1) = |Z| THEN |W| ) )  ).
  ENDMETHOD.

  METHOD calculate_round_results.
    result = VALUE #( FOR line IN score_sheet
                           ( opponent_code = line-opponent_code
                             opponent_decrypt = line-opponent_decrypt
                             my_code = line-my_code
                             my_decrypt = wl_rules[ opponent_decrypt = line-opponent_decrypt
                                                decision = line-result ]-my_decrypt
                             result = line-result ) ).

  ENDMETHOD.

ENDCLASS.

CLASS tc_scoring DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
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
                    ( opponent_code = |A| my_code = |Y| opponent_decrypt = |R| my_decrypt = |P| result = |W| my_shape_score = 2 round_score = 8 )
                    ( opponent_code = |B| my_code = |X| opponent_decrypt = |P| my_decrypt = |R| result = |L| my_shape_score = 1 round_score = 1 )
                    ( opponent_code = |C| my_code = |Z| opponent_decrypt = |S| my_decrypt = |S| result = |D| my_shape_score = 3 round_score = 6 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 15 act = cut->calculate_score( )  ).
  ENDMETHOD.

  METHOD get_overall_score_variant_2.
    DATA(cut) = NEW score_calculator_2( raw_input ).
    DATA(input) = VALUE calculator_types=>score_calculator(
                    ( opponent_code = |A| my_code = |Y| opponent_decrypt = |R| my_decrypt = |R| result = |D| my_shape_score = 2 round_score = 4 )
                    ( opponent_code = |B| my_code = |X| opponent_decrypt = |P| my_decrypt = |S| result = |L| my_shape_score = 1 round_score = 1 )
                    ( opponent_code = |C| my_code = |Z| opponent_decrypt = |S| my_decrypt = |R| result = |W| my_shape_score = 3 round_score = 7 ) ).
    cl_abap_unit_assert=>assert_equals( exp = 12 act = cut->calculate_score( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(rock_paper_scissor1) = NEW score_calculator_1( input ).
  WRITE / |Solution part one:{ rock_paper_scissor1->calculate_score( ) }|.

  DATA(rock_paper_scissor2) = NEW score_calculator_2( input ).
  WRITE / |Solution part one:{ rock_paper_scissor2->calculate_score( ) }|.
