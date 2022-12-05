REPORT ymbh_aoc_2022_day_05.

CLASS cx_movements_end DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS cx_stack_end DEFINITION INHERITING FROM cx_no_check.
ENDCLASS.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221205 | ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS instructions DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF col_data,
             index       TYPE i,
             offset_from TYPE i,
             length      TYPE i,
           END OF col_data.
    TYPES cols_data TYPE STANDARD TABLE OF col_data WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE stringtab.
    METHODS prepare_input.

    METHODS get_index_line_no RETURNING VALUE(result) TYPE i.

    METHODS get_crates RETURNING VALUE(result) TYPE stringtab.

    METHODS get_movements RETURNING VALUE(result) TYPE stringtab.

    METHODS get_column_data RETURNING VALUE(result) TYPE cols_data.

  PRIVATE SECTION.
    DATA instruction_set TYPE stringtab.
    DATA index_line_nr TYPE i.
    DATA crates TYPE stringtab.
    DATA movements TYPE stringtab.
    DATA index_line_columns TYPE cols_data.

    METHODS determine_index_line_number.

    METHODS determine_crates.

    METHODS determine_movements.

    METHODS determine_col_data_from_intel.

ENDCLASS.

CLASS instructions IMPLEMENTATION.

  METHOD constructor.
    instruction_set = input.
  ENDMETHOD.

  METHOD get_index_line_no.
    result = index_line_nr.
  ENDMETHOD.

  METHOD get_crates.
    result = crates.
  ENDMETHOD.

  METHOD get_movements.
    result = movements.
  ENDMETHOD.

  METHOD get_column_data.
    result = index_line_columns.
  ENDMETHOD.

  METHOD prepare_input.
    determine_index_line_number( ).
    determine_crates( ).
    determine_movements( ).
    determine_col_data_from_intel( ).
  ENDMETHOD.

  METHOD determine_index_line_number.
    index_line_nr = line_index( instruction_set[ table_line = || ] ) - 1.
  ENDMETHOD.

  METHOD determine_crates.
    LOOP AT instruction_set REFERENCE INTO DATA(line) FROM 1 TO index_line_nr - 1.
      crates = VALUE #( BASE crates ( line->* ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_movements.
    LOOP AT instruction_set REFERENCE INTO DATA(line) FROM index_line_nr + 2 TO lines( instruction_set ).
      movements = VALUE #( BASE movements ( line->* ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_col_data_from_intel.
    DATA(offset) = 0.
    WHILE offset <= strlen( instruction_set[ index_line_nr ] ) - 1.
      DATA(char) = substring( val = instruction_set[ index_line_nr ] off = offset len = 1 ).
      IF char <> | |.
        index_line_columns = VALUE #( BASE index_line_columns
                                        ( index = CONV #( char ) offset_from = sy-index - 2 length = 3 ) ).
        CLEAR char.
      ENDIF.
      offset = offset + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

CLASS stack DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING name TYPE string.

    METHODS put IMPORTING crates TYPE stringtab.

    METHODS put_first IMPORTING crates TYPE stringtab.

    METHODS get RETURNING VALUE(result) TYPE stringtab.

    METHODS remove_single IMPORTING amount        TYPE i
                          RETURNING VALUE(result) TYPE stringtab.

    METHODS remove_in_bulk IMPORTING amount        TYPE i
                           RETURNING VALUE(result) TYPE stringtab.

    METHODS get_name RETURNING VALUE(result) TYPE string.

    METHODS get_top_crate RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA stack TYPE stringtab.
    DATA name TYPE string.

    METHODS pop IMPORTING amount TYPE i.

ENDCLASS.

CLASS stack IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
  ENDMETHOD.

  METHOD put.
    INSERT LINES OF crates INTO stack INDEX 1.
  ENDMETHOD.

  METHOD put_first.
    APPEND LINES OF crates TO stack.
  ENDMETHOD.

  METHOD get.
    result = stack.
  ENDMETHOD.

  METHOD get_name.
    result = condense( name ).
  ENDMETHOD.

  METHOD pop.
    DELETE stack TO amount.
  ENDMETHOD.

  METHOD remove_single.
    result = VALUE #( FOR i = amount THEN i - 1 WHILE i >= 1
                        ( stack[ i ] ) ).
    pop( amount ).
  ENDMETHOD.

  METHOD remove_in_bulk.
    result = VALUE #( FOR i = 1 THEN i + 1 WHILE i <= amount
                        ( stack[ i ] ) ).
    pop( amount ).
  ENDMETHOD.

  METHOD get_top_crate.
    result = substring( val = stack[ 1 ] off = 1 len = 1 ).
  ENDMETHOD.

ENDCLASS.

CLASS movement DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF move,
             crates     TYPE i,
             from_stack TYPE i,
             to_stack   TYPE i,
           END OF move.
    TYPES moves TYPE STANDARD TABLE OF move WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS get_moves RETURNING VALUE(result) TYPE moves.

    METHODS get_move RETURNING VALUE(result) TYPE move.

  PRIVATE SECTION.
    DATA move_instructions TYPE moves.
    DATA current_item TYPE i.

    METHODS determine_moves IMPORTING input TYPE stringtab.

ENDCLASS.

CLASS movement IMPLEMENTATION.

  METHOD constructor.
    determine_moves( input ).
  ENDMETHOD.

  METHOD determine_moves.
    LOOP AT input REFERENCE INTO DATA(line).
      SPLIT line->* AT space INTO TABLE DATA(movement_lines).
      move_instructions = VALUE #( BASE move_instructions ( crates     = movement_lines[ 2 ]
                                                            from_stack = movement_lines[ 4 ]
                                                            to_stack   = movement_lines[ 6 ] ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_moves.
    result = move_instructions.
  ENDMETHOD.

  METHOD get_move.
    current_item = current_item + 1.
    TRY.
        result = move_instructions[ current_item ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW cx_movements_end( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS stack_collection DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING instructions TYPE REF TO instructions.

    METHODS build.

    METHODS get_stack_by_name IMPORTING name          TYPE string
                              RETURNING VALUE(result) TYPE REF TO stack.

    METHODS get_stack RETURNING VALUE(result) TYPE REF TO stack.

  PRIVATE SECTION.
    DATA collection TYPE STANDARD TABLE OF REF TO stack.
    DATA instructions TYPE REF TO instructions.
    DATA current_item TYPE i.

    METHODS fill_stack IMPORTING instruction_line TYPE instructions=>col_data
                                 crates           TYPE stringtab
                                 stack            TYPE REF TO stack.

ENDCLASS.

CLASS stack_collection IMPLEMENTATION.

  METHOD constructor.
    me->instructions = instructions.
  ENDMETHOD.

  METHOD build.
    DATA(index_cols) = instructions->get_column_data( ).
    LOOP AT index_cols REFERENCE INTO DATA(line).
      DATA(stack) = NEW stack( CONV #( line->index ) ).
      fill_stack( instruction_line = line->*
                  crates = instructions->get_crates( )
                  stack = stack ).
      collection = VALUE #( BASE collection ( stack ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_stack.
    LOOP AT crates REFERENCE INTO DATA(crate_line).
      TRY.
          DATA(crate) = substring( val = crate_line->* off = instruction_line-offset_from len = instruction_line-length ).
          IF crate <> |   |.
            stack->put_first( VALUE #( ( crate ) ) ).
          ENDIF.
        CATCH cx_sy_range_out_of_bounds.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_stack_by_name.
    LOOP AT collection INTO DATA(line).
      IF line->get_name(  ) = condense( name ).
        result = line.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_stack.
    current_item = current_item + 1.
    TRY.
        result = collection[ current_item ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION NEW cx_stack_end( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS stack_arranger DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS prepare.

    METHODS process IMPORTING variant TYPE i.

    METHODS get_top_crates RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA input TYPE stringtab.
    DATA instructions TYPE REF TO instructions.
    DATA stack_collection TYPE REF TO stack_collection.
    DATA moves TYPE REF TO movement.

ENDCLASS.

CLASS stack_arranger IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD prepare.
    instructions = NEW #( input ).
    instructions->prepare_input( ).

    stack_collection = NEW #( instructions ).
    stack_collection->build( ).

    moves = NEW #( instructions->get_movements( ) ).
  ENDMETHOD.

  METHOD process.
    DO.
      TRY.
          DATA(move) = moves->get_move( ).
          DATA(source_stack) = stack_collection->get_stack_by_name( CONV #( move-from_stack ) ).
          DATA(destionation_stack) = stack_collection->get_stack_by_name( CONV #( move-to_stack ) ).
          IF variant = 1.
            DATA(crates) = source_stack->remove_single( move-crates ).
          ELSE.
            crates = source_stack->remove_in_bulk( move-crates ).
          ENDIF.
          destionation_stack->put( crates ).
        CATCH cx_movements_end.
          EXIT.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

  METHOD get_top_crates.
    DO.
      TRY.
          DATA(stack) = stack_collection->get_stack( ).
          result = |{ result }{ stack->get_top_crate( ) }|.
        CATCH cx_stack_end.
          RETURN.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS tc_instructions DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF col_data,
             index       TYPE i,
             offset_from TYPE i,
             length      TYPE i,
           END OF col_data.
    TYPES cols_data TYPE STANDARD TABLE OF col_data WITH EMPTY KEY.

    DATA cut TYPE REF TO instructions.

    METHODS setup.

    METHODS get_table_of_crates        FOR TESTING.
    METHODS get_table_of_movements     FOR TESTING.
    METHODS get_index_number_positions FOR TESTING.

ENDCLASS.

CLASS tc_instructions IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |    [D]| )
                                   ( |[N] [C]| )
                                   ( |[Z] [M] [P]| )
                                   ( | 1   2   3 | )
                                   ( || )
                                   ( |move 1 from 2 to 1| )
                                   ( |move 3 from 1 to 3| )
                                   ( |move 2 from 2 to 1| )
                                   ( |move 1 from 1 to 2| ) ).
    cut = NEW #( input ).
    cut->prepare_input( ).
  ENDMETHOD.

  METHOD get_table_of_crates.
    DATA(expected_values) = VALUE stringtab( ( |    [D]| )
                                             ( |[N] [C]| )
                                             ( |[Z] [M] [P]| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_crates( ) ).
  ENDMETHOD.

  METHOD get_table_of_movements.
    DATA(expected_values) = VALUE stringtab( ( |move 1 from 2 to 1| )
                                             ( |move 3 from 1 to 3| )
                                             ( |move 2 from 2 to 1| )
                                             ( |move 1 from 1 to 2| ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_movements( ) ).
  ENDMETHOD.

  METHOD get_index_number_positions.
    DATA(expected_values) = VALUE cols_data( ( index = 1 offset_from = 0 length = 3 )
                                             ( index = 2 offset_from = 4 length = 3 )
                                             ( index = 3 offset_from = 8 length = 3 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_column_data( )  ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_stack DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO stack.

    METHODS setup.

    METHODS get_name_of_stack           FOR TESTING.


    METHODS put_3_crates_on_stack       FOR TESTING.
    METHODS remove_3_crates_check_all   FOR TESTING.
    METHODS remove_3_crates_all_at_once FOR TESTING.
    METHODS get_top_crate               FOR TESTING.

ENDCLASS.

CLASS tc_stack IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( |TESTSTACK| ).
    cut->put( VALUE #( ( |[N]| ) ( |[Z]| ) ) ).
  ENDMETHOD.

  METHOD get_name_of_stack.
    cl_abap_unit_assert=>assert_equals( exp = |TESTSTACK| act = cut->get_name( ) ).
  ENDMETHOD.

  METHOD put_3_crates_on_stack.
    cut = NEW #( || ).
    DATA(expected_values) = VALUE stringtab( ( |[M]| )
                                             ( |[N]| )
                                             ( |[Z]| )
                                             ( |[P]| ) ).

    DATA(crates_to_put_on_stack) = VALUE stringtab( ( |[M]| )
                                                    ( |[N]| )
                                                    ( |[Z]| ) ).

    cut->put( VALUE #( ( |[P]| ) ) ).
    cut->put( crates_to_put_on_stack ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get( ) ).
  ENDMETHOD.

  METHOD remove_3_crates_check_all.
    cut = NEW #( || ).
    DATA(expected_removed_crates) = VALUE stringtab( ( |[N]| )
                                                     ( |[M]| ) ).

    DATA(expected_remaining_stack) = VALUE stringtab( ( |[Z]| )
                                                      ( |[P]| ) ).

    cut->put( VALUE #( ( |[M]| )
                       ( |[N]| )
                       ( |[Z]| )
                       ( |[P]| ) ) ).
    DATA(removed_crates) = cut->remove_single( 2 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_removed_crates  act = removed_crates  ).
    cl_abap_unit_assert=>assert_equals( exp = expected_remaining_stack act = cut->get( ) ).
  ENDMETHOD.

  METHOD remove_3_crates_all_at_once.
    cut = NEW #( || ).
    cut->put( VALUE #( ( |[M]| )
                       ( |[N]| )
                       ( |[Z]| )
                       ( |[P]| ) ) ).

    DATA(expected_removed_crates) = VALUE stringtab( ( |[M]| )
                                                     ( |[N]| ) ).

    DATA(expected_remaining_stack) = VALUE stringtab( ( |[Z]| )
                                                      ( |[P]| ) ).

    DATA(removed_crates) = cut->remove_in_bulk( 2 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_removed_crates  act = removed_crates  ).
    cl_abap_unit_assert=>assert_equals( exp = expected_remaining_stack act = cut->get( ) ).
  ENDMETHOD.

  METHOD get_top_crate.
    cl_abap_unit_assert=>assert_equals( exp = |N| act = cut->get_top_crate( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS tc_movement DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF move,
             crates     TYPE i,
             from_stack TYPE i,
             to_stack   TYPE i,
           END OF move.
    TYPES moves TYPE STANDARD TABLE OF move WITH EMPTY KEY.

    DATA cut TYPE REF TO movement.

    METHODS setup.
    METHODS get_table_of_movements         FOR TESTING.
    METHODS get_single_moves_from_iterator FOR TESTING.
    METHODS get_all_items_without_error    FOR TESTING.

ENDCLASS.

CLASS tc_movement IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |move 1 from 2 to 1| )
                                   ( |move 3 from 1 to 3| )
                                   ( |move 2 from 2 to 1| )
                                   ( |move 1 from 1 to 2| ) ).
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD get_table_of_movements.
    DATA(expected_values) = VALUE moves( ( crates = 1 from_stack = 2 to_stack = 1 )
                                         ( crates = 3 from_stack = 1 to_stack = 3 )
                                         ( crates = 2 from_stack = 2 to_stack = 1 )
                                         ( crates = 1 from_stack = 1 to_stack = 2 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_moves( )  ).
  ENDMETHOD.

  METHOD get_single_moves_from_iterator.
    DATA(expected_value) = VALUE move( crates = 2 from_stack = 2 to_stack = 1 ).
    cut->get_move( ).
    cut->get_move( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_value act = cut->get_move( ) ).
  ENDMETHOD.

  METHOD get_all_items_without_error.
    TRY.
        DO.
          cut->get_move( ).
        ENDDO.
      CATCH cx_movements_end INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_bound( act = lx_error msg = |The object should be bound!| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS tc_stack_collection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO stack_collection.
    DATA input TYPE stringtab.
    DATA instructions TYPE REF TO instructions.
    DATA stack_collection TYPE STANDARD TABLE OF REF TO stack WITH EMPTY KEY.

    METHODS setup.

    METHODS get_a_particular_stack_by_name FOR TESTING.
    METHODS build_stacks_with_crates       FOR TESTING.
    METHODS get_top_crate_from_each_stack  FOR TESTING.
ENDCLASS.

CLASS tc_stack_collection IMPLEMENTATION.

  METHOD setup.
    input = VALUE #( ( |    [D]| )
                     ( |[N] [C]| )
                     ( |[Z] [M] [P]| )
                     ( | 1   2   3 | )
                     ( || )
                     ( |move 1 from 2 to 1| )
                     ( |move 3 from 1 to 3| )
                     ( |move 2 from 2 to 1| )
                     ( |move 1 from 1 to 2| ) ).
    instructions = NEW #( input ).
    instructions->prepare_input( ).
    cut = NEW #( instructions ).
    cut->build( ).
  ENDMETHOD.

  METHOD get_a_particular_stack_by_name.
    DATA(stack) = cut->get_stack_by_name( |3| ).
    cl_abap_unit_assert=>assert_equals( exp = |3| act = stack->get_name( )  ).
  ENDMETHOD.

  METHOD build_stacks_with_crates.
    DATA(expected_values) = VALUE stringtab( ( |[D]| ) ( |[C]| ) ( |[M]| ) ).
    DATA(stack) = cut->get_stack_by_name( |2| ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = stack->get( ) ).
  ENDMETHOD.

  METHOD get_top_crate_from_each_stack.
    DO.
      TRY.
          cut->get_stack( ).

        CATCH cx_stack_end.
          EXIT.
      ENDTRY.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

CLASS tc_stack_rearrange DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO stack_arranger.

    DATA movements TYPE REF TO movement.
    DATA stack_coll TYPE REF TO stack_collection.

    METHODS setup.
    METHODS follow_intel_for_whole_stack   FOR TESTING.
    METHODS follow_new_intel_4_whole_stack FOR TESTING.

ENDCLASS.

CLASS tc_stack_rearrange IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |    [D]| )
                                   ( |[N] [C]| )
                                   ( |[Z] [M] [P]| )
                                   ( | 1   2   3 | )
                                   ( || )
                                   ( |move 1 from 2 to 1| )
                                   ( |move 3 from 1 to 3| )
                                   ( |move 2 from 2 to 1| )
                                   ( |move 1 from 1 to 2| ) ).
    cut = NEW #( input ).
    cut->prepare( ).

  ENDMETHOD.

  METHOD follow_intel_for_whole_stack.
    cut->process( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = |CMZ| act = cut->get_top_crates( ) ).
  ENDMETHOD.

  METHOD follow_new_intel_4_whole_stack.
    cut->process( 2 ).
    cl_abap_unit_assert=>assert_equals( exp = |MCD| act = cut->get_top_crates( )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).

  DATA(crate_arrenger1) = NEW stack_arranger( input ).
  crate_arrenger1->prepare( ).
  crate_arrenger1->process( 1 ).
  WRITE / |Solution part 1: { crate_arrenger1->get_top_crates( ) }|.

  DATA(crate_arrenger2) = NEW stack_arranger( input ).
  crate_arrenger2->prepare( ).
  crate_arrenger2->process( 2 ).
  WRITE / |Solution part 2: { crate_arrenger2->get_top_crates( ) }|.
