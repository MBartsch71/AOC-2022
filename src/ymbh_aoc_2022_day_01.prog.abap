REPORT ymbh_aoc_2022_day_01.

CLASS input_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_file_in_table RETURNING VALUE(result) TYPE stringtab.
ENDCLASS.

CLASS input_reader IMPLEMENTATION.

  METHOD read_file_in_table.
    DATA(file_reader) = NEW zcl_mbh_file_upload( |/Users/mbartsch71/github/AOC-2022/inputs/20221201 | ).
    result = file_reader->file_upload_in_stringtab( ).
  ENDMETHOD.

ENDCLASS.

CLASS calories_inventar DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS get_greatest_calorie_amount RETURNING VALUE(result) TYPE i.

    METHODS get_top_three_calories_sum RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    TYPES: BEGIN OF calorie,
             elf      TYPE i,
             calories TYPE i,
           END OF calorie.
    TYPES calories TYPE SORTED TABLE OF calorie WITH NON-UNIQUE KEY primary_key COMPONENTS calories.

    DATA elves_calories TYPE calories.

    METHODS sort_input IMPORTING input TYPE stringtab.

    METHODS save_current_calories_for_elf IMPORTING elf            TYPE i
                                                    calories_total TYPE i.
    METHODS separation_line IMPORTING line          TYPE REF TO data
                            RETURNING VALUE(result) TYPE abap_bool.

    METHODS next IMPORTING elf           TYPE i
                 RETURNING VALUE(result) TYPE i.

    METHODS reset_calorie_count RETURNING VALUE(result) TYPE i.

    METHODS add_item_calories IMPORTING current_total TYPE i
                                        new_item      TYPE REF TO data
                              RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS calories_inventar IMPLEMENTATION.

  METHOD constructor.
    sort_input( input ).
  ENDMETHOD.

  METHOD get_greatest_calorie_amount.
    DATA(last_line) = lines( elves_calories ).
    result = elves_calories[ last_line ]-calories.
  ENDMETHOD.

  METHOD sort_input.
    DATA elf TYPE i VALUE 1.
    DATA elf_calories TYPE i.

    LOOP AT input REFERENCE INTO DATA(line).
      IF separation_line( line ).
        save_current_calories_for_elf( elf = elf calories_total = elf_calories ).
        elf = next( elf ).
        elf_calories = reset_calorie_count( ).
        CONTINUE.
      ENDIF.
      elf_calories = add_item_calories( current_total = elf_calories new_item = line ).
    ENDLOOP.
    save_current_calories_for_elf( elf = elf calories_total = elf_calories ).

  ENDMETHOD.

  METHOD get_top_three_calories_sum.
    result = REDUCE #( INIT sum = 0
                       FOR i = lines( elves_calories ) - 2 THEN i + 1 WHILE i <= lines( elves_calories )
                       NEXT sum = sum + elves_calories[ i ]-calories  ).
  ENDMETHOD.

  METHOD save_current_calories_for_elf.
    INSERT VALUE calorie( elf = elf calories = calories_total ) INTO TABLE elves_calories.
  ENDMETHOD.

  METHOD separation_line.
    ASSIGN line->* TO FIELD-SYMBOL(<line>).
    result = xsdbool( <line> IS INITIAL ).
  ENDMETHOD.

  METHOD next.
    result = elf + 1.
  ENDMETHOD.

  METHOD reset_calorie_count.
    result = 0.
  ENDMETHOD.

  METHOD add_item_calories.
    ASSIGN new_item->* TO FIELD-SYMBOL(<item_calories>).
    result = current_total + <item_calories>.
  ENDMETHOD.

ENDCLASS.

CLASS tc_calories_sort DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO calories_inventar.

    METHODS setup.
    METHODS get_the_most_calorie_carrier FOR TESTING.
    METHODS get_total_calories_top_three FOR TESTING.

ENDCLASS.

CLASS tc_calories_sort IMPLEMENTATION.

  METHOD setup.
    DATA(input) = VALUE stringtab( ( |1000| )
                                   ( |2000| )
                                   ( |3000| )
                                   ( || )
                                   ( |4000| )
                                   ( || )
                                   ( |5000| )
                                   ( |6000| )
                                   ( || )
                                   ( |7000| )
                                   ( |8000| )
                                   ( |9000| )
                                   ( || )
                                   ( |10000| ) ).
    cut = NEW #( input ).
  ENDMETHOD.

  METHOD get_the_most_calorie_carrier.
    cl_abap_unit_assert=>assert_equals( exp = |24000| act = cut->get_greatest_calorie_amount( )  ).
  ENDMETHOD.

  METHOD get_total_calories_top_three.
    cl_abap_unit_assert=>assert_equals( exp = |45000| act = cut->get_top_three_calories_sum( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input) = NEW input_reader( )->read_file_in_table( ).
  DATA(calories) = NEW calories_inventar( input ).

  WRITE / |The most calories carried: { calories->get_greatest_calorie_amount( ) }|.
  WRITE / |How many calories the top three elves carrying: { calories->get_top_three_calories_sum( ) }|.
